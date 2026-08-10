use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Attribute, FnArg, GenericArgument, Ident, ImplItem, ImplItemFn, ItemImpl, LitStr,
    PathArguments, ReturnType, Token, Type, Visibility,
    parse::{Parse, ParseStream},
    spanned::Spanned,
};

use crate::boundary::{
    BoundaryParam, BoundaryReturn, ExternTypeExpr, OwnerReturn, classify_init_param,
    classify_param, classify_provider_return_for_owner, flow_tokens, init_return_matches,
    mut_place_type_arg, owned_named_param, param_abi_for_override, param_abi_tokens,
    param_escape_tokens, receiver_abi_tokens, return_abi_for_override, return_abi_tokens,
    type_expr_tokens, type_with_override, validate_callable_signature,
    validate_callback_wrapper_precheck, validate_ctx_param, validate_mut_place_ctx,
};

pub fn expand(attr: TokenStream, item: TokenStream) -> TokenStream {
    match expand_inner(attr, item) {
        Ok(tokens) => tokens,
        Err(err) => err.to_compile_error(),
    }
}

fn expand_inner(attr: TokenStream, item: TokenStream) -> syn::Result<TokenStream> {
    let args: MethodsArgs = syn::parse2(attr)?;
    let imp: ItemImpl = syn::parse2(item)?;
    if imp.trait_.is_some() {
        return Err(syn::Error::new_spanned(
            &imp.self_ty,
            "#[methods] does not support trait impls",
        ));
    }
    let Type::Path(owner_path) = imp.self_ty.as_ref() else {
        return Err(syn::Error::new_spanned(
            &imp.self_ty,
            "#[methods] requires a concrete type path",
        ));
    };
    if owner_path.path.segments.len() != 1 {
        return Err(syn::Error::new_spanned(
            owner_path,
            "#[methods] requires an unqualified type name",
        ));
    }
    let owner_segment = &owner_path.path.segments[0];
    let owner = owner_segment.ident.clone();
    let rust_type_path = quote! { concat!(module_path!(), "::", stringify!(#owner)) };
    let companion = crate::naming::methods_fn_ident(&owner);
    let export_name = args.name.unwrap_or_else(|| owner.to_string());
    let mut plans = vec![];
    for item in &imp.items {
        let ImplItem::Fn(method) = item else {
            continue;
        };
        if !is_public(&method.vis) {
            if method
                .attrs
                .iter()
                .any(|attr| attr.path().is_ident("anvyx"))
            {
                return Err(syn::Error::new_spanned(
                    &method.vis,
                    "#[methods] only exports public methods",
                ));
            }
            continue;
        }
        plans.push(MethodPlan::build(&owner, &export_name, method)?);
    }

    let mut descriptor_methods = vec![];
    let mut descriptor_statics = vec![];
    let mut descriptor_operators = vec![];
    let mut init = None;
    let mut bindings = vec![];
    let mut wrappers = vec![];
    for plan in &plans {
        match &plan.export.role {
            Role::Method(_) | Role::PlaceMethod => descriptor_methods.push(method_descriptor(plan)),
            Role::Static => descriptor_statics.push(static_descriptor(plan)),
            Role::Init => {
                if init.is_some() {
                    return Err(syn::Error::new_spanned(
                        plan.method,
                        "duplicate #[anvyx(init)]",
                    ));
                }
                init = Some(init_descriptor(plan));
            }
            Role::Getter | Role::Setter => {}
            Role::Operator(_) => descriptor_operators.push(operator_descriptor(plan)),
        }
        bindings.push(member_binding(plan));
        wrappers.push(native_wrapper(plan));
    }
    let descriptor_fields = computed_property_descriptors(&plans)?;
    drop(plans);
    let mut imp = imp;
    for item in &mut imp.items {
        if let ImplItem::Fn(method) = item {
            method.attrs.retain(|attr| !attr.path().is_ident("anvyx"));
        }
    }
    let init = init.map_or_else(|| quote! { None }, |init| quote! { Some(#init) });

    let native_mod = methods_native_module_ident(&owner);

    Ok(quote! {
        #imp

        #[doc(hidden)]
        pub mod #native_mod {
            use super::*;
            #(#wrappers)*
        }

        #[doc(hidden)]
        pub fn #companion() -> anvyx_runtime::TypeMemberFragment {
            anvyx_runtime::TypeMemberFragment {
                name: #export_name.to_string(),
                fields: vec![#(#descriptor_fields),*],
                init: #init,
                methods: vec![#(#descriptor_methods),*],
                statics: vec![#(#descriptor_statics),*],
                operators: vec![#(#descriptor_operators),*],
                bindings: vec![#(#bindings),*],
            }
        }

        anvyx_runtime::inventory::submit! {
            anvyx_runtime::TypeMemberExport {
                rust_type_path: #rust_type_path,
                export: #companion,
            }
        }
    })
}

#[derive(Default)]
struct MethodsArgs {
    name: Option<String>,
}

impl Parse for MethodsArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut args = Self::default();
        while !input.is_empty() {
            let key: Ident = input.parse()?;
            let _: Token![=] = input.parse()?;
            match key.to_string().as_str() {
                "name" => {
                    if args.name.is_some() {
                        return Err(syn::Error::new_spanned(
                            key,
                            "duplicate #[methods] key `name`",
                        ));
                    }
                    args.name = Some(input.parse::<LitStr>()?.value());
                }
                _ => {
                    return Err(syn::Error::new_spanned(
                        key,
                        "expected #[methods(name = ...)]",
                    ));
                }
            }
            if !input.is_empty() {
                let _: Token![,] = input.parse()?;
            }
        }
        Ok(args)
    }
}

struct MethodExport<'a> {
    role: Role,
    selector: TokenStream,
    operation: TokenStream,
    ret_override: Option<String>,
    param_overrides: HashMap<String, String>,
    ctx: bool,
    receiver: Option<Receiver>,
    source_receiver: Option<Receiver>,
    visible_params: Vec<&'a syn::PatType>,
    setter_field: Option<String>,
}

#[derive(Clone)]
enum Role {
    Method(Receiver),
    PlaceMethod,
    Static,
    Init,
    Getter,
    Setter,
    Operator(OperatorRole),
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Receiver {
    Shared,
    Mutable,
}

impl<'a> MethodExport<'a> {
    fn parse(owner: &Ident, method: &'a ImplItemFn) -> syn::Result<Self> {
        let attrs = MethodAttrs::parse(&method.attrs)?;
        let rust_receiver = receiver(&method.sig.inputs)?;
        let visible_params = visible_typed_params(method, rust_receiver, attrs.ctx);
        let role = match (attrs.role.clone(), rust_receiver) {
            (Some(role), _) => role,
            (None, Some(receiver)) => Role::Method(receiver),
            (None, None) => Role::Static,
        };
        validate_role(owner, method, &role, rust_receiver, &visible_params, &attrs)?;
        let receiver = match role {
            Role::Method(receiver) => Some(receiver),
            Role::PlaceMethod => Some(Receiver::Mutable),
            Role::Getter | Role::Setter | Role::Operator(_) => {
                Some(rust_receiver.expect("validated receiver"))
            }
            Role::Static | Role::Init => None,
        };
        let setter_field = matches!(role, Role::Setter)
            .then(|| setter_field_name(method))
            .transpose()?;
        let name = method.sig.ident.to_string();
        let selector = match &role {
            Role::Method(_) | Role::PlaceMethod => {
                quote! { anvyx_runtime::ExternMemberSelector::Method(#name.to_string()) }
            }
            Role::Static => {
                quote! { anvyx_runtime::ExternMemberSelector::Static(#name.to_string()) }
            }
            Role::Init => quote! { anvyx_runtime::ExternMemberSelector::Init },
            Role::Getter => {
                quote! { anvyx_runtime::ExternMemberSelector::Field(#name.to_string()) }
            }
            Role::Setter => {
                let field = setter_field.as_ref().expect("setter field was planned");
                quote! { anvyx_runtime::ExternMemberSelector::Field(#field.to_string()) }
            }
            Role::Operator(op) => {
                let op = &op.tokens;
                quote! { anvyx_runtime::ExternMemberSelector::Operator(#op) }
            }
        };
        let operation = match &role {
            Role::Getter => quote! { anvyx_runtime::ExternBindingOp::Get },
            Role::Setter => quote! { anvyx_runtime::ExternBindingOp::Set },
            _ => quote! { anvyx_runtime::ExternBindingOp::Call },
        };
        Ok(Self {
            role,
            selector,
            operation,
            ret_override: attrs.ret,
            param_overrides: attrs.params,
            ctx: attrs.ctx,
            receiver,
            source_receiver: rust_receiver,
            visible_params,
            setter_field,
        })
    }
}

struct MethodPlan<'a> {
    owner: &'a Ident,
    method: &'a ImplItemFn,
    export: MethodExport<'a>,
    value_params: Vec<&'a syn::PatType>,
    params: Vec<BoundaryParam>,
    ret: BoundaryReturn,
    effects: TokenStream,
    runtime_arg: bool,
}

impl<'a> MethodPlan<'a> {
    fn build(owner: &'a Ident, export_name: &str, method: &'a ImplItemFn) -> syn::Result<Self> {
        let export = MethodExport::parse(owner, method)?;
        validate_callable_signature(&method.sig, "#[methods]", "method", export.ctx)?;
        let value_params = method_value_params(&export);
        let params = classify_method_params(method, &export, &value_params, export_name)?;
        let params = apply_param_overrides(method, &export, params)?;
        let ret = classify_method_return(method, &export, owner, export_name)?;
        if matches!(export.role, Role::Init) && matches!(method.sig.output, ReturnType::Default) {
            return Err(syn::Error::new_spanned(
                &method.sig,
                "#[anvyx(init)] must return Self",
            ));
        }
        if matches!(export.role, Role::Init) && !init_return_matches(&ret, export_name) {
            return Err(syn::Error::new_spanned(
                &method.sig.output,
                "#[anvyx(init)] must return Self, RuntimeResult<Self>, or Result<Self, E>",
            ));
        }
        let runtime_arg = export.ctx
            || matches!(export.role, Role::Init)
            || matches!(&export.role, Role::Operator(op) if op.rhs_self);
        let fallible = ret.fallible;
        Ok(Self {
            owner,
            method,
            export,
            value_params,
            params,
            ret,
            effects: quote! { anvyx_runtime::ExternEffects { fallible: #fallible } },
            runtime_arg,
        })
    }
}

fn is_public(vis: &Visibility) -> bool {
    matches!(vis, Visibility::Public(_))
}

fn validate_role(
    owner: &Ident,
    method: &ImplItemFn,
    role: &Role,
    receiver: Option<Receiver>,
    visible_params: &[&syn::PatType],
    attrs: &MethodAttrs,
) -> syn::Result<()> {
    validate_ctx_position(&method.sig.inputs, receiver, attrs.ctx)?;
    match role {
        Role::Method(_) if receiver.is_none() => Err(syn::Error::new_spanned(
            &method.sig.ident,
            "methods require &self or &mut self",
        )),
        Role::Method(Receiver::Mutable) if attrs.ctx => Err(syn::Error::new_spanned(
            &method.sig,
            "mutable methods with runtime context require #[anvyx(place, ctx)]",
        )),
        Role::PlaceMethod => validate_place_method(method, attrs, receiver, visible_params),
        Role::Init => {
            if attrs.ret.is_some() {
                return Err(syn::Error::new_spanned(
                    &method.sig,
                    "#[anvyx(init)] does not support ret overrides",
                ));
            }
            if receiver.is_some() {
                return Err(syn::Error::new_spanned(
                    &method.sig,
                    "#[anvyx(init)] must be an associated function",
                ));
            }
            Ok(())
        }
        Role::Getter if receiver != Some(Receiver::Shared) => Err(syn::Error::new_spanned(
            &method.sig,
            "#[anvyx(getter)] requires &self",
        )),
        Role::Getter if !attrs.params.is_empty() => Err(syn::Error::new_spanned(
            &method.sig,
            "#[anvyx(getter)] cannot have parameter overrides",
        )),
        Role::Getter if !visible_params.is_empty() => Err(syn::Error::new_spanned(
            &method.sig,
            "#[anvyx(getter)] cannot take value parameters",
        )),
        Role::Setter if attrs.ret.is_some() => Err(syn::Error::new_spanned(
            &method.sig,
            "#[anvyx(setter)] does not support ret overrides",
        )),
        Role::Setter if !matches!(method.sig.output, ReturnType::Default) => Err(
            syn::Error::new_spanned(&method.sig.output, "computed setters cannot return a value"),
        ),
        Role::Setter if receiver != Some(Receiver::Mutable) => Err(syn::Error::new_spanned(
            &method.sig,
            "#[anvyx(setter)] requires &mut self",
        )),
        Role::Operator(_) if receiver != Some(Receiver::Shared) => Err(syn::Error::new_spanned(
            &method.sig,
            "#[anvyx(op(...))] requires &self",
        )),
        Role::Operator(op) => {
            if op.rhs_self && (!attrs.params.is_empty() || attrs.ret.is_some()) {
                return Err(syn::Error::new_spanned(
                    &method.sig,
                    "Self operators do not support ret/params overrides",
                ));
            }
            validate_operator_signature(owner, method, op, visible_params)
        }
        Role::Static if receiver.is_some() => Err(syn::Error::new_spanned(
            &method.sig,
            "static extern members cannot take self",
        )),
        _ => Ok(()),
    }
}

fn validate_place_method(
    method: &ImplItemFn,
    attrs: &MethodAttrs,
    receiver: Option<Receiver>,
    params: &[&syn::PatType],
) -> syn::Result<()> {
    if receiver.is_some() {
        return Err(syn::Error::new_spanned(
            &method.sig,
            "#[anvyx(place)] must be an associated function",
        ));
    }
    if !attrs.ctx {
        return Err(syn::Error::new_spanned(
            &method.sig,
            "#[anvyx(place)] requires #[anvyx(ctx)]",
        ));
    }
    let Some(receiver) = params.first() else {
        return Err(syn::Error::new_spanned(
            &method.sig,
            "#[anvyx(place)] requires `receiver: MutPlace<'_, 'cx, Self>`",
        ));
    };
    let Type::Path(path) = receiver.ty.as_ref() else {
        return Err(syn::Error::new_spanned(
            &receiver.ty,
            "place receiver must have type MutPlace<'_, 'cx, Self>",
        ));
    };
    let Some(payload) = mut_place_type_arg(path)? else {
        return Err(syn::Error::new_spanned(
            &receiver.ty,
            "place receiver must have type MutPlace<'_, 'cx, Self>",
        ));
    };
    let Type::Path(payload) = payload else {
        return Err(syn::Error::new_spanned(
            payload,
            "place receiver payload must be Self",
        ));
    };
    let valid = payload.qself.is_none()
        && payload.path.segments.len() == 1
        && payload.path.segments[0].ident == "Self"
        && matches!(payload.path.segments[0].arguments, PathArguments::None);
    if !valid {
        return Err(syn::Error::new_spanned(
            payload,
            "place receiver payload must be Self",
        ));
    }
    Ok(())
}

#[derive(Default)]
struct MethodAttrs {
    role: Option<Role>,
    ret: Option<String>,
    params: HashMap<String, String>,
    ctx: bool,
}

impl MethodAttrs {
    fn parse(attrs: &[Attribute]) -> syn::Result<Self> {
        let mut parsed = Self::default();
        for attr in attrs.iter().filter(|attr| attr.path().is_ident("anvyx")) {
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("place") {
                    parsed.set_role(meta.error("duplicate #[anvyx(...)] role"), Role::PlaceMethod)
                } else if meta.path.is_ident("init") {
                    parsed.set_role(meta.error("duplicate #[anvyx(...)] role"), Role::Init)
                } else if meta.path.is_ident("getter") {
                    parsed.set_role(meta.error("duplicate #[anvyx(...)] role"), Role::Getter)
                } else if meta.path.is_ident("setter") {
                    parsed.set_role(meta.error("duplicate #[anvyx(...)] role"), Role::Setter)
                } else if meta.path.is_ident("op") {
                    let content;
                    syn::parenthesized!(content in meta.input);
                    let op = parse_operator(content.parse()?)?;
                    parsed.set_role(
                        meta.error("duplicate #[anvyx(...)] role"),
                        Role::Operator(op),
                    )
                } else if meta.path.is_ident("ctx") {
                    if parsed.ctx {
                        return Err(meta.error("duplicate #[anvyx(ctx)]"));
                    }
                    parsed.ctx = true;
                    Ok(())
                } else if meta.path.is_ident("trap") {
                    Err(meta.error("#[anvyx(trap)] was replaced by returning RuntimeResult<T>"))
                } else if meta.path.is_ident("ret") {
                    if parsed.ret.is_some() {
                        return Err(meta.error("duplicate #[anvyx(ret = ...)]"));
                    }
                    let value = meta.value()?;
                    parsed.ret = Some(value.parse::<LitStr>()?.value());
                    Ok(())
                } else if meta.path.is_ident("params") {
                    let content;
                    syn::parenthesized!(content in meta.input);
                    while !content.is_empty() {
                        let param: Ident = content.parse()?;
                        let _: Token![=] = content.parse()?;
                        let ty: LitStr = content.parse()?;
                        if parsed
                            .params
                            .insert(param.to_string(), ty.value())
                            .is_some()
                        {
                            return Err(syn::Error::new_spanned(
                                param,
                                "duplicate parameter override",
                            ));
                        }
                        if !content.is_empty() {
                            let _: Token![,] = content.parse()?;
                        }
                    }
                    Ok(())
                } else {
                    Err(meta.error(
                        "expected place, init, getter, setter, ctx, op(...), ret = ..., or params(...)",
                    ))
                }
            })?;
        }
        Ok(parsed)
    }

    fn set_role(&mut self, err: syn::Error, role: Role) -> syn::Result<()> {
        if self.role.is_some() {
            return Err(err);
        }
        self.role = Some(role);
        Ok(())
    }
}

#[derive(Clone)]
struct OperatorRole {
    tokens: TokenStream,
    rhs_self: bool,
}

fn parse_operator(tokens: TokenStream) -> syn::Result<OperatorRole> {
    let text = tokens.to_string().replace(' ', "");
    let (op, self_on_right, rhs_self) = if let Some(rest) = text.strip_prefix("Self") {
        let rhs_self = rest.ends_with("Self");
        (operator_between_operands(rest, rhs_self), false, rhs_self)
    } else if let Some(rest) = text.strip_suffix("Self") {
        (operator_after_left_operand(rest), true, false)
    } else {
        return Err(syn::Error::new_spanned(
            tokens,
            "operator must mention Self",
        ));
    };
    let op = binary_op_tokens(&op)
        .ok_or_else(|| syn::Error::new_spanned(tokens, "unsupported operator"))?;
    Ok(OperatorRole {
        tokens: quote! { anvyx_runtime::ExternOperator::Binary { op: #op, self_on_right: #self_on_right } },
        rhs_self,
    })
}

fn operator_between_operands(rest: &str, rhs_self: bool) -> String {
    let rest = if rhs_self {
        rest.strip_suffix("Self").unwrap_or(rest)
    } else {
        rest
    };
    rest.chars()
        .take_while(|ch| !ch.is_ascii_alphanumeric() && *ch != '_')
        .collect()
}

fn operator_after_left_operand(rest: &str) -> String {
    rest.chars()
        .rev()
        .take_while(|ch| !ch.is_ascii_alphanumeric() && *ch != '_')
        .collect::<String>()
        .chars()
        .rev()
        .collect()
}

fn binary_op_tokens(op: &str) -> Option<TokenStream> {
    Some(match op {
        "+" => quote! { anvyx_runtime::BinaryOp::Add },
        "-" => quote! { anvyx_runtime::BinaryOp::Sub },
        "*" => quote! { anvyx_runtime::BinaryOp::Mul },
        "/" => quote! { anvyx_runtime::BinaryOp::Div },
        "%" => quote! { anvyx_runtime::BinaryOp::Rem },
        "==" => quote! { anvyx_runtime::BinaryOp::Eq },
        "!=" => quote! { anvyx_runtime::BinaryOp::NotEq },
        "<" => quote! { anvyx_runtime::BinaryOp::LessThan },
        ">" => quote! { anvyx_runtime::BinaryOp::GreaterThan },
        "<=" => quote! { anvyx_runtime::BinaryOp::LessThanEq },
        ">=" => quote! { anvyx_runtime::BinaryOp::GreaterThanEq },
        _ => return None,
    })
}

fn validate_operator_signature(
    owner: &Ident,
    method: &ImplItemFn,
    op: &OperatorRole,
    typed_params: &[&syn::PatType],
) -> syn::Result<()> {
    if !op.rhs_self {
        return Ok(());
    }
    let [param] = typed_params else {
        return Err(syn::Error::new_spanned(
            &method.sig,
            "Self binary operators require exactly one Self operand",
        ));
    };
    let Type::Path(path) = param.ty.as_ref() else {
        return Err(syn::Error::new_spanned(
            &param.ty,
            "Self binary operator operand must be Self",
        ));
    };
    let name = &path.path.segments[0].ident;
    let is_self = path.path.segments.len() == 1 && (name == "Self" || name == owner);
    if !is_self {
        return Err(syn::Error::new_spanned(
            &param.ty,
            "Self binary operator operand must be Self",
        ));
    }
    Ok(())
}

fn receiver(
    inputs: &syn::punctuated::Punctuated<FnArg, syn::token::Comma>,
) -> syn::Result<Option<Receiver>> {
    let Some(first) = inputs.first() else {
        return Ok(None);
    };
    match first {
        FnArg::Receiver(receiver)
            if receiver.reference.is_some() && receiver.mutability.is_none() =>
        {
            Ok(Some(Receiver::Shared))
        }
        FnArg::Receiver(receiver)
            if receiver.reference.is_some() && receiver.mutability.is_some() =>
        {
            Ok(Some(Receiver::Mutable))
        }
        FnArg::Receiver(_) => Err(syn::Error::new_spanned(
            first,
            "by-value self is not supported",
        )),
        FnArg::Typed(_) => Ok(None),
    }
}

fn validate_ctx_position(
    inputs: &syn::punctuated::Punctuated<FnArg, syn::token::Comma>,
    receiver: Option<Receiver>,
    needs_ctx: bool,
) -> syn::Result<()> {
    if !needs_ctx {
        return Ok(());
    }
    let index = usize::from(receiver.is_some());
    let Some(arg) = inputs.iter().nth(index) else {
        return Err(syn::Error::new(
            inputs.span(),
            "#[anvyx(ctx)] requires a `ctx` parameter",
        ));
    };
    let FnArg::Typed(param) = arg else {
        return Err(syn::Error::new_spanned(
            arg,
            "#[anvyx(ctx)] requires a `ctx` parameter",
        ));
    };
    validate_ctx_param(param, "#[anvyx(ctx)]")
}

fn visible_typed_params(
    method: &ImplItemFn,
    receiver: Option<Receiver>,
    skip_ctx: bool,
) -> Vec<&syn::PatType> {
    let ctx_index = skip_ctx.then_some(usize::from(receiver.is_some()));
    method
        .sig
        .inputs
        .iter()
        .enumerate()
        .filter_map(|(index, arg)| match arg {
            FnArg::Typed(param) if Some(index) != ctx_index => Some(param),
            FnArg::Typed(_) | FnArg::Receiver(_) => None,
        })
        .collect()
}

fn method_value_params<'a>(export: &MethodExport<'a>) -> Vec<&'a syn::PatType> {
    let mut params = export.visible_params.clone();
    if matches!(export.role, Role::PlaceMethod) {
        params.remove(0);
    }
    params
}

fn method_descriptor(plan: &MethodPlan) -> TokenStream {
    let name = plan.method.sig.ident.to_string();
    let doc = doc_tokens(&plan.method.attrs);
    let receiver = receiver_tokens(plan.export.receiver.expect("method receiver was planned"));
    let signature = signature_tokens(plan);
    let effects = &plan.effects;
    quote! {
        anvyx_runtime::ExternMethodDescriptor {
            name: #name.to_string(),
            doc: #doc,
            receiver: #receiver,
            signature: #signature,
            effects: #effects,
        }
    }
}

fn static_descriptor(plan: &MethodPlan) -> TokenStream {
    let name = plan.method.sig.ident.to_string();
    let doc = doc_tokens(&plan.method.attrs);
    let signature = signature_tokens(plan);
    let effects = &plan.effects;
    quote! {
        anvyx_runtime::ExternStaticDescriptor {
            name: #name.to_string(),
            doc: #doc,
            signature: #signature,
            effects: #effects,
        }
    }
}

fn init_descriptor(plan: &MethodPlan) -> TokenStream {
    let field_init = plan
        .params
        .iter()
        .filter(|param| !param.init_presence)
        .map(|param| {
            let name = &param.name;
            quote! { #name.to_string() }
        });
    let presence_init = plan
        .params
        .iter()
        .filter(|param| param.init_presence)
        .map(|param| {
            let name = &param.name;
            quote! { #name.to_string() }
        });
    let params = extern_params(&plan.params);
    let ret = type_expr_tokens(&plan.ret.ty);
    let fallible = plan.ret.fallible;
    quote! {
        anvyx_runtime::ExternInitDescriptor {
            params: vec![#(#params),*],
            field_init: vec![#(#field_init),*],
            presence_init: vec![#(#presence_init),*],
            ret: #ret,
            effects: anvyx_runtime::ExternEffects { fallible: #fallible },
        }
    }
}

struct ComputedGetter<'a> {
    plan: &'a MethodPlan<'a>,
    ty_tokens: TokenStream,
}

struct ComputedSetter<'a> {
    plan: &'a MethodPlan<'a>,
}

fn computed_property_descriptors(plans: &[MethodPlan]) -> syn::Result<Vec<TokenStream>> {
    let mut getters = HashMap::<String, ComputedGetter>::new();
    let mut setters = HashMap::<String, ComputedSetter>::new();
    let mut order = vec![];
    for plan in plans {
        match &plan.export.role {
            Role::Getter => {
                if plan.ret.fallible {
                    return Err(syn::Error::new_spanned(
                        &plan.method.sig.output,
                        "computed getters cannot return RuntimeResult<T>",
                    ));
                }
                if matches!(plan.method.sig.output, ReturnType::Default) {
                    return Err(syn::Error::new_spanned(
                        &plan.method.sig,
                        "computed getters cannot be void",
                    ));
                }
                let name = plan.method.sig.ident.to_string();
                let ty_tokens = type_expr_tokens(&plan.ret.ty);
                if getters
                    .insert(name.clone(), ComputedGetter { plan, ty_tokens })
                    .is_some()
                {
                    return Err(syn::Error::new_spanned(
                        plan.method,
                        "duplicate computed getter",
                    ));
                }
                order.push(name);
            }
            Role::Setter => {
                let field = plan
                    .export
                    .setter_field
                    .as_ref()
                    .expect("setter field was planned")
                    .clone();
                if plan.params.len() != 1 {
                    return Err(syn::Error::new_spanned(
                        &plan.method.sig,
                        "computed setters require exactly one value parameter",
                    ));
                }
                if setters.insert(field, ComputedSetter { plan }).is_some() {
                    return Err(syn::Error::new_spanned(
                        plan.method,
                        "duplicate computed setter",
                    ));
                }
            }
            _ => {}
        }
    }
    for (field, setter) in &setters {
        if !getters.contains_key(field) {
            return Err(syn::Error::new_spanned(
                setter.plan.method,
                format!("computed setter `set_{field}` requires matching getter `{field}`"),
            ));
        }
    }
    order
        .into_iter()
        .map(|field| {
            let getter = getters.get(&field).expect("ordered getter exists");
            let setter = setters.get(&field).ok_or_else(|| {
                syn::Error::new_spanned(
                    getter.plan.method,
                    format!("computed getter `{field}` requires matching setter `set_{field}`"),
                )
            })?;
            if getter.plan.ret.ty != setter.plan.params[0].ty {
                return Err(syn::Error::new_spanned(
                    getter.plan.method,
                    format!("computed property `{field}` getter/setter types differ"),
                ));
            }
            Ok(computed_field(
                &getter.plan.method.attrs,
                &field,
                &getter.ty_tokens,
            ))
        })
        .collect()
}

fn setter_field_name(method: &ImplItemFn) -> syn::Result<String> {
    method
        .sig
        .ident
        .to_string()
        .strip_prefix("set_")
        .filter(|field| !field.is_empty())
        .map(str::to_string)
        .ok_or_else(|| {
            syn::Error::new_spanned(method, "computed setter names must start with `set_`")
        })
}

fn computed_field(attrs: &[Attribute], name: &str, ty: &TokenStream) -> TokenStream {
    let doc = doc_tokens(attrs);
    quote! {
        anvyx_runtime::ExternFieldDescriptor {
            name: #name.to_string(),
            ty: #ty,
            computed: true,
            readable: true,
            writable: true,
            get_receiver: anvyx_runtime::ReceiverMode::Shared,
            set_receiver: anvyx_runtime::ReceiverMode::Mutable,
            doc: #doc,
        }
    }
}

fn operator_descriptor(plan: &MethodPlan) -> TokenStream {
    let signature = signature_tokens(plan);
    let effects = &plan.effects;
    let Role::Operator(op) = &plan.export.role else {
        unreachable!("operator descriptor requires operator plan");
    };
    let op = &op.tokens;
    quote! {
        anvyx_runtime::ExternOperatorDescriptor {
            op: #op,
            receiver: anvyx_runtime::ReceiverMode::Shared,
            signature: #signature,
            effects: #effects,
        }
    }
}

fn signature_tokens(plan: &MethodPlan) -> TokenStream {
    let ret = type_expr_tokens(&plan.ret.ty);
    let params = extern_params(&plan.params);
    quote! { anvyx_runtime::ExternSignature { params: vec![#(#params),*], ret: #ret } }
}

fn extern_params(params: &[BoundaryParam]) -> impl Iterator<Item = TokenStream> + '_ {
    params.iter().map(|param| {
        let ty = type_expr_tokens(&param.ty);
        let flow = flow_tokens(param.flow);
        let escape = param_escape_tokens(param);
        let name = &param.name;
        quote! {
            anvyx_runtime::ExternParam {
                name: Some(#name.to_string()),
                ty: #ty,
                flow: #flow,
                escape: #escape,
            }
        }
    })
}

fn native_wrapper(plan: &MethodPlan) -> TokenStream {
    let owner = plan.owner;
    let method = plan.method;
    let ident = &method.sig.ident;
    let ctx = quote! { ctx };
    let inputs = wrapper_inputs(plan, &ctx);
    let output = wrapper_output(plan);
    let args = plan
        .value_params
        .iter()
        .map(|param| &param.pat)
        .collect::<Vec<_>>();
    let call_args = if plan.export.ctx {
        quote! { ctx, #(#args),* }
    } else {
        quote! { #(#args),* }
    };
    let call = match plan.export.role {
        Role::Method(_) | Role::Getter | Role::Setter | Role::Operator(_) => {
            quote! { receiver.#ident(#call_args) }
        }
        Role::PlaceMethod => quote! { super::#owner::#ident(ctx, receiver, #(#args),*) },
        Role::Static | Role::Init => quote! { super::#owner::#ident(#call_args) },
    };
    quote! {
        pub fn #ident<'cx>(#inputs) #output {
            #call
        }
    }
}

fn wrapper_inputs(plan: &MethodPlan, ctx: &TokenStream) -> TokenStream {
    let owner = plan.owner;
    let params = plan.value_params.iter().map(|param| {
        let attrs = &param.attrs;
        let pat = &param.pat;
        let colon = &param.colon_token;
        let ty = wrapper_type(owner, &param.ty);
        quote! { #(#attrs)* #pat #colon #ty }
    });
    match &plan.export.role {
        Role::Method(_) | Role::Getter | Role::Operator(_) | Role::Setter => {
            let receiver = if plan.export.receiver == Some(Receiver::Mutable) {
                quote! { receiver: &mut super::#owner }
            } else {
                quote! { receiver: &super::#owner }
            };
            if plan.runtime_arg {
                quote! { #ctx: &mut anvyx_runtime::Ctx<'cx, '_>, #receiver, #(#params),* }
            } else {
                quote! { #receiver, #(#params),* }
            }
        }
        Role::PlaceMethod => {
            let receiver = quote! {
                receiver: anvyx_runtime::MutPlace<'_, 'cx, super::#owner>
            };
            quote! { #ctx: &mut anvyx_runtime::Ctx<'cx, '_>, #receiver, #(#params),* }
        }
        Role::Static | Role::Init => {
            if plan.runtime_arg {
                quote! { #ctx: &mut anvyx_runtime::Ctx<'cx, '_>, #(#params),* }
            } else {
                quote! { #(#params),* }
            }
        }
    }
}

fn wrapper_output(plan: &MethodPlan) -> TokenStream {
    match &plan.method.sig.output {
        ReturnType::Default => quote! {},
        ReturnType::Type(arrow, ty) => {
            let ty = wrapper_type(plan.owner, ty);
            quote! { #arrow #ty }
        }
    }
}

fn wrapper_type(owner: &Ident, ty: &Type) -> TokenStream {
    match ty {
        Type::Path(path) if path.qself.is_none() => {
            if path.path.segments.len() == 1
                && path.path.segments[0].ident == "Self"
                && matches!(path.path.segments[0].arguments, PathArguments::None)
            {
                return quote! { super::#owner };
            }
            let leading = path.path.leading_colon.map(|colon| quote! { #colon });
            let segments = path.path.segments.iter().map(|segment| {
                let ident = &segment.ident;
                let args = match &segment.arguments {
                    PathArguments::None => quote! {},
                    PathArguments::AngleBracketed(args) => {
                        let colon = args.colon2_token.map(|colon| quote! { #colon });
                        let args = args.args.iter().map(|arg| match arg {
                            GenericArgument::Type(ty) => wrapper_type(owner, ty),
                            _ => quote! { #arg },
                        });
                        quote! { #colon <#(#args),*> }
                    }
                    PathArguments::Parenthesized(args) => quote! { #args },
                };
                quote! { #ident #args }
            });
            quote! { #leading #(#segments)::* }
        }
        Type::Reference(reference) => {
            let and = &reference.and_token;
            let lifetime = &reference.lifetime;
            let mutability = &reference.mutability;
            let elem = wrapper_type(owner, &reference.elem);
            quote! { #and #lifetime #mutability #elem }
        }
        Type::Tuple(tuple) => {
            let elems = tuple.elems.iter().map(|elem| wrapper_type(owner, elem));
            if tuple.elems.len() == 1 {
                quote! { (#(#elems),*,) }
            } else {
                quote! { (#(#elems),*) }
            }
        }
        Type::Array(array) => {
            let elem = wrapper_type(owner, &array.elem);
            let len = &array.len;
            quote! { [#elem; #len] }
        }
        _ => quote! { #ty },
    }
}

fn classify_method_params(
    method: &ImplItemFn,
    export: &MethodExport,
    value_params: &[&syn::PatType],
    export_name: &str,
) -> syn::Result<Vec<BoundaryParam>> {
    if matches!(&export.role, Role::Operator(op) if op.rhs_self) {
        let param = value_params
            .first()
            .expect("validated Self operator parameter");
        let syn::Pat::Ident(name) = param.pat.as_ref() else {
            unreachable!("validated Self operator parameter is an identifier");
        };
        let ty = ExternTypeExpr::Named {
            module: None,
            name: export_name.to_string(),
            args: vec![],
        };
        return Ok(vec![owned_named_param(name.ident.to_string(), ty)]);
    }
    let classify = |param: &syn::PatType| {
        if matches!(export.role, Role::Init) {
            classify_init_param(param)
        } else {
            classify_param(param, export.ctx)
        }
    };
    let params = value_params
        .iter()
        .copied()
        .map(classify)
        .collect::<syn::Result<Vec<_>>>()?;
    if export.ctx
        && let Some(FnArg::Typed(ctx)) = method
            .sig
            .inputs
            .iter()
            .nth(usize::from(export.source_receiver.is_some()))
    {
        validate_mut_place_ctx(&method.sig, ctx, &params, "#[anvyx(ctx)]")?;
    }
    let has_callback =
        validate_callback_wrapper_precheck(&method.sig, &params, export.receiver.is_some())?;
    if has_callback && matches!(export.role, Role::Init) {
        return Err(syn::Error::new_spanned(
            &method.sig,
            "#[anvyx(init)] cannot be combined with callback wrapper parameters",
        ));
    }
    if has_callback && export.ctx {
        return Err(syn::Error::new_spanned(
            &method.sig,
            "#[anvyx(ctx)] cannot be combined with callback wrapper parameters",
        ));
    }
    Ok(params)
}

fn apply_param_overrides(
    method: &ImplItemFn,
    export: &MethodExport,
    params: Vec<BoundaryParam>,
) -> syn::Result<Vec<BoundaryParam>> {
    let mut used = std::collections::HashSet::new();
    let params = params
        .into_iter()
        .map(|param| {
            let override_ty = export.param_overrides.get(&param.name);
            if override_ty.is_some() {
                used.insert(param.name.clone());
            }
            let ty = type_with_override(
                &param.ty,
                override_ty.map(String::as_str),
                method.sig.span(),
                format!(
                    "parameter override `{}` does not match Rust ABI",
                    param.name
                ),
            )?;
            Ok(BoundaryParam {
                abi: param_abi_for_override(&param.abi, &ty, param.flow),
                ty,
                ..param
            })
        })
        .collect::<syn::Result<Vec<_>>>()?;
    for name in export.param_overrides.keys() {
        if !used.contains(name) {
            return Err(syn::Error::new_spanned(
                &method.sig,
                format!("unknown parameter override `{name}`"),
            ));
        }
    }
    Ok(params)
}

fn classify_method_return(
    method: &ImplItemFn,
    export: &MethodExport,
    owner: &Ident,
    export_name: &str,
) -> syn::Result<BoundaryReturn> {
    let ret = classify_provider_return_for_owner(
        &method.sig.output,
        OwnerReturn {
            rust_owner: owner,
            export_name,
        },
    )?;
    let ty = type_with_override(
        &ret.ty,
        export.ret_override.as_deref(),
        method.sig.span(),
        "return override does not match Rust ABI",
    )?;
    Ok(BoundaryReturn {
        abi: return_abi_for_override(&ret.abi, &ty),
        ty,
        fallible: ret.fallible,
    })
}

fn member_binding(plan: &MethodPlan) -> TokenStream {
    let symbol = plan.method.sig.ident.to_string();
    let module = methods_native_module_ident_string(plan.owner);
    let mut abis = match (&plan.export.role, plan.export.receiver) {
        (Role::PlaceMethod, _) => vec![receiver_abi_tokens(false, true)],
        (_, Some(Receiver::Shared)) => vec![receiver_abi_tokens(false, false)],
        (_, Some(Receiver::Mutable)) => vec![receiver_abi_tokens(true, false)],
        (_, None) => vec![],
    };
    abis.extend(plan.params.iter().map(|param| param_abi_tokens(&param.abi)));
    let ret = return_abi_tokens(&plan.ret.abi);
    let wrapper_ctx = if plan.runtime_arg {
        quote! { anvyx_runtime::RustCallContext::HiddenRuntime }
    } else {
        quote! { anvyx_runtime::RustCallContext::None }
    };
    let selector = &plan.export.selector;
    let operation = &plan.export.operation;
    quote! {
        anvyx_runtime::RustMemberBinding {
            selector: #selector,
            operation: #operation,
            module: #module.to_string(),
            symbol: #symbol.to_string(),
            abi: anvyx_runtime::RustExternAbi {
                params: vec![#(#abis),*],
                ret: #ret,
                ctx: #wrapper_ctx,
            },
        }
    }
}

fn methods_native_module_ident(owner: &Ident) -> Ident {
    quote::format_ident!(
        "__anvyx_methods_native_{}",
        owner.to_string().to_lowercase()
    )
}

fn methods_native_module_ident_string(owner: &Ident) -> String {
    methods_native_module_ident(owner).to_string()
}

fn receiver_tokens(receiver: Receiver) -> TokenStream {
    match receiver {
        Receiver::Shared => quote! { anvyx_runtime::ReceiverMode::Shared },
        Receiver::Mutable => quote! { anvyx_runtime::ReceiverMode::Mutable },
    }
}

fn doc_tokens(attrs: &[Attribute]) -> TokenStream {
    crate::util::extract_doc(attrs)
        .map_or_else(|| quote! { None }, |doc| quote! { Some(#doc.to_string()) })
}
