use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Attribute, FnArg, GenericArgument, Ident, ImplItem, ImplItemFn, ItemImpl, LitStr,
    PathArguments, ReturnType, Token, Type, Visibility,
    parse::{Parse, ParseStream},
    spanned::Spanned,
};

use crate::clean_type_map::{
    BoundaryConversion, CleanParam, CleanReturn, CleanReturnAbi, CleanType, OwnerReturn,
    callback_wrapper_has_visible_borrow, classify_init_param, classify_param,
    classify_provider_return_for_owner, classify_return, conversion_tokens, flow_tokens,
    has_callback_wrapper, merge_conversions, named_type_expr_tokens, param_abi_for_override,
    param_abi_tokens, param_escape_tokens, return_abi_for_override, return_abi_tokens,
    signature_conversion, type_expr_tokens, type_with_override, validate_callable_signature,
    validate_ctx_param, validate_mut_place_ctx,
};

pub fn expand(attr: TokenStream, item: TokenStream) -> TokenStream {
    match expand_inner(attr, item) {
        Ok(tokens) => tokens,
        Err(err) => err.to_compile_error(),
    }
}

fn expand_inner(attr: TokenStream, item: TokenStream) -> syn::Result<TokenStream> {
    let args: MethodsArgs = syn::parse2(attr)?;
    let mut imp: ItemImpl = syn::parse2(item)?;
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
    let owner = owner_path.path.segments[0].ident.clone();
    let companion = crate::naming::methods_fn_ident(&owner);
    let export_name = args.name.unwrap_or_else(|| owner.to_string());
    let mut descriptor_methods = vec![];
    let mut descriptor_statics = vec![];
    let mut descriptor_operators = vec![];
    let mut init = None;
    let mut bindings = vec![];
    let mut wrappers = vec![];

    for item in &mut imp.items {
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
        let export = MethodExport::parse(&owner, method)?;
        validate_callable_signature(&method.sig, "#[methods]", "method", export.ctx)?;
        let params = method_clean_params(method, &export)?;
        match &export.role {
            Role::Method(receiver) => {
                descriptor_methods.push(method_descriptor(
                    method,
                    *receiver,
                    &export,
                    &params,
                    &owner,
                    &export_name,
                )?);
            }
            Role::Static => descriptor_statics.push(static_descriptor(
                method,
                &export,
                &params,
                &owner,
                &export_name,
            )?),
            Role::Init => {
                if init.is_some() {
                    return Err(syn::Error::new_spanned(method, "duplicate #[anvyx(init)]"));
                }
                init = Some(init_descriptor(
                    method,
                    &export,
                    &params,
                    &owner,
                    &export_name,
                )?);
            }
            Role::Getter | Role::Setter => {}
            Role::Operator(op) => descriptor_operators.push(operator_descriptor(
                method,
                &owner,
                &export_name,
                op,
                &export,
                &params,
            )?),
        }
        bindings.push(member_binding(
            &owner,
            &export_name,
            method,
            &export,
            &params,
        )?);
        wrappers.push(native_wrapper(
            &owner,
            &export_name,
            method,
            &export,
            &params,
        )?);
    }
    let descriptor_fields = computed_property_descriptors(&imp.items)?;
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
        pub fn #companion() -> anvyx_runtime::TypeExport {
            anvyx_runtime::TypeExport {
                rust_type_path: concat!(module_path!(), "::", stringify!(#owner)),
                descriptor: anvyx_runtime::ExternTypeDescriptor {
                    name: #export_name.to_string(),
                    doc: None,
                    rep: anvyx_runtime::ExternRep::Inline,
                    fields: vec![#(#descriptor_fields),*],
                    variants: vec![],
                    init: #init,
                    methods: vec![#(#descriptor_methods),*],
                    statics: vec![#(#descriptor_statics),*],
                    operators: vec![#(#descriptor_operators),*],
                },
                bindings: vec![#(#bindings),*],
            }
        }

        anvyx_runtime::inventory::submit! {
            anvyx_runtime::TypeMemberExport {
                rust_type_path: concat!(module_path!(), "::", stringify!(#owner)),
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

struct MethodExport {
    role: Role,
    selector: TokenStream,
    operation: TokenStream,
    ret_override: Option<String>,
    param_overrides: HashMap<String, String>,
    ctx: bool,
}

#[derive(Clone)]
enum Role {
    Method(Receiver),
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

impl MethodExport {
    fn parse(owner: &Ident, method: &ImplItemFn) -> syn::Result<Self> {
        let attrs = MethodAttrs::parse(&method.attrs)?;
        let receiver = receiver(&method.sig.inputs)?;
        let role = match (attrs.role.clone(), receiver) {
            (Some(role), _) => role,
            (None, Some(receiver)) => Role::Method(receiver),
            (None, None) => Role::Static,
        };
        validate_role(owner, method, &role, receiver, &attrs)?;
        let name = method.sig.ident.to_string();
        let selector = match &role {
            Role::Method(_) => {
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
                let field = setter_field_name(method)?;
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
    attrs: &MethodAttrs,
) -> syn::Result<()> {
    validate_ctx_position(&method.sig.inputs, receiver, attrs.ctx)?;
    match role {
        Role::Method(_) if receiver.is_none() => Err(syn::Error::new_spanned(
            &method.sig.ident,
            "methods require &self or &mut self",
        )),
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
        Role::Getter if !visible_typed_params(method, attrs.ctx)?.is_empty() => Err(
            syn::Error::new_spanned(&method.sig, "#[anvyx(getter)] cannot take value parameters"),
        ),
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
            validate_operator_signature(owner, method, op, attrs.ctx)
        }
        Role::Static if receiver.is_some() => Err(syn::Error::new_spanned(
            &method.sig,
            "static extern members cannot take self",
        )),
        _ => Ok(()),
    }
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
                if meta.path.is_ident("init") {
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
                        "expected init, getter, setter, ctx, op(...), ret = ..., or params(...)",
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
    skip_ctx: bool,
) -> syn::Result<()> {
    if !op.rhs_self {
        return Ok(());
    }
    let typed_params = visible_typed_params(method, skip_ctx)?;
    let [param] = typed_params.as_slice() else {
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

fn visible_typed_params(method: &ImplItemFn, skip_ctx: bool) -> syn::Result<Vec<&syn::PatType>> {
    let receiver = receiver(&method.sig.inputs)?;
    let ctx_index = skip_ctx.then_some(usize::from(receiver.is_some()));
    method
        .sig
        .inputs
        .iter()
        .enumerate()
        .filter_map(|(index, arg)| match arg {
            FnArg::Typed(param) if Some(index) != ctx_index => Some(Ok(param)),
            FnArg::Typed(_) | FnArg::Receiver(_) => None,
        })
        .collect()
}

fn method_descriptor(
    method: &ImplItemFn,
    receiver: Receiver,
    export: &MethodExport,
    params: &[CleanParam],
    owner: &Ident,
    export_name: &str,
) -> syn::Result<TokenStream> {
    let name = method.sig.ident.to_string();
    let doc = doc_tokens(&method.attrs);
    let receiver = receiver_tokens(receiver);
    let signature = signature_tokens(method, Some(export), params, owner, export_name)?;
    let effects = effects_tokens(method, owner, export_name)?;
    Ok(quote! {
        anvyx_runtime::ExternMethodDescriptor {
            name: #name.to_string(),
            doc: #doc,
            receiver: #receiver,
            signature: #signature,
            effects: #effects,
        }
    })
}

fn static_descriptor(
    method: &ImplItemFn,
    export: &MethodExport,
    params: &[CleanParam],
    owner: &Ident,
    export_name: &str,
) -> syn::Result<TokenStream> {
    let name = method.sig.ident.to_string();
    let doc = doc_tokens(&method.attrs);
    let signature = signature_tokens(method, Some(export), params, owner, export_name)?;
    let effects = effects_tokens(method, owner, export_name)?;
    Ok(quote! {
        anvyx_runtime::ExternStaticDescriptor {
            name: #name.to_string(),
            doc: #doc,
            signature: #signature,
            effects: #effects,
        }
    })
}

fn init_descriptor(
    method: &ImplItemFn,
    export: &MethodExport,
    params: &[CleanParam],
    owner: &Ident,
    export_name: &str,
) -> syn::Result<TokenStream> {
    let init = init_return(method, owner, export_name)?;
    let params = params_with_overrides(method, Some(export), params)?;
    let field_init = params
        .iter()
        .filter(|param| !param.init_presence)
        .map(|param| {
            let name = &param.name;
            quote! { #name.to_string() }
        });
    let presence_init = params
        .iter()
        .filter(|param| param.init_presence)
        .map(|param| {
            let name = &param.name;
            quote! { #name.to_string() }
        });
    let params = params.iter().map(|param| {
        let name = &param.name;
        let ty = type_expr_tokens(&param.ty);
        let flow = flow_tokens(param.flow);
        let escape = param_escape_tokens(param);
        quote! {
            anvyx_runtime::ExternParam {
                name: Some(#name.to_string()),
                ty: #ty,
                flow: #flow,
                escape: #escape,
            }
        }
    });
    let ret = init.descriptor_ret;
    let fallible = init.fallible;
    Ok(quote! {
        anvyx_runtime::ExternInitDescriptor {
            params: vec![#(#params),*],
            field_init: vec![#(#field_init),*],
            presence_init: vec![#(#presence_init),*],
            ret: #ret,
            effects: anvyx_runtime::ExternEffects { fallible: #fallible },
        }
    })
}

struct InitReturn {
    descriptor_ret: TokenStream,
    rust_ret: TokenStream,
    fallible: bool,
    conversion: BoundaryConversion,
}

fn init_return(method: &ImplItemFn, owner: &Ident, export_name: &str) -> syn::Result<InitReturn> {
    if matches!(method.sig.output, ReturnType::Default) {
        return Err(syn::Error::new_spanned(
            &method.sig,
            "#[anvyx(init)] must return Self",
        ));
    }
    let ret = classify_provider_return_for_owner(
        &method.sig.output,
        OwnerReturn {
            rust_owner: owner,
            export_name,
        },
    )?;
    if !valid_init_return(&ret, export_name) {
        return Err(syn::Error::new_spanned(
            &method.sig.output,
            "#[anvyx(init)] must return Self, RuntimeResult<Self>, or Result<Self, E>",
        ));
    }
    Ok(InitReturn {
        descriptor_ret: type_expr_tokens(&ret.ty),
        rust_ret: return_abi_tokens(&ret.abi),
        fallible: ret.fallible,
        conversion: ret.conversion,
    })
}

fn valid_init_return(ret: &CleanReturn, export_name: &str) -> bool {
    match (&ret.ty, &ret.abi, ret.fallible) {
        (CleanType::Named(name), CleanReturnAbi::OwnedNamed(_), _) => name == export_name,
        (CleanType::Result(ok, _), CleanReturnAbi::Result(ok_abi, _), false) => {
            matches!(ok.as_ref(), CleanType::Named(name) if name == export_name)
                && matches!(ok_abi.as_ref(), CleanReturnAbi::OwnedNamed(_))
        }
        _ => false,
    }
}

struct ComputedGetter<'a> {
    method: &'a ImplItemFn,
    ty: CleanType,
    ty_tokens: TokenStream,
}

struct ComputedSetter<'a> {
    method: &'a ImplItemFn,
    ty: CleanType,
}

fn computed_property_descriptors(items: &[ImplItem]) -> syn::Result<Vec<TokenStream>> {
    let mut getters = HashMap::<String, ComputedGetter>::new();
    let mut setters = HashMap::<String, ComputedSetter>::new();
    let mut order = vec![];
    for item in items {
        let ImplItem::Fn(method) = item else {
            continue;
        };
        if !is_public(&method.vis) {
            continue;
        }
        let attrs = MethodAttrs::parse(&method.attrs)?;
        match attrs.role {
            Some(Role::Getter) => {
                let name = method.sig.ident.to_string();
                let (ty, ty_tokens) = getter_ty(method, &attrs)?;
                if getters
                    .insert(
                        name.clone(),
                        ComputedGetter {
                            method,
                            ty,
                            ty_tokens,
                        },
                    )
                    .is_some()
                {
                    return Err(syn::Error::new_spanned(method, "duplicate computed getter"));
                }
                order.push(name);
            }
            Some(Role::Setter) => {
                let field = setter_field_name(method)?;
                let ty = setter_ty(method, &attrs)?;
                if setters
                    .insert(field, ComputedSetter { method, ty })
                    .is_some()
                {
                    return Err(syn::Error::new_spanned(method, "duplicate computed setter"));
                }
            }
            _ => {}
        }
    }
    for (field, setter) in &setters {
        if !getters.contains_key(field) {
            return Err(syn::Error::new_spanned(
                setter.method,
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
                    getter.method,
                    format!("computed getter `{field}` requires matching setter `set_{field}`"),
                )
            })?;
            if getter.ty != setter.ty {
                return Err(syn::Error::new_spanned(
                    getter.method,
                    format!("computed property `{field}` getter/setter types differ"),
                ));
            }
            Ok(computed_field(
                &getter.method.attrs,
                &field,
                &getter.ty_tokens,
            ))
        })
        .collect()
}

fn getter_ty(method: &ImplItemFn, attrs: &MethodAttrs) -> syn::Result<(CleanType, TokenStream)> {
    let ty = match &method.sig.output {
        ReturnType::Type(_, _) => {
            let ret = classify_return(&method.sig.output)?;
            if ret.fallible {
                return Err(syn::Error::new_spanned(
                    &method.sig.output,
                    "computed getters cannot return RuntimeResult<T>",
                ));
            }
            type_with_override(
                &ret.ty,
                attrs.ret.as_deref(),
                method.sig.span(),
                "getter return override does not match Rust ABI",
            )?
        }
        ReturnType::Default => {
            return Err(syn::Error::new_spanned(
                &method.sig,
                "computed getters cannot be void",
            ));
        }
    };
    let tokens = type_expr_tokens(&ty);
    Ok((ty, tokens))
}

fn setter_ty(method: &ImplItemFn, attrs: &MethodAttrs) -> syn::Result<CleanType> {
    let typed_params = visible_typed_params(method, attrs.ctx)?;
    let [param] = typed_params.as_slice() else {
        return Err(syn::Error::new_spanned(
            &method.sig,
            "computed setters require exactly one value parameter",
        ));
    };
    let param = classify_param(param, false)?;
    for override_name in attrs.params.keys() {
        if override_name != &param.name {
            return Err(syn::Error::new_spanned(
                &method.sig,
                format!("unknown parameter override `{override_name}`"),
            ));
        }
    }
    type_with_override(
        &param.ty,
        attrs.params.get(&param.name).map(String::as_str),
        method.sig.span(),
        "setter parameter override does not match Rust ABI",
    )
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

fn operator_descriptor(
    method: &ImplItemFn,
    owner: &Ident,
    export_name: &str,
    op: &OperatorRole,
    export: &MethodExport,
    params: &[CleanParam],
) -> syn::Result<TokenStream> {
    let signature = if op.rhs_self {
        self_operator_signature(method, owner, export_name, export.ctx)?
    } else {
        signature_tokens(method, Some(export), params, owner, export_name)?
    };
    let effects = effects_tokens(method, owner, export_name)?;
    let op = &op.tokens;
    Ok(quote! {
        anvyx_runtime::ExternOperatorDescriptor {
            op: #op,
            receiver: anvyx_runtime::ReceiverMode::Shared,
            signature: #signature,
            effects: #effects,
        }
    })
}

fn self_operator_signature(
    method: &ImplItemFn,
    owner: &Ident,
    export_name: &str,
    skip_ctx: bool,
) -> syn::Result<TokenStream> {
    let typed_params = visible_typed_params(method, skip_ctx)?;
    let [param] = typed_params.as_slice() else {
        return Err(syn::Error::new_spanned(
            &method.sig,
            "Self binary operators require exactly one Self operand",
        ));
    };
    let name = match param.pat.as_ref() {
        syn::Pat::Ident(ident) => ident.ident.to_string(),
        _ => "rhs".to_string(),
    };
    let owner_ty = named_type_expr_tokens(export_name);
    let ret = classify_provider_return_for_owner(
        &method.sig.output,
        OwnerReturn {
            rust_owner: owner,
            export_name,
        },
    )?;
    let ret_ty = type_expr_tokens(&ret.ty);
    Ok(quote! {
        anvyx_runtime::ExternSignature {
            params: vec![anvyx_runtime::ExternParam {
                name: Some(#name.to_string()),
                ty: #owner_ty,
                flow: anvyx_runtime::ParamFlow::Value,
                escape: anvyx_runtime::CallbackEscape::NonEscaping,
            }],
            ret: #ret_ty,
        }
    })
}

fn signature_tokens(
    method: &ImplItemFn,
    export: Option<&MethodExport>,
    params: &[CleanParam],
    owner: &Ident,
    export_name: &str,
) -> syn::Result<TokenStream> {
    let params = params_with_overrides(method, export, params)?
        .into_iter()
        .map(|param| {
            let ty = type_expr_tokens(&param.ty);
            let flow = flow_tokens(param.flow);
            let escape = param_escape_tokens(&param);
            let name = param.name;
            quote! {
                anvyx_runtime::ExternParam {
                    name: Some(#name.to_string()),
                    ty: #ty,
                    flow: #flow,
                    escape: #escape,
                }
            }
        })
        .collect::<Vec<_>>();
    let ret = return_with_override(method, export, owner, export_name)?;
    let ret_ty = type_expr_tokens(&ret.ty);
    Ok(quote! { anvyx_runtime::ExternSignature { params: vec![#(#params),*], ret: #ret_ty } })
}

fn params_with_overrides(
    method: &ImplItemFn,
    export: Option<&MethodExport>,
    params: &[CleanParam],
) -> syn::Result<Vec<CleanParam>> {
    let mut used_overrides = std::collections::HashSet::new();
    let params = params
        .iter()
        .map(|param| {
            let override_ty = export.and_then(|export| export.param_overrides.get(&param.name));
            if override_ty.is_some() {
                used_overrides.insert(param.name.clone());
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
            let abi = param_abi_for_override(&param.abi, &ty, param.flow);
            Ok(CleanParam {
                name: param.name.clone(),
                abi,
                ty,
                flow: param.flow,
                conversion: param.conversion,
                init_presence: param.init_presence,
            })
        })
        .collect::<syn::Result<Vec<_>>>()?;
    if let Some(export) = export {
        for name in export.param_overrides.keys() {
            if !used_overrides.contains(name) {
                return Err(syn::Error::new_spanned(
                    &method.sig,
                    format!("unknown parameter override `{name}`"),
                ));
            }
        }
    }
    Ok(params)
}

fn return_with_override(
    method: &ImplItemFn,
    export: Option<&MethodExport>,
    owner: &Ident,
    export_name: &str,
) -> syn::Result<CleanReturn> {
    let ret = classify_provider_return_for_owner(
        &method.sig.output,
        OwnerReturn {
            rust_owner: owner,
            export_name,
        },
    )?;
    let ty = type_with_override(
        &ret.ty,
        export.and_then(|export| export.ret_override.as_deref()),
        method.sig.span(),
        "return override does not match Rust ABI",
    )?;
    Ok(CleanReturn {
        abi: return_abi_for_override(&ret.abi, &ty),
        ty,
        fallible: ret.fallible,
        conversion: ret.conversion,
    })
}

fn effects_tokens(
    method: &ImplItemFn,
    owner: &Ident,
    export_name: &str,
) -> syn::Result<TokenStream> {
    let ret = classify_provider_return_for_owner(
        &method.sig.output,
        OwnerReturn {
            rust_owner: owner,
            export_name,
        },
    )?;
    let fallible = ret.fallible;
    Ok(quote! { anvyx_runtime::ExternEffects { fallible: #fallible } })
}

fn native_wrapper(
    owner: &Ident,
    export_name: &str,
    method: &ImplItemFn,
    export: &MethodExport,
    params: &[CleanParam],
) -> syn::Result<TokenStream> {
    let ident = &method.sig.ident;
    let runtime_arg = needs_runtime_arg(method, export, params, owner, export_name)?;
    let ctx = quote! { ctx };
    let inputs = wrapper_inputs(owner, method, export, &ctx, runtime_arg);
    let output = wrapper_output(owner, method);
    let args = visible_typed_params(method, export.ctx)
        .expect("validated method signature")
        .into_iter()
        .map(|param| &param.pat)
        .collect::<Vec<_>>();
    let call_args = if export.ctx {
        quote! { ctx, #(#args),* }
    } else {
        quote! { #(#args),* }
    };
    let call = match export.role {
        Role::Method(_) | Role::Getter | Role::Setter | Role::Operator(_) => {
            quote! { receiver.#ident(#call_args) }
        }
        Role::Static | Role::Init => quote! { super::#owner::#ident(#call_args) },
    };
    Ok(quote! {
        pub fn #ident<'cx>(#inputs) #output {
            #call
        }
    })
}

fn needs_runtime_arg(
    method: &ImplItemFn,
    export: &MethodExport,
    params: &[CleanParam],
    owner: &Ident,
    export_name: &str,
) -> syn::Result<bool> {
    if export.ctx
        || matches!(export.role, Role::Init)
        || matches!(
            export.role,
            Role::Operator(OperatorRole { rhs_self: true, .. })
        )
    {
        return Ok(true);
    }
    let ret = return_with_override(method, Some(export), owner, export_name)?;
    Ok(
        signature_conversion(params, &ret) == BoundaryConversion::NeedsWrapper
            && !has_callback_wrapper(params),
    )
}

fn wrapper_inputs(
    owner: &Ident,
    method: &ImplItemFn,
    export: &MethodExport,
    ctx: &TokenStream,
    runtime_arg: bool,
) -> TokenStream {
    let params = visible_typed_params(method, export.ctx)
        .expect("validated method signature")
        .into_iter()
        .map(|param| {
            let attrs = &param.attrs;
            let pat = &param.pat;
            let colon = &param.colon_token;
            let ty = wrapper_type(owner, &param.ty);
            quote! { #(#attrs)* #pat #colon #ty }
        });
    match &export.role {
        Role::Method(_) | Role::Getter | Role::Operator(_) | Role::Setter => {
            let receiver = if role_receiver(method, &export.role) == Some(Receiver::Mutable) {
                quote! { receiver: &mut super::#owner }
            } else {
                quote! { receiver: &super::#owner }
            };
            if runtime_arg {
                quote! { #ctx: &mut anvyx_runtime::Ctx<'cx, '_>, #receiver, #(#params),* }
            } else {
                quote! { #receiver, #(#params),* }
            }
        }
        Role::Static | Role::Init => {
            if runtime_arg {
                quote! { #ctx: &mut anvyx_runtime::Ctx<'cx, '_>, #(#params),* }
            } else {
                quote! { #(#params),* }
            }
        }
    }
}

fn wrapper_output(owner: &Ident, method: &ImplItemFn) -> TokenStream {
    match &method.sig.output {
        ReturnType::Default => quote! {},
        ReturnType::Type(arrow, ty) => {
            let ty = wrapper_type(owner, ty);
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

fn method_clean_params(method: &ImplItemFn, export: &MethodExport) -> syn::Result<Vec<CleanParam>> {
    if matches!(&export.role, Role::Operator(op) if op.rhs_self) {
        return Ok(vec![]);
    }
    let classify = |param: &syn::PatType| {
        if matches!(export.role, Role::Init) {
            classify_init_param(param)
        } else {
            classify_param(param, export.ctx)
        }
    };
    let params = visible_typed_params(method, export.ctx)?
        .into_iter()
        .map(classify)
        .collect::<syn::Result<Vec<_>>>()?;
    if export.ctx {
        let receiver = role_receiver(method, &export.role);
        if let Some(FnArg::Typed(ctx)) = method
            .sig
            .inputs
            .iter()
            .nth(usize::from(receiver.is_some()))
        {
            validate_mut_place_ctx(&method.sig, ctx, &params, "#[anvyx(ctx)]")?;
        }
    }
    validate_callback_wrapper_method(method, export, &params)?;
    Ok(params)
}

fn validate_callback_wrapper_method(
    method: &ImplItemFn,
    export: &MethodExport,
    params: &[CleanParam],
) -> syn::Result<()> {
    if !has_callback_wrapper(params) {
        return Ok(());
    }
    if export.ctx {
        return Err(syn::Error::new_spanned(
            &method.sig,
            "#[anvyx(ctx)] cannot be combined with callback wrapper parameters",
        ));
    }
    if role_receiver(method, &export.role).is_some() {
        return Err(syn::Error::new_spanned(
            &method.sig,
            "callback wrapper parameters cannot be combined with method receivers",
        ));
    }
    if callback_wrapper_has_visible_borrow(params) {
        return Err(syn::Error::new_spanned(
            &method.sig,
            "callback wrapper parameters cannot be combined with borrowed or mutable-place provider parameters",
        ));
    }
    Ok(())
}

fn member_binding(
    owner: &Ident,
    export_name: &str,
    method: &ImplItemFn,
    export: &MethodExport,
    params: &[CleanParam],
) -> syn::Result<TokenStream> {
    let symbol = method.sig.ident.to_string();
    let module = methods_native_module_ident_string(owner);
    let owner_ty = named_type_expr_tokens(export_name);
    let self_operator = matches!(&export.role, Role::Operator(op) if op.rhs_self);
    let abi_params = if self_operator {
        vec![]
    } else {
        params_with_overrides(method, Some(export), params)?
    };
    let mut abis = match role_receiver(method, &export.role) {
        Some(Receiver::Shared) => vec![quote! { anvyx_runtime::RustParamAbi::Borrow(#owner_ty) }],
        Some(Receiver::Mutable) => {
            vec![quote! { anvyx_runtime::RustParamAbi::MutBorrow(#owner_ty) }]
        }
        None => vec![],
    };
    if self_operator {
        abis.push(quote! { anvyx_runtime::RustParamAbi::OwnedNamed(#owner_ty) });
    } else {
        abis.extend(abi_params.iter().map(|param| param_abi_tokens(&param.abi)));
    }
    let (ret_abi, support, fallible) = if matches!(export.role, Role::Init) {
        let init = init_return(method, owner, export_name)?;
        (
            init.rust_ret,
            {
                let conversion = merge_conversions(
                    abi_params
                        .iter()
                        .map(|param| param.conversion)
                        .chain(std::iter::once(init.conversion)),
                );
                if conversion == BoundaryConversion::Unsupported {
                    return Err(syn::Error::new_spanned(
                        &method.sig,
                        "unsupported native ABI conversion",
                    ));
                }
                conversion_tokens(conversion)
            },
            init.fallible,
        )
    } else if self_operator {
        let ret = classify_provider_return_for_owner(
            &method.sig.output,
            OwnerReturn {
                rust_owner: owner,
                export_name,
            },
        )?;
        if ret.conversion == BoundaryConversion::Unsupported {
            return Err(syn::Error::new_spanned(
                &method.sig,
                "unsupported native ABI conversion",
            ));
        }
        (
            return_abi_tokens(&ret.abi),
            conversion_tokens(ret.conversion),
            ret.fallible,
        )
    } else {
        let ret = return_with_override(method, Some(export), owner, export_name)?;
        let conversion = signature_conversion(&abi_params, &ret);
        if conversion == BoundaryConversion::Unsupported {
            return Err(syn::Error::new_spanned(
                &method.sig,
                "unsupported native ABI conversion",
            ));
        }
        let ret_abi = return_abi_tokens(&ret.abi);
        (ret_abi, conversion_tokens(conversion), ret.fallible)
    };
    let wrapper_ctx = if needs_runtime_arg(method, export, &abi_params, owner, export_name)? {
        quote! { anvyx_runtime::RustWrapperCtx::HiddenRuntime }
    } else {
        quote! { anvyx_runtime::RustWrapperCtx::None }
    };
    let selector = &export.selector;
    let operation = &export.operation;
    Ok(quote! {
        anvyx_runtime::RustMemberBinding {
            selector: #selector,
            operation: #operation,
            module: #module.to_string(),
            symbol: #symbol.to_string(),
            abi: anvyx_runtime::RustExternAbi {
                params: vec![#(#abis),*],
                ret: #ret_abi,
                fallible: #fallible,
                support: #support,
                ctx: #wrapper_ctx,
            },
        }
    })
}

fn role_receiver(method: &ImplItemFn, role: &Role) -> Option<Receiver> {
    match role {
        Role::Method(receiver) => Some(*receiver),
        Role::Getter | Role::Setter | Role::Operator(_) => {
            receiver(&method.sig.inputs).ok().flatten()
        }
        Role::Static | Role::Init => None,
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
    crate::codegen::extract_doc(attrs)
        .map_or_else(|| quote! { None }, |doc| quote! { Some(#doc.to_string()) })
}
