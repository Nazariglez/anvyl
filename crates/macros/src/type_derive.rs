use proc_macro2::TokenStream;
use quote::quote;
use syn::{Attribute, Data, DeriveInput, Fields, GenericArgument, LitStr, PathArguments, Type};

use crate::boundary::{classify_field_type, type_expr_tokens};

#[derive(Clone, Copy)]
pub enum TypeDeriveKind {
    Inline,
    Ref,
    Enum,
}

impl TypeDeriveKind {
    fn macro_name(self) -> &'static str {
        match self {
            Self::Inline => "AnvyxInline",
            Self::Ref => "AnvyxRef",
            Self::Enum => "AnvyxEnum",
        }
    }

    fn marker_trait(self) -> TokenStream {
        match self {
            Self::Inline => quote! { anvyx_runtime::AnvyxInlineExport },
            Self::Ref => quote! { anvyx_runtime::AnvyxRefExport },
            Self::Enum => quote! { anvyx_runtime::AnvyxEnumExport },
        }
    }

    fn rep(self) -> TokenStream {
        match self {
            Self::Inline | Self::Enum => quote! { anvyx_runtime::ExternRep::Inline },
            Self::Ref => quote! { anvyx_runtime::ExternRep::Shared },
        }
    }
}

pub fn expand(input: TokenStream, kind: TypeDeriveKind) -> TokenStream {
    match expand_inner(input, kind) {
        Ok(tokens) => tokens,
        Err(err) => err.to_compile_error(),
    }
}

fn expand_inner(input: TokenStream, kind: TypeDeriveKind) -> syn::Result<TokenStream> {
    let item: DeriveInput = syn::parse2(input)?;
    let generic_error = match kind {
        TypeDeriveKind::Ref if unsupported_generics(&item.generics) => Some(format!(
            "{} only supports an optional `'cx` lifetime",
            kind.macro_name()
        )),
        TypeDeriveKind::Inline | TypeDeriveKind::Enum if !item.generics.params.is_empty() => Some(
            format!("{} does not support generic types yet", kind.macro_name()),
        ),
        _ => None,
    };
    if let Some(message) = generic_error {
        return Err(syn::Error::new_spanned(item.generics, message));
    }
    let owns_heap_edges;
    let (fields, variants) = match (&item.data, kind) {
        (Data::Struct(strukt), TypeDeriveKind::Inline | TypeDeriveKind::Ref) => {
            let Fields::Named(fields) = &strukt.fields else {
                return Err(syn::Error::new_spanned(
                    strukt.fields.clone(),
                    format!("{} requires a named-field struct", kind.macro_name()),
                ));
            };
            owns_heap_edges = validate_ref_fields(fields)?;
            (
                fields
                    .named
                    .iter()
                    .filter_map(field_descriptor)
                    .collect::<syn::Result<Vec<_>>>()?,
                vec![],
            )
        }
        (Data::Enum(enm), TypeDeriveKind::Enum) => {
            owns_heap_edges = enm
                .variants
                .iter()
                .flat_map(|variant| &variant.fields)
                .any(|field| classify_ref_field_type(&field.ty).owns_heap_edges);
            (
                vec![],
                enm.variants
                    .iter()
                    .map(variant_descriptor)
                    .collect::<syn::Result<Vec<_>>>()?,
            )
        }
        (_, TypeDeriveKind::Enum) => {
            return Err(syn::Error::new_spanned(
                item.ident,
                "AnvyxEnum can only be derived for enums",
            ));
        }
        _ => {
            return Err(syn::Error::new_spanned(
                item.ident,
                format!("{} can only be derived for structs", kind.macro_name()),
            ));
        }
    };

    let ident = &item.ident;
    let (impl_generics, ty_generics, where_clause) = item.generics.split_for_impl();
    let export_name = type_name(&item.attrs)?.unwrap_or_else(|| ident.to_string());
    if !matches!(item.vis, syn::Visibility::Public(_)) {
        return Err(syn::Error::new_spanned(
            ident,
            format!("{} requires a public exported type", kind.macro_name()),
        ));
    }
    let companion = crate::naming::fn_companion_ident(ident);
    let native_mod = crate::naming::native_export_module_ident(ident);
    let materializer_fn = crate::naming::materializer_fn_ident();
    let doc = crate::util::extract_doc(&item.attrs)
        .map_or_else(|| quote! { None }, |doc| quote! { Some(#doc.to_string()) });
    let rust_type_path = quote! { concat!(module_path!(), "::", stringify!(#ident)) };
    let context_lifetime = !item.generics.params.is_empty();
    let marker_trait = kind.marker_trait();
    let rep = kind.rep();
    let trace_assert = (owns_heap_edges && !matches!(kind, TypeDeriveKind::Ref)).then(|| {
        quote! {
            fn __anvyx_assert_trace<T: for<'trace> anvyx_runtime::Trace<'trace>>() {}
            __anvyx_assert_trace::<#ident #ty_generics>();
        }
    });
    let (layout, materialization, heap_edges) = match kind {
        TypeDeriveKind::Inline => (
            quote! { Some(anvyx_runtime::ExternLayout { size: ::core::mem::size_of::<#ident #ty_generics>() as u64, align: ::core::mem::align_of::<#ident #ty_generics>() as u64 }) },
            quote! { Some(anvyx_runtime::ExternMaterialization::Copy) },
            quote! { Some(#owns_heap_edges) },
        ),
        TypeDeriveKind::Enum => (
            quote! { Some(anvyx_runtime::ExternLayout { size: ::core::mem::size_of::<#ident #ty_generics>() as u64, align: ::core::mem::align_of::<#ident #ty_generics>() as u64 }) },
            quote! { Some(anvyx_runtime::ExternMaterialization::Materialize) },
            quote! { Some(#owns_heap_edges) },
        ),
        TypeDeriveKind::Ref => (
            quote! { None },
            quote! { None },
            quote! { Some(#owns_heap_edges) },
        ),
    };
    let native_materializer = match kind {
        TypeDeriveKind::Inline => quote! {
            pub fn #materializer_fn(value: &super::#ident #ty_generics) -> super::#ident #ty_generics {
                *value
            }
        },
        TypeDeriveKind::Enum => quote! {
            pub fn #materializer_fn(value: &super::#ident #ty_generics) -> super::#ident #ty_generics {
                value.clone()
            }
        },
        TypeDeriveKind::Ref => quote! {},
    };
    let export_ctor = match kind {
        TypeDeriveKind::Inline => {
            quote! { anvyx_runtime::TypeExport::copy::<#ident #ty_generics> }
        }
        TypeDeriveKind::Enum => {
            quote! { anvyx_runtime::TypeExport::enumeration::<#ident #ty_generics> }
        }
        TypeDeriveKind::Ref if context_lifetime => {
            quote! { anvyx_runtime::TypeExport::shared_context_lifetime::<#ident #ty_generics> }
        }
        TypeDeriveKind::Ref => {
            quote! { anvyx_runtime::TypeExport::shared::<#ident #ty_generics> }
        }
    };
    let marker_impl = match kind {
        TypeDeriveKind::Ref => quote! {
            unsafe impl #impl_generics #marker_trait for #ident #ty_generics #where_clause {
                const OWNS_ANVYX_HEAP_EDGES: bool = #owns_heap_edges;
            }
        },
        TypeDeriveKind::Inline | TypeDeriveKind::Enum => quote! {
            unsafe impl #impl_generics #marker_trait for #ident #ty_generics #where_clause {
                const OWNS_ANVYX_HEAP_EDGES: bool = #owns_heap_edges;
                const __ANVYX_MATERIALIZER: fn(&Self) -> Self = #native_mod::#materializer_fn;
            }
        },
    };

    Ok(quote! {
        #marker_impl

        #[doc(hidden)]
        pub fn #companion #impl_generics() -> anvyx_runtime::ModuleExport #where_clause {
            fn __anvyx_assert_type<T: #marker_trait>() {}
            __anvyx_assert_type::<#ident #ty_generics>();
            #trace_assert
            anvyx_runtime::ModuleExport::ty(anvyx_runtime::merge_type_members(#export_ctor(
                #rust_type_path,
                anvyx_runtime::ExternTypeDescriptor {
                    name: #export_name.to_string(),
                    doc: #doc,
                    rep: #rep,
                    layout: #layout,
                    materialization: #materialization,
                    owns_heap_edges: #heap_edges,
                    fields: vec![#(#fields),*],
                    variants: vec![#(#variants),*],
                    init: None,
                    methods: vec![],
                    statics: vec![],
                    operators: vec![],
                },
                vec![],
            )))
        }

        #[doc(hidden)]
        pub mod #native_mod {
            #native_materializer
        }
    })
}

fn unsupported_generics(generics: &syn::Generics) -> bool {
    generics.where_clause.is_some()
        || generics.params.iter().any(|param| {
            !matches!(param, syn::GenericParam::Lifetime(lifetime) if lifetime.lifetime.ident == "cx")
        })
}

#[derive(Clone, Copy, Default)]
struct RefFieldType {
    contains_escaping_lambda: bool,
    owns_heap_edges: bool,
}

impl RefFieldType {
    fn merge(&mut self, other: Self) {
        self.contains_escaping_lambda |= other.contains_escaping_lambda;
        self.owns_heap_edges |= other.owns_heap_edges;
    }
}

fn validate_ref_fields(fields: &syn::FieldsNamed) -> syn::Result<bool> {
    let mut owns_heap_edges = false;
    for field in &fields.named {
        let ty = classify_ref_field_type(&field.ty);
        if ty.contains_escaping_lambda {
            return Err(syn::Error::new_spanned(
                &field.ty,
                "AnvyxRef resources must not store EscapingLambda leases; store a traceable AnvCallback carrier or keep the callback outside the heap resource",
            ));
        }
        owns_heap_edges |= ty.owns_heap_edges;
    }
    Ok(owns_heap_edges)
}

fn classify_ref_field_type(ty: &Type) -> RefFieldType {
    match ty {
        Type::Path(path) => classify_ref_field_path(path),
        Type::Array(array) => classify_ref_field_type(&array.elem),
        Type::Group(group) => classify_ref_field_type(&group.elem),
        Type::Paren(paren) => classify_ref_field_type(&paren.elem),
        Type::Reference(reference) => classify_ref_field_type(&reference.elem),
        Type::Slice(slice) => classify_ref_field_type(&slice.elem),
        Type::Tuple(tuple) => {
            let mut class = RefFieldType::default();
            for ty in &tuple.elems {
                class.merge(classify_ref_field_type(ty));
            }
            class
        }
        Type::Ptr(ptr) => {
            let mut class = classify_ref_field_type(&ptr.elem);
            class.owns_heap_edges = true;
            class
        }
        _ => RefFieldType {
            owns_heap_edges: true,
            ..RefFieldType::default()
        },
    }
}

fn classify_ref_field_path(path: &syn::TypePath) -> RefFieldType {
    let Some(segment) = path.path.segments.last() else {
        return RefFieldType {
            owns_heap_edges: true,
            ..RefFieldType::default()
        };
    };
    if path_is(path, &["std", "marker", "PhantomData"])
        || path_is(path, &["core", "marker", "PhantomData"])
    {
        return RefFieldType::default();
    }
    let mut class = classify_path_arguments(&segment.arguments);
    let name = segment.ident.to_string();
    if name == "EscapingLambda" {
        class.contains_escaping_lambda = true;
    }
    if class.contains_escaping_lambda || !path_known_edge_free(path, class) {
        class.owns_heap_edges = true;
    }
    class
}

fn classify_path_arguments(args: &PathArguments) -> RefFieldType {
    let mut class = RefFieldType::default();
    match args {
        PathArguments::AngleBracketed(args) => {
            for arg in &args.args {
                if let GenericArgument::Type(ty) = arg {
                    class.merge(classify_ref_field_type(ty));
                }
            }
        }
        PathArguments::Parenthesized(args) => {
            for ty in &args.inputs {
                class.merge(classify_ref_field_type(ty));
            }
            if let syn::ReturnType::Type(_, ty) = &args.output {
                class.merge(classify_ref_field_type(ty));
            }
        }
        PathArguments::None => {}
    }
    class
}

fn path_known_edge_free(path: &syn::TypePath, class: RefFieldType) -> bool {
    path_is(path, &["std", "string", "String"])
        || path_is(path, &["alloc", "string", "String"])
        || path_is(path, &["anvyx_runtime", "AnvString"])
        || ((path_is(path, &["std", "option", "Option"])
            || path_is(path, &["core", "option", "Option"])
            || path_is(path, &["std", "boxed", "Box"])
            || path_is(path, &["alloc", "boxed", "Box"])
            || path_is(path, &["std", "result", "Result"])
            || path_is(path, &["core", "result", "Result"]))
            && !class.contains_escaping_lambda
            && !class.owns_heap_edges)
        || path.qself.is_none()
            && path.path.segments.len() == 1
            && matches!(
                path.path.segments[0].ident.to_string().as_str(),
                "bool"
                    | "char"
                    | "u8"
                    | "u16"
                    | "u32"
                    | "u64"
                    | "u128"
                    | "usize"
                    | "i8"
                    | "i16"
                    | "i32"
                    | "i64"
                    | "i128"
                    | "isize"
                    | "f32"
                    | "f64"
            )
}

fn path_is(path: &syn::TypePath, segments: &[&str]) -> bool {
    path.qself.is_none()
        && path.path.segments.len() == segments.len()
        && path
            .path
            .segments
            .iter()
            .zip(segments)
            .all(|(actual, expected)| actual.ident == *expected)
}

fn variant_descriptor(variant: &syn::Variant) -> syn::Result<TokenStream> {
    let name = variant.ident.to_string();
    let fields = match &variant.fields {
        Fields::Unit => vec![],
        Fields::Unnamed(fields) => fields
            .unnamed
            .iter()
            .map(|field| variant_field_descriptor(None, field))
            .collect::<syn::Result<Vec<_>>>()?,
        Fields::Named(fields) => fields
            .named
            .iter()
            .map(|field| {
                variant_field_descriptor(field.ident.as_ref().map(ToString::to_string), field)
            })
            .collect::<syn::Result<Vec<_>>>()?,
    };
    let doc = crate::util::extract_doc(&variant.attrs)
        .map_or_else(|| quote! { None }, |doc| quote! { Some(#doc.to_string()) });
    Ok(quote! {
        anvyx_runtime::ExternEnumVariantDescriptor {
            name: #name.to_string(),
            fields: vec![#(#fields),*],
            doc: #doc,
        }
    })
}

fn variant_field_descriptor(name: Option<String>, field: &syn::Field) -> syn::Result<TokenStream> {
    let ty = field_type_tokens(&field.ty)?;
    let name = name.map_or_else(
        || quote! { None },
        |name| quote! { Some(#name.to_string()) },
    );
    Ok(quote! {
        anvyx_runtime::ExternEnumVariantFieldDescriptor {
            name: #name,
            ty: #ty,
        }
    })
}

fn field_descriptor(field: &syn::Field) -> Option<syn::Result<TokenStream>> {
    match field_exported(&field.attrs) {
        Ok(true) => Some(field_descriptor_inner(field)),
        Ok(false) => None,
        Err(err) => Some(Err(err)),
    }
}

fn field_descriptor_inner(field: &syn::Field) -> syn::Result<TokenStream> {
    if !matches!(field.vis, syn::Visibility::Public(_)) {
        return Err(syn::Error::new_spanned(
            field,
            "#[anvyx(field)] requires a public field",
        ));
    }
    let name = field
        .ident
        .as_ref()
        .expect("named fields have identifiers")
        .to_string();
    let ty = field_type_tokens(&field.ty)?;
    let doc = crate::util::extract_doc(&field.attrs)
        .map_or_else(|| quote! { None }, |doc| quote! { Some(#doc.to_string()) });
    Ok(quote! {
        anvyx_runtime::ExternFieldDescriptor {
            name: #name.to_string(),
            ty: #ty,
            computed: false,
            readable: true,
            writable: true,
            get_receiver: anvyx_runtime::ReceiverMode::Shared,
            set_receiver: anvyx_runtime::ReceiverMode::Mutable,
            doc: #doc,
        }
    })
}

fn field_type_tokens(ty: &Type) -> syn::Result<TokenStream> {
    let ty = classify_field_type(ty)?;
    Ok(type_expr_tokens(&ty))
}

fn type_name(attrs: &[Attribute]) -> syn::Result<Option<String>> {
    let mut name = None;
    for attr in attrs.iter().filter(|attr| attr.path().is_ident("anvyx")) {
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("name") {
                if name.is_some() {
                    return Err(meta.error("duplicate #[anvyx(name = ...)]"));
                }
                let value = meta.value()?;
                name = Some(value.parse::<LitStr>()?.value());
                Ok(())
            } else {
                Err(meta.error("expected #[anvyx(name = ...)]"))
            }
        })?;
    }
    Ok(name)
}

fn field_exported(attrs: &[Attribute]) -> syn::Result<bool> {
    let mut exported = false;
    for attr in attrs.iter().filter(|attr| attr.path().is_ident("anvyx")) {
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("field") {
                if exported {
                    return Err(meta.error("duplicate #[anvyx(field)]"));
                }
                exported = true;
                Ok(())
            } else {
                Err(meta.error("expected #[anvyx(field)]"))
            }
        })?;
    }
    Ok(exported)
}
