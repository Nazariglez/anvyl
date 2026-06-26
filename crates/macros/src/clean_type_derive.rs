use proc_macro2::TokenStream;
use quote::quote;
use syn::{Attribute, Data, DeriveInput, Fields, LitStr, Type};

use crate::clean_type_map::{classify_field_type, type_expr_tokens};

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
    if !item.generics.params.is_empty() {
        return Err(syn::Error::new_spanned(
            item.generics,
            format!("{} does not support generic types yet", kind.macro_name()),
        ));
    }
    let (fields, variants) = match (&item.data, kind) {
        (Data::Struct(strukt), TypeDeriveKind::Inline | TypeDeriveKind::Ref) => {
            let Fields::Named(fields) = &strukt.fields else {
                return Err(syn::Error::new_spanned(
                    strukt.fields.clone(),
                    format!("{} requires a named-field struct", kind.macro_name()),
                ));
            };
            (
                fields
                    .named
                    .iter()
                    .filter_map(field_descriptor)
                    .collect::<syn::Result<Vec<_>>>()?,
                vec![],
            )
        }
        (Data::Enum(enm), TypeDeriveKind::Enum) => (
            vec![],
            enm.variants
                .iter()
                .map(variant_descriptor)
                .collect::<syn::Result<Vec<_>>>()?,
        ),
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
    let export_name = type_name(&item.attrs)?.unwrap_or_else(|| ident.to_string());
    let companion = crate::naming::fn_companion_ident(ident);
    let native_mod = crate::naming::native_export_module_ident(ident);
    let doc = crate::codegen::extract_doc(&item.attrs)
        .map_or_else(|| quote! { None }, |doc| quote! { Some(#doc.to_string()) });
    let marker_trait = kind.marker_trait();
    let rep = kind.rep();

    Ok(quote! {
        impl #marker_trait for #ident {}

        #[doc(hidden)]
        pub fn #companion() -> anvyx_runtime::TypeExport {
            fn __anvyx_assert_type<T: #marker_trait>() {}
            __anvyx_assert_type::<#ident>();
            anvyx_runtime::merge_type_members(anvyx_runtime::TypeExport {
                rust_type_path: concat!(module_path!(), "::", stringify!(#ident)),
                descriptor: anvyx_runtime::ExternTypeDescriptor {
                    name: #export_name.to_string(),
                    doc: #doc,
                    rep: #rep,
                    fields: vec![#(#fields),*],
                    variants: vec![#(#variants),*],
                    init: None,
                    methods: vec![],
                    statics: vec![],
                    operators: vec![],
                },
                bindings: vec![],
            })
        }

        #[doc(hidden)]
        pub mod #native_mod {}
    })
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
    let doc = crate::codegen::extract_doc(&variant.attrs)
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
    let name = field
        .ident
        .as_ref()
        .expect("named fields have identifiers")
        .to_string();
    let ty = field_type_tokens(&field.ty)?;
    let doc = crate::codegen::extract_doc(&field.attrs)
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
