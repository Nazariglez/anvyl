use proc_macro::TokenStream;
use proc_macro_crate::{FoundCrate, crate_name};
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{
    Attribute, Data, DataEnum, DataStruct, DeriveInput, Error, Field, Fields, GenericParam, Ident,
    Lifetime, LifetimeParam, Path, Result, Type, parse_macro_input, parse_quote,
};

#[proc_macro_derive(Trace, attributes(trace))]
pub fn derive_trace(input: TokenStream) -> TokenStream {
    match expand_trace(parse_macro_input!(input as DeriveInput)) {
        Ok(tokens) => tokens.into(),
        Err(error) => error.to_compile_error().into(),
    }
}

fn expand_trace(input: DeriveInput) -> Result<TokenStream2> {
    let attrs = trace_args(&input.attrs)?;
    let ctx = trace_lifetime(&input, attrs.ctx)?;
    let heap_path = resolve_heap_path(attrs.heap_path.as_ref())?;
    let driver = fresh_driver_ident(&input.generics);
    let fields = trace_fields(&input.data)?;
    let mut generics = input.generics.clone();
    if !generics.params.iter().any(|param| match param {
        GenericParam::Lifetime(param) => param.lifetime == ctx,
        _ => false,
    }) {
        generics
            .params
            .insert(0, GenericParam::Lifetime(LifetimeParam::new(ctx.clone())));
    }

    for ty in fields.iter().map(|field| &field.ty) {
        generics
            .make_where_clause()
            .predicates
            .push(parse_quote!(#ty: #heap_path::Trace<#ctx>));
    }

    let ident = input.ident;
    let (_, ty_generics, _) = input.generics.split_for_impl();
    let (impl_generics, _, where_clause) = generics.split_for_impl();
    let body = trace_body(&input.data, &heap_path)?;

    Ok(quote! {
        // SAFETY: generated tracing visits each non-skipped field exactly once; skipped fields are caller-asserted.
        unsafe impl #impl_generics #heap_path::Trace<#ctx> for #ident #ty_generics #where_clause {
            fn trace<#driver: #heap_path::TraceDriver<#ctx>>(
                &self,
                visitor: &mut #heap_path::Visitor<#ctx, '_, #driver>,
            ) {
                #body
            }
        }
    })
}

#[derive(Default)]
struct TraceArgs {
    ctx: Option<Lifetime>,
    heap_path: Option<Path>,
}

fn fresh_driver_ident(generics: &syn::Generics) -> Ident {
    for index in 0.. {
        let candidate = if index == 0 {
            "AnvyxTraceDriver".to_owned()
        } else {
            format!("AnvyxTraceDriver{index}")
        };
        let taken = generics.params.iter().any(|param| match param {
            GenericParam::Type(param) => param.ident == candidate,
            GenericParam::Const(param) => param.ident == candidate,
            GenericParam::Lifetime(_) => false,
        });
        if !taken {
            return format_ident!("{candidate}");
        }
    }

    unreachable!()
}

fn trace_args(attrs: &[Attribute]) -> Result<TraceArgs> {
    let mut args = TraceArgs::default();
    for attr in trace_attrs(attrs) {
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("ctx") {
                if args.ctx.is_some() {
                    return Err(meta.error("duplicate `ctx` trace attribute"));
                }
                args.ctx = Some(meta.value()?.parse()?);
                Ok(())
            } else if meta.path.is_ident("crate") {
                if args.heap_path.is_some() {
                    return Err(meta.error("duplicate `crate` trace attribute"));
                }
                args.heap_path = Some(meta.value()?.parse()?);
                Ok(())
            } else if meta.path.is_ident("skip_unchecked") {
                Err(meta.error("`trace(skip_unchecked)` is only supported on fields"))
            } else {
                Err(meta.error("unknown trace attribute"))
            }
        })?;
    }
    Ok(args)
}

fn resolve_heap_path(explicit: Option<&Path>) -> Result<Path> {
    if let Some(path) = explicit {
        return Ok(path.clone());
    }

    let heap = resolve_crate("anvyx-heap");
    if matches!(heap, Some(ResolvedCrate::Itself)) {
        return Ok(parse_quote!(crate));
    }
    if let Some(runtime) = resolve_crate("anvyx-runtime") {
        return runtime.path();
    }
    if let Some(heap) = heap {
        return heap.path();
    }

    Err(Error::new(
        Span::call_site(),
        "could not find `anvyx-heap` or `anvyx-runtime`; add `#[trace(crate = path)]`",
    ))
}

enum ResolvedCrate {
    Itself,
    Name(String),
}

impl ResolvedCrate {
    fn path(self) -> Result<Path> {
        match self {
            Self::Itself => Ok(parse_quote!(crate)),
            Self::Name(name) => syn::parse_str::<Path>(&format!("::{name}"))
                .map_err(|error| Error::new(Span::call_site(), error)),
        }
    }
}

fn resolve_crate(package: &str) -> Option<ResolvedCrate> {
    match crate_name(package) {
        Ok(FoundCrate::Itself) => Some(ResolvedCrate::Itself),
        Ok(FoundCrate::Name(name)) => Some(ResolvedCrate::Name(name)),
        Err(_) => None,
    }
}

fn trace_lifetime(input: &DeriveInput, explicit: Option<Lifetime>) -> Result<Lifetime> {
    let cx = Lifetime::new("'cx", Span::call_site());
    let declares_cx = input.generics.lifetimes().any(|param| param.lifetime == cx);

    if let Some(ctx) = explicit {
        if ctx != cx {
            return Err(Error::new_spanned(
                ctx,
                "heap context lifetime must be named `'cx`",
            ));
        }
        if input.generics.lifetimes().next().is_some() && !declares_cx {
            return Err(Error::new_spanned(
                ctx,
                "trace lifetime is not declared on this type",
            ));
        }
        return Ok(ctx);
    }

    if declares_cx || input.generics.lifetimes().next().is_none() {
        return Ok(cx);
    }

    Err(Error::new_spanned(
        &input.ident,
        "heap context lifetime must be named `'cx`",
    ))
}

struct TraceField<'a> {
    ty: &'a Type,
}

fn trace_fields(data: &Data) -> Result<Vec<TraceField<'_>>> {
    match data {
        Data::Struct(data) => fields_to_trace(&data.fields),
        Data::Enum(data) => data
            .variants
            .iter()
            .map(|variant| fields_to_trace(&variant.fields))
            .collect::<Result<Vec<_>>>()
            .map(|fields| fields.into_iter().flatten().collect()),
        Data::Union(data) => Err(Error::new_spanned(
            data.union_token,
            "Trace cannot be derived for unions",
        )),
    }
}

fn fields_to_trace(fields: &Fields) -> Result<Vec<TraceField<'_>>> {
    fields
        .iter()
        .filter_map(|field| match field_skip(field) {
            Ok(true) => None,
            Ok(false) => Some(Ok(TraceField { ty: &field.ty })),
            Err(error) => Some(Err(error)),
        })
        .collect()
}

fn field_skip(field: &Field) -> Result<bool> {
    let mut skip_unchecked = false;
    for attr in trace_attrs(&field.attrs) {
        attr.parse_nested_meta(|meta| {
            if !meta.path.is_ident("skip_unchecked") {
                return Err(meta.error("unknown trace attribute"));
            }
            if skip_unchecked {
                return Err(meta.error("duplicate `trace(skip_unchecked)` attribute"));
            }
            skip_unchecked = true;
            Ok(())
        })?;
    }
    Ok(skip_unchecked)
}

fn trace_attrs(attrs: &[Attribute]) -> impl Iterator<Item = &Attribute> {
    attrs.iter().filter(|attr| attr.path().is_ident("trace"))
}

fn trace_body(data: &Data, heap_path: &Path) -> Result<TokenStream2> {
    match data {
        Data::Struct(data) => struct_body(data, heap_path),
        Data::Enum(data) => enum_body(data, heap_path),
        Data::Union(data) => Err(Error::new_spanned(
            data.union_token,
            "Trace cannot be derived for unions",
        )),
    }
}

fn struct_body(data: &DataStruct, heap_path: &Path) -> Result<TokenStream2> {
    match &data.fields {
        Fields::Named(fields) => fields
            .named
            .iter()
            .filter_map(|field| field_trace(field, heap_path).transpose())
            .collect::<Result<Vec<_>>>()
            .map(|fields| quote! { #(#fields)* }),
        Fields::Unnamed(fields) => fields
            .unnamed
            .iter()
            .enumerate()
            .filter_map(|(index, field)| match field_skip(field) {
                Ok(true) => None,
                Ok(false) => {
                    let index = syn::Index::from(index);
                    Some(Ok(
                        quote! { #heap_path::Trace::trace(&self.#index, visitor); },
                    ))
                }
                Err(error) => Some(Err(error)),
            })
            .collect::<Result<Vec<_>>>()
            .map(|fields| quote! { #(#fields)* }),
        Fields::Unit => Ok(TokenStream2::new()),
    }
}

fn field_trace(field: &Field, heap_path: &Path) -> Result<Option<TokenStream2>> {
    if field_skip(field)? {
        return Ok(None);
    }
    let ident = field.ident.as_ref().expect("named field has an ident");
    Ok(Some(
        quote! { #heap_path::Trace::trace(&self.#ident, visitor); },
    ))
}

fn enum_body(data: &DataEnum, heap_path: &Path) -> Result<TokenStream2> {
    let arms = data
        .variants
        .iter()
        .map(|variant| {
            let variant_ident = &variant.ident;
            let (pattern, traces) = enum_variant_pattern(&variant.fields, heap_path)?;
            Ok(quote! { Self::#variant_ident #pattern => { #(#traces)* } })
        })
        .collect::<Result<Vec<_>>>()?;
    Ok(quote! { match self { #(#arms,)* } })
}

fn enum_variant_pattern(
    fields: &Fields,
    heap_path: &Path,
) -> Result<(TokenStream2, Vec<TokenStream2>)> {
    match fields {
        Fields::Named(fields) => {
            let mut bindings = vec![];
            let mut traces = vec![];
            let mut has_skip = false;
            for (index, field) in fields.named.iter().enumerate() {
                let ident = field.ident.as_ref().expect("named field has an ident");
                if field_skip(field)? {
                    has_skip = true;
                } else {
                    let binding = format_ident!("field{index}");
                    bindings.push(quote! { #ident: #binding });
                    traces.push(quote! { #heap_path::Trace::trace(#binding, visitor); });
                }
            }
            let rest = has_skip.then(|| quote! { .. });
            Ok((quote! { { #(#bindings,)* #rest } }, traces))
        }
        Fields::Unnamed(fields) => {
            let mut patterns = vec![];
            let mut traces = vec![];
            for (index, field) in fields.unnamed.iter().enumerate() {
                if field_skip(field)? {
                    patterns.push(quote! { _ });
                } else {
                    let ident = format_ident!("field{index}");
                    patterns.push(quote! { #ident });
                    traces.push(quote! { #heap_path::Trace::trace(#ident, visitor); });
                }
            }
            Ok((quote! { ( #(#patterns,)* ) }, traces))
        }
        Fields::Unit => Ok((TokenStream2::new(), vec![])),
    }
}
