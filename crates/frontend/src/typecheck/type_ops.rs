use crate::ast::{ArrayLen, ConstArg, Ident, Type, TypeVisitor};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct UnresolvedTypeRef {
    pub(crate) qualifier: Option<Ident>,
    pub(crate) name: Ident,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct TypeClosureFacts {
    pub(crate) first_unresolved: Option<UnresolvedTypeRef>,
    pub(crate) infer: TypeInferFacts,
    pub(crate) contains_unresolved_const: bool,
    pub(crate) contains_any: bool,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct TypeInferFacts {
    pub(crate) contains_type: bool,
    pub(crate) contains_return: bool,
}

pub(crate) fn type_closure_facts(ty: &Type) -> TypeClosureFacts {
    let mut facts = TypeClosureFacts::default();
    facts.visit_type(ty);
    facts
}

pub(crate) fn type_depends_on_generics(ty: &Type) -> bool {
    let mut visitor = GenericDependencyVisitor;
    visitor.visit_type(ty)
}

pub(crate) fn type_has_unfinished_facts(ty: &Type) -> bool {
    let closure = type_closure_facts(ty);
    closure.first_unresolved.is_some()
        || closure.infer.contains_type
        || closure.infer.contains_return
        || closure.contains_unresolved_const
        || type_depends_on_generics(ty)
}

pub(super) fn contains_borrowed_slice_view(ty: &Type) -> bool {
    contains_stored_slice_view(ty)
}

pub(super) fn contains_stored_slice_view(ty: &Type) -> bool {
    contains_slice_view(ty, true)
}

pub(super) fn contains_nested_stored_slice_view(ty: &Type) -> bool {
    contains_slice_view(ty, false)
}

fn contains_slice_view(ty: &Type, stored: bool) -> bool {
    match ty {
        Type::Slice { .. } => stored,
        Type::Tuple(items) => items.iter().any(|ty| contains_slice_view(ty, stored)),
        Type::Nominal(nominal) => nominal
            .type_args
            .iter()
            .any(|ty| contains_slice_view(ty, stored)),
        Type::List { elem } | Type::Array { elem, .. } => contains_slice_view(elem, true),
        Type::Map { key, value } => {
            contains_slice_view(key, true) || contains_slice_view(value, true)
        }
        Type::Optional { inner } => contains_slice_view(inner, stored),
        Type::Func { .. }
        | Type::Infer
        | Type::InferReturn
        | Type::Any
        | Type::Int
        | Type::Float
        | Type::Bool
        | Type::String
        | Type::Char
        | Type::Void
        | Type::Dyn(_)
        | Type::Var(_)
        | Type::UnresolvedName(_)
        | Type::UnresolvedNominal { .. } => false,
    }
}

struct GenericDependencyVisitor;

impl TypeVisitor for GenericDependencyVisitor {
    fn visit_type_leaf(&mut self, ty: &Type) -> bool {
        matches!(ty, Type::Var(_))
    }

    fn visit_const_arg(&mut self, arg: &ConstArg) -> bool {
        matches!(arg, ConstArg::Param(_))
    }

    fn visit_array_len(&mut self, len: ArrayLen) -> bool {
        matches!(len, ArrayLen::Param(_))
    }
}

impl TypeVisitor for TypeClosureFacts {
    fn visit_type_leaf(&mut self, ty: &Type) -> bool {
        match ty {
            Type::UnresolvedName(name) => {
                self.first_unresolved.get_or_insert(UnresolvedTypeRef {
                    qualifier: None,
                    name: *name,
                });
            }
            Type::UnresolvedNominal {
                qualifier, name, ..
            } => {
                self.first_unresolved.get_or_insert(UnresolvedTypeRef {
                    qualifier: *qualifier,
                    name: *name,
                });
            }
            Type::Infer => self.infer.contains_type = true,
            Type::InferReturn => self.infer.contains_return = true,
            Type::Any => self.contains_any = true,
            _ => {}
        }
        false
    }

    fn visit_const_arg(&mut self, arg: &ConstArg) -> bool {
        if matches!(arg, ConstArg::Name(_)) {
            self.contains_unresolved_const = true;
        }
        false
    }

    fn visit_array_len(&mut self, len: ArrayLen) -> bool {
        match len {
            ArrayLen::Named(_) => self.contains_unresolved_const = true,
            ArrayLen::Infer | ArrayLen::Fixed(_) | ArrayLen::Param(_) | ArrayLen::Expr(_) => {}
        }
        false
    }
}

pub(super) fn type_contains_dyn_value(
    ty: &Type,
    decls: &super::DeclarationIndex,
    seen: &mut std::collections::HashSet<super::NominalKey>,
) -> bool {
    match ty {
        Type::Dyn(_) => true,
        Type::Tuple(elems) => elems
            .iter()
            .any(|elem| type_contains_dyn_value(elem, decls, seen)),
        Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem } => {
            type_contains_dyn_value(elem, decls, seen)
        }
        Type::Optional { inner } => type_contains_dyn_value(inner, decls, seen),
        Type::Map { key, value } => {
            type_contains_dyn_value(key, decls, seen) || type_contains_dyn_value(value, decls, seen)
        }
        Type::Nominal(_) => nominal_contains_dyn_value(ty, decls, seen),
        Type::Func { .. }
        | Type::Infer
        | Type::InferReturn
        | Type::Any
        | Type::Int
        | Type::Float
        | Type::Bool
        | Type::String
        | Type::Char
        | Type::Void
        | Type::Var(_)
        | Type::UnresolvedName(_)
        | Type::UnresolvedNominal { .. } => false,
    }
}

fn nominal_contains_dyn_value(
    ty: &Type,
    decls: &super::DeclarationIndex,
    seen: &mut std::collections::HashSet<super::NominalKey>,
) -> bool {
    let Some(key) = decls.key_for_type(ty) else {
        return false;
    };
    if !seen.insert(key.clone()) {
        return false;
    }
    let contains = match key.kind {
        super::NominalKind::Struct | super::NominalKind::DataRef => {
            decls.aggregate(&key).is_some_and(|agg| {
                let generics = agg.all_generics();
                agg.fields.values().any(|field| {
                    let field_ty = super::substitute_aggregate_member(ty, &generics, &field.ty);
                    type_contains_dyn_value(&field_ty, decls, seen)
                })
            })
        }
        super::NominalKind::Enum => decls.enum_schema(&key).is_some_and(|schema| {
            let Some(nominal) = ty.as_nominal() else {
                return false;
            };
            let args = super::GenericArgs {
                type_args: nominal.type_args.clone(),
                const_args: super::ConstTerm::from_args(&nominal.const_args),
            };
            let (type_subst, const_subst) = schema.all_generics().substitutions(&args);
            schema
                .body
                .variants
                .values()
                .any(|variant| match &variant.payload {
                    super::VariantPayload::Unit => false,
                    super::VariantPayload::Tuple(types) => types.iter().any(|ty| {
                        let ty = super::substitute(ty, &type_subst, &const_subst);
                        type_contains_dyn_value(&ty, decls, seen)
                    }),
                    super::VariantPayload::Struct(fields) => fields.values().any(|field| {
                        let ty = super::substitute(&field.ty, &type_subst, &const_subst);
                        type_contains_dyn_value(&ty, decls, seen)
                    }),
                })
        }),
        super::NominalKind::Extern => false,
    };
    seen.remove(&key);
    contains
}
