use crate::ast::{
    AnonymousContract, AnonymousContractParam, AnonymousContractRequirement, ArrayLen, ConstArg,
    ContractRef, FuncParam, GenericArg, Ident, ReturnSpec, Type, TypeVarId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct UnresolvedTypeRef {
    pub(crate) qualifier: Option<Ident>,
    pub(crate) name: Ident,
}

pub(crate) fn bare_type_name(ty: &Type) -> Option<Ident> {
    match ty {
        Type::UnresolvedName(name) => Some(*name),
        Type::UnresolvedNominal {
            qualifier: None,
            name,
            generic_args,
        } if generic_args.is_empty() => Some(*name),
        _ => None,
    }
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
            ArrayLen::Infer => {}
            ArrayLen::Named(_) => self.contains_unresolved_const = true,
            ArrayLen::Fixed(_) | ArrayLen::Param(_) => {}
        }
        false
    }
}

pub(crate) trait TypeFolder {
    fn fold_type(&mut self, ty: &Type) -> Type {
        match ty {
            Type::Infer => Type::Infer,
            Type::InferReturn => Type::InferReturn,
            Type::Any => Type::Any,
            Type::Int => Type::Int,
            Type::Float => Type::Float,
            Type::Bool => Type::Bool,
            Type::String => Type::String,
            Type::Void => Type::Void,
            Type::Func { params, ret } => Type::Func {
                params: params
                    .iter()
                    .map(|param| self.fold_func_param(param))
                    .collect(),
                ret: Box::new(self.fold_return_spec(ret)),
            },
            Type::Dyn(contract) => Type::Dyn(self.fold_contract_ref(contract)),
            Type::Var(id) => self.fold_var(*id),
            Type::UnresolvedName(name) => self.fold_unresolved_name(*name),
            Type::UnresolvedNominal {
                qualifier,
                name,
                generic_args,
            } => self.fold_unresolved_nominal(*qualifier, *name, generic_args),
            Type::Tuple(elems) => Type::Tuple(elems.iter().map(|ty| self.fold_type(ty)).collect()),
            Type::Nominal(nominal) => Type::nominal_with_origin(
                nominal.kind,
                nominal.name,
                nominal
                    .type_args
                    .iter()
                    .map(|ty| self.fold_type(ty))
                    .collect(),
                nominal
                    .const_args
                    .iter()
                    .map(|arg| self.fold_const_arg(arg))
                    .collect(),
                nominal.origin.clone(),
            ),
            Type::List { elem } => Type::List {
                elem: Box::new(self.fold_type(elem)),
            },
            Type::Array { elem, len } => Type::Array {
                elem: Box::new(self.fold_type(elem)),
                len: self.fold_array_len(*len),
            },
            Type::Map { key, value } => Type::Map {
                key: Box::new(self.fold_type(key)),
                value: Box::new(self.fold_type(value)),
            },
            Type::Slice { elem } => Type::Slice {
                elem: Box::new(self.fold_type(elem)),
            },
        }
    }

    fn fold_func_param(&mut self, param: &FuncParam) -> FuncParam {
        FuncParam::new(
            self.fold_type(&param.ty),
            param.mutable,
            param.cast_accept,
            param.escape,
        )
    }

    fn fold_return_spec(&mut self, ret: &ReturnSpec) -> ReturnSpec {
        ret.with_ty(self.fold_type(&ret.ty))
    }

    fn fold_generic_arg(&mut self, arg: &GenericArg) -> GenericArg {
        match arg {
            GenericArg::Type(ty) => GenericArg::Type(self.fold_type(ty)),
            GenericArg::Const(arg) => GenericArg::Const(self.fold_const_arg(arg)),
        }
    }

    fn fold_const_arg(&mut self, arg: &ConstArg) -> ConstArg {
        arg.clone()
    }

    fn fold_array_len(&mut self, len: ArrayLen) -> ArrayLen {
        len
    }

    fn fold_contract_ref(&mut self, contract: &ContractRef) -> ContractRef {
        let contract = match contract {
            ContractRef::Anonymous(surface) => ContractRef::Anonymous(AnonymousContract {
                requirements: surface
                    .requirements
                    .iter()
                    .map(|req| AnonymousContractRequirement {
                        receiver: req.receiver,
                        name: req.name,
                        params: req
                            .params
                            .iter()
                            .map(|param| AnonymousContractParam {
                                mutable: param.mutable,
                                escape: param.escape,
                                name: param.name,
                                ty: self.fold_type(&param.ty),
                            })
                            .collect(),
                        ret: self.fold_return_spec(&req.ret),
                    })
                    .collect(),
            }),
            ContractRef::Intersection(contracts) => ContractRef::Intersection(
                contracts
                    .iter()
                    .map(|c| self.fold_contract_ref(c))
                    .collect(),
            ),
            ContractRef::Named { .. } | ContractRef::Infer | ContractRef::Hole(_) => {
                contract.clone()
            }
        };
        self.fold_contract_ref_leaf(contract)
    }

    fn fold_contract_ref_leaf(&mut self, contract: ContractRef) -> ContractRef {
        contract
    }

    fn fold_var(&mut self, id: TypeVarId) -> Type {
        Type::Var(id)
    }

    fn fold_unresolved_name(&mut self, name: Ident) -> Type {
        Type::UnresolvedName(name)
    }

    fn fold_unresolved_nominal(
        &mut self,
        qualifier: Option<Ident>,
        name: Ident,
        generic_args: &[GenericArg],
    ) -> Type {
        self.fold_unresolved_nominal_default(qualifier, name, generic_args)
    }

    fn fold_unresolved_nominal_default(
        &mut self,
        qualifier: Option<Ident>,
        name: Ident,
        generic_args: &[GenericArg],
    ) -> Type {
        Type::UnresolvedNominal {
            qualifier,
            name,
            generic_args: generic_args
                .iter()
                .map(|arg| self.fold_generic_arg(arg))
                .collect(),
        }
    }
}

pub(crate) trait TypeVisitor {
    fn visit_type(&mut self, ty: &Type) -> bool {
        self.visit_type_leaf(ty) || self.visit_type_children(ty)
    }

    fn visit_type_leaf(&mut self, _ty: &Type) -> bool {
        false
    }

    fn visit_type_children(&mut self, ty: &Type) -> bool {
        match ty {
            Type::Func { params, ret } => {
                params.iter().any(|param| self.visit_func_param(param))
                    || self.visit_return_spec(ret)
            }
            Type::Dyn(contract) => self.visit_contract_ref(contract),
            Type::Tuple(elems) => elems.iter().any(|ty| self.visit_type(ty)),
            Type::Nominal(nominal) => {
                nominal.type_args.iter().any(|ty| self.visit_type(ty))
                    || nominal
                        .const_args
                        .iter()
                        .any(|arg| self.visit_const_arg(arg))
            }
            Type::List { elem } | Type::Slice { elem } => self.visit_type(elem),
            Type::Array { elem, len } => self.visit_type(elem) || self.visit_array_len(*len),
            Type::Map { key, value } => self.visit_type(key) || self.visit_type(value),
            Type::UnresolvedNominal { generic_args, .. } => {
                generic_args.iter().any(|arg| self.visit_generic_arg(arg))
            }
            Type::Infer
            | Type::InferReturn
            | Type::Any
            | Type::Int
            | Type::Float
            | Type::Bool
            | Type::String
            | Type::Void
            | Type::Var(_)
            | Type::UnresolvedName(_) => false,
        }
    }

    fn visit_contract_ref(&mut self, contract: &ContractRef) -> bool {
        self.visit_contract_ref_leaf(contract)
            || match contract {
                ContractRef::Anonymous(surface) => surface.requirements.iter().any(|req| {
                    req.params.iter().any(|param| self.visit_type(&param.ty))
                        || self.visit_return_spec(&req.ret)
                }),
                ContractRef::Intersection(contracts) => {
                    contracts.iter().any(|c| self.visit_contract_ref(c))
                }
                ContractRef::Named { .. } | ContractRef::Infer | ContractRef::Hole(_) => false,
            }
    }

    fn visit_contract_ref_leaf(&mut self, _contract: &ContractRef) -> bool {
        false
    }

    fn visit_func_param(&mut self, param: &FuncParam) -> bool {
        self.visit_type(&param.ty)
    }

    fn visit_return_spec(&mut self, ret: &ReturnSpec) -> bool {
        self.visit_type(&ret.ty)
    }

    fn visit_generic_arg(&mut self, arg: &GenericArg) -> bool {
        match arg {
            GenericArg::Type(ty) => self.visit_type(ty),
            GenericArg::Const(arg) => self.visit_const_arg(arg),
        }
    }

    fn visit_const_arg(&mut self, _arg: &ConstArg) -> bool {
        false
    }

    fn visit_array_len(&mut self, _len: ArrayLen) -> bool {
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
                agg.fields.values().any(|field| {
                    let field_ty = super::substitute_aggregate_member(ty, &agg.generics, &field.ty);
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
            let (type_subst, const_subst) = schema.generics.substitutions(&args);
            schema
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
