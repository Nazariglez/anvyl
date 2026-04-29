use crate::ast::{ArrayLen, ConstArg, FuncParam, GenericArg, Ident, Type, TypeVarId};

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

pub(crate) fn first_unresolved_type_ref(ty: &Type) -> Option<UnresolvedTypeRef> {
    match ty {
        Type::UnresolvedName(name) => Some(UnresolvedTypeRef {
            qualifier: None,
            name: *name,
        }),
        Type::UnresolvedNominal {
            qualifier,
            name,
            generic_args,
        } => Some(UnresolvedTypeRef {
            qualifier: *qualifier,
            name: *name,
        })
        .or_else(|| first_unresolved_generic_arg(generic_args)),
        Type::Func { params, ret } => params
            .iter()
            .find_map(|param| first_unresolved_type_ref(&param.ty))
            .or_else(|| first_unresolved_type_ref(ret)),
        Type::Tuple(elems) => elems.iter().find_map(first_unresolved_type_ref),
        Type::NamedTuple(fields) => fields
            .iter()
            .find_map(|(_, ty)| first_unresolved_type_ref(ty)),
        Type::Nominal(nominal) => nominal.type_args.iter().find_map(first_unresolved_type_ref),
        Type::List { elem } | Type::Slice { elem } | Type::Array { elem, .. } => {
            first_unresolved_type_ref(elem)
        }
        Type::Map { key, value } => {
            first_unresolved_type_ref(key).or_else(|| first_unresolved_type_ref(value))
        }
        Type::Infer
        | Type::Any
        | Type::Int
        | Type::Float
        | Type::Bool
        | Type::String
        | Type::Void
        | Type::Var(_) => None,
    }
}

pub(crate) fn type_contains_unresolved_ref(ty: &Type) -> bool {
    first_unresolved_type_ref(ty).is_some()
}

fn first_unresolved_generic_arg(args: &[GenericArg]) -> Option<UnresolvedTypeRef> {
    args.iter().find_map(|arg| match arg {
        GenericArg::Type(ty) => first_unresolved_type_ref(ty),
        GenericArg::Const(_) => None,
    })
}

pub(crate) trait TypeFolder {
    fn fold_type(&mut self, ty: &Type) -> Type {
        match ty {
            Type::Infer => Type::Infer,
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
                ret: Box::new(self.fold_type(ret)),
            },
            Type::Var(id) => self.fold_var(*id),
            Type::UnresolvedName(name) => self.fold_unresolved_name(*name),
            Type::UnresolvedNominal {
                qualifier,
                name,
                generic_args,
            } => self.fold_unresolved_nominal(*qualifier, *name, generic_args),
            Type::Tuple(elems) => Type::Tuple(elems.iter().map(|ty| self.fold_type(ty)).collect()),
            Type::NamedTuple(fields) => Type::NamedTuple(
                fields
                    .iter()
                    .map(|(name, ty)| (*name, self.fold_type(ty)))
                    .collect(),
            ),
            Type::Nominal(nominal) => Type::nominal(
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
        FuncParam::new(self.fold_type(&param.ty), param.mutable)
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
                params.iter().any(|param| self.visit_func_param(param)) || self.visit_type(ret)
            }
            Type::Tuple(elems) => elems.iter().any(|ty| self.visit_type(ty)),
            Type::NamedTuple(fields) => fields.iter().any(|(_, ty)| self.visit_type(ty)),
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

    fn visit_func_param(&mut self, param: &FuncParam) -> bool {
        self.visit_type(&param.ty)
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
