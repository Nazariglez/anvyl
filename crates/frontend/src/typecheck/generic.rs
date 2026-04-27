use std::collections::HashMap;

use super::{
    const_term::ConstTerm,
    decls::CallableId,
    type_ops::{TypeFolder, TypeVisitor},
};
use crate::{
    ast::{
        ArrayLen, ConstArg, ConstParam, ConstParamId, ConstValue, ExprId, GenericArg, Ident, Type,
        TypeParam, TypeVarId,
    },
    span::Span,
};

pub(crate) type TypeSubst = HashMap<TypeVarId, Type>;
pub(crate) type ConstSubst = HashMap<ConstParamId, ConstTerm>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ArityError {
    TypeArgs { expected: usize, found: usize },
    ConstArgs { expected: usize, found: usize },
    NotGeneric,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct GenericParams {
    pub(crate) type_params: Vec<TypeParam>,
    pub(crate) const_params: Vec<ConstParam>,
}

impl GenericParams {
    pub(crate) fn is_empty(&self) -> bool {
        self.type_params.is_empty() && self.const_params.is_empty()
    }

    pub(crate) fn substitutions(&self, args: &GenericArgs) -> (TypeSubst, ConstSubst) {
        let types = self
            .type_params
            .iter()
            .zip(&args.type_args)
            .map(|(param, ty)| (param.id, ty.clone()))
            .collect();
        let consts = self
            .const_params
            .iter()
            .zip(&args.const_args)
            .map(|(param, term)| (param.id, term.clone()))
            .collect();
        (types, consts)
    }

    pub(crate) fn contains_param(&self, ty: &Type) -> bool {
        ContainsGenericParam { generics: self }.visit_type(ty)
    }

    pub(crate) fn validate_explicit_args(&self, args: &GenericArgs) -> Result<(), ArityError> {
        let non_generic_with_args = self.is_empty() && !args.is_empty();
        if non_generic_with_args {
            return Err(ArityError::NotGeneric);
        }
        if args.type_args.len() != self.type_params.len() {
            return Err(ArityError::TypeArgs {
                expected: self.type_params.len(),
                found: args.type_args.len(),
            });
        }
        if args.const_args.len() != self.const_params.len() {
            return Err(ArityError::ConstArgs {
                expected: self.const_params.len(),
                found: args.const_args.len(),
            });
        }
        Ok(())
    }
}

struct ContainsGenericParam<'a> {
    generics: &'a GenericParams,
}

impl TypeVisitor for ContainsGenericParam<'_> {
    fn visit_type_leaf(&mut self, ty: &Type) -> bool {
        match ty {
            Type::Var(id) => self
                .generics
                .type_params
                .iter()
                .any(|param| param.id == *id),
            Type::UnresolvedName(name) => self
                .generics
                .type_params
                .iter()
                .any(|param| param.name == *name),
            Type::UnresolvedNominal {
                qualifier, name, ..
            } => {
                qualifier.is_none()
                    && self
                        .generics
                        .type_params
                        .iter()
                        .any(|param| param.name == *name)
            }
            _ => false,
        }
    }

    fn visit_const_arg(&mut self, arg: &ConstArg) -> bool {
        matches!(arg, ConstArg::Param(id) if self.generics.const_params.iter().any(|param| param.id == *id))
    }

    fn visit_array_len(&mut self, len: ArrayLen) -> bool {
        matches!(len, ArrayLen::Param(id) if self.generics.const_params.iter().any(|param| param.id == id))
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub(crate) struct GenericArgs {
    pub(crate) type_args: Vec<Type>,
    pub(crate) const_args: Vec<ConstTerm>,
}

impl GenericArgs {
    pub(crate) fn empty() -> Self {
        Self::default()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.type_args.is_empty() && self.const_args.is_empty()
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct Inference {
    types: TypeSubst,
    consts: ConstSubst,
}

impl Inference {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn bind_type(&mut self, id: TypeVarId, ty: Type) -> bool {
        match self.types.get(&id) {
            Some(existing) if existing != &ty => return false,
            Some(_) => {}
            None => {
                self.types.insert(id, ty);
            }
        }
        true
    }

    pub(crate) fn bind_const(&mut self, id: ConstParamId, term: ConstTerm) -> bool {
        match self.consts.get(&id) {
            Some(existing) if existing != &term => return false,
            Some(_) => {}
            None => {
                self.consts.insert(id, term);
            }
        }
        true
    }

    pub(crate) fn type_subst(&self) -> &TypeSubst {
        &self.types
    }

    pub(crate) fn const_subst(&self) -> &ConstSubst {
        &self.consts
    }

    pub(crate) fn is_complete(&self, params: &GenericParams) -> bool {
        let is_type_complete = params
            .type_params
            .iter()
            .all(|p| self.types.contains_key(&p.id));
        let is_const_complete = params
            .const_params
            .iter()
            .all(|p| self.consts.contains_key(&p.id));
        is_type_complete && is_const_complete
    }

    pub(crate) fn unbound(&self, params: &GenericParams) -> Vec<Ident> {
        let mut names = Vec::new();
        for p in &params.type_params {
            if !self.types.contains_key(&p.id) {
                names.push(p.name);
            }
        }
        for p in &params.const_params {
            if !self.consts.contains_key(&p.id) {
                names.push(p.name);
            }
        }
        names
    }

    pub(crate) fn into_args(self, params: &GenericParams) -> Result<GenericArgs, Vec<Ident>> {
        let type_args = params
            .type_params
            .iter()
            .map(|p| self.types.get(&p.id).cloned())
            .collect::<Option<Vec<_>>>();
        let const_args = params
            .const_params
            .iter()
            .map(|p| self.consts.get(&p.id).cloned())
            .collect::<Option<Vec<_>>>();
        match (type_args, const_args) {
            (Some(type_args), Some(const_args)) => Ok(GenericArgs {
                type_args,
                const_args,
            }),
            _ => Err(self.unbound(params)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct SpecializationKey {
    pub(crate) target: CallableId,
    pub(crate) args: GenericArgs,
}

pub(crate) type SpecializedBodyTypes = HashMap<ExprId, (Span, Type)>;

#[derive(Clone)]
pub(crate) enum SpecializationState {
    InProgress,
    Done(SpecializedBodyTypes),
}

fn infer_all<'a>(
    pairs: impl IntoIterator<Item = (&'a Type, &'a Type)>,
    inf: &mut Inference,
) -> bool {
    pairs
        .into_iter()
        .all(|(template, concrete)| infer(template, concrete, inf))
}

fn infer_len(template: &ArrayLen, concrete: &ArrayLen, inf: &mut Inference) -> bool {
    match template {
        ArrayLen::Param(id) => match concrete {
            ArrayLen::Infer => false,
            _ => inf.bind_const(*id, ConstTerm::from_array_len(*concrete)),
        },
        _ => template == concrete,
    }
}

fn infer_const_arg(template: &ConstArg, concrete: &ConstArg, inf: &mut Inference) -> bool {
    match template {
        ConstArg::Param(id) => inf.bind_const(*id, ConstTerm::from_arg(concrete)),
        _ => template == concrete,
    }
}

fn infer_const_args(template: &[ConstArg], concrete: &[ConstArg], inf: &mut Inference) -> bool {
    template.len() == concrete.len()
        && template
            .iter()
            .zip(concrete)
            .all(|(template, concrete)| infer_const_arg(template, concrete, inf))
}

fn infer_generic_args(
    template: &[GenericArg],
    concrete: &[GenericArg],
    inf: &mut Inference,
) -> bool {
    template.len() == concrete.len()
        && template
            .iter()
            .zip(concrete)
            .all(|(template, concrete)| match (template, concrete) {
                (GenericArg::Type(template), GenericArg::Type(concrete)) => {
                    infer(template, concrete, inf)
                }
                (GenericArg::Const(template), GenericArg::Const(concrete)) => {
                    infer_const_arg(template, concrete, inf)
                }
                _ => false,
            })
}

pub(crate) fn infer(template: &Type, concrete: &Type, inf: &mut Inference) -> bool {
    match (template, concrete) {
        (Type::Var(id), ty) => inf.bind_type(*id, ty.clone()),
        (Type::Array { elem, len }, Type::Array { elem: ce, len: cl }) => {
            infer(elem, ce, inf) && infer_len(len, cl, inf)
        }
        (
            Type::Func { params, ret },
            Type::Func {
                params: concrete_params,
                ret: concrete_ret,
            },
        ) => {
            params.len() == concrete_params.len()
                && infer_all(
                    params
                        .iter()
                        .map(|param| &param.ty)
                        .zip(concrete_params.iter().map(|param| &param.ty)),
                    inf,
                )
                && infer(ret, concrete_ret, inf)
        }
        (Type::Tuple(a), Type::Tuple(b)) => a.len() == b.len() && infer_all(a.iter().zip(b), inf),
        (Type::NamedTuple(a), Type::NamedTuple(b)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b)
                    .all(|((an, at), (bn, bt))| an == bn && infer(at, bt, inf))
        }
        (Type::Nominal(a), Type::Nominal(b)) => {
            a.kind == b.kind
                && a.name == b.name
                && a.origin == b.origin
                && a.type_args.len() == b.type_args.len()
                && infer_all(a.type_args.iter().zip(&b.type_args), inf)
                && infer_const_args(&a.const_args, &b.const_args, inf)
        }
        (
            Type::UnresolvedNominal {
                qualifier,
                name,
                generic_args,
            },
            Type::UnresolvedNominal {
                qualifier: concrete_qualifier,
                name: concrete_name,
                generic_args: concrete_args,
            },
        ) => {
            qualifier == concrete_qualifier
                && name == concrete_name
                && infer_generic_args(generic_args, concrete_args, inf)
        }
        (
            Type::List { elem },
            Type::List {
                elem: concrete_elem,
            },
        )
        | (
            Type::Slice { elem },
            Type::Slice {
                elem: concrete_elem,
            },
        ) => infer(elem, concrete_elem, inf),
        (
            Type::Map { key, value },
            Type::Map {
                key: concrete_key,
                value: concrete_value,
            },
        ) => infer(key, concrete_key, inf) && infer(value, concrete_value, inf),
        (
            Type::Infer
            | Type::Any
            | Type::Int
            | Type::Float
            | Type::Bool
            | Type::String
            | Type::Void
            | Type::UnresolvedName(_),
            other,
        ) => template == other,
        _ => false,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Specificity {
    MoreSpecific,
    LessSpecific,
    Equal,
    Incomparable,
}

#[derive(Default)]
struct Cover<'a> {
    types: HashMap<TypeVarId, &'a Type>,
    consts: HashMap<ConstParamId, ConstTerm>,
}

pub(crate) fn compare_specificity(a: &Type, b: &Type) -> Specificity {
    let a_subset_b = covers(b, a);
    let b_subset_a = covers(a, b);
    match (a_subset_b, b_subset_a) {
        (true, false) => Specificity::MoreSpecific,
        (false, true) => Specificity::LessSpecific,
        (true, true) => Specificity::Equal,
        (false, false) => Specificity::Incomparable,
    }
}

fn covers(general: &Type, specific: &Type) -> bool {
    covers_type(general, specific, &mut Cover::default())
}

fn covers_type<'a>(general: &Type, specific: &'a Type, cover: &mut Cover<'a>) -> bool {
    match (general, specific) {
        (Type::Var(id), ty) => match cover.types.get(id) {
            Some(bound) => *bound == ty,
            None => cover.types.insert(*id, ty).is_none(),
        },
        (Type::Array { elem, len }, Type::Array { elem: se, len: sl }) => {
            covers_type(elem, se, cover) && covers_len(*len, *sl, cover)
        }
        (
            Type::Func { params, ret },
            Type::Func {
                params: specific_params,
                ret: specific_ret,
            },
        ) => {
            params.len() == specific_params.len()
                && params
                    .iter()
                    .zip(specific_params)
                    .all(|(a, b)| covers_type(&a.ty, &b.ty, cover))
                && covers_type(ret, specific_ret, cover)
        }
        (Type::Tuple(a), Type::Tuple(b)) => covers_types(a, b, cover),
        (Type::NamedTuple(a), Type::NamedTuple(b)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b)
                    .all(|((an, at), (bn, bt))| an == bn && covers_type(at, bt, cover))
        }
        (Type::Nominal(a), Type::Nominal(b)) => {
            a.kind == b.kind
                && a.name == b.name
                && a.origin == b.origin
                && covers_types(&a.type_args, &b.type_args, cover)
                && covers_const_args(&a.const_args, &b.const_args, cover)
        }
        (
            Type::UnresolvedNominal {
                qualifier,
                name,
                generic_args,
            },
            Type::UnresolvedNominal {
                qualifier: specific_qualifier,
                name: specific_name,
                generic_args: specific_args,
            },
        ) => {
            qualifier == specific_qualifier
                && name == specific_name
                && covers_generic_args(generic_args, specific_args, cover)
        }
        (
            Type::List { elem },
            Type::List {
                elem: specific_elem,
            },
        )
        | (
            Type::Slice { elem },
            Type::Slice {
                elem: specific_elem,
            },
        ) => covers_type(elem, specific_elem, cover),
        (
            Type::Map { key, value },
            Type::Map {
                key: specific_key,
                value: specific_value,
            },
        ) => covers_type(key, specific_key, cover) && covers_type(value, specific_value, cover),
        _ => general == specific,
    }
}

fn covers_types<'a>(general: &[Type], specific: &'a [Type], cover: &mut Cover<'a>) -> bool {
    general.len() == specific.len()
        && general
            .iter()
            .zip(specific)
            .all(|(general, specific)| covers_type(general, specific, cover))
}

fn covers_generic_args<'a>(
    general: &[GenericArg],
    specific: &'a [GenericArg],
    cover: &mut Cover<'a>,
) -> bool {
    general.len() == specific.len()
        && general
            .iter()
            .zip(specific)
            .all(|(general, specific)| match (general, specific) {
                (GenericArg::Type(general), GenericArg::Type(specific)) => {
                    covers_type(general, specific, cover)
                }
                (GenericArg::Const(general), GenericArg::Const(specific)) => {
                    covers_const_arg(general, specific, cover)
                }
                _ => false,
            })
}

fn covers_len(general: ArrayLen, specific: ArrayLen, cover: &mut Cover<'_>) -> bool {
    match general {
        ArrayLen::Param(id) => cover_const(id, const_term_len(specific), cover),
        _ => const_term_len(general) == const_term_len(specific),
    }
}

fn covers_const_args<'a>(
    general: &[ConstArg],
    specific: &'a [ConstArg],
    cover: &mut Cover<'a>,
) -> bool {
    general.len() == specific.len()
        && general
            .iter()
            .zip(specific)
            .all(|(general, specific)| covers_const_arg(general, specific, cover))
}

fn covers_const_arg(general: &ConstArg, specific: &ConstArg, cover: &mut Cover<'_>) -> bool {
    match general {
        ConstArg::Param(id) => cover_const(*id, const_term_arg(specific), cover),
        _ => const_term_arg(general) == const_term_arg(specific),
    }
}

fn cover_const(id: ConstParamId, term: ConstTerm, cover: &mut Cover<'_>) -> bool {
    match cover.consts.get(&id) {
        Some(bound) => *bound == term,
        None => cover.consts.insert(id, term).is_none(),
    }
}

fn const_term_len(len: ArrayLen) -> ConstTerm {
    ConstTerm::from_array_len(len)
}

fn const_term_arg(arg: &ConstArg) -> ConstTerm {
    ConstTerm::from_arg(arg)
}

struct Substituter<'a> {
    types: &'a TypeSubst,
    consts: &'a ConstSubst,
}

impl TypeFolder for Substituter<'_> {
    fn fold_var(&mut self, id: TypeVarId) -> Type {
        self.types.get(&id).cloned().unwrap_or(Type::Var(id))
    }

    fn fold_const_arg(&mut self, arg: &ConstArg) -> ConstArg {
        match arg {
            ConstArg::Param(id) => self
                .consts
                .get(id)
                .and_then(ConstTerm::to_arg_no_infer)
                .unwrap_or_else(|| arg.clone()),
            ConstArg::Value(_) | ConstArg::Name(_) => arg.clone(),
        }
    }

    fn fold_array_len(&mut self, len: ArrayLen) -> ArrayLen {
        match len {
            ArrayLen::Param(id) => self
                .consts
                .get(&id)
                .and_then(ConstTerm::to_array_len_no_infer)
                .unwrap_or(ArrayLen::Param(id)),
            other => other,
        }
    }
}

pub(crate) fn substitute(ty: &Type, ts: &TypeSubst, cs: &ConstSubst) -> Type {
    Substituter {
        types: ts,
        consts: cs,
    }
    .fold_type(ty)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{FuncParam, Ident, NominalKind};

    fn tv(id: u32) -> TypeVarId {
        TypeVarId(id)
    }

    fn cp(id: u32) -> ConstParamId {
        ConstParamId(id)
    }

    fn array_ty(elem: Type, len: ArrayLen) -> Type {
        Type::Array {
            elem: Box::new(elem),
            len,
        }
    }

    fn carg(n: i64) -> ConstArg {
        ConstArg::Value(ConstValue::Int(n))
    }

    fn cterm(n: usize) -> ConstTerm {
        ConstTerm::from_usize(n)
    }

    fn nominal(
        kind: NominalKind,
        name: &str,
        type_args: Vec<Type>,
        const_args: Vec<ConstArg>,
    ) -> Type {
        Type::nominal(kind, Ident::new(name), type_args, const_args, None)
    }

    fn struct_ty(name: &str, type_args: Vec<Type>) -> Type {
        nominal(NominalKind::Struct, name, type_args, vec![])
    }

    fn struct_const(name: &str, type_args: Vec<Type>, const_args: Vec<ConstArg>) -> Type {
        nominal(NominalKind::Struct, name, type_args, const_args)
    }

    #[test]
    fn substitute_type_var() {
        let ts = HashMap::from([(tv(0), Type::Int)]);
        let result = substitute(&Type::Var(tv(0)), &ts, &HashMap::new());
        assert_eq!(result, Type::Int);
    }

    #[test]
    fn substitute_type_var_unbound() {
        let result = substitute(&Type::Var(tv(99)), &HashMap::new(), &HashMap::new());
        assert_eq!(result, Type::Var(tv(99)));
    }

    #[test]
    fn array_const_param() {
        let cs = HashMap::from([(cp(0), cterm(4))]);
        let ty = array_ty(Type::Int, ArrayLen::Param(cp(0)));
        let result = substitute(&ty, &HashMap::new(), &cs);
        assert_eq!(result, array_ty(Type::Int, ArrayLen::Fixed(4)));
    }

    #[test]
    fn type_and_const() {
        let ts = HashMap::from([(tv(0), Type::String)]);
        let cs = HashMap::from([(cp(1), cterm(3))]);
        let ty = array_ty(Type::Var(tv(0)), ArrayLen::Param(cp(1)));
        let result = substitute(&ty, &ts, &cs);
        assert_eq!(result, array_ty(Type::String, ArrayLen::Fixed(3)));
    }

    #[test]
    fn substitute_nominal_const_param_to_bool() {
        let cs = HashMap::from([(cp(0), ConstTerm::Value(ConstValue::Bool(true)))]);
        let ty = struct_const("Flag", vec![], vec![ConstArg::Param(cp(0))]);
        let result = substitute(&ty, &HashMap::new(), &cs);
        assert_eq!(
            result,
            struct_const(
                "Flag",
                vec![],
                vec![ConstArg::Value(ConstValue::Bool(true))],
            ),
        );
    }

    #[test]
    fn substitute_array_const_param_to_name() {
        let name = Ident::new("N");
        let cs = HashMap::from([(cp(0), ConstTerm::Name(name))]);
        let ty = array_ty(Type::Int, ArrayLen::Param(cp(0)));
        let result = substitute(&ty, &HashMap::new(), &cs);
        assert_eq!(result, array_ty(Type::Int, ArrayLen::Named(name)));
    }

    #[test]
    fn substitute_array_const_param_rejects_non_int_term_without_infer() {
        let cs = HashMap::from([(cp(0), ConstTerm::Value(ConstValue::Bool(true)))]);
        let ty = array_ty(Type::Int, ArrayLen::Param(cp(0)));
        let result = substitute(&ty, &HashMap::new(), &cs);
        assert_eq!(result, array_ty(Type::Int, ArrayLen::Param(cp(0))));
    }

    #[test]
    fn substitute_func_type() {
        let ts = HashMap::from([(tv(0), Type::Int), (tv(1), Type::Bool)]);
        let ty = Type::Func {
            params: vec![FuncParam::new(Type::Var(tv(0)), false)],
            ret: Box::new(Type::Var(tv(1))),
        };
        let result = substitute(&ty, &ts, &HashMap::new());
        assert_eq!(
            result,
            Type::Func {
                params: vec![FuncParam::new(Type::Int, false)],
                ret: Box::new(Type::Bool),
            }
        );
    }

    #[test]
    fn substitute_nested_struct() {
        let ts = HashMap::from([(tv(0), Type::Int)]);
        let ty = struct_ty(
            "Wrapper",
            vec![Type::Tuple(vec![Type::Var(tv(0)), Type::String])],
        );
        let result = substitute(&ty, &ts, &HashMap::new());
        assert_eq!(
            result,
            struct_ty("Wrapper", vec![Type::Tuple(vec![Type::Int, Type::String])])
        );
    }

    #[test]
    fn substitute_unresolved_nominal_args() {
        let ts = HashMap::from([(tv(0), Type::Int)]);
        let cs = HashMap::from([(cp(1), cterm(4))]);
        let ty = Type::UnresolvedNominal {
            qualifier: None,
            name: Ident::new("Box"),
            generic_args: vec![
                GenericArg::Type(Type::Var(tv(0))),
                GenericArg::Const(ConstArg::Param(cp(1))),
            ],
        };
        let result = substitute(&ty, &ts, &cs);

        assert_eq!(
            result,
            Type::UnresolvedNominal {
                qualifier: None,
                name: Ident::new("Box"),
                generic_args: vec![GenericArg::Type(Type::Int), GenericArg::Const(carg(4)),],
            }
        );
    }

    #[test]
    fn primitives_unchanged() {
        for ty in [Type::Int, Type::Float, Type::Bool, Type::String, Type::Void] {
            assert_eq!(substitute(&ty, &HashMap::new(), &HashMap::new()), ty);
        }
    }

    fn tp(id: u32, name: &str) -> TypeParam {
        TypeParam {
            name: Ident::new(name),
            id: tv(id),
        }
    }

    fn cparam(id: u32, name: &str) -> ConstParam {
        ConstParam {
            name: Ident::new(name),
            id: cp(id),
        }
    }

    fn params() -> GenericParams {
        GenericParams {
            type_params: vec![tp(0, "T")],
            const_params: vec![cparam(1, "N")],
        }
    }

    #[test]
    fn contains_param_in_unresolved_nominal_type_arg() {
        let ty = Type::UnresolvedNominal {
            qualifier: None,
            name: Ident::new("Foo"),
            generic_args: vec![GenericArg::Type(Type::UnresolvedName(Ident::new("T")))],
        };
        assert!(params().contains_param(&ty));
    }

    #[test]
    fn contains_param_in_unresolved_nominal_const_arg() {
        let ty = Type::UnresolvedNominal {
            qualifier: None,
            name: Ident::new("Foo"),
            generic_args: vec![GenericArg::Const(ConstArg::Param(cp(1)))],
        };
        assert!(params().contains_param(&ty));
    }

    #[test]
    fn contains_param_in_array_len() {
        let ty = array_ty(Type::Int, ArrayLen::Param(cp(1)));
        assert!(params().contains_param(&ty));
    }

    #[test]
    fn contains_param_ignores_unrelated_types() {
        assert!(!params().contains_param(&Type::Int));
        assert!(!params().contains_param(&struct_ty("Foo", vec![Type::String])));
    }

    #[test]
    fn bind_type_ok() {
        let mut inf = Inference::new();
        assert!(inf.bind_type(tv(0), Type::Int));
        assert!(inf.bind_type(tv(0), Type::Int));
        assert!(inf.bind_type(tv(1), Type::String));
        assert_eq!(inf.type_subst().len(), 2);
    }

    #[test]
    fn bind_type_conflict() {
        let mut inf = Inference::new();
        assert!(inf.bind_type(tv(0), Type::Int));
        assert!(!inf.bind_type(tv(0), Type::String));
        assert_eq!(inf.type_subst()[&tv(0)], Type::Int);
    }

    #[test]
    fn bind_const_ok() {
        let mut inf = Inference::new();
        assert!(inf.bind_const(cp(0), cterm(4)));
        assert!(inf.bind_const(cp(0), cterm(4)));
        assert!(inf.bind_const(cp(1), cterm(8)));
        assert_eq!(inf.const_subst().len(), 2);
    }

    #[test]
    fn bind_const_conflict() {
        let mut inf = Inference::new();
        assert!(inf.bind_const(cp(0), cterm(4)));
        assert!(!inf.bind_const(cp(0), cterm(8)));
        assert_eq!(inf.const_subst()[&cp(0)], cterm(4));
    }

    #[test]
    fn inference_is_complete() {
        let params = GenericParams {
            type_params: vec![tp(0, "T")],
            const_params: vec![cparam(1, "N")],
        };
        let mut inf = Inference::new();
        assert!(!inf.is_complete(&params));
        inf.bind_type(tv(0), Type::Int);
        assert!(!inf.is_complete(&params));
        inf.bind_const(cp(1), cterm(3));
        assert!(inf.is_complete(&params));
    }

    #[test]
    fn inference_unbound() {
        let params = GenericParams {
            type_params: vec![tp(0, "T"), tp(1, "U")],
            const_params: vec![cparam(2, "N")],
        };
        let mut inf = Inference::new();
        inf.bind_type(tv(0), Type::Int);
        assert_eq!(inf.unbound(&params), vec![Ident::new("U"), Ident::new("N")]);
    }

    #[test]
    fn into_args_ok() {
        let params = GenericParams {
            type_params: vec![tp(0, "T"), tp(1, "U")],
            const_params: vec![cparam(2, "N")],
        };
        let mut inf = Inference::new();
        inf.bind_type(tv(0), Type::Int);
        inf.bind_type(tv(1), Type::String);
        inf.bind_const(cp(2), cterm(5));
        let args = inf.into_args(&params).unwrap();
        assert_eq!(
            args,
            GenericArgs {
                type_args: vec![Type::Int, Type::String],
                const_args: vec![cterm(5)],
            }
        );
    }

    #[test]
    fn into_args_incomplete() {
        let params = GenericParams {
            type_params: vec![tp(0, "T")],
            const_params: vec![cparam(1, "N")],
        };
        let mut inf = Inference::new();
        inf.bind_type(tv(0), Type::Int);
        let Err(unbound) = inf.into_args(&params) else {
            panic!("expected Err");
        };
        assert_eq!(unbound, vec![Ident::new("N")]);
    }

    #[test]
    fn infer_type_var() {
        let mut inf = Inference::new();
        assert!(infer(&Type::Var(tv(0)), &Type::Int, &mut inf));
        assert_eq!(inf.type_subst()[&tv(0)], Type::Int);
    }

    #[test]
    fn infer_type_var_conflict() {
        let mut inf = Inference::new();
        assert!(infer(&Type::Var(tv(0)), &Type::Int, &mut inf));
        assert!(!infer(&Type::Var(tv(0)), &Type::String, &mut inf));
    }

    #[test]
    fn infer_const_arg_terms() {
        let mut inf = Inference::new();
        assert!(infer_const_arg(
            &ConstArg::Param(cp(0)),
            &ConstArg::Value(ConstValue::Bool(true)),
            &mut inf,
        ));
        assert_eq!(
            inf.const_subst()[&cp(0)],
            ConstTerm::Value(ConstValue::Bool(true)),
        );

        let mut inf = Inference::new();
        let name = Ident::new("N");
        assert!(infer_const_arg(
            &ConstArg::Param(cp(0)),
            &ConstArg::Name(name),
            &mut inf,
        ));
        assert_eq!(inf.const_subst()[&cp(0)], ConstTerm::Name(name));
    }

    #[test]
    fn infer_const_arg_conflicting_terms_fail() {
        let mut inf = Inference::new();
        assert!(infer_const_arg(&ConstArg::Param(cp(0)), &carg(1), &mut inf));
        assert!(!infer_const_arg(
            &ConstArg::Param(cp(0)),
            &ConstArg::Value(ConstValue::Bool(true)),
            &mut inf,
        ));
    }

    #[test]
    fn infer_array_len_infer_does_not_bind_const_param() {
        let mut inf = Inference::new();
        assert!(!infer_len(
            &ArrayLen::Param(cp(0)),
            &ArrayLen::Infer,
            &mut inf,
        ));
        assert!(inf.const_subst().is_empty());
    }

    #[test]
    fn infer_struct_type_args() {
        let mut inf = Inference::new();
        let tmpl = struct_ty("Box", vec![Type::Var(tv(0))]);
        let concrete = struct_ty("Box", vec![Type::String]);
        assert!(infer(&tmpl, &concrete, &mut inf));
        assert_eq!(inf.type_subst()[&tv(0)], Type::String);
    }

    #[test]
    fn infer_struct_name_mismatch() {
        let mut inf = Inference::new();
        let tmpl = struct_ty("Box", vec![Type::Var(tv(0))]);
        let concrete = struct_ty("Wrapper", vec![Type::Int]);
        assert!(!infer(&tmpl, &concrete, &mut inf));
    }

    #[test]
    fn infer_tuple_multi() {
        let mut inf = Inference::new();
        let tmpl = Type::Tuple(vec![Type::Var(tv(0)), Type::Var(tv(1))]);
        let concrete = Type::Tuple(vec![Type::Int, Type::String]);
        assert!(infer(&tmpl, &concrete, &mut inf));
        assert_eq!(inf.type_subst()[&tv(0)], Type::Int);
        assert_eq!(inf.type_subst()[&tv(1)], Type::String);
    }

    #[test]
    fn infer_tuple_arity_mismatch() {
        let mut inf = Inference::new();
        let tmpl = Type::Tuple(vec![Type::Var(tv(0))]);
        let concrete = Type::Tuple(vec![Type::Int, Type::String]);
        assert!(!infer(&tmpl, &concrete, &mut inf));
    }

    #[test]
    fn array_param_const() {
        let mut inf = Inference::new();
        let tmpl = array_ty(Type::Var(tv(0)), ArrayLen::Param(cp(1)));
        let concrete = array_ty(Type::Int, ArrayLen::Fixed(3));
        assert!(infer(&tmpl, &concrete, &mut inf));
        assert_eq!(inf.type_subst()[&tv(0)], Type::Int);
        assert_eq!(inf.const_subst()[&cp(1)], cterm(3));
    }

    #[test]
    fn infer_array_fixed_match() {
        let mut inf = Inference::new();
        let tmpl = array_ty(Type::Int, ArrayLen::Fixed(4));
        let concrete = array_ty(Type::Int, ArrayLen::Fixed(4));
        assert!(infer(&tmpl, &concrete, &mut inf));
    }

    #[test]
    fn infer_array_fixed_mismatch() {
        let mut inf = Inference::new();
        let tmpl = array_ty(Type::Int, ArrayLen::Fixed(4));
        let concrete = array_ty(Type::Int, ArrayLen::Fixed(2));
        assert!(!infer(&tmpl, &concrete, &mut inf));
    }

    #[test]
    fn infer_array_const_conflict() {
        let mut inf = Inference::new();
        let tmpl = array_ty(Type::Int, ArrayLen::Param(cp(0)));
        let a = array_ty(Type::Int, ArrayLen::Fixed(4));
        let b = array_ty(Type::Int, ArrayLen::Fixed(8));
        assert!(infer(&tmpl, &a, &mut inf));
        assert!(!infer(&tmpl, &b, &mut inf));
    }

    #[test]
    fn array_param_mismatch() {
        let mut inf = Inference::new();
        let tmpl = array_ty(Type::Int, ArrayLen::Fixed(5));
        let concrete = array_ty(Type::Int, ArrayLen::Param(cp(0)));
        assert!(!infer(&tmpl, &concrete, &mut inf));
        assert!(inf.const_subst().is_empty());
    }

    #[test]
    fn array_nested_type_const() {
        let mut inf = Inference::new();
        let tmpl = Type::Tuple(vec![
            Type::Array {
                elem: Box::new(Type::Var(tv(0))),
                len: ArrayLen::Param(cp(1)),
            },
            Type::Var(tv(2)),
        ]);
        let concrete = Type::Tuple(vec![
            Type::Array {
                elem: Box::new(Type::Bool),
                len: ArrayLen::Fixed(7),
            },
            Type::String,
        ]);
        assert!(infer(&tmpl, &concrete, &mut inf));
        assert_eq!(inf.type_subst()[&tv(0)], Type::Bool);
        assert_eq!(inf.const_subst()[&cp(1)], cterm(7));
        assert_eq!(inf.type_subst()[&tv(2)], Type::String);
    }

    #[test]
    fn infer_nested_struct() {
        let mut inf = Inference::new();
        let tmpl = struct_ty("Entry", vec![Type::Var(tv(0)), Type::Var(tv(1))]);
        let concrete = struct_ty("Entry", vec![Type::String, Type::Int]);
        assert!(infer(&tmpl, &concrete, &mut inf));
        assert_eq!(inf.type_subst()[&tv(0)], Type::String);
        assert_eq!(inf.type_subst()[&tv(1)], Type::Int);
    }

    #[test]
    fn infer_func_type() {
        let mut inf = Inference::new();
        let tmpl = Type::Func {
            params: vec![FuncParam::new(Type::Var(tv(0)), false)],

            ret: Box::new(Type::Var(tv(0))),
        };
        let concrete = Type::Func {
            params: vec![FuncParam::new(Type::Int, false)],
            ret: Box::new(Type::Int),
        };
        assert!(infer(&tmpl, &concrete, &mut inf));
        assert_eq!(inf.type_subst()[&tv(0)], Type::Int);
    }

    #[test]
    fn func_arity_mismatch() {
        let mut inf = Inference::new();
        let tmpl = Type::Func {
            params: vec![FuncParam::new(Type::Var(tv(0)), false)],
            ret: Box::new(Type::Var(tv(0))),
        };
        let concrete = Type::Func {
            params: vec![],
            ret: Box::new(Type::Int),
        };
        assert!(!infer(&tmpl, &concrete, &mut inf));
    }

    #[test]
    fn infer_map() {
        let mut inf = Inference::new();
        let tmpl = Type::Map {
            key: Box::new(Type::Var(tv(0))),
            value: Box::new(Type::Var(tv(1))),
        };
        let concrete = Type::Map {
            key: Box::new(Type::String),
            value: Box::new(Type::Int),
        };
        assert!(infer(&tmpl, &concrete, &mut inf));
        assert_eq!(inf.type_subst()[&tv(0)], Type::String);
        assert_eq!(inf.type_subst()[&tv(1)], Type::Int);
    }

    #[test]
    fn infer_primitive_match() {
        let mut inf = Inference::new();
        assert!(infer(&Type::Int, &Type::Int, &mut inf));
        assert!(!infer(&Type::Int, &Type::String, &mut inf));
    }

    #[test]
    fn infer_shape_mismatch() {
        let mut inf = Inference::new();
        assert!(!infer(
            &Type::Int,
            &Type::List {
                elem: Box::new(Type::Int)
            },
            &mut inf
        ));
    }

    #[test]
    fn infer_list() {
        let mut inf = Inference::new();
        assert!(infer(
            &Type::List {
                elem: Box::new(Type::Var(tv(0)))
            },
            &Type::List {
                elem: Box::new(Type::Bool)
            },
            &mut inf,
        ));
        assert_eq!(inf.type_subst()[&tv(0)], Type::Bool);
    }

    #[test]
    fn infer_deeply_nested() {
        let mut inf = Inference::new();
        let tmpl = Type::List {
            elem: Box::new(struct_ty(
                "Box",
                vec![Type::Tuple(vec![Type::Var(tv(0)), Type::Var(tv(1))])],
            )),
        };
        let concrete = Type::List {
            elem: Box::new(struct_ty(
                "Box",
                vec![Type::Tuple(vec![Type::Int, Type::String])],
            )),
        };
        assert!(infer(&tmpl, &concrete, &mut inf));
        assert_eq!(inf.type_subst()[&tv(0)], Type::Int);
        assert_eq!(inf.type_subst()[&tv(1)], Type::String);
    }

    #[test]
    fn infer_enum() {
        let mut inf = Inference::new();
        let tmpl = nominal(NominalKind::Enum, "Option", vec![Type::Var(tv(0))], vec![]);
        let concrete = nominal(NominalKind::Enum, "Option", vec![Type::Int], vec![]);
        assert!(infer(&tmpl, &concrete, &mut inf));
        assert_eq!(inf.type_subst()[&tv(0)], Type::Int);
    }

    #[test]
    fn infer_dataref() {
        let mut inf = Inference::new();
        let tmpl = nominal(
            NominalKind::DataRef,
            "Buf",
            vec![Type::Var(tv(0)), Type::Var(tv(1))],
            vec![],
        );
        let concrete = nominal(
            NominalKind::DataRef,
            "Buf",
            vec![Type::Float, Type::Bool],
            vec![],
        );
        assert!(infer(&tmpl, &concrete, &mut inf));
        assert_eq!(inf.type_subst()[&tv(0)], Type::Float);
        assert_eq!(inf.type_subst()[&tv(1)], Type::Bool);
    }

    #[test]
    fn infer_slice() {
        let mut inf = Inference::new();
        assert!(infer(
            &Type::Slice {
                elem: Box::new(Type::Var(tv(0)))
            },
            &Type::Slice {
                elem: Box::new(Type::String)
            },
            &mut inf,
        ));
        assert_eq!(inf.type_subst()[&tv(0)], Type::String);
    }

    #[test]
    fn infer_named_tuple() {
        let mut inf = Inference::new();
        let tmpl = Type::NamedTuple(vec![
            (Ident::new("x"), Type::Var(tv(0))),
            (Ident::new("y"), Type::Var(tv(1))),
        ]);
        let concrete = Type::NamedTuple(vec![
            (Ident::new("x"), Type::Int),
            (Ident::new("y"), Type::Float),
        ]);
        assert!(infer(&tmpl, &concrete, &mut inf));
        assert_eq!(inf.type_subst()[&tv(0)], Type::Int);
        assert_eq!(inf.type_subst()[&tv(1)], Type::Float);
    }

    #[test]
    fn named_tuple_label_mismatch() {
        let mut inf = Inference::new();
        let tmpl = Type::NamedTuple(vec![(Ident::new("x"), Type::Var(tv(0)))]);
        let concrete = Type::NamedTuple(vec![(Ident::new("y"), Type::Int)]);
        assert!(!infer(&tmpl, &concrete, &mut inf));
    }

    #[test]
    fn array_struct_tuple() {
        let mut inf = Inference::new();
        let tmpl = Type::Tuple(vec![
            struct_ty(
                "Wrapper",
                vec![array_ty(Type::Var(tv(0)), ArrayLen::Param(cp(1)))],
            ),
            Type::Var(tv(2)),
        ]);
        let concrete = Type::Tuple(vec![
            struct_ty("Wrapper", vec![array_ty(Type::Bool, ArrayLen::Fixed(5))]),
            Type::String,
        ]);
        assert!(infer(&tmpl, &concrete, &mut inf));
        assert_eq!(inf.type_subst()[&tv(0)], Type::Bool);
        assert_eq!(inf.const_subst()[&cp(1)], cterm(5));
        assert_eq!(inf.type_subst()[&tv(2)], Type::String);
    }

    #[test]
    fn nested_conflict() {
        let mut inf = Inference::new();
        let tmpl = Type::Tuple(vec![
            struct_ty("Box", vec![Type::Var(tv(0))]),
            struct_ty("Box", vec![Type::Var(tv(0))]),
        ]);
        let concrete = Type::Tuple(vec![
            struct_ty("Box", vec![Type::Int]),
            struct_ty("Box", vec![Type::String]),
        ]);
        assert!(!infer(&tmpl, &concrete, &mut inf));
    }

    #[test]
    fn func_nested_generics() {
        let mut inf = Inference::new();
        let tmpl = Type::Func {
            params: vec![FuncParam::new(
                struct_ty("Box", vec![Type::Var(tv(0))]),
                false,
            )],
            ret: Box::new(Type::List {
                elem: Box::new(Type::Var(tv(1))),
            }),
        };
        let concrete = Type::Func {
            params: vec![FuncParam::new(struct_ty("Box", vec![Type::Int]), false)],
            ret: Box::new(Type::List {
                elem: Box::new(Type::String),
            }),
        };
        assert!(infer(&tmpl, &concrete, &mut inf));
        assert_eq!(inf.type_subst()[&tv(0)], Type::Int);
        assert_eq!(inf.type_subst()[&tv(1)], Type::String);
    }

    #[test]
    fn spec_exact() {
        assert_eq!(
            compare_specificity(&Type::Int, &Type::Var(tv(0))),
            Specificity::MoreSpecific
        );
        assert_eq!(
            compare_specificity(&Type::Var(tv(0)), &Type::Int),
            Specificity::LessSpecific
        );
    }

    #[test]
    fn spec_equal() {
        assert_eq!(
            compare_specificity(&Type::Var(tv(0)), &Type::Var(tv(1))),
            Specificity::Equal
        );
        assert_eq!(
            compare_specificity(&Type::Int, &Type::Int),
            Specificity::Equal
        );
    }

    #[test]
    fn spec_const() {
        let exact = array_ty(Type::Var(tv(0)), ArrayLen::Fixed(3));
        let generic = array_ty(Type::Var(tv(0)), ArrayLen::Param(cp(0)));
        assert_eq!(
            compare_specificity(&exact, &generic),
            Specificity::MoreSpecific
        );
    }

    #[test]
    fn spec_nested() {
        let nested = array_ty(
            array_ty(Type::Var(tv(0)), ArrayLen::Param(cp(0))),
            ArrayLen::Param(cp(1)),
        );
        let flat = array_ty(Type::Var(tv(2)), ArrayLen::Param(cp(1)));
        assert_eq!(
            compare_specificity(&nested, &flat),
            Specificity::MoreSpecific
        );
    }

    #[test]
    fn spec_repeat() {
        let repeat = Type::Tuple(vec![Type::Var(tv(0)), Type::Var(tv(0))]);
        let loose = Type::Tuple(vec![Type::Var(tv(1)), Type::Var(tv(2))]);
        assert_eq!(
            compare_specificity(&repeat, &loose),
            Specificity::MoreSpecific
        );
    }

    #[test]
    fn spec_bool_const_arg_exact() {
        let a = struct_const(
            "Flag",
            vec![],
            vec![ConstArg::Value(ConstValue::Bool(true))],
        );
        let b = struct_const(
            "Flag",
            vec![],
            vec![ConstArg::Value(ConstValue::Bool(true))],
        );
        assert_eq!(compare_specificity(&a, &b), Specificity::Equal);
    }

    #[test]
    fn spec_repeated_const_across_nominal_and_array_requires_equal_terms() {
        let repeated = Type::Tuple(vec![
            struct_const("Buf", vec![], vec![ConstArg::Param(cp(0))]),
            array_ty(Type::Int, ArrayLen::Param(cp(0))),
        ]);
        let same = Type::Tuple(vec![
            struct_const("Buf", vec![], vec![carg(3)]),
            array_ty(Type::Int, ArrayLen::Fixed(3)),
        ]);
        let different = Type::Tuple(vec![
            struct_const("Buf", vec![], vec![ConstArg::Value(ConstValue::Bool(true))]),
            array_ty(Type::Int, ArrayLen::Fixed(1)),
        ]);
        assert_eq!(
            compare_specificity(&same, &repeated),
            Specificity::MoreSpecific
        );
        assert_eq!(
            compare_specificity(&different, &repeated),
            Specificity::Incomparable
        );
    }

    #[test]
    fn spec_negative_const_arg_not_array_len() {
        let repeated = Type::Tuple(vec![
            struct_const("Buf", vec![], vec![ConstArg::Param(cp(0))]),
            array_ty(Type::Int, ArrayLen::Param(cp(0))),
        ]);
        let negative = Type::Tuple(vec![
            struct_const("Buf", vec![], vec![carg(-1)]),
            array_ty(Type::Int, ArrayLen::Fixed(0)),
        ]);
        assert_eq!(
            compare_specificity(&negative, &repeated),
            Specificity::Incomparable
        );
    }

    #[test]
    fn spec_ambig() {
        let cap = struct_const("FixedBuf", vec![Type::Var(tv(0))], vec![carg(5)]);
        let ints = struct_const("FixedBuf", vec![Type::Int], vec![ConstArg::Param(cp(0))]);
        assert_eq!(compare_specificity(&cap, &ints), Specificity::Incomparable);
    }

    #[test]
    fn arity_ok() {
        let params = GenericParams {
            type_params: vec![tp(0, "T")],
            const_params: vec![cparam(1, "N")],
        };
        let args = GenericArgs {
            type_args: vec![Type::Int],
            const_args: vec![cterm(4)],
        };
        assert!(params.validate_explicit_args(&args).is_ok());
    }

    #[test]
    fn type_arg_arity() {
        let params = GenericParams {
            type_params: vec![tp(0, "T")],
            const_params: vec![],
        };
        let args = GenericArgs {
            type_args: vec![Type::Int, Type::String],
            const_args: vec![],
        };
        let Err(ArityError::TypeArgs { expected, found }) = params.validate_explicit_args(&args)
        else {
            panic!("expected TypeArgs error");
        };
        assert_eq!((expected, found), (1, 2));
    }

    #[test]
    fn const_arg_arity() {
        let params = GenericParams {
            type_params: vec![],
            const_params: vec![cparam(0, "N")],
        };
        let args = GenericArgs {
            type_args: vec![],
            const_args: vec![],
        };
        let Err(ArityError::ConstArgs { expected, found }) = params.validate_explicit_args(&args)
        else {
            panic!("expected ConstArgs error");
        };
        assert_eq!((expected, found), (1, 0));
    }

    #[test]
    fn empty_params_empty_args() {
        let params = GenericParams::default();
        let args = GenericArgs::empty();
        assert!(params.validate_explicit_args(&args).is_ok());
    }

    #[test]
    fn not_generic_with_args() {
        let params = GenericParams::default();
        let args = GenericArgs {
            type_args: vec![Type::Int],
            const_args: vec![],
        };
        assert_eq!(
            params.validate_explicit_args(&args),
            Err(ArityError::NotGeneric)
        );
    }

    #[test]
    fn not_generic_empty_ok() {
        let params = GenericParams::default();
        assert!(params.validate_explicit_args(&GenericArgs::empty()).is_ok());
    }

    #[test]
    fn generic_empty_args_err() {
        let params = GenericParams {
            type_params: vec![tp(0, "T")],
            const_params: vec![],
        };
        let Err(ArityError::TypeArgs { expected, found }) =
            params.validate_explicit_args(&GenericArgs::empty())
        else {
            panic!("expected TypeArgs error");
        };
        assert_eq!((expected, found), (1, 0));
    }
}
