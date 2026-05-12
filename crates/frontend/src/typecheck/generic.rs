use std::collections::HashMap;

use super::{
    ArgumentProjectionMap, CallMap, ContractWitnessMap, DynCallMap, DynConversionMap, ExternUseMap,
    MemberPathMap, const_term::ConstTerm, decls::CallableId, type_ops::TypeFolder,
};
use crate::{
    ast::{
        ArrayLen, ConstArg, ConstParam, ConstParamId, ExprId, GenericArg, Type, TypeParam,
        TypeVarId,
    },
    span::Span,
};

pub(crate) type TypeSubst = HashMap<TypeVarId, Type>;
pub(crate) type ConstSubst = HashMap<ConstParamId, ConstTerm>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ArityError {
    TypeArgs { expected: usize, found: usize },
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
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub(crate) struct GenericArgs {
    pub(crate) type_args: Vec<Type>,
    pub(crate) const_args: Vec<ConstTerm>,
}

impl GenericArgs {
    pub(crate) fn is_empty(&self) -> bool {
        self.type_args.is_empty() && self.const_args.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct SpecializationKey {
    pub(crate) target: CallableId,
    pub(crate) args: GenericArgs,
}

pub(crate) type SpecializedBodyTypes = HashMap<ExprId, (Span, Type)>;

#[derive(Clone, Default)]
pub(crate) struct SpecializedBodyFacts {
    pub(crate) types: SpecializedBodyTypes,
    pub(crate) calls: CallMap,
    pub(crate) extern_uses: ExternUseMap,
    pub(crate) member_paths: MemberPathMap,
    pub(crate) argument_projections: ArgumentProjectionMap,
    pub(crate) contract_witnesses: ContractWitnessMap,
    pub(crate) dyn_conversions: DynConversionMap,
    pub(crate) dyn_calls: DynCallMap,
}

#[derive(Clone)]
pub(crate) struct SpecializedBody {
    pub(crate) facts: SpecializedBodyFacts,
    pub(crate) inferred_ret: Option<Type>,
}

#[derive(Clone)]
pub(crate) enum SpecializationState {
    InProgress,
    Done(Box<SpecializedBody>),
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
    use crate::ast::{ConstValue, FuncParam, Ident, NominalKind};

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
            params: vec![FuncParam::new(Type::Var(tv(0)), false, false)],
            ret: Box::new(Type::Var(tv(1))),
        };
        let result = substitute(&ty, &ts, &HashMap::new());
        assert_eq!(
            result,
            Type::Func {
                params: vec![FuncParam::new(Type::Int, false, false)],
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
}
