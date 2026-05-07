use super::{
    ArityError, GenericArgs, GenericParams, TypeChecker, TypeError, const_term::ConstTerm,
    infer::GenericSolverSeeds, type_ops::bare_type_name,
};
use crate::{
    ast::{ConstParamId, GenericArg, Type, TypeVarId},
    span::Span,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ExplicitGenericMode {
    Exact,
    Prefix,
}

#[derive(Debug, Clone, Default, PartialEq)]
struct ExplicitGenericBindings {
    type_args: Vec<(TypeVarId, Type)>,
    const_args: Vec<(ConstParamId, ConstTerm)>,
}

pub(super) fn bind_exact_generic_args(
    tc: &mut TypeChecker,
    generics: &GenericParams,
    args: &[GenericArg],
    span: Span,
) -> Option<GenericArgs> {
    let mut binder = TypeCheckerGenericBinder { tc };
    let bindings = bind_explicit_generic_args(
        generics,
        args,
        span,
        ExplicitGenericMode::Exact,
        &mut binder,
    )?;
    Some(materialize_exact(generics, &bindings))
}

pub(super) fn bind_prefix_generic_seeds(
    tc: &mut TypeChecker,
    generics: &GenericParams,
    args: &[GenericArg],
    span: Span,
) -> Option<GenericSolverSeeds> {
    let mut binder = TypeCheckerGenericBinder { tc };
    let bindings = bind_explicit_generic_args(
        generics,
        args,
        span,
        ExplicitGenericMode::Prefix,
        &mut binder,
    )?;
    Some(GenericSolverSeeds {
        type_args: bindings.type_args.into_iter().collect(),
        const_args: bindings.const_args.into_iter().collect(),
    })
}

fn materialize_exact(generics: &GenericParams, bindings: &ExplicitGenericBindings) -> GenericArgs {
    GenericArgs {
        type_args: generics
            .type_params
            .iter()
            .map(|param| type_binding(bindings, param.id))
            .collect(),
        const_args: generics
            .const_params
            .iter()
            .map(|param| const_binding(bindings, param.id))
            .collect(),
    }
}

fn type_binding(bindings: &ExplicitGenericBindings, id: TypeVarId) -> Type {
    bindings
        .type_args
        .iter()
        .find_map(|(binding_id, ty)| (*binding_id == id).then(|| ty.clone()))
        .expect("exact generic binder must bind every type parameter")
}

fn const_binding(bindings: &ExplicitGenericBindings, id: ConstParamId) -> ConstTerm {
    bindings
        .const_args
        .iter()
        .find_map(|(binding_id, term)| (*binding_id == id).then(|| term.clone()))
        .expect("exact generic binder must bind every const parameter")
}

fn bind_explicit_generic_args(
    generics: &GenericParams,
    args: &[GenericArg],
    span: Span,
    mode: ExplicitGenericMode,
    binder: &mut impl ExplicitGenericBinder,
) -> Option<ExplicitGenericBindings> {
    let type_len = generics.type_params.len();
    let total = type_len + generics.const_params.len();
    let arity_ok = match mode {
        ExplicitGenericMode::Exact => args.len() == total,
        ExplicitGenericMode::Prefix => args.len() <= total,
    };
    if !arity_ok {
        binder.push_arity_error(total, args.len(), span);
        return None;
    }

    let mut bindings = ExplicitGenericBindings::default();
    for (index, arg) in args.iter().enumerate() {
        if index < type_len {
            let GenericArg::Type(ty) = arg else {
                binder.push_kind_error("type", span);
                return None;
            };
            let ty = binder.resolve_type_arg(ty, span)?;
            bindings
                .type_args
                .push((generics.type_params[index].id, ty));
            continue;
        }

        let const_index = index - type_len;
        let term = explicit_const_term(arg).or_else(|| {
            binder.push_kind_error("const", span);
            None
        })?;
        let term = binder.eval_const_arg(term, span)?;
        bindings
            .const_args
            .push((generics.const_params[const_index].id, term));
    }
    Some(bindings)
}

fn explicit_const_term(arg: &GenericArg) -> Option<ConstTerm> {
    match arg {
        GenericArg::Const(arg) => Some(ConstTerm::from_arg(arg)),
        GenericArg::Type(ty) => bare_type_name(ty).map(ConstTerm::Name),
    }
}

trait ExplicitGenericBinder {
    fn resolve_type_arg(&mut self, ty: &Type, span: Span) -> Option<Type>;
    fn eval_const_arg(&mut self, term: ConstTerm, span: Span) -> Option<ConstTerm>;
    fn push_arity_error(&mut self, expected: usize, found: usize, span: Span);
    fn push_kind_error(&mut self, expected: &'static str, span: Span);
}

struct TypeCheckerGenericBinder<'tc> {
    tc: &'tc mut TypeChecker,
}

impl ExplicitGenericBinder for TypeCheckerGenericBinder<'_> {
    fn resolve_type_arg(&mut self, ty: &Type, _: Span) -> Option<Type> {
        Some(self.tc.resolve_type_for_tc(ty))
    }

    fn eval_const_arg(&mut self, term: ConstTerm, span: Span) -> Option<ConstTerm> {
        self.tc.eval_const_term(term, span)
    }

    fn push_arity_error(&mut self, expected: usize, found: usize, _: Span) {
        self.tc
            .push_error(TypeError::GenericArity(ArityError::TypeArgs {
                expected,
                found,
            }));
    }

    fn push_kind_error(&mut self, expected: &'static str, span: Span) {
        self.tc
            .push_error(TypeError::GenericArgKindMismatch { expected, span });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{ConstArg, ConstParam, ConstValue, Ident, Program, TypeParam},
        lexer::tokenize,
        parser,
        test_support::empty_resolved,
        typecheck::{DeclarationIndex, ModuleScope, TypecheckConfig},
    };

    fn ident(name: &str) -> Ident {
        Ident::new(name)
    }

    fn span() -> Span {
        Span::new(10, 20)
    }

    fn tv(id: u32) -> TypeVarId {
        TypeVarId(id)
    }

    fn cp(id: u32) -> ConstParamId {
        ConstParamId(id)
    }

    fn type_param(name: &str, id: u32) -> TypeParam {
        TypeParam {
            name: ident(name),
            id: tv(id),
        }
    }

    fn const_param(name: &str, id: u32) -> ConstParam {
        ConstParam {
            name: ident(name),
            id: cp(id),
        }
    }

    fn generics(type_params: &[(&str, u32)], const_params: &[(&str, u32)]) -> GenericParams {
        GenericParams {
            type_params: type_params
                .iter()
                .map(|(name, id)| type_param(name, *id))
                .collect(),
            const_params: const_params
                .iter()
                .map(|(name, id)| const_param(name, *id))
                .collect(),
        }
    }

    fn int_arg(value: i64) -> GenericArg {
        GenericArg::Const(ConstArg::Value(ConstValue::Int(value)))
    }

    fn type_arg(ty: Type) -> GenericArg {
        GenericArg::Type(ty)
    }

    fn parse(source: &str) -> Program {
        let tokens = tokenize(source).expect("lexer error");
        parser::parse_ast(&tokens).expect("parse error")
    }

    fn tc(source: &str) -> TypeChecker {
        let program = parse(source);
        let resolved = empty_resolved();
        let decls = DeclarationIndex::from_root_and_modules(
            &program,
            &resolved,
            &crate::externs::RawExterns::default(),
        );
        let mut tc = TypeChecker::new(
            decls,
            crate::externs::catalog::ExternCatalog::default(),
            TypecheckConfig::default(),
        );
        tc.collect_const_decls(ModuleScope::Root, &program);
        tc.push_scope();
        tc
    }

    fn bind_exact(
        source: &str,
        generics: &GenericParams,
        args: &[GenericArg],
    ) -> (TypeChecker, Option<GenericArgs>) {
        let mut tc = tc(source);
        let result = bind_exact_generic_args(&mut tc, generics, args, span());
        (tc, result)
    }

    fn bind_prefix(
        source: &str,
        generics: &GenericParams,
        args: &[GenericArg],
    ) -> (TypeChecker, Option<GenericSolverSeeds>) {
        let mut tc = tc(source);
        let result = bind_prefix_generic_seeds(&mut tc, generics, args, span());
        (tc, result)
    }

    #[test]
    fn exact_nominal_args_succeed() {
        let generics = generics(&[("T", 0)], &[("N", 0)]);
        let (_, args) = bind_exact("", &generics, &[type_arg(Type::Int), int_arg(3)]);

        assert_eq!(
            args,
            Some(GenericArgs {
                type_args: vec![Type::Int],
                const_args: vec![ConstTerm::Value(ConstValue::Int(3))],
            })
        );
    }

    #[test]
    fn prefix_type_seed() {
        let generics = generics(&[("T", 0), ("U", 1)], &[("N", 0)]);
        let (_, seeds) = bind_prefix("", &generics, &[type_arg(Type::Int)]);
        let seeds = seeds.expect("seeds");

        assert_eq!(seeds.type_args.get(&tv(0)), Some(&Type::Int));
        assert_eq!(seeds.type_args.get(&tv(1)), None);
        assert_eq!(seeds.const_args.get(&cp(0)), None);
    }

    #[test]
    fn prefix_type_const_seeds() {
        let generics = generics(&[("T", 0)], &[("N", 0)]);
        let (_, seeds) = bind_prefix("", &generics, &[type_arg(Type::Int), int_arg(4)]);
        let seeds = seeds.expect("seeds");

        assert_eq!(seeds.type_args.get(&tv(0)), Some(&Type::Int));
        assert_eq!(
            seeds.const_args.get(&cp(0)),
            Some(&ConstTerm::Value(ConstValue::Int(4)))
        );
    }

    #[test]
    fn visible_const_name() {
        let generics = generics(&[("T", 0)], &[("N", 0)]);
        let (_, args) = bind_exact(
            "const CAP = 4;",
            &generics,
            &[
                type_arg(Type::Int),
                type_arg(Type::UnresolvedName(ident("CAP"))),
            ],
        );

        assert_eq!(
            args.expect("args").const_args,
            vec![ConstTerm::Value(ConstValue::Int(4))]
        );
    }

    #[test]
    fn unknown_const_name() {
        let generics = generics(&[("T", 0)], &[("N", 0)]);
        let (tc, args) = bind_exact(
            "",
            &generics,
            &[
                type_arg(Type::Int),
                type_arg(Type::UnresolvedName(ident("N"))),
            ],
        );

        assert_eq!(args, None);
        assert!(matches!(
            tc.errors.first(),
            Some(TypeError::UnknownConst { name, span: err_span })
                if *name == ident("N") && *err_span == span()
        ));
    }

    #[test]
    fn const_in_type_slot() {
        let generics = generics(&[("T", 0)], &[]);
        let (tc, args) = bind_exact("", &generics, &[int_arg(3)]);

        assert_eq!(args, None);
        assert!(matches!(
            tc.errors.first(),
            Some(TypeError::GenericArgKindMismatch { expected: "type", span: err_span })
                if *err_span == span()
        ));
    }

    #[test]
    fn non_bare_type_in_const_slot() {
        let generics = generics(&[("T", 0)], &[("N", 0)]);
        let list_int = Type::List {
            elem: Box::new(Type::Int),
        };
        let (tc, args) = bind_exact("", &generics, &[type_arg(Type::Int), type_arg(list_int)]);

        assert_eq!(args, None);
        assert!(matches!(
            tc.errors.first(),
            Some(TypeError::GenericArgKindMismatch { expected: "const", span: err_span })
                if *err_span == span()
        ));
    }

    #[test]
    fn exact_arity_rejects_too_few_args() {
        let generics = generics(&[("T", 0)], &[("N", 0)]);
        let (tc, args) = bind_exact("", &generics, &[type_arg(Type::Int)]);

        assert_eq!(args, None);
        assert!(matches!(
            tc.errors.first(),
            Some(TypeError::GenericArity(ArityError::TypeArgs {
                expected: 2,
                found: 1
            }))
        ));
    }

    #[test]
    fn prefix_arity_rejects_too_many_args() {
        let generics = generics(&[("T", 0)], &[]);
        let (tc, seeds) = bind_prefix(
            "",
            &generics,
            &[type_arg(Type::Int), type_arg(Type::String)],
        );

        assert!(seeds.is_none());
        assert!(matches!(
            tc.errors.first(),
            Some(TypeError::GenericArity(ArityError::TypeArgs {
                expected: 1,
                found: 2
            }))
        ));
    }
}
