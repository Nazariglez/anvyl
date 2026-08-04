use super::{
    ArityError, GenericArgs, GenericParams, TypeChecker, TypeError,
    const_eval::const_type,
    const_term::ConstTerm,
    infer::{GenericSolverSeeds, GenericSolverVars},
};
use crate::{
    ast::{ConstParamId, ConstValue, GenericArg, Type, TypeVarId},
    span::Span,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ExplicitGenericMode {
    Exact,
    Prefix,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub(super) struct ExplicitGenericBindings {
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

pub(super) struct GenericSolveSession<'a> {
    generics: &'a GenericParams,
    vars: GenericSolverVars,
    span: Span,
}

impl<'a> GenericSolveSession<'a> {
    pub(super) fn new(
        tc: &mut TypeChecker,
        generics: &'a GenericParams,
        seeds: &GenericSolverSeeds,
        span: Span,
    ) -> Self {
        Self {
            generics,
            vars: tc
                .solver
                .generic_solver_vars(generics, seeds, tc.error_span(span)),
            span,
        }
    }

    pub(super) fn vars(&self) -> &GenericSolverVars {
        &self.vars
    }

    pub(super) fn finish(&self, tc: &mut TypeChecker) -> Option<GenericArgs> {
        let args = match tc.solver.finalize_generic_args(self.generics, &self.vars) {
            Ok(args) => args,
            Err(unbound) => {
                tc.push_unbound_generic_errors(unbound, self.span);
                return None;
            }
        };
        tc.check_generic_bounds(self.generics, &args, self.span)
            .then_some(args)
    }
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

pub(super) fn materialize_exact(
    generics: &GenericParams,
    bindings: &ExplicitGenericBindings,
) -> GenericArgs {
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

pub(super) fn bind_explicit_generic_args(
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
        let term = binder.const_term_arg(arg, span)?;
        let term = binder.eval_const_arg(term, span)?;
        let term = int_const_arg(term, span, binder)?;
        bindings
            .const_args
            .push((generics.const_params[const_index].id, term));
    }
    Some(bindings)
}

fn explicit_const_term(arg: &GenericArg) -> Option<ConstTerm> {
    match arg {
        GenericArg::Const(arg) => Some(ConstTerm::from_arg(arg)),
        GenericArg::Type(ty) => ty.bare_unresolved_name().map(ConstTerm::Name),
    }
}

fn int_const_arg(
    term: ConstTerm,
    span: Span,
    binder: &mut impl ExplicitGenericBinder,
) -> Option<ConstTerm> {
    match &term {
        ConstTerm::Value(value) if !matches!(value, ConstValue::Int(_)) => {
            binder.push_expected_int_const(const_type(value), span);
            None
        }
        _ => Some(term),
    }
}

pub(super) trait ExplicitGenericBinder {
    fn resolve_type_arg(&mut self, ty: &Type, span: Span) -> Option<Type>;
    fn eval_const_arg(&mut self, term: ConstTerm, span: Span) -> Option<ConstTerm>;
    fn push_arity_error(&mut self, expected: usize, found: usize, span: Span);
    fn push_kind_error(&mut self, expected: &'static str, span: Span);
    fn push_expected_int_const(&mut self, found: Type, span: Span);

    fn const_term_arg(&mut self, arg: &GenericArg, span: Span) -> Option<ConstTerm> {
        explicit_const_term(arg).or_else(|| {
            self.push_kind_error("const", span);
            None
        })
    }
}

struct TypeCheckerGenericBinder<'tc> {
    tc: &'tc mut TypeChecker,
}

impl ExplicitGenericBinder for TypeCheckerGenericBinder<'_> {
    fn resolve_type_arg(&mut self, ty: &Type, span: Span) -> Option<Type> {
        Some(self.tc.resolve_type_for_tc_at(ty, span))
    }

    fn eval_const_arg(&mut self, term: ConstTerm, span: Span) -> Option<ConstTerm> {
        self.tc.eval_const_term(term, span, true)
    }

    fn push_arity_error(&mut self, expected: usize, found: usize, _: Span) {
        self.tc
            .push_error(TypeError::GenericArity(ArityError::TypeArgs {
                expected,
                found,
            }));
    }

    fn push_kind_error(&mut self, expected: &'static str, span: Span) {
        self.tc.push_error(TypeError::GenericArgKindMismatch {
            expected,
            span: self.tc.error_span(span),
        });
    }

    fn push_expected_int_const(&mut self, found: Type, span: Span) {
        self.tc.push_error(TypeError::ExpectedIntConst {
            found,
            span: self.tc.error_span(span),
        });
    }
}
