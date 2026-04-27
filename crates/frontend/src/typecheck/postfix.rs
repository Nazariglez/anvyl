use super::{
    ConstSubst, GenericArgs, GenericParams, TypeChecker, TypeError, TypeSubst,
    call_map::CallTarget,
    check_expr, check_expr_checked_with_hint, check_specialized_extend_body,
    check_specialized_func_body, check_specialized_method_body,
    const_term::ConstTerm,
    decls::{
        AggregateSchema, ExtendMethodMatch, ExtendMethodSchema, ExtendSchema, FuncSig,
        MethodSchema, ModuleScope, NominalKey, ValueDecl, VariantSchema, generic_template_type,
        nominal_type, nominal_type_with_args,
    },
    generic_bind::bind_prefix_generic_seeds,
    infer::{GenericSolverSeeds, GenericSolverVars, TypeHandle},
    substitute,
};
use crate::{
    ast::{
        CallNode, ConstArg, ConstValue, ExprId, ExprKind, ExprNode, FieldAccessNode, FuncParam,
        Ident, Type,
    },
    span::Span,
};

pub(super) enum Subject {
    Value(Type),
    Module(ModuleScope),
    Type(NominalKey),
    NamedValue {
        module: ModuleScope,
        name: Ident,
        ty: Type,
        generic: Option<FuncSig>,
    },
    Method {
        target: MethodTarget,
        name: Ident,
        owner_generics: GenericParams,
        owner_args: GenericArgs,
        generics: GenericParams,
        params: Vec<FuncParam>,
        ret: Type,
    },
    EnumVariant {
        enum_key: NominalKey,
        variant: Ident,
        schema: VariantSchema,
    },
}

pub(super) enum MethodTarget {
    Aggregate {
        owner: NominalKey,
        receiver: Option<Type>,
    },
    Extend {
        extend: super::ExtendId,
        receiver: Type,
    },
}

pub(super) enum PostfixStep<'a> {
    Field {
        node: &'a FieldAccessNode,
        id: ExprId,
    },
    Call {
        node: &'a CallNode,
        id: ExprId,
    },
}

pub(super) struct PostfixChain<'a> {
    pub base: &'a ExprNode,
    pub steps: Vec<PostfixStep<'a>>,
}

pub(super) fn collect_postfix_chain(expr: &ExprNode) -> Option<PostfixChain<'_>> {
    let (base, steps) = collect_steps(expr)?;
    Some(PostfixChain { base, steps })
}

fn collect_steps(expr: &ExprNode) -> Option<(&ExprNode, Vec<PostfixStep<'_>>)> {
    match &expr.node.kind {
        ExprKind::Field(field) => {
            let (base, mut steps) = collect_steps_or_base(&field.node.target);
            steps.push(PostfixStep::Field {
                node: field,
                id: expr.node.id,
            });
            Some((base, steps))
        }
        ExprKind::Call(call) => {
            let (base, mut steps) = collect_steps_or_base(&call.node.func);
            steps.push(PostfixStep::Call {
                node: call,
                id: expr.node.id,
            });
            Some((base, steps))
        }
        _ => None,
    }
}

fn collect_steps_or_base(expr: &ExprNode) -> (&ExprNode, Vec<PostfixStep<'_>>) {
    match collect_steps(expr) {
        Some(result) => result,
        None => (expr, vec![]),
    }
}

pub(super) fn resolve_base(expr: &ExprNode, tc: &mut TypeChecker) -> Option<Subject> {
    match &expr.node.kind {
        ExprKind::Ident(name) => {
            if let Some((module, value_name, value)) = tc.lookup_named_value(*name) {
                return Some(named_value_subject(module, value_name, &value));
            }
            if let Some(ty) = tc.lookup_type(*name) {
                return Some(Subject::Value(ty));
            }
            if let Some(scope) = tc.lookup_module_alias(*name) {
                return Some(Subject::Module(scope));
            }
            if let Some(key) = tc.lookup_type_name(*name) {
                return Some(Subject::Type(key));
            }
            None
        }
        _ => Some(Subject::Value(check_expr(expr, tc))),
    }
}

pub(super) fn check_postfix_chain(
    chain: &PostfixChain,
    expr: &ExprNode,
    expected: Option<&TypeHandle>,
    tc: &mut TypeChecker,
) -> Type {
    let Some(mut subject) = resolve_base(chain.base, tc) else {
        if let ExprKind::Ident(name) = &chain.base.node.kind {
            tc.push_error(TypeError::UndefinedVariable {
                name: *name,
                span: chain.base.span,
            });
        }
        tc.set_type(chain.base.node.id, Type::Infer, chain.base.span);
        return finish_chain(chain, expr, tc);
    };

    tc.set_type(chain.base.node.id, subject_type(&subject), chain.base.span);

    for (i, step) in chain.steps.iter().enumerate() {
        let is_last_step = i + 1 == chain.steps.len();
        let next_is_call = matches!(chain.steps.get(i + 1), Some(PostfixStep::Call { .. }));
        subject = match step {
            PostfixStep::Field { node, id } => {
                let subject = apply_field(&subject, node, next_is_call, tc);
                tc.set_type(*id, subject_type(&subject), node.span);
                subject
            }
            PostfixStep::Call { node, id } => {
                let call_expected = is_last_step.then(|| expected.cloned()).flatten();
                let ty = apply_call(&subject, node, *id, call_expected, tc);
                tc.set_type(*id, ty.clone(), node.span);
                Subject::Value(ty)
            }
        };
    }

    let ty = subject_type(&subject);
    tc.set_type(expr.node.id, ty.clone(), expr.span);
    ty
}

fn finish_chain(chain: &PostfixChain, expr: &ExprNode, tc: &mut TypeChecker) -> Type {
    for step in &chain.steps {
        match step {
            PostfixStep::Field { node, id } => {
                tc.set_type(*id, Type::Infer, node.span);
            }
            PostfixStep::Call { node, id } => {
                for arg in &node.node.args {
                    check_expr(arg, tc);
                }
                tc.set_type(*id, Type::Infer, node.span);
            }
        }
    }
    tc.set_type(expr.node.id, Type::Infer, expr.span);
    Type::Infer
}

fn subject_type(subject: &Subject) -> Type {
    match subject {
        Subject::Value(ty) | Subject::NamedValue { ty, .. } => ty.clone(),
        Subject::Module(_) => Type::Void,
        Subject::Type(key) => nominal_type(key),
        Subject::EnumVariant { enum_key, .. } => nominal_type(enum_key),
        Subject::Method { params, ret, .. } => func_type(params, ret),
    }
}

fn apply_field(
    subject: &Subject,
    field: &FieldAccessNode,
    next_is_call: bool,
    tc: &mut TypeChecker,
) -> Subject {
    match subject {
        Subject::Value(ty) | Subject::NamedValue { ty, .. } => {
            apply_value_field(ty.clone(), field.node.field, field.span, next_is_call, tc)
        }
        Subject::Module(scope) => apply_module_field(scope, field.node.field, field.span, tc),
        Subject::Type(key) => apply_type_field(key, field.node.field, field.span, tc),
        Subject::EnumVariant { .. } | Subject::Method { .. } => {
            field_access_on_non_aggregate(subject, field.node.field, field.span, tc)
        }
    }
}

fn field_access_on_non_aggregate(
    subject: &Subject,
    field: Ident,
    span: Span,
    tc: &mut TypeChecker,
) -> Subject {
    tc.push_error(TypeError::FieldAccessOnNonAggregate {
        ty: subject_type(subject),
        field,
        span,
    });
    Subject::Value(Type::Infer)
}

fn owner_member_type(receiver: &Type, generics: &GenericParams, ty: &Type) -> Type {
    let Some(receiver) = receiver.as_aggregate() else {
        return ty.clone();
    };

    let type_subst: TypeSubst = generics
        .type_params
        .iter()
        .zip(receiver.type_args)
        .map(|(param, arg)| (param.id, arg.clone()))
        .collect();
    let const_subst: ConstSubst = generics
        .const_params
        .iter()
        .zip(receiver.const_args)
        .map(|(param, arg)| (param.id, ConstTerm::from_arg(arg)))
        .collect();
    substitute(ty, &type_subst, &const_subst)
}

fn aggregate_method_subject(
    agg: &AggregateSchema,
    receiver: Option<Type>,
    name: Ident,
    method: &MethodSchema,
) -> Subject {
    let owner_generics = agg.generics.clone();
    let (params, ret) = match receiver.as_ref() {
        Some(receiver) => (
            method
                .params
                .iter()
                .map(|param| {
                    FuncParam::new(
                        owner_member_type(receiver, &agg.generics, &param.ty),
                        param.mutable,
                    )
                })
                .collect(),
            owner_member_type(receiver, &agg.generics, &method.ret),
        ),
        None => (method.params.clone(), method.ret.clone()),
    };

    Subject::Method {
        target: MethodTarget::Aggregate {
            owner: agg.key.clone(),
            receiver,
        },
        name,
        owner_generics,
        owner_args: GenericArgs::empty(),
        generics: method.generics.clone(),
        params,
        ret,
    }
}

fn generic_sig(value: &ValueDecl) -> Option<FuncSig> {
    match value {
        ValueDecl::Func(sig) if !sig.generics.is_empty() => Some(sig.clone()),
        _ => None,
    }
}

fn named_value_subject(module: ModuleScope, name: Ident, value: &ValueDecl) -> Subject {
    Subject::NamedValue {
        module,
        name,
        ty: value.ty().clone(),
        generic: generic_sig(value),
    }
}

fn extend_method_subject(
    receiver: Type,
    extend: &ExtendSchema,
    name: Ident,
    method: &ExtendMethodSchema,
    owner_args: GenericArgs,
) -> Subject {
    let (type_subst, const_subst) = extend.generics.substitutions(&owner_args);
    let template_params = method
        .params
        .iter()
        .map(|param| {
            FuncParam::new(
                generic_template_type(&param.ty, &extend.generics),
                param.mutable,
            )
        })
        .collect::<Vec<_>>();
    let template_ret = generic_template_type(&method.ret, &extend.generics);
    let params = substitute_params(&template_params, &type_subst, &const_subst);
    let ret = substitute(&template_ret, &type_subst, &const_subst);

    Subject::Method {
        target: MethodTarget::Extend {
            extend: extend.id.clone(),
            receiver,
        },
        name,
        owner_generics: extend.generics.clone(),
        owner_args,
        generics: method.generics.clone(),
        params,
        ret,
    }
}

fn enum_variant_subject(enum_key: &NominalKey, variant: Ident, schema: &VariantSchema) -> Subject {
    Subject::EnumVariant {
        enum_key: enum_key.clone(),
        variant,
        schema: schema.clone(),
    }
}

fn unknown_enum_variant(
    enum_key: &NominalKey,
    variant: Ident,
    span: Span,
    tc: &mut TypeChecker,
) -> Subject {
    tc.push_error(TypeError::UnknownEnumVariant {
        enum_name: enum_key.name,
        variant,
        span,
    });
    Subject::Value(Type::Infer)
}

fn apply_value_field(
    receiver: Type,
    name: Ident,
    span: Span,
    next_is_call: bool,
    tc: &mut TypeChecker,
) -> Subject {
    let key = tc.decls.key_for_type(&receiver);

    if let Some(key) = key.as_ref()
        && let Some(agg) = tc.decls.aggregate(key)
        && let Some(subject) = aggregate_field_subject(agg, &receiver, name, next_is_call, tc)
    {
        return subject;
    }

    if let Some(matched) = tc.find_extend_method(&receiver, name) {
        return match extend_method_match_subject(receiver, name, &matched) {
            Ok(subject) => subject,
            Err(ExtendMethodError::Unbound(names)) => {
                tc.push_unbound_generic_errors(names, span);
                Subject::Value(Type::Infer)
            }
            Err(ExtendMethodError::Ambiguous { receiver, name }) => {
                tc.push_error(TypeError::AmbiguousExtendMethod {
                    receiver,
                    name,
                    span,
                });
                Subject::Value(Type::Infer)
            }
        };
    }

    match key {
        Some(_) => unknown_field(receiver, name, span, tc),
        None => non_aggregate_field(receiver, name, span, tc),
    }
}

fn aggregate_field_subject(
    agg: &AggregateSchema,
    receiver: &Type,
    name: Ident,
    prefer_method: bool,
    tc: &TypeChecker,
) -> Option<Subject> {
    let field_ty = tc.decls.aggregate_field_type(receiver, name);
    let method = agg.methods.get(&name);
    match (prefer_method, method, field_ty) {
        (true, Some(method), _) | (false, Some(method), None) => Some(aggregate_method_subject(
            agg,
            Some(receiver.clone()),
            name,
            method,
        )),
        (_, _, Some(field_ty)) => Some(Subject::Value(field_ty)),
        _ => None,
    }
}

enum ExtendMethodError {
    Unbound(Vec<Ident>),
    Ambiguous { receiver: Type, name: Ident },
}

fn extend_method_match_subject(
    receiver: Type,
    name: Ident,
    matched: &ExtendMethodMatch<'_>,
) -> Result<Subject, ExtendMethodError> {
    match matched {
        ExtendMethodMatch::Match {
            extend,
            method,
            owner_args: Ok(owner_args),
        } => Ok(extend_method_subject(
            receiver,
            extend,
            name,
            method,
            owner_args.clone(),
        )),
        ExtendMethodMatch::Match {
            owner_args: Err(unbound),
            ..
        } => Err(ExtendMethodError::Unbound(unbound.clone())),
        ExtendMethodMatch::Ambiguous => Err(ExtendMethodError::Ambiguous { receiver, name }),
    }
}

fn apply_module_field(
    scope: &ModuleScope,
    name: Ident,
    span: Span,
    tc: &mut TypeChecker,
) -> Subject {
    if let Some(module) = tc.exported_module_in_module(scope, name) {
        return Subject::Module(module);
    }
    if let Some((module, value_name, decl)) = tc.exported_value_in_module(scope, name) {
        return named_value_subject(module, value_name, &decl);
    }
    if let Some(key) = tc.exported_type_in_module(scope, name) {
        return Subject::Type(key);
    }
    tc.push_error(TypeError::UndefinedModuleMember {
        module: scope.clone(),
        name,
        span,
    });
    Subject::Value(Type::Infer)
}

fn apply_type_field(key: &NominalKey, name: Ident, span: Span, tc: &mut TypeChecker) -> Subject {
    if let Some(schema) = tc.decls.enum_schema(key) {
        if let Some(variant) = schema.variants.get(&name) {
            return enum_variant_subject(&schema.key, name, variant);
        }

        if let Some(agg) = tc.decls.aggregate(key)
            && let Some(method) = agg.methods.get(&name)
            && method.receiver.is_none()
        {
            return aggregate_method_subject(agg, None, name, method);
        }
        return unknown_enum_variant(key, name, span, tc);
    }
    let Some(agg) = tc.decls.aggregate(key) else {
        return unknown_field(nominal_type(key), name, span, tc);
    };
    let Some(method) = agg.methods.get(&name) else {
        return unknown_field(nominal_type(key), name, span, tc);
    };
    if method.receiver.is_some() {
        return unknown_field(nominal_type(key), name, span, tc);
    }
    aggregate_method_subject(agg, None, name, method)
}

fn apply_call(
    subject: &Subject,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> Type {
    match subject {
        Subject::Value(ty) => call_value(ty.clone(), call, tc),
        Subject::NamedValue {
            module,
            name,
            ty,
            generic,
        } => call_named_value(
            module,
            *name,
            ty.clone(),
            generic.as_ref(),
            call,
            call_id,
            expected,
            tc,
        ),
        Subject::Method {
            target,
            name,
            owner_generics,
            owner_args,
            generics,
            params,
            ret,
        } => {
            let method = MethodCall {
                target,
                name: *name,
                owner_generics,
                owner_args,
                generics,
                params,
                ret,
            };
            call_method(method, call, call_id, expected, tc)
        }
        Subject::EnumVariant {
            enum_key,
            variant,
            schema,
        } => call_enum_variant(enum_key, *variant, schema, call, call_id, expected, tc),
        Subject::Module(_) | Subject::Type(_) => not_callable(subject_type(subject), call, tc),
    }
}

fn call_value(callee_ty: Type, call: &CallNode, tc: &mut TypeChecker) -> Type {
    match &callee_ty {
        Type::Func { params, ret } => {
            check_args(&call.node.args, params, call.span, tc);
            (**ret).clone()
        }
        _ => not_callable(callee_ty, call, tc),
    }
}

fn call_named_value(
    module: &ModuleScope,
    name: Ident,
    callee_ty: Type,
    generic: Option<&FuncSig>,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> Type {
    if let Some(sig) = generic {
        return call_generic(module, name, sig, call, call_id, expected, tc);
    }
    let is_func = matches!(&callee_ty, Type::Func { .. });
    let ret = call_value(callee_ty, call, tc);
    if is_func {
        tc.record_call(call_id, named_call_target(module, name));
    }
    ret
}

struct GenericCallInstantiation {
    args: GenericArgs,
    type_subst: TypeSubst,
    const_subst: ConstSubst,
    concrete_params: Vec<FuncParam>,
    ret: Type,
}

#[derive(Clone, Copy)]
struct MethodCall<'a> {
    target: &'a MethodTarget,
    name: Ident,
    owner_generics: &'a GenericParams,
    owner_args: &'a GenericArgs,
    generics: &'a GenericParams,
    params: &'a [FuncParam],
    ret: &'a Type,
}

fn solve_generic_call_with(
    generics: &GenericParams,
    seeds: &GenericSolverSeeds,
    template_params: &[FuncParam],
    template_ret: &Type,
    call: &CallNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
    add_constraints: impl FnOnce(&GenericSolverVars, &mut TypeChecker),
) -> Option<GenericCallInstantiation> {
    if !check_arg_count(&call.node.args, template_params.len(), call.span, tc) {
        return None;
    }

    let error_count = tc.errors.len();
    for param in template_params {
        tc.substitute_checked(&param.ty, &seeds.type_args, &seeds.const_args, call.span);
    }
    tc.substitute_checked(template_ret, &seeds.type_args, &seeds.const_args, call.span);
    if tc.errors.len() != error_count {
        for arg in &call.node.args {
            check_expr(arg, tc);
        }
        return None;
    }

    let vars = tc.solver.generic_solver_vars(generics, seeds, call.span);
    add_constraints(&vars, tc);
    let mut failed = tc.solve_constraints();

    let param_handles = instantiate_param_handles(template_params, &vars, tc);
    let ret_handle = tc.solver.instantiate_generic_type(template_ret, &vars);

    failed |= check_call_arg_handles(&call.node.args, &param_handles, tc);
    failed |= constrain_expected_return(call.span, ret_handle, expected, tc);

    if failed {
        return None;
    }

    let args = match tc.solver.finalize_generic_args(generics, &vars) {
        Ok(args) => args,
        Err(unbound) => {
            tc.push_unbound_generic_errors(unbound, call.span);
            return None;
        }
    };

    let (type_subst, const_subst) = generics.substitutions(&args);
    let concrete_params =
        substitute_params_checked(template_params, &type_subst, &const_subst, call.span, tc);
    let ret = tc.substitute_checked(template_ret, &type_subst, &const_subst, call.span);

    Some(GenericCallInstantiation {
        args,
        type_subst,
        const_subst,
        concrete_params,
        ret,
    })
}

fn instantiate_param_handles(
    params: &[FuncParam],
    vars: &GenericSolverVars,
    tc: &mut TypeChecker,
) -> Vec<TypeHandle> {
    params
        .iter()
        .map(|param| tc.solver.instantiate_generic_type(&param.ty, vars))
        .collect()
}

fn check_call_arg_handles(args: &[ExprNode], params: &[TypeHandle], tc: &mut TypeChecker) -> bool {
    let mut failed = false;
    for (arg, param) in args.iter().zip(params) {
        let checked = check_expr_checked_with_hint(arg, Some(param.clone()), tc);
        tc.expect_assignable(arg.span, checked.handle, param.clone());
        failed |= tc.solve_constraints();
    }
    failed
}

fn constrain_expected_return(
    span: Span,
    ret: TypeHandle,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> bool {
    match expected {
        Some(expected) => {
            tc.expect_assignable(span, ret, expected);
            tc.solve_constraints()
        }
        None => false,
    }
}

fn substitute_params(
    params: &[FuncParam],
    type_subst: &TypeSubst,
    const_subst: &ConstSubst,
) -> Vec<FuncParam> {
    params
        .iter()
        .map(|param| {
            FuncParam::new(
                substitute(&param.ty, type_subst, const_subst),
                param.mutable,
            )
        })
        .collect()
}

fn substitute_params_checked(
    params: &[FuncParam],
    type_subst: &TypeSubst,
    const_subst: &ConstSubst,
    span: Span,
    tc: &mut TypeChecker,
) -> Vec<FuncParam> {
    params
        .iter()
        .map(|param| {
            FuncParam::new(
                tc.substitute_checked(&param.ty, type_subst, const_subst, span),
                param.mutable,
            )
        })
        .collect()
}

fn call_generic(
    module: &ModuleScope,
    name: Ident,
    sig: &FuncSig,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> Type {
    let Type::Func {
        params: template_params,
        ret: template_ret,
    } = &sig.ty
    else {
        return not_callable(sig.ty.clone(), call, tc);
    };
    let Some(seeds) =
        bind_prefix_generic_seeds(tc, &sig.generics, &call.node.generic_args, call.span)
    else {
        return Type::Infer;
    };
    let Some(inst) = solve_generic_call_with(
        &sig.generics,
        &seeds,
        template_params,
        template_ret,
        call,
        expected,
        tc,
        |_, _| {},
    ) else {
        return Type::Infer;
    };

    check_specialized_func_body(
        module,
        name,
        sig,
        &inst.args,
        inst.type_subst,
        inst.const_subst,
        tc,
    );
    tc.record_call(
        call_id,
        CallTarget::GenericDirect {
            module: module.clone(),
            name,
            type_args: inst.args.type_args,
            const_args: inst.args.const_args,
        },
    );
    inst.ret
}

fn combine_generic_args(owner_args: &GenericArgs, method_args: &GenericArgs) -> GenericArgs {
    let mut args = owner_args.clone();
    args.type_args.extend(method_args.type_args.clone());
    args.const_args.extend(method_args.const_args.clone());
    args
}

fn combined_generic_params(owner: &GenericParams, method: &GenericParams) -> GenericParams {
    let mut params = owner.clone();
    params.type_params.extend(method.type_params.clone());
    params.const_params.extend(method.const_params.clone());
    params
}

fn seed_owner_args(seeds: &mut GenericSolverSeeds, generics: &GenericParams, args: &GenericArgs) {
    for (param, ty) in generics.type_params.iter().zip(&args.type_args) {
        seeds.type_args.insert(param.id, ty.clone());
    }
    for (param, term) in generics.const_params.iter().zip(&args.const_args) {
        seeds.const_args.insert(param.id, term.clone());
    }
}

fn owner_template(owner: &NominalKey, generics: &GenericParams) -> Type {
    let type_args = generics
        .type_params
        .iter()
        .map(|param| Type::Var(param.id))
        .collect::<Vec<_>>();
    let const_args = generics
        .const_params
        .iter()
        .map(|param| ConstArg::Param(param.id))
        .collect::<Vec<_>>();
    nominal_type_with_args(owner, &type_args, &const_args)
}

fn split_generic_args(
    args: &GenericArgs,
    owner_generics: &GenericParams,
    method_generics: &GenericParams,
) -> (GenericArgs, GenericArgs) {
    let owner_type_len = owner_generics.type_params.len();
    let owner_const_len = owner_generics.const_params.len();
    let method_type_len = method_generics.type_params.len();
    let method_const_len = method_generics.const_params.len();
    let owner_args = GenericArgs {
        type_args: args.type_args[..owner_type_len].to_vec(),
        const_args: args.const_args[..owner_const_len].to_vec(),
    };
    let method_args = GenericArgs {
        type_args: args.type_args[owner_type_len..owner_type_len + method_type_len].to_vec(),
        const_args: args.const_args[owner_const_len..owner_const_len + method_const_len].to_vec(),
    };
    (owner_args, method_args)
}

fn call_method(
    method: MethodCall<'_>,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> Type {
    let Some(mut seeds) =
        bind_prefix_generic_seeds(tc, method.generics, &call.node.generic_args, call.span)
    else {
        return Type::Infer;
    };
    seed_owner_args(&mut seeds, method.owner_generics, method.owner_args);

    let all_generics = combined_generic_params(method.owner_generics, method.generics);
    let Some(inst) = solve_generic_call_with(
        &all_generics,
        &seeds,
        method.params,
        method.ret,
        call,
        expected,
        tc,
        |vars, tc| constrain_aggregate_method_owner(method, vars, call.span, tc),
    ) else {
        return Type::Infer;
    };

    let (inferred_owner_args, method_args) =
        split_generic_args(&inst.args, method.owner_generics, method.generics);
    let all_args = combine_generic_args(&inferred_owner_args, &method_args);
    let const_bindings = super::combined_const_param_bindings(
        method.owner_generics,
        &inferred_owner_args,
        method.generics,
        &method_args,
    );

    let ret = if all_args.is_empty() {
        inst.ret
    } else {
        check_specialized_method_like_body(method, inst, &all_args, const_bindings, tc)
    };

    tc.record_call(
        call_id,
        method_call_target(method.target, method.name, &all_args),
    );
    ret
}

fn constrain_aggregate_method_owner(
    method: MethodCall<'_>,
    vars: &GenericSolverVars,
    span: Span,
    tc: &mut TypeChecker,
) {
    let MethodTarget::Aggregate {
        owner,
        receiver: Some(receiver),
    } = method.target
    else {
        return;
    };

    let template = owner_template(owner, method.owner_generics);
    let owner_handle = tc.solver.instantiate_generic_type(&template, vars);
    let receiver_handle = tc.type_handle(receiver);
    tc.expect_equal(span, owner_handle, receiver_handle);
}

fn check_specialized_method_like_body(
    method: MethodCall<'_>,
    inst: GenericCallInstantiation,
    args: &GenericArgs,
    const_bindings: Vec<(Ident, ConstValue)>,
    tc: &mut TypeChecker,
) -> Type {
    let GenericCallInstantiation {
        type_subst,
        const_subst,
        concrete_params,
        ret,
        ..
    } = inst;
    let body_ret = ret.clone();

    match method.target {
        MethodTarget::Aggregate { owner, receiver } => check_specialized_method_body(
            owner,
            method.name,
            receiver.clone(),
            &concrete_params,
            body_ret,
            args,
            type_subst,
            const_subst,
            const_bindings,
            tc,
        ),
        MethodTarget::Extend { extend, receiver } => check_specialized_extend_body(
            extend,
            method.name,
            receiver.clone(),
            &concrete_params,
            body_ret,
            args,
            type_subst,
            const_subst,
            const_bindings,
            tc,
        ),
    }
    ret
}

fn call_enum_variant(
    enum_key: &NominalKey,
    variant: Ident,
    schema: &VariantSchema,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> Type {
    let template_params = match schema {
        VariantSchema::Unit => vec![],
        VariantSchema::Tuple(tys) => tys.iter().cloned().map(FuncParam::immut).collect(),
        VariantSchema::Struct(_) => {
            return not_callable(nominal_type(enum_key), call, tc);
        }
    };
    let Some(enum_schema) = tc.decls.enum_schema(enum_key) else {
        return Type::Infer;
    };
    let generics = enum_schema.generics.clone();
    let Some(seeds) = bind_prefix_generic_seeds(tc, &generics, &call.node.generic_args, call.span)
    else {
        return Type::Infer;
    };
    let ret_template = owner_template(enum_key, &generics);
    let Some(inst) = solve_generic_call_with(
        &generics,
        &seeds,
        &template_params,
        &ret_template,
        call,
        expected,
        tc,
        |_, _| {},
    ) else {
        return Type::Infer;
    };

    tc.record_call(
        call_id,
        CallTarget::EnumVariant {
            enum_key: enum_key.clone(),
            variant,
        },
    );
    inst.ret
}

fn not_callable(ty: Type, call: &CallNode, tc: &mut TypeChecker) -> Type {
    tc.push_error(TypeError::NotCallable {
        ty,
        span: call.span,
    });
    for arg in &call.node.args {
        check_expr(arg, tc);
    }
    Type::Infer
}

fn func_type(params: &[FuncParam], ret: &Type) -> Type {
    Type::Func {
        params: params.to_vec(),
        ret: Box::new(ret.clone()),
    }
}

fn non_aggregate_field(ty: Type, field: Ident, span: Span, tc: &mut TypeChecker) -> Subject {
    tc.push_error(TypeError::FieldAccessOnNonAggregate { ty, field, span });
    Subject::Value(Type::Infer)
}

fn unknown_field(ty: Type, field: Ident, span: Span, tc: &mut TypeChecker) -> Subject {
    tc.push_error(TypeError::UnknownField { ty, field, span });
    Subject::Value(Type::Infer)
}

fn method_call_target(target: &MethodTarget, name: Ident, args: &GenericArgs) -> CallTarget {
    match target {
        MethodTarget::Aggregate { owner, .. } => CallTarget::Method {
            owner: owner.clone(),
            name,
            type_args: args.type_args.clone(),
            const_args: args.const_args.clone(),
        },
        MethodTarget::Extend { extend, receiver } => CallTarget::Extend {
            target: super::extend_callable_id(extend, name),
            receiver: receiver.clone(),
            args: args.clone(),
        },
    }
}

fn named_call_target(module: &ModuleScope, name: Ident) -> CallTarget {
    match module {
        ModuleScope::Root => CallTarget::Direct {
            module: ModuleScope::Root,
            name,
        },
        ModuleScope::Named(path) => CallTarget::ModuleFunction {
            module: path.clone(),
            name,
        },
    }
}

pub(super) fn check_args(
    args: &[ExprNode],
    params: &[FuncParam],
    call_span: Span,
    tc: &mut TypeChecker,
) {
    if !check_arg_count(args, params.len(), call_span, tc) {
        return;
    }

    let param_handles = params
        .iter()
        .map(|param| tc.type_handle(&param.ty))
        .collect::<Vec<_>>();
    check_call_arg_handles(args, &param_handles, tc);
}

fn check_arg_count(
    args: &[ExprNode],
    expected: usize,
    call_span: Span,
    tc: &mut TypeChecker,
) -> bool {
    if args.len() == expected {
        return true;
    }

    tc.push_error(TypeError::WrongArgCount {
        expected,
        found: args.len(),
        span: call_span,
    });
    for arg in args {
        check_expr(arg, tc);
    }
    false
}
