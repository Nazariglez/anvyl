use super::{
    ConstSubst, GenericArgs, GenericParams, TypeChecker, TypeError, TypeSubst,
    call_map::CallTarget,
    check_expr, check_expr_checked_with_hint,
    decls::{
        AggregateSchema, CallableKind, CallableParent, CallableRef, DeclarationIndex,
        ExtendMethodMatch, ExtendMethodSchema, ExtendSchema, MethodSchema, ModuleScope, NominalKey,
        ResolvedValue, ValueDecl, VariantSchema, nominal_type, owner_template,
    },
    generic_bind::bind_prefix_generic_seeds,
    infer::{GenericSolverSeeds, GenericSolverVars, TypeHandle},
};
use crate::{
    ast::{CallNode, ExprId, ExprKind, ExprNode, FieldAccessNode, FuncParam, Ident, Type},
    span::Span,
};

pub(super) enum Subject {
    Value(Type),
    NonAggregate(Type),
    Module(ModuleScope),
    Type(NominalKey),
    Callable {
        callee: CallableRef,
        surface_ty: Type,
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
                return Some(named_value_subject(&tc.decls, module, value_name, &value));
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
        Subject::Value(ty)
        | Subject::NonAggregate(ty)
        | Subject::Callable { surface_ty: ty, .. } => ty.clone(),
        Subject::Module(_) => Type::Void,
        Subject::Type(key) => nominal_type(key),
    }
}

fn apply_field(
    subject: &Subject,
    field: &FieldAccessNode,
    next_is_call: bool,
    tc: &mut TypeChecker,
) -> Subject {
    match subject {
        Subject::Value(ty) => {
            apply_value_field(ty.clone(), field.node.field, field.span, next_is_call, tc)
        }
        Subject::Module(scope) => apply_module_field(scope, field.node.field, field.span, tc),
        Subject::Type(key) => apply_type_field(key, field.node.field, field.span, tc),
        Subject::NonAggregate(_) | Subject::Callable { .. } => {
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

fn aggregate_method_subject(
    decls: &DeclarationIndex,
    agg: &AggregateSchema,
    receiver: Option<Type>,
    name: Ident,
    method: &MethodSchema,
) -> Subject {
    let callee = decls.callable_for_aggregate_method(agg, name, method, receiver);
    func_callable_subject(callee)
}

fn named_value_subject(
    decls: &DeclarationIndex,
    module: ModuleScope,
    name: Ident,
    value: &ValueDecl,
) -> Subject {
    let resolved = ResolvedValue {
        module,
        name,
        decl: value.clone(),
    };
    match decls.callable_for_value(&resolved) {
        Some(callee) => Subject::Callable {
            callee,
            surface_ty: value.ty().clone(),
        },
        None => Subject::Value(value.ty().clone()),
    }
}

fn extend_method_subject(
    decls: &DeclarationIndex,
    receiver: Type,
    extend: &ExtendSchema,
    name: Ident,
    method: &ExtendMethodSchema,
    owner_args: GenericArgs,
) -> Option<Subject> {
    decls
        .callable_for_extend_method(receiver, extend, name, method, owner_args)
        .map(func_callable_subject)
}

fn enum_variant_subject(
    decls: &DeclarationIndex,
    enum_key: &NominalKey,
    variant: Ident,
    schema: &VariantSchema,
) -> Subject {
    match decls.callable_for_variant(enum_key, variant, schema) {
        Some(callee) => Subject::Callable {
            callee,
            surface_ty: nominal_type(enum_key),
        },
        None => Subject::NonAggregate(nominal_type(enum_key)),
    }
}

fn func_callable_subject(callee: CallableRef) -> Subject {
    let surface_ty = func_type(&callee.def.sig.params, &callee.def.sig.ret);
    Subject::Callable { callee, surface_ty }
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
        return match extend_method_match_subject(&tc.decls, receiver, name, &matched) {
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
            &tc.decls,
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
    decls: &DeclarationIndex,
    receiver: Type,
    name: Ident,
    matched: &ExtendMethodMatch<'_>,
) -> Result<Subject, ExtendMethodError> {
    match matched {
        ExtendMethodMatch::Match {
            extend,
            method,
            owner_args: Ok(owner_args),
        } => Ok(
            extend_method_subject(decls, receiver, extend, name, method, owner_args.clone())
                .unwrap_or(Subject::Value(Type::Infer)),
        ),
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
        return named_value_subject(&tc.decls, module, value_name, &decl);
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
            return enum_variant_subject(&tc.decls, &schema.key, name, variant);
        }

        if let Some(agg) = tc.decls.aggregate(key)
            && let Some(method) = agg.methods.get(&name)
            && method.receiver.is_none()
        {
            return aggregate_method_subject(&tc.decls, agg, None, name, method);
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
    aggregate_method_subject(&tc.decls, agg, None, name, method)
}

fn apply_call(
    subject: &Subject,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> Type {
    match subject {
        Subject::Callable { callee, .. } => {
            check_callable_call(callee, call, call_id, expected, tc)
        }
        Subject::Value(ty) => call_value(ty.clone(), call, tc),
        Subject::NonAggregate(_) | Subject::Module(_) | Subject::Type(_) => {
            not_callable(subject_type(subject), call, tc)
        }
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

struct GenericCallInstantiation {
    args: GenericArgs,
    type_subst: TypeSubst,
    const_subst: ConstSubst,
    concrete_params: Vec<FuncParam>,
    ret: Type,
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

fn check_callable_call(
    callee: &CallableRef,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> Type {
    let Some(mut seeds) = bind_prefix_generic_seeds(
        tc,
        syntactic_generics(callee),
        &call.node.generic_args,
        call.span,
    ) else {
        return Type::Infer;
    };
    seed_owner_args(
        &mut seeds,
        &callee.def.sig.owner_generics,
        &callee.owner_args,
    );

    let all_generics =
        combined_generic_params(&callee.def.sig.owner_generics, &callee.def.sig.generics);
    let Some(inst) = solve_generic_call_with(
        &all_generics,
        &seeds,
        &callee.def.sig.params,
        &callee.def.sig.ret,
        call,
        expected,
        tc,
        |vars, tc| constrain_callable_owner(callee, vars, call.span, tc),
    ) else {
        return Type::Infer;
    };

    let (owner_args, callable_args) = split_generic_args(
        &inst.args,
        &callee.def.sig.owner_generics,
        &callee.def.sig.generics,
    );
    let args = combine_generic_args(&owner_args, &callable_args);
    let const_bindings = super::callable_const_bindings(
        &callee.def.sig.owner_generics,
        &owner_args,
        &callee.def.sig.generics,
        &callable_args,
    );
    let GenericCallInstantiation {
        type_subst,
        const_subst,
        concrete_params,
        ret,
        ..
    } = inst;
    let body_ret = ret.clone();

    super::check_specialized_callable_body(
        callee,
        &concrete_params,
        body_ret,
        &args,
        type_subst,
        const_subst,
        const_bindings,
        tc,
    );
    tc.record_call(
        call_id,
        CallTarget::Callable {
            id: callee.def.id.clone(),
            args,
        },
    );
    ret
}

fn syntactic_generics(callee: &CallableRef) -> &GenericParams {
    match callee.def.id.kind {
        CallableKind::EnumVariant => &callee.def.sig.owner_generics,
        CallableKind::Function
        | CallableKind::ExternFunction
        | CallableKind::StaticMethod
        | CallableKind::InstanceMethod
        | CallableKind::ExtendMethod => &callee.def.sig.generics,
    }
}

fn constrain_callable_owner(
    callee: &CallableRef,
    vars: &GenericSolverVars,
    span: Span,
    tc: &mut TypeChecker,
) {
    let CallableKind::InstanceMethod = callee.def.id.kind else {
        return;
    };
    let Some(receiver) = &callee.receiver_ty else {
        return;
    };
    let Some(CallableParent::Nominal(owner)) = &callee.def.id.parent else {
        return;
    };

    let template = owner_template(owner, &callee.def.sig.owner_generics);
    let owner_handle = tc.solver.instantiate_generic_type(&template, vars);
    let receiver_handle = tc.type_handle(receiver);
    tc.expect_equal(span, owner_handle, receiver_handle);
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
