use anvyx_externs::ReceiverMode;

use super::{
    CallForm, CallTarget, CheckedType, ConstSubst, ExternUseTarget, GenericArgs, GenericParams,
    MemberAccessKind, PlaceAccess, TypeChecker, TypeError, TypeSubst, VariantShape,
    check_arg_count, check_expr_checked, checked_type,
    decls::{
        AggregateSchema, CallableKind, CallableParent, CallableRef, DeclarationIndex,
        ExtendMethodMatch, ExtendMethodSchema, ExtendSchema, MethodSchema, ModuleScope, NominalKey,
        ResolvedValue, ValueDecl, VariantSchema, nominal_type, owner_template,
    },
    enum_variant::{self, ResolvedEnumVariant},
    extern_boundary,
    generic_bind::bind_prefix_generic_seeds,
    infer::{GenericSolverSeeds, GenericSolverVars, TypeHandle},
    place::{self, PlaceUseFacts, PlaceValue},
};
use crate::{
    ast::{
        CallNode, ExprId, ExprKind, ExprNode, FieldAccessNode, FuncParam, GenericArg, Ident, Type,
    },
    externs::catalog::{
        ExternMethodRef, ExternStaticRef, ExternTypeId, FunctionKey, ResolvedExternSignature,
        ResolvedExternTy,
    },
    span::Span,
};

pub(super) enum Subject {
    Value(PlaceValue),
    Module(ModuleScope),
    Type(NominalKey),
    QualifiedExtend {
        module: ModuleScope,
        name: Ident,
        span: Span,
    },
    Callable {
        callee: Box<CallableRef>,
        surface_ty: Type,
    },
    EnumVariant {
        resolved: ResolvedEnumVariant,
        surface_ty: Type,
    },
    ExternMethod {
        method_ref: ExternMethodRef,
        receiver: ReceiverMode,
        receiver_access: PlaceAccess,
        receiver_place: Option<PlaceUseFacts>,
        receiver_id: ExprId,
        name: Ident,
        signature: ResolvedExternSignature,
    },
    ExternStatic {
        static_ref: ExternStaticRef,
        signature: ResolvedExternSignature,
    },
    Error,
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
                return Some(named_value_subject(tc, module, value_name, &value));
            }
            if let Some(info) = tc.lookup(*name).cloned() {
                let checked = super::checked_from_handle(expr, tc.local_handle(info.type_id), tc);
                let access = info.kind.place_access();
                return Some(Subject::Value(PlaceValue {
                    checked,
                    access,
                    facts: Some(PlaceUseFacts::default()),
                }));
            }
            if let Some(scope) = tc.lookup_module_alias(*name) {
                return Some(Subject::Module(scope));
            }
            if let Some(key) = tc.resolve_visible_type_key(None, *name) {
                return Some(Subject::Type(key));
            }
            None
        }
        _ => Some(value_subject_checked(check_expr_checked(expr, tc))),
    }
}

pub(super) fn check_postfix_chain(
    chain: &PostfixChain,
    expr: &ExprNode,
    expected: Option<&TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let Some(mut subject) = resolve_base(chain.base, tc) else {
        if let ExprKind::Ident(name) = &chain.base.node.kind {
            tc.push_error(TypeError::UndefinedVariable {
                name: *name,
                span: chain.base.span,
            });
        }
        tc.set_type(chain.base.node.id, Type::Infer, chain.base.span);
        return checked_type(finish_chain(chain, expr, tc), tc);
    };

    tc.set_type(chain.base.node.id, subject_type(&subject), chain.base.span);

    for (i, step) in chain.steps.iter().enumerate() {
        let is_last_step = i + 1 == chain.steps.len();
        let next_is_call = matches!(chain.steps.get(i + 1), Some(PostfixStep::Call { .. }));
        subject = match step {
            PostfixStep::Field { node, id } => {
                let field_expected = (is_last_step && !next_is_call)
                    .then(|| expected.cloned())
                    .flatten();
                let subject =
                    apply_field(&subject, node, next_is_call, field_expected.as_ref(), tc);
                tc.set_type(*id, subject_type(&subject), node.span);
                subject
            }
            PostfixStep::Call { node, id } => {
                let call_expected = is_last_step.then(|| expected.cloned()).flatten();
                let checked = apply_call(&subject, node, *id, call_expected, tc);
                tc.set_type(*id, checked.ty.clone(), node.span);
                if matches!(subject, Subject::Error) {
                    Subject::Error
                } else {
                    value_subject_checked(checked)
                }
            }
        };
    }

    if let Subject::Value(value) = &subject {
        place::record_value_read(expr.node.id, value, tc);
    }

    let ty = subject_type(&subject);
    let handle = tc.set_type(expr.node.id, ty.clone(), expr.span);
    CheckedType {
        ty,
        handle,
        contains_extern_any: subject_contains_extern_any(&subject),
    }
}

fn finish_chain(chain: &PostfixChain, expr: &ExprNode, tc: &mut TypeChecker) -> Type {
    for step in &chain.steps {
        match step {
            PostfixStep::Field { node, id } => {
                tc.set_type(*id, Type::Infer, node.span);
            }
            PostfixStep::Call { node, id } => {
                for arg in &node.node.args {
                    check_expr_checked(arg, tc);
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
        Subject::Value(value) => value.checked.ty.clone(),
        Subject::Callable { surface_ty: ty, .. } | Subject::EnumVariant { surface_ty: ty, .. } => {
            ty.clone()
        }
        Subject::ExternMethod { signature, .. } | Subject::ExternStatic { signature, .. } => {
            signature.to_func_type()
        }
        Subject::Module(_) => Type::Void,
        Subject::Type(key) => nominal_type(key),
        Subject::QualifiedExtend { .. } => Type::Infer,
        Subject::Error => Type::Infer,
    }
}

fn value_subject(ty: Type, tc: &TypeChecker) -> Subject {
    value_subject_checked(checked_type(ty, tc))
}

fn value_subject_checked(checked: CheckedType) -> Subject {
    Subject::Value(PlaceValue::not_place(checked))
}

fn subject_contains_extern_any(subject: &Subject) -> bool {
    match subject {
        Subject::Value(value) => value.checked.contains_extern_any,
        _ => false,
    }
}

fn apply_field(
    subject: &Subject,
    field: &FieldAccessNode,
    next_is_call: bool,
    expected: Option<&TypeHandle>,
    tc: &mut TypeChecker,
) -> Subject {
    let kind = if next_is_call {
        MemberAccessKind::Method
    } else {
        MemberAccessKind::Field
    };

    match subject {
        Subject::Value(value) => apply_value_field(
            value.checked.ty.clone(),
            value.access,
            value.facts.as_ref(),
            field.node.target.node.id,
            field.node.field,
            field.span,
            kind,
            tc,
        ),
        Subject::Module(scope) => apply_module_field(scope, field.node.field, field.span, kind, tc),
        Subject::Type(key) => {
            apply_type_field(key, field.node.field, field.span, kind, expected, tc)
        }
        Subject::Callable { .. }
        | Subject::EnumVariant { .. }
        | Subject::QualifiedExtend { .. }
        | Subject::ExternMethod { .. }
        | Subject::ExternStatic { .. } => {
            field_access_on_non_aggregate(subject, field.node.field, kind, field.span, tc)
        }
        Subject::Error => Subject::Error,
    }
}

fn field_access_on_non_aggregate(
    subject: &Subject,
    member: Ident,
    kind: MemberAccessKind,
    span: Span,
    tc: &mut TypeChecker,
) -> Subject {
    tc.push_error(TypeError::MemberAccessOnNonAggregate {
        ty: subject_type(subject),
        member,
        kind,
        span,
    });
    Subject::Error
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
    tc: &TypeChecker,
    module: ModuleScope,
    name: Ident,
    value: &ValueDecl,
) -> Subject {
    let resolved = ResolvedValue {
        module,
        name,
        decl: value.clone(),
    };
    match tc.decls.callable_for_value(&resolved) {
        Some(callee) => Subject::Callable {
            callee: Box::new(callee),
            surface_ty: value.ty().clone(),
        },
        None => value_subject(value.ty().clone(), tc),
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
    resolved: ResolvedEnumVariant,
    expected: Option<&TypeHandle>,
    tc: &TypeChecker,
) -> Subject {
    let expected_ty = expected.map(|handle| tc.handle_type(handle));
    let surface_ty = expected_ty
        .as_ref()
        .filter(|ty| tc.decls.key_for_type(ty).as_ref() == Some(&resolved.key))
        .cloned()
        .unwrap_or_else(|| resolved.owner_ty());
    Subject::EnumVariant {
        resolved,
        surface_ty,
    }
}

fn func_callable_subject(callee: CallableRef) -> Subject {
    let surface_ty = func_type(&callee.def.sig.params, &callee.def.sig.ret);
    Subject::Callable {
        callee: Box::new(callee),
        surface_ty,
    }
}

fn apply_value_field(
    receiver: Type,
    receiver_access: PlaceAccess,
    receiver_place: Option<&PlaceUseFacts>,
    receiver_id: ExprId,
    name: Ident,
    span: Span,
    kind: MemberAccessKind,
    tc: &mut TypeChecker,
) -> Subject {
    let key = tc.decls.key_for_type(&receiver);

    if let Some(owner) = tc.extern_type_id(&receiver) {
        return extern_value_member_subject(
            owner,
            name,
            receiver_access,
            receiver_place,
            receiver_id,
            span,
            kind,
            tc,
        );
    }
    if let Some(facts) = receiver_place {
        place::record_facts_read(receiver_id, facts, tc);
    }

    if let Some(key) = key.as_ref()
        && let Some(agg) = tc.decls.aggregate(key)
        && let Some(subject) = aggregate_field_subject(agg, &receiver, name, kind, tc)
    {
        return subject;
    }

    if let Some(matched) = tc.find_extend_method(&receiver, name) {
        return match extend_method_match_subject(tc, receiver, name, &matched) {
            Ok(subject) => subject,
            Err(ExtendMethodError::Unbound(names)) => {
                tc.push_unbound_generic_errors(names, span);
                Subject::Error
            }
            Err(ExtendMethodError::Ambiguous { receiver, name }) => {
                tc.push_error(TypeError::AmbiguousExtendMethod {
                    receiver,
                    name,
                    span,
                });
                Subject::Error
            }
        };
    }

    if key.is_some() {
        unknown_member(receiver, name, kind, span, tc)
    } else {
        non_aggregate_member(receiver, name, kind, span, tc)
    }
}

fn extern_value_member_subject(
    owner: ExternTypeId,
    name: Ident,
    receiver_access: PlaceAccess,
    receiver_place: Option<&PlaceUseFacts>,
    receiver_id: ExprId,
    span: Span,
    kind: MemberAccessKind,
    tc: &mut TypeChecker,
) -> Subject {
    let owner_ty = nominal_type(&tc.extern_type(owner).nominal);
    match kind {
        MemberAccessKind::Field => {
            let Some(field) = place::resolve_extern_field(owner, name, receiver_access, tc) else {
                return unknown_member(owner_ty, name, kind, span, tc);
            };
            let field_ref = field.field_ref;
            let access = field.access;
            let ty = field.decl.ty.ty.clone();
            let contains_any = field.decl.ty.contains_any();
            let handle = tc.type_handle(&ty);
            Subject::Value(PlaceValue::new(
                CheckedType {
                    ty,
                    handle,
                    contains_extern_any: contains_any,
                },
                access,
                Some(PlaceUseFacts::for_extern_field(receiver_place, field_ref)),
            ))
        }
        MemberAccessKind::Method => {
            let Some((method, decl)) = tc.externs.method(owner, name) else {
                return unknown_member(owner_ty, name, kind, span, tc);
            };
            Subject::ExternMethod {
                method_ref: method,
                receiver: decl.receiver,
                receiver_access,
                receiver_place: receiver_place.cloned(),
                receiver_id,
                name: decl.name,
                signature: decl.signature.clone(),
            }
        }
    }
}

fn aggregate_field_subject(
    agg: &AggregateSchema,
    receiver: &Type,
    name: Ident,
    kind: MemberAccessKind,
    tc: &TypeChecker,
) -> Option<Subject> {
    match kind {
        MemberAccessKind::Field => tc
            .decls
            .aggregate_field_type(receiver, name)
            .map(|ty| value_subject(ty, tc))
            .or_else(|| {
                let method = agg.methods.get(&name)?;
                Some(aggregate_method_subject(
                    &tc.decls,
                    agg,
                    Some(receiver.clone()),
                    name,
                    method,
                ))
            }),
        MemberAccessKind::Method => {
            let method = agg.methods.get(&name)?;
            method.receiver?;
            Some(aggregate_method_subject(
                &tc.decls,
                agg,
                Some(receiver.clone()),
                name,
                method,
            ))
        }
    }
}

enum ExtendMethodError {
    Unbound(Vec<Ident>),
    Ambiguous { receiver: Type, name: Ident },
}

fn extend_method_match_subject(
    tc: &TypeChecker,
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
            &tc.decls,
            receiver,
            extend,
            name,
            method,
            owner_args.clone(),
        )
        .unwrap_or_else(|| value_subject(Type::Infer, tc))),
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
    kind: MemberAccessKind,
    tc: &mut TypeChecker,
) -> Subject {
    if let Some(module) = tc.exported_module_in_module(scope, name) {
        return Subject::Module(module);
    }
    if let Some((module, value_name, decl)) = tc.exported_value_in_module(scope, name) {
        return named_value_subject(tc, module, value_name, &decl);
    }
    if let Some(key) = tc.exported_type_in_module(scope, name) {
        return Subject::Type(key);
    }
    if kind == MemberAccessKind::Method && tc.decls.module_surface_has_extend_method(scope, name) {
        return Subject::QualifiedExtend {
            module: scope.clone(),
            name,
            span,
        };
    }
    tc.push_error(TypeError::UndefinedModuleMember {
        module: scope.clone(),
        name,
        span,
    });
    Subject::Error
}

fn apply_type_field(
    key: &NominalKey,
    name: Ident,
    span: Span,
    kind: MemberAccessKind,
    expected: Option<&TypeHandle>,
    tc: &mut TypeChecker,
) -> Subject {
    if let Some(owner) = tc.externs.type_by_nominal(key) {
        return extern_type_member_subject(owner, name, span, kind, tc);
    }

    if let Some(schema) = tc.decls.enum_schema(key) {
        if schema.variants.contains_key(&name) {
            let resolved = enum_variant::resolve_member(tc, key, name, span)
                .expect("variant exists in enum schema");
            return enum_variant_subject(resolved, expected, tc);
        }

        if let Some(agg) = tc.decls.aggregate(key)
            && let Some(method) = agg.methods.get(&name)
            && method.receiver.is_none()
        {
            return aggregate_method_subject(&tc.decls, agg, None, name, method);
        }
        enum_variant::resolve_member(tc, key, name, span);
        return Subject::Error;
    }
    let Some(agg) = tc.decls.aggregate(key) else {
        return unknown_member(nominal_type(key), name, kind, span, tc);
    };
    let Some(method) = agg.methods.get(&name) else {
        return unknown_member(nominal_type(key), name, kind, span, tc);
    };
    if method.receiver.is_some() {
        return unknown_member(nominal_type(key), name, kind, span, tc);
    }
    aggregate_method_subject(&tc.decls, agg, None, name, method)
}

fn extern_type_member_subject(
    owner: ExternTypeId,
    name: Ident,
    span: Span,
    kind: MemberAccessKind,
    tc: &mut TypeChecker,
) -> Subject {
    let owner_ty = nominal_type(&tc.extern_type(owner).nominal);
    let MemberAccessKind::Method = kind else {
        return unknown_member(owner_ty, name, kind, span, tc);
    };
    let Some((method, decl)) = tc.externs.static_method(owner, name) else {
        return unknown_member(owner_ty, name, kind, span, tc);
    };
    Subject::ExternStatic {
        static_ref: method,
        signature: decl.signature.clone(),
    }
}

fn apply_call(
    subject: &Subject,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    match subject {
        Subject::Callable { callee, .. } => {
            check_callable_call(callee, call, call_id, expected, tc)
        }
        Subject::QualifiedExtend { module, name, span } => {
            check_qualified_extend_call(module, *name, *span, call, call_id, expected, tc)
        }
        Subject::EnumVariant { resolved, .. } => {
            check_enum_variant_call(resolved, call, call_id, expected, tc)
        }
        Subject::ExternMethod {
            method_ref,
            receiver,
            receiver_access,
            receiver_place,
            receiver_id,
            name,
            signature,
        } => check_extern_method_call(
            ExternMethodCall {
                method_ref: *method_ref,
                receiver: *receiver,
                receiver_access: *receiver_access,
                receiver_place: receiver_place.as_ref(),
                receiver_id: *receiver_id,
                name: *name,
                signature,
            },
            call,
            call_id,
            expected,
            tc,
        ),
        Subject::ExternStatic {
            static_ref,
            signature,
        } => check_extern_static_call(*static_ref, signature, call, call_id, expected, tc),
        Subject::Value(value) => {
            place::record_value_read(call.node.func.node.id, value, tc);
            checked_type(call_value(value.checked.ty.clone(), call, tc), tc)
        }
        Subject::Module(_) | Subject::Type(_) => {
            checked_type(not_callable(subject_type(subject), call, tc), tc)
        }
        Subject::Error => {
            for arg in &call.node.args {
                check_expr_checked(arg, tc);
            }
            checked_type(Type::Infer, tc)
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

struct ExternMethodCall<'a> {
    method_ref: ExternMethodRef,
    receiver: ReceiverMode,
    receiver_access: PlaceAccess,
    receiver_place: Option<&'a PlaceUseFacts>,
    receiver_id: ExprId,
    name: Ident,
    signature: &'a ResolvedExternSignature,
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
    args: &[ExprNode],
    call_span: Span,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
    add_constraints: impl FnOnce(&GenericSolverVars, &mut TypeChecker),
) -> Option<GenericCallInstantiation> {
    if !check_arg_count(args, template_params.len(), call_span, tc) {
        return None;
    }

    let error_count = tc.errors.len();
    for param in template_params {
        tc.substitute_checked(&param.ty, &seeds.type_args, &seeds.const_args, call_span);
    }
    tc.substitute_checked(template_ret, &seeds.type_args, &seeds.const_args, call_span);
    if tc.errors.len() != error_count {
        for arg in args {
            check_expr_checked(arg, tc);
        }
        return None;
    }

    let vars = tc.solver.generic_solver_vars(generics, seeds, call_span);
    add_constraints(&vars, tc);
    let mut failed = tc.solve_constraints();

    let param_handles = instantiate_param_handles(template_params, &vars, tc);
    let ret_handle = tc.solver.instantiate_generic_type(template_ret, &vars);

    failed |= check_call_arg_handles(args, &param_handles, tc);
    failed |= constrain_expected_return(call_span, ret_handle, expected, tc);

    if failed {
        return None;
    }

    let args = match tc.solver.finalize_generic_args(generics, &vars) {
        Ok(args) => args,
        Err(unbound) => {
            tc.push_unbound_generic_errors(unbound, call_span);
            return None;
        }
    };

    let (type_subst, const_subst) = generics.substitutions(&args);
    let concrete_params =
        substitute_params_checked(template_params, &type_subst, &const_subst, call_span, tc);
    let ret = tc.substitute_checked(template_ret, &type_subst, &const_subst, call_span);

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
        let checked = super::check_value_expr_checked_with_hint(arg, Some(param.clone()), tc);
        tc.reject_extern_any_escape(&checked, arg.span);
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

fn check_enum_variant_call(
    resolved: &ResolvedEnumVariant,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let can_call = match &resolved.schema {
        VariantSchema::Tuple(_) => true,
        VariantSchema::Unit => call.node.args.is_empty(),
        VariantSchema::Struct(_) => false,
    };
    if !can_call {
        enum_variant::push_shape_mismatch(tc, resolved, VariantShape::Tuple, call.span);
        return check_unhinted_args(&call.node.args, tc);
    }
    let Some(callee) =
        tc.decls
            .callable_for_variant(&resolved.key, resolved.variant, &resolved.schema)
    else {
        return checked_type(Type::Infer, tc);
    };
    check_callable_call_with_args(
        &callee,
        &call.node.args,
        &call.node.generic_args,
        call.span,
        call_id,
        CallForm::Normal,
        expected,
        tc,
    )
}

fn check_callable_call(
    callee: &CallableRef,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    if callee.def.id.kind == CallableKind::ExternFunction {
        return check_extern_function_call(callee, call, call_id, expected, tc);
    }
    check_callable_call_with_args(
        callee,
        &call.node.args,
        &call.node.generic_args,
        call.span,
        call_id,
        CallForm::Normal,
        expected,
        tc,
    )
}

fn check_callable_call_with_args(
    callee: &CallableRef,
    args: &[ExprNode],
    generic_args: &[GenericArg],
    call_span: Span,
    call_id: ExprId,
    form: CallForm,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let Some(mut seeds) =
        bind_prefix_generic_seeds(tc, syntactic_generics(callee), generic_args, call_span)
    else {
        return checked_type(Type::Infer, tc);
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
        args,
        call_span,
        expected,
        tc,
        |vars, tc| constrain_callable_owner(callee, vars, call_span, tc),
    ) else {
        return checked_type(Type::Infer, tc);
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
    let id = callee.def.id.clone();
    let target = match form {
        CallForm::Normal => CallTarget::new(id, args),
        CallForm::QualifiedExtend { receiver } => CallTarget::qualified_extend(id, args, receiver),
    };
    tc.record_call(call_id, target);
    checked_type(ret, tc)
}

fn check_qualified_extend_call(
    module: &ModuleScope,
    name: Ident,
    span: Span,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let Some((receiver_expr, args)) = call.node.args.split_first() else {
        tc.push_error(TypeError::WrongArgCount {
            expected: 1,
            found: 0,
            span: call.span,
        });
        return checked_type(Type::Infer, tc);
    };

    let receiver = check_expr_checked(receiver_expr, tc);
    tc.reject_extern_any_escape(&receiver, receiver_expr.span);

    let Some(matched) = tc
        .decls
        .find_extend_method_in_module_surface(module, &receiver.ty, name)
    else {
        tc.push_error(TypeError::UnknownMember {
            ty: receiver.ty,
            member: name,
            kind: MemberAccessKind::Method,
            span,
        });
        return check_unhinted_args(args, tc);
    };

    let (extend, method, owner_args) = match matched {
        ExtendMethodMatch::Match {
            extend,
            method,
            owner_args: Ok(owner_args),
        } => (extend, method, owner_args),
        ExtendMethodMatch::Match {
            owner_args: Err(unbound),
            ..
        } => {
            tc.push_unbound_generic_errors(unbound, span);
            return check_unhinted_args(args, tc);
        }
        ExtendMethodMatch::Ambiguous => {
            tc.push_error(TypeError::AmbiguousExtendMethod {
                receiver: receiver.ty,
                name,
                span,
            });
            return check_unhinted_args(args, tc);
        }
    };

    let Some(callee) =
        tc.decls
            .callable_for_extend_method(receiver.ty, extend, name, method, owner_args)
    else {
        return checked_type(Type::Infer, tc);
    };
    tc.set_type(
        call.node.func.node.id,
        func_type(&callee.def.sig.params, &callee.def.sig.ret),
        call.node.func.span,
    );
    check_callable_call_with_args(
        &callee,
        args,
        &call.node.generic_args,
        call.span,
        call_id,
        CallForm::QualifiedExtend {
            receiver: receiver_expr.node.id,
        },
        expected,
        tc,
    )
}

fn check_unhinted_args(args: &[ExprNode], tc: &mut TypeChecker) -> CheckedType {
    for arg in args {
        check_expr_checked(arg, tc);
    }
    checked_type(Type::Infer, tc)
}

fn check_extern_function_call(
    callee: &CallableRef,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let key = FunctionKey {
        module: callee.def.id.module.clone(),
        name: callee.def.id.name,
    };
    let Some(id) = tc.externs.function_by_key(&key) else {
        debug_assert!(false, "extern function declaration missing catalog target");
        tc.push_error(TypeError::NotCallable {
            ty: Type::Func {
                params: callee.def.sig.params.clone(),
                ret: Box::new(callee.def.sig.ret.clone()),
            },
            span: call.span,
        });
        for arg in &call.node.args {
            check_expr_checked(arg, tc);
        }
        return checked_type(Type::Infer, tc);
    };

    let signature = tc.externs.function(id).signature.clone();
    check_extern_call(
        ExternUseTarget::Function(id),
        &signature,
        call,
        call_id,
        expected,
        tc,
    )
}

fn check_extern_method_call(
    method: ExternMethodCall<'_>,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    match method.receiver {
        ReceiverMode::Mutable => {
            if let Some(error) = method
                .receiver_access
                .mut_borrow_error(method.name, call.span)
            {
                tc.push_error(error);
            } else if let Some(facts) = method.receiver_place {
                place::record_facts_write(method.receiver_id, facts, tc);
            }
        }
        ReceiverMode::Value | ReceiverMode::Shared => {
            if let Some(facts) = method.receiver_place {
                place::record_facts_read(method.receiver_id, facts, tc);
            }
        }
    }
    check_extern_call(
        ExternUseTarget::Method(method.method_ref),
        method.signature,
        call,
        call_id,
        expected,
        tc,
    )
}

fn check_extern_static_call(
    static_ref: ExternStaticRef,
    signature: &ResolvedExternSignature,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    check_extern_call(
        ExternUseTarget::Static(static_ref),
        signature,
        call,
        call_id,
        expected,
        tc,
    )
}

fn check_extern_call(
    target: ExternUseTarget,
    signature: &ResolvedExternSignature,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    if !call.node.generic_args.is_empty() {
        tc.push_error(TypeError::GenericArity(super::ArityError::TypeArgs {
            expected: 0,
            found: call.node.generic_args.len(),
        }));
        for arg in &call.node.args {
            check_expr_checked(arg, tc);
        }
        return checked_type(Type::Infer, tc);
    }
    if extern_boundary::check_call(signature, &call.node.args, call.span, expected, tc) {
        tc.record_extern_use(call_id, target);
    }
    checked_extern_ret(&signature.ret, tc)
}

fn checked_extern_ret(ret: &ResolvedExternTy, tc: &TypeChecker) -> CheckedType {
    CheckedType {
        ty: ret.ty.clone(),
        handle: tc.type_handle(&ret.ty),
        contains_extern_any: ret.contains_any(),
    }
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
        check_expr_checked(arg, tc);
    }
    Type::Infer
}

fn func_type(params: &[FuncParam], ret: &Type) -> Type {
    Type::Func {
        params: params.to_vec(),
        ret: Box::new(ret.clone()),
    }
}

fn non_aggregate_member(
    ty: Type,
    member: Ident,
    kind: MemberAccessKind,
    span: Span,
    tc: &mut TypeChecker,
) -> Subject {
    tc.push_error(TypeError::MemberAccessOnNonAggregate {
        ty,
        member,
        kind,
        span,
    });
    Subject::Error
}

fn unknown_member(
    ty: Type,
    member: Ident,
    kind: MemberAccessKind,
    span: Span,
    tc: &mut TypeChecker,
) -> Subject {
    tc.push_error(TypeError::UnknownMember {
        ty,
        member,
        kind,
        span,
    });
    Subject::Error
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
