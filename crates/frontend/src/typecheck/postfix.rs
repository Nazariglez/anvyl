use anvyx_externs::ReceiverMode;

use super::{
    CallForm, CallTarget, CheckedType, ConstSubst, ExternUseTarget, GenericArgs, GenericParams,
    MemberAccessKind, PlaceAccess, TypeChecker, TypeError, TypeSubst, VariantShape,
    check_arg_count, check_arg_range, check_expr_checked, checked_type,
    decls::{
        CallableKind, CallableParent, CallableRef, DeclError, ExtendMethodMatch,
        ExtendMethodSchema, ExtendSchema, MethodKey, MethodMode, MethodSurface, ModuleMemberLookup,
        ModuleScope, ResolvedValue, ValueDecl, VariantSchema, nominal_type, owner_template,
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
    Type(Type),
    QualifiedExtend {
        module: ModuleScope,
        name: Ident,
        span: Span,
    },
    Callable {
        callee: Box<CallableRef>,
        surface_ty: Type,
        receiver: Option<SourceReceiver>,
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

pub(super) struct SourceReceiver {
    mutable: bool,
    access: PlaceAccess,
    facts: Option<PlaceUseFacts>,
    expr_id: ExprId,
    name: Ident,
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
            if let Some(ty) = tc.visible_type_subject(*name) {
                return Some(Subject::Type(ty));
            }
            None
        }
        ExprKind::TypeSubject(ty) => Some(match tc.resolve_type_subject(ty, expr.span) {
            Some(ty) => Subject::Type(ty),
            None => Subject::Error,
        }),
        _ => Some(Subject::Value(check_receiver_value(expr, tc))),
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

    let mut optional_chain = false;
    for (i, step) in chain.steps.iter().enumerate() {
        let is_last_step = i + 1 == chain.steps.len();
        let next_is_call = matches!(chain.steps.get(i + 1), Some(PostfixStep::Call { .. }));
        subject = match step {
            PostfixStep::Field { node, id } => {
                if node.node.safe {
                    subject = safe_subject(&subject, node.span, tc);
                    optional_chain = true;
                }
                let field_expected = (is_last_step && !next_is_call)
                    .then(|| expected_for_chain(expected, optional_chain, tc))
                    .flatten();
                let subject =
                    apply_field(&subject, node, next_is_call, field_expected.as_ref(), tc);
                tc.set_type(*id, chain_type(&subject, optional_chain, tc), node.span);
                subject
            }
            PostfixStep::Call { node, id } => {
                if node.node.safe {
                    subject = safe_subject(&subject, node.span, tc);
                    optional_chain = true;
                }
                let call_expected = is_last_step
                    .then(|| expected_for_chain(expected, optional_chain, tc))
                    .flatten();
                let checked = apply_call(&subject, node, *id, call_expected, tc);
                tc.set_type(
                    *id,
                    wrap_optional(checked.ty.clone(), optional_chain, tc),
                    node.span,
                );
                if matches!(subject, Subject::Error) {
                    Subject::Error
                } else {
                    value_subject_checked(checked)
                }
            }
        };
    }

    if let Subject::Callable { callee, .. } = &subject
        && matches!(callee.def.sig.ret, Type::InferReturn)
    {
        tc.push_error(TypeError::InferReturnValue { span: expr.span });
    }

    if let Subject::Value(value) = &subject {
        place::record_value_read(expr.node.id, value, tc);
    }

    if let Subject::Type(ty) = &subject {
        tc.push_error(TypeError::TypeUsedAsValue {
            ty: ty.clone(),
            span: expr.span,
        });
        return super::checked_from_type(expr, Type::Infer, tc);
    }

    let ty = chain_type(&subject, optional_chain, tc);
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
        Subject::Type(ty) => ty.clone(),
        Subject::QualifiedExtend { .. } | Subject::Error => Type::Infer,
    }
}

fn chain_type(subject: &Subject, optional: bool, tc: &TypeChecker) -> Type {
    wrap_optional(subject_type(subject), optional, tc)
}

fn wrap_optional(ty: Type, optional: bool, tc: &TypeChecker) -> Type {
    if optional {
        tc.optional_chain_result_type(ty)
    } else {
        ty
    }
}

fn expected_for_chain(
    expected: Option<&TypeHandle>,
    optional: bool,
    tc: &TypeChecker,
) -> Option<TypeHandle> {
    let expected = expected.cloned()?;
    if !optional {
        return Some(expected);
    }
    let ty = tc.handle_type(&expected);
    tc.decls
        .core_option_inner(&ty)
        .map(|inner| tc.type_handle(inner))
}

fn safe_subject(subject: &Subject, span: Span, tc: &mut TypeChecker) -> Subject {
    let Subject::Value(value) = subject else {
        tc.push_error(TypeError::OptionalChainingOnNonOptional { span });
        return Subject::Error;
    };
    if matches!(value.checked.ty, Type::Infer) {
        return Subject::Value(value.clone());
    }
    let inner = tc.optional_chain_inner_type(&value.checked.ty, span);
    let mut checked = checked_type(inner, tc);
    checked.contains_extern_any = value.checked.contains_extern_any;
    Subject::Value(PlaceValue::not_place(checked))
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
        Subject::Type(ty) => apply_type_field(ty, field.node.field, field.span, kind, expected, tc),
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
        Some(callee) => callable_subject(callee, None),
        None => value_subject(value.ty().clone(), tc),
    }
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

fn callable_subject(callee: CallableRef, receiver: Option<SourceReceiver>) -> Subject {
    let surface_ty = func_type(&callee.def.sig.params, &callee.def.sig.ret);
    Subject::Callable {
        callee: Box::new(callee),
        surface_ty,
        receiver,
    }
}

fn check_receiver_value(expr: &ExprNode, tc: &mut TypeChecker) -> PlaceValue {
    let ExprKind::Index(index) = &expr.node.kind else {
        return place::check_place(expr, tc).value;
    };

    let target = place::check_place(&index.node.target, tc);
    let indexed = super::check_index_access(index, &target.value.checked, tc);
    let indexed_place = !index.node.safe
        && matches!(
            &target.value.checked.ty,
            Type::List { .. } | Type::Array { .. } | Type::Slice { .. }
        );
    let ty = if indexed_place {
        indexed.write_ty
    } else {
        indexed.read_ty
    };
    let mut checked = super::checked_from_type(expr, ty, tc);
    checked.contains_extern_any = indexed.contains_extern_any;

    if indexed_place {
        return PlaceValue::new(
            checked,
            place::projected_field_access(target.value.access),
            target.value.facts,
        );
    }

    place::record_value_read(index.node.target.node.id, &target.value, tc);
    PlaceValue::not_place(checked)
}

fn source_receiver(
    mode: MethodMode,
    access: PlaceAccess,
    facts: Option<&PlaceUseFacts>,
    expr_id: ExprId,
    name: Ident,
) -> SourceReceiver {
    SourceReceiver {
        mutable: matches!(mode, MethodMode::Instance { mutable: true }),
        access,
        facts: facts.cloned(),
        expr_id,
        name,
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
        let static_on_value = kind == MemberAccessKind::Method
            && tc.externs.method(owner, name).is_none()
            && (tc.externs.static_method(owner, name).is_some()
                || tc.find_static_extend_method(&receiver, name).is_some());
        if static_on_value {
            tc.push_error(TypeError::StaticMethodOnValue {
                ty: receiver,
                method: name,
                span,
            });
            return Subject::Error;
        }
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
    let mut static_method_on_value = false;
    if let Some(key) = key.as_ref()
        && let Some(agg) = tc.decls.aggregate(key).cloned()
    {
        if kind == MemberAccessKind::Field
            && let Some(ty) = tc.decls.aggregate_field_type(&receiver, name)
        {
            tc.check_field_access_policy(&receiver, name, span);
            return Subject::Value(PlaceValue::new(
                checked_type(ty, tc),
                place::projected_field_access(receiver_access),
                receiver_place.cloned(),
            ));
        }
        if let Some(method) = agg.methods.get(&MethodKey::instance(name)) {
            tc.check_access_policy(
                &method.policy,
                MemberAccessKind::Method,
                name,
                &receiver,
                &key.module,
                span,
            );
            let callee =
                tc.decls
                    .callable_for_aggregate_method(&agg, name, method, receiver.clone());
            return callable_subject(
                callee,
                Some(source_receiver(
                    method.mode,
                    receiver_access,
                    receiver_place,
                    receiver_id,
                    name,
                )),
            );
        }
        static_method_on_value = agg.methods.contains_key(&MethodKey::static_(name));
    }

    if let Some(matched) = tc.find_extend_method(&receiver, name) {
        let parts = extend_method_parts(receiver.clone(), name, &matched);
        return match parts {
            Ok((extend, method, receiver_ty, owner_args)) => {
                let policy = method.policy.clone();
                let origin = extend.origin.clone();
                let owner_ty = receiver.clone();
                let callee = tc.decls.callable_for_extend_method(
                    receiver_ty,
                    extend,
                    name,
                    method,
                    owner_args,
                );
                let receiver = source_receiver(
                    method.mode,
                    receiver_access,
                    receiver_place,
                    receiver_id,
                    name,
                );
                tc.check_access_policy(
                    &policy,
                    MemberAccessKind::Method,
                    name,
                    &owner_ty,
                    &origin,
                    span,
                );
                callable_subject(callee, Some(receiver))
            }
            Err(error) => {
                push_extend_method_error(tc, error, span);
                Subject::Error
            }
        };
    }

    static_method_on_value |= tc.find_static_extend_method(&receiver, name).is_some();

    if static_method_on_value {
        tc.push_error(TypeError::StaticMethodOnValue {
            ty: receiver,
            method: name,
            span,
        });
        Subject::Error
    } else if key.is_some() {
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

enum ExtendMethodError {
    Unbound(Vec<Ident>),
    Ambiguous { receiver: Type, name: Ident },
}

fn extend_method_parts<'a>(
    receiver: Type,
    name: Ident,
    matched: &'a ExtendMethodMatch<'a>,
) -> Result<(&'a ExtendSchema, &'a ExtendMethodSchema, Type, GenericArgs), ExtendMethodError> {
    match matched {
        ExtendMethodMatch::Match {
            extend,
            method,
            receiver_ty,
            owner_args: Ok(owner_args),
        } => Ok((extend, method, receiver_ty.clone(), owner_args.clone())),
        ExtendMethodMatch::Match {
            owner_args: Err(unbound),
            ..
        } => Err(ExtendMethodError::Unbound(unbound.clone())),
        ExtendMethodMatch::Ambiguous => Err(ExtendMethodError::Ambiguous { receiver, name }),
    }
}

fn push_extend_method_error(tc: &mut TypeChecker, error: ExtendMethodError, span: Span) {
    match error {
        ExtendMethodError::Unbound(names) => tc.push_unbound_generic_errors(names, span),
        ExtendMethodError::Ambiguous { receiver, name } => {
            tc.push_error(TypeError::AmbiguousExtendMethod {
                receiver,
                name,
                span,
            });
        }
    }
}

fn apply_module_field(
    scope: &ModuleScope,
    name: Ident,
    span: Span,
    kind: MemberAccessKind,
    tc: &mut TypeChecker,
) -> Subject {
    let mut private = false;
    match tc.decls.module_module(scope, name) {
        ModuleMemberLookup::Found(module) => return Subject::Module(module),
        ModuleMemberLookup::Private => private = true,
        ModuleMemberLookup::Missing => {}
    }
    match tc.decls.module_value(scope, name) {
        ModuleMemberLookup::Found(value) => {
            let (module, value_name, decl) = TypeChecker::resolved_value(value);
            return named_value_subject(tc, module, value_name, &decl);
        }
        ModuleMemberLookup::Private => private = true,
        ModuleMemberLookup::Missing => {}
    }
    match tc.decls.module_type(scope, name) {
        ModuleMemberLookup::Found(key) => return Subject::Type(nominal_type(&key)),
        ModuleMemberLookup::Private => private = true,
        ModuleMemberLookup::Missing => {}
    }
    if kind == MemberAccessKind::Method && tc.decls.module_surface_has_extend_method(scope, name) {
        return Subject::QualifiedExtend {
            module: scope.clone(),
            name,
            span,
        };
    }
    if private {
        tc.push_error(TypeError::PrivateModuleMember {
            module: scope.clone(),
            name,
            span,
        });
    } else {
        tc.push_error(TypeError::UndefinedModuleMember {
            module: scope.clone(),
            name,
            span,
        });
    }
    Subject::Error
}

fn apply_type_field(
    target: &Type,
    name: Ident,
    span: Span,
    kind: MemberAccessKind,
    expected: Option<&TypeHandle>,
    tc: &mut TypeChecker,
) -> Subject {
    let has_static_extend = tc.find_static_extend_method(target, name).is_some();
    let mut enum_key = None;
    let mut has_instance = false;

    if let Some(owner) = tc.extern_type_id(target) {
        let has_extern_static = tc.externs.static_method(owner, name).is_some();
        if has_extern_static && has_static_extend {
            return static_extend_conflict(target, name, span, tc);
        }
        if let Some(subject) = extern_type_member_subject(owner, name, kind, tc) {
            return subject;
        }
        has_instance = kind == MemberAccessKind::Method && tc.externs.method(owner, name).is_some();
    }

    if let Some(key) = tc.decls.key_for_type(target) {
        if let Some(schema) = tc.decls.enum_schema(&key) {
            let has_variant = schema.variants.contains_key(&name);
            if has_variant && has_static_extend {
                return static_extend_conflict(target, name, span, tc);
            }
            if has_variant {
                let resolved = enum_variant::resolve_member(tc, &key, name, span)
                    .expect("variant exists in enum schema");
                return enum_variant_subject(resolved, expected, tc);
            }
            enum_key = Some(key);
        } else if let Some(agg) = tc.decls.aggregate(&key).cloned() {
            let static_key = MethodKey::static_(name);
            let instance_key = MethodKey::instance(name);
            let has_static = agg.methods.contains_key(&static_key);
            has_instance |= agg.methods.contains_key(&instance_key);
            if has_static && has_static_extend {
                return static_extend_conflict(target, name, span, tc);
            }
            if let Some(method) = agg.methods.get(&static_key) {
                tc.check_access_policy(
                    &method.policy,
                    MemberAccessKind::Method,
                    name,
                    target,
                    &key.module,
                    span,
                );
                return callable_subject(
                    tc.decls
                        .callable_for_aggregate_static_method(&agg, name, method, Some(target)),
                    None,
                );
            }
        }
    }

    if has_static_extend {
        return static_extend_subject(target, name, span, tc).expect("static extension exists");
    }

    has_instance |= tc.find_extend_method(target, name).is_some();
    if has_instance {
        tc.push_error(TypeError::InstanceMethodOnType {
            ty: target.clone(),
            method: name,
            span,
        });
        return Subject::Error;
    }

    if let Some(key) = enum_key {
        enum_variant::resolve_member(tc, &key, name, span);
        return Subject::Error;
    }

    unknown_member(target.clone(), name, kind, span, tc)
}

fn static_extend_conflict(ty: &Type, name: Ident, span: Span, tc: &mut TypeChecker) -> Subject {
    tc.push_error(TypeError::Decl(DeclError::ExtendMethodConflict {
        ty: ty.clone(),
        name,
        surface: MethodSurface::Static,
        span,
    }));
    Subject::Error
}

fn extern_type_member_subject(
    owner: ExternTypeId,
    name: Ident,
    kind: MemberAccessKind,
    tc: &TypeChecker,
) -> Option<Subject> {
    let MemberAccessKind::Method = kind else {
        return None;
    };
    let (method, decl) = tc.externs.static_method(owner, name)?;
    Some(Subject::ExternStatic {
        static_ref: method,
        signature: decl.signature.clone(),
    })
}

fn static_extend_subject(
    target: &Type,
    name: Ident,
    span: Span,
    tc: &mut TypeChecker,
) -> Option<Subject> {
    let matched = tc.find_static_extend_method(target, name)?;
    let subject = match extend_method_parts(target.clone(), name, &matched) {
        Ok((extend, method, _, owner_args)) => {
            let policy = method.policy.clone();
            let origin = extend.origin.clone();
            let callee = tc
                .decls
                .callable_for_static_extend_method(extend, name, method, owner_args);
            tc.check_access_policy(
                &policy,
                MemberAccessKind::Method,
                name,
                target,
                &origin,
                span,
            );
            callable_subject(callee, None)
        }
        Err(error) => {
            push_extend_method_error(tc, error, span);
            Subject::Error
        }
    };
    Some(subject)
}

fn apply_call(
    subject: &Subject,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    match subject {
        Subject::Callable {
            callee, receiver, ..
        } => {
            if let Some(receiver) = receiver {
                check_source_receiver(receiver, call.span, tc);
            }
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

fn check_source_receiver(receiver: &SourceReceiver, span: Span, tc: &mut TypeChecker) {
    if receiver.mutable {
        if let Some(error) = receiver.access.mut_borrow_error(receiver.name, span) {
            tc.push_error(error);
        } else if let Some(facts) = &receiver.facts {
            place::record_facts_write(receiver.expr_id, facts, tc);
        }
    } else if let Some(facts) = &receiver.facts {
        place::record_facts_read(receiver.expr_id, facts, tc);
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

#[derive(Clone, Copy)]
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

struct CallParam {
    ty: TypeHandle,
    mutable: bool,
}

fn solve_generic_call_with(
    generics: &GenericParams,
    seeds: &GenericSolverSeeds,
    template_params: &[FuncParam],
    required_params: usize,
    template_ret: &Type,
    args: &[ExprNode],
    call_span: Span,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
    add_constraints: impl FnOnce(&GenericSolverVars, &mut TypeChecker),
) -> Option<GenericCallInstantiation> {
    if !check_arg_range(args, required_params, template_params.len(), call_span, tc) {
        return None;
    }

    let inferred_ret = matches!(template_ret, Type::InferReturn);
    let error_count = tc.errors.len();
    for param in template_params {
        tc.substitute_checked(&param.ty, &seeds.type_args, &seeds.const_args, call_span);
    }
    if !inferred_ret {
        tc.substitute_checked(template_ret, &seeds.type_args, &seeds.const_args, call_span);
    }
    if tc.errors.len() != error_count {
        for arg in args {
            check_expr_checked(arg, tc);
        }
        return None;
    }

    let vars = tc.solver.generic_solver_vars(generics, seeds, call_span);
    add_constraints(&vars, tc);
    let mut failed = tc.solve_constraints();

    let params = instantiate_call_params(template_params, &vars, tc);
    failed |= check_source_args(args, &params, tc);
    if !inferred_ret {
        let ret_handle = tc.solver.instantiate_generic_type(template_ret, &vars);
        failed |= constrain_expected_return(call_span, ret_handle, expected, tc);
    }

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
    let ret = if inferred_ret {
        Type::InferReturn
    } else {
        tc.substitute_checked(template_ret, &type_subst, &const_subst, call_span)
    };

    Some(GenericCallInstantiation {
        args,
        type_subst,
        const_subst,
        concrete_params,
        ret,
    })
}

fn instantiate_call_params(
    params: &[FuncParam],
    vars: &GenericSolverVars,
    tc: &mut TypeChecker,
) -> Vec<CallParam> {
    params
        .iter()
        .map(|param| CallParam {
            ty: tc.solver.instantiate_generic_type(&param.ty, vars),
            mutable: param.mutable,
        })
        .collect()
}

fn check_source_args(args: &[ExprNode], params: &[CallParam], tc: &mut TypeChecker) -> bool {
    let mut failed = false;
    for (arg, param) in args.iter().zip(params) {
        let checked = if param.mutable {
            let place = place::check_place(arg, tc);
            if let Some(error) = place
                .value
                .access
                .mut_borrow_error(super::assignment_target_name(arg), arg.span)
            {
                tc.push_error(error);
            } else {
                place::record_write(arg.node.id, &place, tc);
            }
            place.into_checked()
        } else {
            super::check_value_expr_checked_with_hint(arg, Some(param.ty.clone()), tc)
        };
        tc.reject_extern_any_escape(&checked, arg.span);
        tc.expect_assignable(arg.span, checked.handle, param.ty.clone());
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
    if let VariantSchema::Tuple(params) = &resolved.schema
        && params.len() != call.node.args.len()
    {
        enum_variant::push_arg_count_mismatch(
            tc,
            resolved.key.name,
            resolved.variant,
            params.len(),
            call.node.args.len(),
            call.span,
        );
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
    let checked = check_callable_call_with_args(
        callee,
        &call.node.args,
        &call.node.generic_args,
        call.span,
        call_id,
        CallForm::Normal,
        expected,
        tc,
    );
    if matches!(callee.def.sig.ret, Type::InferReturn) {
        tc.set_type(
            call.node.func.node.id,
            func_type(&callee.def.sig.params, &checked.ty),
            call.node.func.span,
        );
    }
    checked
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
        callee.def.sig.required_params,
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
        mut ret,
        ..
    } = inst;
    let body_ret = ret.clone();

    if let Some(inferred_ret) = super::check_specialized_callable_body(
        callee,
        &concrete_params,
        body_ret,
        &args,
        type_subst,
        const_subst,
        const_bindings,
        tc,
    ) {
        ret = inferred_ret;
    }
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

    let receiver = check_receiver_value(receiver_expr, tc);
    tc.reject_extern_any_escape(&receiver.checked, receiver_expr.span);

    let Some(matched) =
        tc.decls
            .find_extend_method_in_module_surface(module, &receiver.checked.ty, name)
    else {
        tc.push_error(TypeError::UnknownMember {
            ty: receiver.checked.ty.clone(),
            member: name,
            kind: MemberAccessKind::Method,
            span,
        });
        return check_unhinted_args(args, tc);
    };

    let (extend, method, receiver_ty, owner_args) =
        match extend_method_parts(receiver.checked.ty.clone(), name, &matched) {
            Ok(parts) => parts,
            Err(error) => {
                push_extend_method_error(tc, error, span);
                return check_unhinted_args(args, tc);
            }
        };

    let receiver_use = source_receiver(
        method.mode,
        receiver.access,
        receiver.facts.as_ref(),
        receiver_expr.node.id,
        name,
    );
    let callee = tc
        .decls
        .callable_for_extend_method(receiver_ty, extend, name, method, owner_args);
    check_source_receiver(&receiver_use, call.span, tc);
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
        | CallableKind::ExtendMethod(_) => &callee.def.sig.generics,
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

    let params = params
        .iter()
        .map(|param| CallParam {
            ty: tc.type_handle(&param.ty),
            mutable: param.mutable,
        })
        .collect::<Vec<_>>();
    check_source_args(args, &params, tc);
}
