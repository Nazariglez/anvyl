use anvyx_externs::ReceiverMode;

use super::{
    ArgumentProjectionFact, CallForm, CallTarget, CheckedType, ConstSubst, DynCallFact, Exposure,
    ExternUseTarget, GenericArgs, GenericParams, MemberAccessKind, MemberPathFact, MemberPathKind,
    PlaceAccess, TypeChecker, TypeError, TypeSubst, VariantShape, check_arg_count, check_arg_range,
    check_expr_checked, checked_type,
    contracts::{self, DynamicMethodError},
    decls::{
        CallableKind, CallableParent, CallableRef, ContractRequirementSchema, ContractSetKey,
        DeclError, ExtendMethodSchema, ExtendSchema, MethodKey, MethodMode, MethodSurface,
        ModuleMemberLookup, ModuleScope, ProjectionLookup, ResolvedValue, TypeBinding, ValueDecl,
        VariantPayload, nominal_type, owner_template,
    },
    enum_variant::{self, ResolvedEnumVariant},
    extern_boundary,
    generic_bind::bind_prefix_generic_seeds,
    infer::{GenericSolverSeeds, GenericSolverVars, TypeHandle},
    member,
    place::{self, PlaceUseFacts, PlaceValue},
};
use crate::{
    ast::{
        CallNode, ExprId, ExprKind, ExprNode, FieldAccessNode, FuncParam, GenericArg, Ident,
        MethodReceiver, ReturnSpec, Type,
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
        receiver_place: PlaceUseFacts,
        receiver_identity: place::PlaceIdentity,
        receiver_root_name: Option<Ident>,
        receiver_id: ExprId,
        name: Ident,
        signature: ResolvedExternSignature,
    },
    DynMethod {
        contract: ContractSetKey,
        requirement: ContractRequirementSchema,
        receiver_access: PlaceAccess,
        receiver_place: PlaceUseFacts,
        receiver_identity: place::PlaceIdentity,
        receiver_root_name: Option<Ident>,
        receiver_id: ExprId,
        name: Ident,
    },
    DynHoleMethod {
        hole: crate::ast::DynContractHoleId,
        receiver_access: PlaceAccess,
        receiver_place: PlaceUseFacts,
        receiver_identity: place::PlaceIdentity,
        receiver_root_name: Option<Ident>,
        receiver_id: ExprId,
        name: Ident,
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
    facts: PlaceUseFacts,
    identity: place::PlaceIdentity,
    root_name: Option<Ident>,
    expr_id: ExprId,
    name: Ident,
}

struct DynReceiver<'a> {
    access: PlaceAccess,
    facts: &'a PlaceUseFacts,
    identity: &'a place::PlaceIdentity,
    root_name: Option<Ident>,
    id: ExprId,
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

fn local_value_subject(
    expr: &ExprNode,
    name: Ident,
    value: &super::LocalValue,
    tc: &mut TypeChecker,
) -> Subject {
    let checked = super::checked_from_handle(expr, tc.local_handle(value.info.type_id), tc);
    let access = tc.local_value_access(value);
    let mut value = PlaceValue::new(checked, access.access, access.facts);
    value.identity = access.identity;
    value.root_name = Some(name);
    Subject::Value(value)
}

pub(super) fn resolve_base(expr: &ExprNode, tc: &mut TypeChecker) -> Option<Subject> {
    match &expr.node.kind {
        ExprKind::Ident(name) => {
            let local = match tc.lookup_local_symbol_checked(*name, expr.span) {
                super::LocalSymbolLookup::Found(super::LocalSymbol::Callable(info), depth)
                    if depth > 0 =>
                {
                    return Some(callable_subject(info.callee.clone(), None));
                }
                super::LocalSymbolLookup::Found(super::LocalSymbol::Value(info), depth)
                    if depth > 0 =>
                {
                    let requires_runtime_capture = info.kind.requires_runtime_capture();
                    return Some(local_value_subject(
                        expr,
                        *name,
                        &super::LocalValue {
                            info,
                            depth,
                            requires_runtime_capture,
                        },
                        tc,
                    ));
                }
                super::LocalSymbolLookup::Blocked(error) => {
                    tc.push_error(error);
                    return Some(Subject::Error);
                }
                local => local,
            };
            if let Some((module, value_name, value)) = tc.lookup_named_value(*name) {
                return Some(named_value_subject(
                    tc, module, value_name, &value, expr.span,
                ));
            }
            if let super::LocalSymbolLookup::Found(super::LocalSymbol::Value(info), depth) = local {
                let requires_runtime_capture = info.kind.requires_runtime_capture();
                return Some(local_value_subject(
                    expr,
                    *name,
                    &super::LocalValue {
                        info,
                        depth,
                        requires_runtime_capture,
                    },
                    tc,
                ));
            }
            if let Some(scope) = tc.lookup_module_alias(*name) {
                return Some(Subject::Module(scope));
            }
            if let Some(ty) = tc.visible_type_subject(*name, expr.span) {
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
    check_postfix_chain_place(chain, expr, expected, true, tc).checked
}

pub(super) fn check_postfix_chain_place(
    chain: &PostfixChain,
    expr: &ExprNode,
    expected: Option<&TypeHandle>,
    record_final_read: bool,
    tc: &mut TypeChecker,
) -> PlaceValue {
    let Some(mut subject) = resolve_base(chain.base, tc) else {
        if let ExprKind::Ident(name) = &chain.base.node.kind {
            tc.push_error(TypeError::UndefinedVariable {
                name: *name,
                span: tc.error_span(chain.base.span),
            });
        }
        tc.set_type(chain.base.node.id, Type::Infer, chain.base.span);
        return PlaceValue::not_place(checked_type(finish_chain(chain, expr, tc), tc));
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
                let subject = apply_field(
                    &subject,
                    node,
                    *id,
                    next_is_call,
                    field_expected.as_ref(),
                    tc,
                );
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
                let value = apply_call(&subject, node, *id, call_expected, tc);
                tc.set_type(
                    *id,
                    wrap_optional(value.checked.ty.clone(), optional_chain, tc),
                    node.span,
                );
                if matches!(subject, Subject::Error) {
                    Subject::Error
                } else {
                    Subject::Value(value)
                }
            }
        };
    }

    if let Subject::Callable { callee, .. } = &subject
        && callee.def.sig.ret.is_infer()
    {
        tc.push_error(TypeError::InferReturnValue {
            span: tc.error_span(expr.span),
        });
    }

    if let Subject::Value(value) = &subject {
        tc.check_mut_downcast_root_use(value.root_name, &value.identity, expr.span);
        if record_final_read {
            place::record_value_read(expr.node.id, value, tc);
        }
    }

    if let Subject::Type(ty) = &subject {
        tc.push_error(TypeError::TypeUsedAsValue {
            ty: ty.clone(),
            span: tc.error_span(expr.span),
        });
        return PlaceValue::not_place(super::checked_from_type(expr, Type::Infer, tc));
    }

    let ty = chain_type(&subject, optional_chain, tc);
    let checked = CheckedType {
        handle: tc.set_type(expr.node.id, ty.clone(), expr.span),
        ty,
        contains_extern_any: subject_contains_extern_any(&subject),
    };
    if optional_chain {
        return PlaceValue::not_place(checked);
    }
    match subject {
        Subject::Value(mut value) => {
            value.checked = checked;
            value
        }
        _ => PlaceValue::not_place(checked),
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
        Subject::DynMethod { requirement, .. } => func_type(&requirement.params, &requirement.ret),
        Subject::DynHoleMethod { .. } => Type::Func {
            params: vec![],
            ret: Box::new(ReturnSpec::void()),
        },
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
        tc.push_error(TypeError::OptionalChainingOnNonOptional {
            span: tc.error_span(span),
        });
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
    field_id: ExprId,
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
            value,
            field.node.target.node.id,
            field_id,
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
        | Subject::ExternStatic { .. }
        | Subject::DynMethod { .. }
        | Subject::DynHoleMethod { .. } => {
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
        span: tc.error_span(span),
    });
    Subject::Error
}

fn named_value_subject(
    tc: &mut TypeChecker,
    module: ModuleScope,
    name: Ident,
    value: &ValueDecl,
    span: Span,
) -> Subject {
    tc.warn_named_value_deprecated(value, name, span);
    let resolved = ResolvedValue {
        module: module.clone(),
        name,
        decl: value.clone(),
    };
    let local_dyn_infer_ty = (module == tc.current_module
        && super::dyn_infer::DynInference::has_raw_hole(value.ty()))
    .then(|| tc.lookup(name))
    .flatten()
    .map(|info| tc.solver.local_type_to_type(info.type_id));
    match tc.decls.callable_for_value(&resolved) {
        Some(mut callee) => {
            if let Some(Type::Func { params, ret }) = local_dyn_infer_ty {
                callee.def.sig.params = params;
                callee.def.sig.ret = ret.as_ref().clone();
            }
            callable_subject(callee, None)
        }
        None => value_subject(local_dyn_infer_ty.unwrap_or_else(|| value.ty().clone()), tc),
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
        return materialized_receiver_value(expr.node.id, place::check_place(expr, tc).value);
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
        let mut value = PlaceValue::new(
            checked,
            place::projected_field_access(target.value.access),
            target.value.facts,
        );
        value.identity = target.value.identity.index();
        value.root_name = target.value.root_name;
        return value;
    }

    place::record_value_read(index.node.target.node.id, &target.value, tc);
    materialized_receiver_value(expr.node.id, PlaceValue::not_place(checked))
}

fn materialized_receiver_value(expr_id: ExprId, mut value: PlaceValue) -> PlaceValue {
    if value.access != PlaceAccess::NotPlace || matches!(value.checked.ty, Type::Infer | Type::Void)
    {
        return value;
    }
    value.access = PlaceAccess::Mutable;
    value.identity = place::PlaceIdentity::root(place::PlaceRoot::Temporary(expr_id));
    value
}

fn source_receiver(
    mode: MethodMode,
    access: PlaceAccess,
    facts: &PlaceUseFacts,
    identity: place::PlaceIdentity,
    root_name: Option<Ident>,
    expr_id: ExprId,
    name: Ident,
) -> SourceReceiver {
    SourceReceiver {
        mutable: matches!(mode, MethodMode::Instance { mutable: true }),
        access,
        facts: facts.clone(),
        identity,
        root_name,
        expr_id,
        name,
    }
}

fn apply_value_field(
    receiver: &PlaceValue,
    receiver_id: ExprId,
    field_id: ExprId,
    name: Ident,
    span: Span,
    kind: MemberAccessKind,
    tc: &mut TypeChecker,
) -> Subject {
    let receiver_ty = &receiver.checked.ty;
    let receiver_access = receiver.access;
    let receiver_place = &receiver.facts;
    let receiver_identity = receiver.identity.clone();

    match kind {
        MemberAccessKind::Field => {
            match place::field_value(None, receiver, field_id, name, span, tc) {
                place::FieldValueResult::Value(value, _) => Subject::Value(*value),
                place::FieldValueResult::StaticOnValue(ty) => {
                    tc.push_error(TypeError::StaticMethodOnValue {
                        ty,
                        method: name,
                        span: tc.error_span(span),
                    });
                    Subject::Error
                }
                place::FieldValueResult::NonAggregate(ty) => {
                    non_aggregate_member(ty, name, kind, span, tc)
                }
                place::FieldValueResult::Error => Subject::Error,
            }
        }
        MemberAccessKind::Method => {
            if let Type::Dyn(contract) = receiver_ty {
                return apply_dyn_method(
                    contract,
                    receiver_access,
                    receiver_place.clone(),
                    receiver_identity,
                    receiver.root_name,
                    receiver_id,
                    name,
                    span,
                    tc,
                );
            }
            match member::resolve_method(receiver_ty, name, tc) {
                member::MethodResolution::Direct(method) => {
                    tc.check_access_policy(
                        &method.policy,
                        MemberAccessKind::Method,
                        name,
                        receiver_ty,
                        &method.origin,
                        span,
                    );
                    callable_subject(
                        method.callee,
                        Some(source_receiver(
                            method.mode,
                            receiver_access,
                            receiver_place,
                            receiver_identity,
                            receiver.root_name,
                            receiver_id,
                            name,
                        )),
                    )
                }
                member::MethodResolution::Extend(method) => {
                    check_extend_method_access(
                        &mut super::AccessPolicyOutput {
                            source: tc.source_id(),
                            current_module: &tc.current_module,
                            config: &tc.config,
                            warnings: &mut tc.warnings,
                            errors: &mut tc.errors,
                        },
                        &method.extend,
                        &method.method,
                        receiver_ty,
                        name,
                        span,
                    );
                    callable_subject(
                        method.callee,
                        Some(source_receiver(
                            method.mode,
                            receiver_access,
                            receiver_place,
                            receiver_identity,
                            receiver.root_name,
                            receiver_id,
                            name,
                        )),
                    )
                }
                member::MethodResolution::Promoted(promoted) => {
                    tc.record_member_path(MemberPathFact {
                        expr_id: field_id,
                        kind: MemberPathKind::MethodReceiver,
                        path: promoted.path.clone(),
                        origin_owner: promoted.origin_owner.clone(),
                        origin_member: promoted.origin_method,
                    });
                    if promoted.exposure == Exposure::Implicit {
                        tc.check_stored_field_path_access(receiver_ty, &promoted.path, span);
                    }
                    let promoted_access = place::projected_field_access(receiver_access);
                    let promoted_identity = receiver_identity.fields(&promoted.path);
                    match promoted.target {
                        member::PromotedMethodTarget::Aggregate(method) => {
                            tc.check_access_policy(
                                &method.policy,
                                MemberAccessKind::Method,
                                promoted.origin_method,
                                &promoted.origin_owner,
                                &method.origin,
                                span,
                            );
                            callable_subject(
                                method.callee,
                                Some(source_receiver(
                                    method.mode,
                                    promoted_access,
                                    receiver_place,
                                    promoted_identity,
                                    receiver.root_name,
                                    field_id,
                                    name,
                                )),
                            )
                        }
                        member::PromotedMethodTarget::Extern(method) => Subject::ExternMethod {
                            method_ref: method.method_ref,
                            receiver: method.receiver,
                            receiver_access: promoted_access,
                            receiver_place: receiver_place.clone(),
                            receiver_identity: promoted_identity,
                            receiver_root_name: receiver.root_name,
                            receiver_id: field_id,
                            name,
                            signature: method.signature,
                        },
                    }
                }
                member::MethodResolution::AmbiguousPromoted {
                    ty,
                    name,
                    candidates,
                } => {
                    tc.push_error(TypeError::AmbiguousPromotedMethod {
                        ty,
                        member: name,
                        candidates,
                        span: tc.error_span(span),
                    });
                    Subject::Error
                }
                member::MethodResolution::Extern(method) => Subject::ExternMethod {
                    method_ref: method.method_ref,
                    receiver: method.receiver,
                    receiver_access,
                    receiver_place: receiver_place.clone(),
                    receiver_identity,
                    receiver_root_name: receiver.root_name,
                    receiver_id,
                    name: method.name,
                    signature: method.signature,
                },
                member::MethodResolution::StaticOnValue { ty } => {
                    tc.push_error(TypeError::StaticMethodOnValue {
                        ty,
                        method: name,
                        span: tc.error_span(span),
                    });
                    Subject::Error
                }
                member::MethodResolution::ExtendError(error) => {
                    push_extend_method_error(tc, error, span);
                    Subject::Error
                }
                member::MethodResolution::Missing { ty } => {
                    unknown_member(ty, name, kind, span, tc)
                }
                member::MethodResolution::NonAggregate { ty } => {
                    non_aggregate_member(ty, name, kind, span, tc)
                }
            }
        }
    }
}

fn apply_dyn_method(
    contract: &crate::ast::ContractRef,
    receiver_access: PlaceAccess,
    receiver_place: PlaceUseFacts,
    receiver_identity: place::PlaceIdentity,
    receiver_root_name: Option<Ident>,
    receiver_id: ExprId,
    name: Ident,
    span: Span,
    tc: &mut TypeChecker,
) -> Subject {
    if let Some(hole) = super::dyn_infer::hole_id(contract) {
        return Subject::DynHoleMethod {
            hole,
            receiver_access,
            receiver_place,
            receiver_identity,
            receiver_root_name,
            receiver_id,
            name,
        };
    }

    match contracts::resolve_dynamic_method(tc, contract, name) {
        Ok((contract, requirement)) => Subject::DynMethod {
            contract,
            requirement,
            receiver_access,
            receiver_place,
            receiver_identity,
            receiver_root_name,
            receiver_id,
            name,
        },
        Err(DynamicMethodError::Missing { contract }) => {
            tc.push_error(TypeError::DynamicMethodMissing {
                contract,
                method: name,
                span: tc.error_span(span),
            });
            Subject::Error
        }
        Err(DynamicMethodError::ConflictingRequirement(requirement)) => {
            tc.push_error(TypeError::CompileError {
                message: format!("conflicting contract requirement '{requirement}'"),
                span: tc.error_span(span),
            });
            Subject::Error
        }
        Err(DynamicMethodError::UnknownContract) => {
            tc.push_error(TypeError::CompileError {
                message: format!("unknown dynamic contract in method call '{name}'"),
                span: tc.error_span(span),
            });
            Subject::Error
        }
    }
}

fn check_extend_method_access(
    out: &mut super::AccessPolicyOutput<'_>,
    extend: &ExtendSchema,
    method: &ExtendMethodSchema,
    owner_ty: &Type,
    name: Ident,
    span: Span,
) {
    super::emit_access_policy(
        &method.policy,
        MemberAccessKind::Method,
        name,
        owner_ty,
        &extend.origin,
        span,
        out,
    );
}

fn push_extend_method_error(tc: &mut TypeChecker, error: member::ExtendMethodError, span: Span) {
    match error {
        member::ExtendMethodError::Unbound(names) => tc.push_unbound_generic_errors(names, span),
        member::ExtendMethodError::Ambiguous { receiver, name } => {
            tc.push_error(TypeError::AmbiguousExtendMethod {
                receiver,
                name,
                span: tc.error_span(span),
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
            return named_value_subject(tc, module, value_name, &decl, span);
        }
        ModuleMemberLookup::Private => private = true,
        ModuleMemberLookup::Missing => {}
    }
    match tc.decls.module_type(scope, name) {
        ModuleMemberLookup::Found(TypeBinding::Nominal(key)) => {
            tc.warn_extern_type_deprecated(&key, span);
            return Subject::Type(nominal_type(&key));
        }
        ModuleMemberLookup::Found(binding @ (TypeBinding::Alias(_) | TypeBinding::Contract(_))) => {
            let ty = tc.resolve_type_binding_for_tc_at(binding, &[], span, name);
            if !matches!(ty, Type::Infer) {
                return Subject::Type(ty);
            }
        }
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
            span: tc.error_span(span),
        });
    } else {
        tc.push_error(TypeError::UndefinedModuleMember {
            module: scope.clone(),
            name,
            span: tc.error_span(span),
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
                let resolved = enum_variant::resolve_use(tc, &key, name, span)
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
            span: tc.error_span(span),
        });
        return Subject::Error;
    }

    if let Some(key) = enum_key {
        enum_variant::resolve_use(tc, &key, name, span);
        return Subject::Error;
    }

    unknown_member(target.clone(), name, kind, span, tc)
}

fn static_extend_conflict(ty: &Type, name: Ident, span: Span, tc: &mut TypeChecker) -> Subject {
    tc.push_error(TypeError::Decl(DeclError::ExtendMethodConflict {
        ty: ty.clone(),
        name,
        surface: MethodSurface::Static,
        span: Some(tc.source_span(span)),
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
    let decls = &tc.decls;
    let current_module = &tc.current_module;
    let matched = decls.find_static_extend_method(target, name, |ext| {
        TypeChecker::extend_visible_in(decls, current_module, ext)
    })?;
    let subject = match member::extend_method_parts(target.clone(), name, &matched) {
        Ok((extend, method, _, owner_args)) => {
            check_extend_method_access(
                &mut super::AccessPolicyOutput {
                    source: tc.source_id(),
                    current_module: &tc.current_module,
                    config: &tc.config,
                    warnings: &mut tc.warnings,
                    errors: &mut tc.errors,
                },
                extend,
                method,
                target,
                name,
                span,
            );
            let callee = tc
                .decls
                .callable_for_static_extend_method(extend, name, method, owner_args);
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
) -> PlaceValue {
    match subject {
        Subject::Callable {
            callee, receiver, ..
        } => {
            let receiver_arg = receiver
                .as_ref()
                .and_then(|receiver| check_source_receiver(receiver, call.span, tc));
            check_callable_call(callee, call, call_id, receiver_arg, expected, tc)
                .into_place_value()
        }
        Subject::QualifiedExtend { module, name, span } => {
            check_qualified_extend_call(module, *name, *span, call, call_id, expected, tc)
                .into_place_value()
        }
        Subject::EnumVariant { resolved, .. } => PlaceValue::not_place(check_enum_variant_call(
            resolved, call, call_id, expected, tc,
        )),
        Subject::ExternMethod {
            method_ref,
            receiver,
            receiver_access,
            receiver_place,
            receiver_identity,
            receiver_root_name,
            receiver_id,
            name,
            signature,
        } => PlaceValue::not_place(check_extern_method_call(
            ExternMethodCall {
                method_ref: *method_ref,
                receiver: *receiver,
                receiver_access: *receiver_access,
                receiver_place,
                receiver_identity,
                receiver_root_name: *receiver_root_name,
                receiver_id: *receiver_id,
                name: *name,
                signature,
            },
            call,
            call_id,
            expected,
            tc,
        )),
        Subject::DynMethod {
            contract,
            requirement,
            receiver_access,
            receiver_place,
            receiver_identity,
            receiver_root_name,
            receiver_id,
            name,
        } => PlaceValue::not_place(check_dyn_method_call(
            contract,
            requirement,
            DynReceiver {
                access: *receiver_access,
                facts: receiver_place,
                identity: receiver_identity,
                root_name: *receiver_root_name,
                id: *receiver_id,
            },
            *name,
            call,
            call_id,
            expected,
            tc,
        )),
        Subject::DynHoleMethod {
            hole,
            receiver_access,
            receiver_place,
            receiver_identity,
            receiver_root_name,
            receiver_id,
            name,
        } => PlaceValue::not_place(check_dyn_hole_method_call(
            *hole,
            DynReceiver {
                access: *receiver_access,
                facts: receiver_place,
                identity: receiver_identity,
                root_name: *receiver_root_name,
                id: *receiver_id,
            },
            *name,
            call,
            call_id,
            expected,
            tc,
        )),
        Subject::ExternStatic {
            static_ref,
            signature,
        } => PlaceValue::not_place(check_extern_static_call(
            *static_ref,
            signature,
            call,
            call_id,
            expected,
            tc,
        )),
        Subject::Value(value) => {
            tc.check_mut_downcast_root_use(value.root_name, &value.identity, call.node.func.span);
            place::record_value_read(call.node.func.node.id, value, tc);
            call_value(value.checked.ty.clone(), call, call_id, expected, tc).into_place_value()
        }
        Subject::Module(_) | Subject::Type(_) => PlaceValue::not_place(checked_type(
            not_callable(subject_type(subject), call, tc),
            tc,
        )),
        Subject::Error => {
            for arg in &call.node.args {
                check_expr_checked(arg, tc);
            }
            PlaceValue::not_place(checked_type(Type::Infer, tc))
        }
    }
}

fn check_source_receiver(
    receiver: &SourceReceiver,
    span: Span,
    tc: &mut TypeChecker,
) -> Option<MutableArg> {
    tc.check_mut_downcast_root_use(receiver.root_name, &receiver.identity, span);
    if receiver.mutable {
        if let Some(error) =
            mutating_receiver_error(receiver.access, receiver.name, tc.error_span(span))
        {
            tc.push_error(error);
            return None;
        }
        place::record_facts_write(receiver.expr_id, &receiver.facts, tc);
        return Some(MutableArg {
            identity: receiver.identity.clone(),
            span,
            source: ReturnPlaceSource {
                access: receiver.access,
                facts: receiver.facts.clone(),
                identity: receiver.identity.clone(),
                root_name: receiver.root_name,
            },
        });
    }
    place::record_facts_read(receiver.expr_id, &receiver.facts, tc);
    None
}

fn mutating_receiver_error(
    access: PlaceAccess,
    name: Ident,
    span: Option<crate::span::SourceSpan>,
) -> Option<TypeError> {
    match access {
        PlaceAccess::Mutable | PlaceAccess::DynView => None,
        PlaceAccess::Settable => Some(TypeError::RequiresMutablePlace { name, span }),
        PlaceAccess::Captured => Some(TypeError::CannotMutateCapturedVariable { name, span }),
        PlaceAccess::Immutable
        | PlaceAccess::Const
        | PlaceAccess::ReadonlySelf
        | PlaceAccess::NotPlace => Some(TypeError::MutatingMethodImmutableReceiver { name, span }),
    }
}

fn call_value(
    callee_ty: Type,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedCall {
    match &callee_ty {
        Type::Func { params, ret } => {
            let params = params
                .iter()
                .map(|param| CallParam {
                    ty: tc.type_handle(&param.ty),
                    mutable: param.mutable,
                    cast_accept: param.cast_accept,
                })
                .collect::<Vec<_>>();
            let args_check = if check_arg_count(&call.node.args, params.len(), call.span, tc) {
                check_source_args(&call.node.args, &params, call_id, None, tc)
            } else {
                SourceArgsCheck {
                    failed: true,
                    place_source: None,
                }
            };
            let ret_handle = tc.type_handle(&ret.ty);
            constrain_expected_return(call.span, ret_handle.clone(), expected, tc);
            CheckedCall {
                checked: checked_type(ret.ty.clone(), tc),
                returns_place: ret.is_place() && !args_check.failed,
                source: args_check.place_source,
            }
        }
        _ => CheckedCall::value(checked_type(not_callable(callee_ty, call, tc), tc)),
    }
}

fn check_dyn_hole_method_call(
    hole: crate::ast::DynContractHoleId,
    receiver: DynReceiver<'_>,
    name: Ident,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let method_receiver = if matches!(receiver.access, PlaceAccess::Mutable | PlaceAccess::DynView)
    {
        MethodReceiver::Var
    } else {
        MethodReceiver::Value
    };
    let requires_mutable = matches!(method_receiver, MethodReceiver::Var);
    tc.check_mut_downcast_root_use(receiver.root_name, receiver.identity, call.span);
    if requires_mutable {
        match mutating_receiver_error(receiver.access, name, tc.error_span(call.span)) {
            Some(error) => {
                tc.push_error(error);
            }
            None => {
                place::record_facts_write(receiver.id, receiver.facts, tc);
            }
        }
    } else {
        place::record_facts_read(receiver.id, receiver.facts, tc);
    }

    let mut failed = false;
    let mut params = Vec::with_capacity(call.node.args.len());
    for arg in &call.node.args {
        let checked = check_expr_checked(arg, tc);
        let ty = checked.ty;
        if matches!(ty, Type::Infer) {
            tc.push_error(TypeError::CompileError {
                message: format!("cannot infer parameter type for dynamic method '{name}'"),
                span: tc.error_span(arg.span),
            });
            failed = true;
        }
        params.push(FuncParam::new(ty, false, false));
    }

    let ret = match expected {
        Some(expected) => tc.handle_type(&expected),
        None if tc.discard_depth > 0 => Type::Void,
        None => {
            tc.push_error(TypeError::CompileError {
                message: format!("cannot infer return type for dynamic method '{name}'"),
                span: tc.error_span(call.span),
            });
            failed = true;
            Type::Infer
        }
    };

    if !failed {
        match tc.dyn_infer.collect_method(
            hole,
            name,
            method_receiver,
            params,
            ret.clone(),
            tc.source_span(call.span),
        ) {
            Ok(()) => tc.dyn_infer.add_call(
                tc.current_module.clone(),
                call_id,
                receiver.id,
                hole,
                name,
                call.node.args.len(),
                requires_mutable,
                tc.source_span(call.span),
            ),
            Err(message) => tc.push_error(TypeError::CompileError {
                message,
                span: tc.error_span(call.span),
            }),
        }
    }

    checked_type(ret, tc)
}

fn check_dyn_method_call(
    contract: &ContractSetKey,
    requirement: &ContractRequirementSchema,
    receiver: DynReceiver<'_>,
    name: Ident,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let method_receiver = requirement
        .receiver
        .expect("contract requirements are finalized with receivers");
    let requires_mutable = matches!(method_receiver, MethodReceiver::Var);
    tc.check_mut_downcast_root_use(receiver.root_name, receiver.identity, call.span);
    let mut failed = false;
    if requires_mutable {
        if let Some(error) =
            mutating_receiver_error(receiver.access, name, tc.error_span(call.span))
        {
            tc.push_error(error);
            failed = true;
        } else {
            place::record_facts_write(receiver.id, receiver.facts, tc);
        }
    } else {
        place::record_facts_read(receiver.id, receiver.facts, tc);
    }

    failed |= check_args(&call.node.args, &requirement.params, call.span, call_id, tc);
    let ret = tc.type_handle(&requirement.ret.ty);
    constrain_expected_return(call.span, ret.clone(), expected, tc);
    if !failed {
        tc.record_dyn_call(DynCallFact {
            call_id,
            receiver_id: receiver.id,
            contract: contract.clone(),
            method: name,
            arg_count: call.node.args.len(),
            requires_mutable,
            span: tc.source_span(call.span),
        });
    }
    checked_type(requirement.ret.ty.clone(), tc)
}

#[derive(Clone, Copy)]
struct ExternMethodCall<'a> {
    method_ref: ExternMethodRef,
    receiver: ReceiverMode,
    receiver_access: PlaceAccess,
    receiver_place: &'a PlaceUseFacts,
    receiver_identity: &'a place::PlaceIdentity,
    receiver_root_name: Option<Ident>,
    receiver_id: ExprId,
    name: Ident,
    signature: &'a ResolvedExternSignature,
}

struct GenericCallInstantiation {
    args: GenericArgs,
    type_subst: TypeSubst,
    const_subst: ConstSubst,
    concrete_params: Vec<FuncParam>,
    ret: ReturnSpec,
    place_source: Option<ReturnPlaceSource>,
}

struct CallParam {
    ty: TypeHandle,
    mutable: bool,
    cast_accept: bool,
}

struct GenericCallSource<'a> {
    args: &'a [ExprNode],
    call_span: Span,
    call_id: ExprId,
    receiver_arg: Option<MutableArg>,
    expected: Option<TypeHandle>,
}

fn solve_generic_call_with(
    generics: &GenericParams,
    seeds: &GenericSolverSeeds,
    template_params: &[FuncParam],
    required_params: usize,
    template_ret: &ReturnSpec,
    source: GenericCallSource<'_>,
    tc: &mut TypeChecker,
    add_constraints: impl FnOnce(&GenericSolverVars, &mut TypeChecker),
) -> Option<GenericCallInstantiation> {
    let GenericCallSource {
        args,
        call_span,
        call_id,
        receiver_arg,
        expected,
    } = source;
    if !check_arg_range(args, required_params, template_params.len(), call_span, tc) {
        return None;
    }

    let inferred_ret = template_ret.is_infer();
    let error_count = tc.errors.len();
    for param in template_params {
        tc.substitute_checked(&param.ty, &seeds.type_args, &seeds.const_args, call_span);
    }
    if !inferred_ret {
        tc.substitute_checked(
            &template_ret.ty,
            &seeds.type_args,
            &seeds.const_args,
            call_span,
        );
    }
    if tc.errors.len() != error_count {
        for arg in args {
            check_expr_checked(arg, tc);
        }
        return None;
    }

    let vars = tc
        .solver
        .generic_solver_vars(generics, seeds, tc.error_span(call_span));
    add_constraints(&vars, tc);
    let mut failed = tc.solve_constraints();

    let params = instantiate_call_params(template_params, &vars, tc);
    let args_check = check_source_args(args, &params, call_id, receiver_arg, tc);
    failed |= args_check.failed;
    if !inferred_ret {
        let ret_handle = tc.solver.instantiate_generic_type(&template_ret.ty, &vars);
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
    if !tc.check_generic_bounds(generics, &args, call_span) {
        return None;
    }

    let (type_subst, const_subst) = generics.substitutions(&args);
    let concrete_params =
        substitute_params_checked(template_params, &type_subst, &const_subst, call_span, tc);
    let ret = template_ret.with_ty(if inferred_ret {
        Type::InferReturn
    } else {
        tc.substitute_checked(&template_ret.ty, &type_subst, &const_subst, call_span)
    });

    Some(GenericCallInstantiation {
        args,
        type_subst,
        const_subst,
        concrete_params,
        ret,
        place_source: args_check.place_source,
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
            cast_accept: param.cast_accept,
        })
        .collect()
}

fn check_source_args(
    args: &[ExprNode],
    params: &[CallParam],
    call_id: ExprId,
    receiver_arg: Option<MutableArg>,
    tc: &mut TypeChecker,
) -> SourceArgsCheck {
    let mut failed = false;
    let mut mutable_args = receiver_arg.into_iter().collect::<Vec<_>>();
    for (index, (arg, param)) in args.iter().zip(params).enumerate() {
        let checked = check_source_arg(arg, param, call_id, index, tc);
        failed |= checked.failed;
        if let Some(mutable_arg) = checked.mutable_arg {
            mutable_args.push(mutable_arg);
        }
    }
    let alias_failed = validate_mutable_aliases(&mutable_args, tc);
    SourceArgsCheck {
        failed: failed || alias_failed,
        place_source: mutable_args.first().map(|arg| arg.source.clone()),
    }
}

struct SourceArgsCheck {
    failed: bool,
    place_source: Option<ReturnPlaceSource>,
}

struct SourceArgCheck {
    failed: bool,
    mutable_arg: Option<MutableArg>,
}

#[derive(Clone)]
struct ReturnPlaceSource {
    access: PlaceAccess,
    facts: PlaceUseFacts,
    identity: place::PlaceIdentity,
    root_name: Option<Ident>,
}

struct CheckedCall {
    checked: CheckedType,
    returns_place: bool,
    source: Option<ReturnPlaceSource>,
}

impl CheckedCall {
    fn value(checked: CheckedType) -> Self {
        Self {
            checked,
            returns_place: false,
            source: None,
        }
    }

    fn into_place_value(self) -> PlaceValue {
        let Some(source) = self.source.filter(|_| self.returns_place) else {
            return PlaceValue::not_place(self.checked);
        };
        PlaceValue {
            checked: self.checked,
            access: source.access,
            facts: source.facts,
            identity: source.identity.returned_place(),
            root_name: source.root_name,
        }
    }
}

struct MutableArg {
    identity: place::PlaceIdentity,
    span: Span,
    source: ReturnPlaceSource,
}

fn mutable_arg(span: Span, value: &PlaceValue) -> MutableArg {
    MutableArg {
        identity: value.identity.clone(),
        span,
        source: ReturnPlaceSource {
            access: value.access,
            facts: value.facts.clone(),
            identity: value.identity.clone(),
            root_name: value.root_name,
        },
    }
}

fn validate_mutable_aliases(args: &[MutableArg], tc: &mut TypeChecker) -> bool {
    let mut failed = false;
    for (index, arg) in args.iter().enumerate() {
        for previous in &args[..index] {
            if previous.identity.conflicts_with(&arg.identity) {
                tc.push_error(TypeError::MutableAlias {
                    span: tc.error_span(arg.span),
                });
                failed = true;
                break;
            }
        }
    }
    failed
}

fn var_arg_error(
    access: PlaceAccess,
    name: Ident,
    span: Option<crate::span::SourceSpan>,
) -> Option<TypeError> {
    match access {
        PlaceAccess::Mutable | PlaceAccess::DynView => None,
        PlaceAccess::Settable => Some(TypeError::RequiresMutablePlace { name, span }),
        PlaceAccess::Captured => Some(TypeError::CannotMutateCapturedVariable { name, span }),
        PlaceAccess::Immutable | PlaceAccess::Const => {
            Some(TypeError::VarArgImmutableBinding { name, span })
        }
        PlaceAccess::ReadonlySelf => Some(TypeError::ReadonlyMethodMutation { span }),
        PlaceAccess::NotPlace => Some(TypeError::VarArgNonLvalue { span }),
    }
}

fn check_source_arg(
    arg: &ExprNode,
    param: &CallParam,
    call_id: ExprId,
    arg_index: usize,
    tc: &mut TypeChecker,
) -> SourceArgCheck {
    match (param.mutable, param.cast_accept) {
        (true, true) => check_projecting_var_arg(arg, param, call_id, arg_index, tc),
        (true, false) => check_var_arg(arg, param, tc),
        (false, true) => check_cast_accept_arg(arg, param, tc),
        (false, false) => check_value_arg(arg, param, tc),
    }
}

fn check_var_arg(arg: &ExprNode, param: &CallParam, tc: &mut TypeChecker) -> SourceArgCheck {
    let place = place::check_place(arg, tc);
    let name = super::assignment_target_name(arg);
    let error = var_arg_error(place.value.access, name, tc.error_span(arg.span));
    finish_var_arg(arg, param, place, error, tc)
}

fn check_projecting_var_arg(
    arg: &ExprNode,
    param: &CallParam,
    call_id: ExprId,
    arg_index: usize,
    tc: &mut TypeChecker,
) -> SourceArgCheck {
    let place = place::check_place(arg, tc);
    let name = super::assignment_target_name(arg);
    let mutability_error = var_arg_error(place.value.access, name, tc.error_span(arg.span));
    if can_assign_without_errors(
        arg.span,
        place.value.checked.handle.clone(),
        param.ty.clone(),
        tc,
    ) {
        return finish_var_arg(arg, param, place, mutability_error, tc);
    }

    if let Some(error) = mutability_error {
        tc.push_error(error);
        let checked = place.into_checked();
        tc.reject_extern_any_escape(&checked, arg.span);
        tc.expect_assignable_expr(arg.span, arg.node.id, checked.handle, param.ty.clone());
        return SourceArgCheck {
            failed: tc.solve_constraints(),
            mutable_arg: None,
        };
    }

    let target = tc.handle_type(&param.ty);
    if matches!(target, Type::Dyn(_)) {
        place::record_write(arg.node.id, &place, tc);
        let mutable_arg = mutable_arg(arg.span, &place.value);
        let checked = place.into_checked();
        tc.reject_extern_any_escape(&checked, arg.span);
        tc.expect_assignable_expr(arg.span, arg.node.id, checked.handle, param.ty.clone());
        return SourceArgCheck {
            failed: tc.solve_constraints(),
            mutable_arg: Some(mutable_arg),
        };
    }

    let projection = match tc.decls.projection_from(&place.value.checked.ty, &target) {
        ProjectionLookup::Match(projection) => projection,
        ProjectionLookup::Missing => {
            if let Some(entry) = tc
                .decls
                .chained_projection_from(&place.value.checked.ty, &target)
            {
                tc.push_error(TypeError::ChainedProjection {
                    source: place.value.checked.ty.clone(),
                    target: target.clone(),
                    via: entry.field_path,
                    span: tc.error_span(arg.span),
                });
                return SourceArgCheck {
                    failed: true,
                    mutable_arg: None,
                };
            }
            tc.push_error(TypeError::MissingProjection {
                source: place.value.checked.ty.clone(),
                target: target.clone(),
                paths: tc
                    .decls
                    .field_paths_to_type(&place.value.checked.ty, &target),
                span: tc.error_span(arg.span),
            });
            return SourceArgCheck {
                failed: true,
                mutable_arg: None,
            };
        }
        ProjectionLookup::Conflict(conflict) => {
            tc.push_error(TypeError::DuplicateProjectionTarget {
                source: place.value.checked.ty.clone(),
                target: conflict.target,
                paths: conflict.paths,
                span: tc.error_span(arg.span),
            });
            return SourceArgCheck {
                failed: true,
                mutable_arg: None,
            };
        }
    };

    let mut projected = PlaceValue::new(
        checked_type(projection.target_ty.clone(), tc),
        place::projected_field_access(place.value.access),
        place.value.facts.clone(),
    );
    projected.identity = place.value.identity.fields(&projection.field_path);
    projected.root_name = place.value.root_name;
    tc.reject_extern_any_escape(&projected.checked, arg.span);
    tc.expect_assignable_expr(
        arg.span,
        arg.node.id,
        projected.checked.handle.clone(),
        param.ty.clone(),
    );
    place::record_value_write(arg.node.id, &projected, tc);
    tc.record_argument_projection(ArgumentProjectionFact {
        call_id,
        arg_index,
        path: projection.field_path,
        target_ty: target,
    });
    SourceArgCheck {
        failed: tc.solve_constraints(),
        mutable_arg: Some(mutable_arg(arg.span, &projected)),
    }
}

fn finish_var_arg(
    arg: &ExprNode,
    param: &CallParam,
    place: place::CheckedPlace,
    mutability_error: Option<TypeError>,
    tc: &mut TypeChecker,
) -> SourceArgCheck {
    let mutable_arg = if let Some(error) = mutability_error {
        tc.push_error(error);
        None
    } else {
        place::record_write(arg.node.id, &place, tc);
        Some(mutable_arg(arg.span, &place.value))
    };
    let checked = place.into_checked();
    tc.reject_extern_any_escape(&checked, arg.span);
    tc.expect_assignable_expr(arg.span, arg.node.id, checked.handle, param.ty.clone());
    SourceArgCheck {
        failed: tc.solve_constraints(),
        mutable_arg,
    }
}

fn check_cast_accept_arg(
    arg: &ExprNode,
    param: &CallParam,
    tc: &mut TypeChecker,
) -> SourceArgCheck {
    let checked = super::check_value_expr_checked_with_hint(arg, None, tc);
    tc.reject_extern_any_escape(&checked, arg.span);
    if can_assign_without_errors(arg.span, checked.handle.clone(), param.ty.clone(), tc) {
        tc.expect_assignable_expr(arg.span, arg.node.id, checked.handle, param.ty.clone());
        return SourceArgCheck {
            failed: tc.solve_constraints(),
            mutable_arg: None,
        };
    }
    let target = tc.handle_type(&param.ty);
    if tc.has_cast_from_conversion(&checked.ty, &target) {
        return SourceArgCheck {
            failed: tc.solve_constraints(),
            mutable_arg: None,
        };
    }
    tc.expect_assignable_expr(arg.span, arg.node.id, checked.handle, param.ty.clone());
    SourceArgCheck {
        failed: tc.solve_constraints(),
        mutable_arg: None,
    }
}

fn check_value_arg(arg: &ExprNode, param: &CallParam, tc: &mut TypeChecker) -> SourceArgCheck {
    let checked = super::check_value_expr_checked_with_hint(arg, Some(param.ty.clone()), tc);
    tc.reject_extern_any_escape(&checked, arg.span);
    let dyn_format = matches!(tc.handle_type(&param.ty), Type::Any)
        && tc.reject_dyn_implicit_format(&checked.ty, arg.span);
    tc.expect_assignable_expr(arg.span, arg.node.id, checked.handle, param.ty.clone());
    SourceArgCheck {
        failed: tc.solve_constraints() || dyn_format,
        mutable_arg: None,
    }
}

fn can_assign_without_errors(
    span: Span,
    from: TypeHandle,
    to: TypeHandle,
    tc: &TypeChecker,
) -> bool {
    let mut solver = tc.solver.clone();
    solver.add_handle_assignable(tc.error_span(span), from, to);
    solver.solve_pending().is_empty()
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
                param.cast_accept,
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
    let can_call = match &resolved.schema.payload {
        VariantPayload::Tuple(_) => true,
        VariantPayload::Unit => call.node.args.is_empty(),
        VariantPayload::Struct(_) => false,
    };
    if !can_call {
        enum_variant::push_shape_mismatch(tc, resolved, VariantShape::Tuple, call.span);
        return check_unhinted_args(&call.node.args, tc);
    }
    if let VariantPayload::Tuple(params) = &resolved.schema.payload
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
        None,
        expected,
        tc,
    )
    .checked
}

fn check_callable_call(
    callee: &CallableRef,
    call: &CallNode,
    call_id: ExprId,
    receiver_arg: Option<MutableArg>,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedCall {
    if matches!(callee.def.id.kind, CallableKind::ExternFunction) {
        return CheckedCall::value(check_extern_function_call(
            callee, call, call_id, expected, tc,
        ));
    }
    let checked = check_callable_call_with_args(
        callee,
        &call.node.args,
        &call.node.generic_args,
        call.span,
        call_id,
        CallForm::Normal,
        receiver_arg,
        expected,
        tc,
    );
    if callee.def.sig.ret.is_infer() {
        let ret = callee.def.sig.ret.with_ty(checked.checked.ty.clone());
        tc.set_type(
            call.node.func.node.id,
            func_type(&callee.def.sig.params, &ret),
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
    receiver_arg: Option<MutableArg>,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedCall {
    let Some(mut seeds) =
        bind_prefix_generic_seeds(tc, syntactic_generics(callee), generic_args, call_span)
    else {
        return CheckedCall::value(checked_type(Type::Infer, tc));
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
        GenericCallSource {
            args,
            call_span,
            call_id,
            receiver_arg,
            expected,
        },
        tc,
        |vars, tc| constrain_callable_owner(callee, vars, call_span, tc),
    ) else {
        return CheckedCall::value(checked_type(Type::Infer, tc));
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
        place_source,
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
        ret.ty = inferred_ret;
    }
    let id = callee.def.id.clone();
    let target = match form {
        CallForm::Normal => CallTarget::new(id, args),
        CallForm::QualifiedExtend { receiver } => CallTarget::qualified_extend(id, args, receiver),
    };
    tc.record_call(call_id, target);
    let returns_place = ret.is_place();
    CheckedCall {
        checked: checked_type(ret.ty, tc),
        returns_place,
        source: place_source,
    }
}

fn check_qualified_extend_call(
    module: &ModuleScope,
    name: Ident,
    span: Span,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedCall {
    let Some((receiver_expr, args)) = call.node.args.split_first() else {
        tc.push_error(TypeError::WrongArgCount {
            expected: 1,
            found: 0,
            span: tc.error_span(call.span),
        });
        return CheckedCall::value(checked_type(Type::Infer, tc));
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
            span: tc.error_span(span),
        });
        return CheckedCall::value(check_unhinted_args(args, tc));
    };

    let (extend, method, receiver_ty, owner_args) =
        match member::extend_method_parts(receiver.checked.ty.clone(), name, &matched) {
            Ok(parts) => parts,
            Err(error) => {
                push_extend_method_error(tc, error, span);
                return CheckedCall::value(check_unhinted_args(args, tc));
            }
        };

    let receiver_use = source_receiver(
        method.mode,
        receiver.access,
        &receiver.facts,
        receiver.identity.clone(),
        receiver.root_name,
        receiver_expr.node.id,
        name,
    );
    check_extend_method_access(
        &mut super::AccessPolicyOutput {
            source: tc.source_id(),
            current_module: &tc.current_module,
            config: &tc.config,
            warnings: &mut tc.warnings,
            errors: &mut tc.errors,
        },
        extend,
        method,
        &receiver.checked.ty,
        name,
        span,
    );
    let callee = tc
        .decls
        .callable_for_extend_method(receiver_ty, extend, name, method, owner_args);
    let receiver_arg = check_source_receiver(&receiver_use, call.span, tc);
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
        receiver_arg,
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
            span: tc.error_span(call.span),
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
    tc.check_mut_downcast_root_use(
        method.receiver_root_name,
        method.receiver_identity,
        call.span,
    );
    match method.receiver {
        ReceiverMode::Mutable => {
            if let Some(error) = method
                .receiver_access
                .mut_borrow_error(method.name, tc.error_span(call.span))
            {
                tc.push_error(error);
            } else {
                place::record_facts_write(method.receiver_id, method.receiver_place, tc);
            }
        }
        ReceiverMode::Value | ReceiverMode::Shared => {
            place::record_facts_read(method.receiver_id, method.receiver_place, tc);
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
        span: tc.error_span(call.span),
    });
    for arg in &call.node.args {
        check_expr_checked(arg, tc);
    }
    Type::Infer
}

fn func_type(params: &[FuncParam], ret: &ReturnSpec) -> Type {
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
        span: tc.error_span(span),
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
        span: tc.error_span(span),
    });
    Subject::Error
}

pub(super) fn check_args(
    args: &[ExprNode],
    params: &[FuncParam],
    call_span: Span,
    call_id: ExprId,
    tc: &mut TypeChecker,
) -> bool {
    if !check_arg_count(args, params.len(), call_span, tc) {
        return true;
    }

    let params = params
        .iter()
        .map(|param| CallParam {
            ty: tc.type_handle(&param.ty),
            mutable: param.mutable,
            cast_accept: param.cast_accept,
        })
        .collect::<Vec<_>>();
    check_source_args(args, &params, call_id, None, tc).failed
}
