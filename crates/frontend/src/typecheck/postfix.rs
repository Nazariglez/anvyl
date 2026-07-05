use anvyx_externs::ReceiverMode;

use super::{
    CallForm, CallTarget, CheckedType, ConstSubst, Exposure, ExternUseTarget, FunctionValueArgFact,
    FunctionValueCallFact, FunctionValueKind, FunctionValueOrigin, GenericArgs, GenericParams,
    MemberAccessKind, MemberPathFact, MemberPathKind, PlaceAccess, TypeChecker, TypeError,
    TypeSubst,
    annotation::{AccessPolicyOutput, emit_access_policy},
    body::check_specialized_callable_body,
    check_arg_count, check_arg_range, check_expected_value_expr, check_expr_checked,
    check_value_expr_checked_with_hint, checked_from_type, checked_type, collection_loan,
    contracts::{self, DynamicMethodError},
    decls::{
        CallableKind, CallableParent, CallableRef, ContractRequirementSchema, ContractSetKey,
        DeclError, ExtendMethodSchema, ExtendSchema, MethodKey, MethodMode, MethodSurface,
        ModuleMemberLookup, ModuleScope, ResolvedValue, TypeBinding, ValueDecl, nominal_type,
        owner_template,
    },
    enum_variant::{self, ResolvedEnumVariant},
    extern_boundary,
    generic_bind::{GenericSolveSession, bind_prefix_generic_seeds},
    infer::{GenericSolverSeeds, GenericSolverVars, SemanticLocalId, TypeHandle},
    member,
    place::{self, MutableUseKind, PlaceUseFacts, PlaceValue},
    projection::{
        ExpectedFit, ExpectedPlaceProjection, ExpectedProjectionDecision, ExpectedProjectionMode,
        SourceAcceptance, apply_value_projection, classify_expected_fit, constrain_expected_return,
        expected_place_projection,
    },
};
use crate::{
    ast::{
        ArrayLen, CallNode, ConstValue, EscapeMode, ExprId, ExprKind, ExprNode, FieldAccessNode,
        FuncParam, GenericArg, Ident, IndexNode, MethodReceiver, ReturnSpec, TupleIndexNode, Type,
    },
    collection_effect::{self, CollectionKind, CollectionStructuralEffect},
    externs::catalog::{
        ExternMethodRef, ExternStaticRef, ExternTypeId, FunctionKey, ResolvedExternSignature,
        ResolvedExternTy,
    },
    span::Span,
};

enum Subject {
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
        explicit_args: Option<GenericArgs>,
        surface_ty: Type,
    },
    ExternMethod {
        method_ref: ExternMethodRef,
        receiver: ReceiverMode,
        receiver_use: ReceiverUse,
        name: Ident,
        signature: ResolvedExternSignature,
    },
    DynMethod {
        contract: ContractSetKey,
        requirement: ContractRequirementSchema,
        receiver_use: ReceiverUse,
        name: Ident,
    },
    DynHoleMethod {
        hole: crate::ast::DynContractHoleId,
        receiver_use: ReceiverUse,
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
    Index {
        node: &'a IndexNode,
        id: ExprId,
    },
    TupleIndex {
        node: &'a TupleIndexNode,
        id: ExprId,
    },
}

pub(super) struct PostfixChain<'a> {
    pub base: &'a ExprNode,
    pub steps: Vec<PostfixStep<'a>>,
}

struct ReceiverUse {
    access: PlaceAccess,
    facts: PlaceUseFacts,
    identity: place::PlaceIdentity,
    root_local: Option<SemanticLocalId>,
    root_binding: Option<super::BindingId>,
    root_source_depth: Option<usize>,
    root_name: Option<Ident>,
    global: Option<place::GlobalPlace>,
    id: ExprId,
}

struct SourceReceiver {
    mutable: bool,
    use_: ReceiverUse,
    name: Ident,
    collection_effect: Option<CollectionStructuralEffect>,
}

fn receiver_use(
    source: &PlaceValue,
    access: PlaceAccess,
    facts: &PlaceUseFacts,
    identity: place::PlaceIdentity,
    global: Option<place::GlobalPlace>,
    id: ExprId,
) -> ReceiverUse {
    ReceiverUse {
        access,
        facts: facts.clone(),
        identity,
        root_local: source.root_local,
        root_binding: source.root_binding,
        root_source_depth: source.root_source_depth,
        root_name: source.root_name,
        global,
        id,
    }
}

impl ReceiverUse {
    fn check_root(&self, span: Span, tc: &mut TypeChecker) {
        tc.check_mut_alias_root_use(self.root_name, &self.identity, span);
    }

    fn value(&self, tc: &TypeChecker) -> PlaceValue {
        PlaceValue {
            checked: checked_type(Type::Infer, tc),
            access: self.access,
            facts: self.facts.clone(),
            identity: self.identity.clone(),
            root_local: self.root_local,
            root_binding: self.root_binding,
            root_source_depth: self.root_source_depth,
            root_name: self.root_name,
            global: self.global.clone(),
            map_entry_alias: false,
        }
    }

    fn record_read(&self, tc: &mut TypeChecker) {
        place::record_value_read(self.id, &self.value(tc), tc);
    }

    fn record_mut(&self, tc: &mut TypeChecker) {
        place::record_mut_receiver(self.id, &self.value(tc), tc);
    }

    fn return_source(&self) -> ReturnPlaceSource {
        ReturnPlaceSource {
            access: self.access,
            facts: self.facts.clone(),
            identity: self.identity.clone(),
            root_local: self.root_local,
            root_binding: self.root_binding,
            root_source_depth: self.root_source_depth,
            root_name: self.root_name,
            global: self.global.clone(),
        }
    }
}

pub(super) fn collect_postfix_chain(expr: &ExprNode) -> Option<PostfixChain<'_>> {
    let (base, steps) = collect_steps(expr)?;
    Some(PostfixChain { base, steps })
}

pub(super) fn chain_has_safe(chain: &PostfixChain<'_>) -> bool {
    chain.steps.iter().any(|step| match step {
        PostfixStep::Field { node, .. } => node.node.safe,
        PostfixStep::Call { node, .. } => node.node.safe,
        PostfixStep::Index { node, .. } => node.node.safe,
        PostfixStep::TupleIndex { .. } => false,
    })
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
        ExprKind::Index(index) => {
            let (base, mut steps) = collect_steps_or_base(&index.node.target);
            steps.push(PostfixStep::Index {
                node: index,
                id: expr.node.id,
            });
            Some((base, steps))
        }
        ExprKind::TupleIndex(tuple) => {
            let (base, mut steps) = collect_steps_or_base(&tuple.node.target);
            steps.push(PostfixStep::TupleIndex {
                node: tuple,
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
    let (value, _) = tc.local_place_value(expr, name, value, None);
    tc.record_function_value_expr(
        expr.node.id,
        &value.checked.ty,
        FunctionValueKind::Storage(FunctionValueOrigin::KnownLocal),
    );
    Subject::Value(value)
}

fn warn_local_callable_deprecated(
    info: &super::LocalCallableInfo,
    span: Span,
    tc: &mut TypeChecker,
) {
    let id = &info.callee.def.id;
    if id.parent.is_none()
        && let Some(value) = tc.decls.local_value(&id.module, id.name)
    {
        tc.warn_named_value_deprecated(&value.decl, id.name, span);
    }
}

fn resolve_base(expr: &ExprNode, tc: &mut TypeChecker) -> Option<Subject> {
    match &expr.node.kind {
        ExprKind::Ident(name) => {
            match tc.resolve_ident_subject(*name, expr.span, super::NameSubjectMode::PostfixBase) {
                super::ResolvedIdentSubject::Local(super::LocalSymbol::Callable(info), _) => {
                    warn_local_callable_deprecated(&info, expr.span, tc);
                    Some(callable_subject(info.callee.clone(), None))
                }
                super::ResolvedIdentSubject::Local(super::LocalSymbol::Value(info), depth) => {
                    if let Some(checked) = tc.check_local_const_value_expr(expr, *name, &info) {
                        return Some(value_subject_checked(checked));
                    }
                    let value = tc.local_value_from_info(info, depth);
                    Some(local_value_subject(expr, *name, &value, tc))
                }
                super::ResolvedIdentSubject::Blocked(error) => {
                    tc.push_error(*error);
                    Some(Subject::Error)
                }
                super::ResolvedIdentSubject::Named(module, value_name, value) => Some(
                    named_value_subject(tc, &module, value_name, &value, expr.node.id, expr.span),
                ),
                super::ResolvedIdentSubject::Module(scope) => Some(Subject::Module(scope)),
                super::ResolvedIdentSubject::Type(ty) => Some(Subject::Type(ty)),
                super::ResolvedIdentSubject::Missing => None,
            }
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
    let mut last_function_origin = None;
    for (i, step) in chain.steps.iter().enumerate() {
        let is_last_step = i + 1 == chain.steps.len();
        let next_is_call = matches!(chain.steps.get(i + 1), Some(PostfixStep::Call { .. }));
        subject = match step {
            PostfixStep::Field { node, id } => {
                if node.node.safe {
                    subject = safe_subject(&subject, node.node.target.node.id, node.span, tc);
                    optional_chain = true;
                }
                let field_expected = (is_last_step && !next_is_call)
                    .then(|| expected_for_chain(expected, optional_chain, tc))
                    .flatten();
                let origin = field_function_origin(&subject);
                let subject = apply_field(
                    &subject,
                    node,
                    *id,
                    next_is_call,
                    field_expected.as_ref(),
                    tc,
                );
                let ty = set_postfix_step_type(*id, &subject, optional_chain, node.span, tc);
                tc.record_function_value_expr(*id, &ty, FunctionValueKind::Storage(origin));
                last_function_origin = Some(origin);
                subject
            }
            PostfixStep::Call { node, id } => {
                if node.node.safe {
                    subject = safe_subject(&subject, node.node.func.node.id, node.span, tc);
                    optional_chain = true;
                }
                let call_expected = is_last_step
                    .then(|| expected_for_chain(expected, optional_chain, tc))
                    .flatten();
                if let Subject::Value(value) = &subject
                    && !matches!(node.node.func.node.kind, ExprKind::Lambda(_))
                    && !super::type_ops::type_has_unfinished_facts(&value.checked.ty)
                {
                    let origin = last_function_origin
                        .unwrap_or_else(|| value_function_origin(node.node.func.node.id, value));
                    tc.record_function_value_expr(
                        node.node.func.node.id,
                        &value.checked.ty,
                        FunctionValueKind::Storage(origin),
                    );
                }
                let value = apply_call(&subject, node, *id, call_expected, tc);
                let returned_function = tc.type_carries_function_value(&value.checked.ty);
                let next_subject = if matches!(subject, Subject::Error) {
                    Subject::Error
                } else {
                    Subject::Value(value)
                };
                let ty = set_postfix_step_type(*id, &next_subject, optional_chain, node.span, tc);
                if returned_function {
                    let origin = tc.call_return_function_value_origin(*id);
                    if !super::type_ops::type_has_unfinished_facts(&ty) {
                        tc.record_function_value_expr(*id, &ty, FunctionValueKind::Storage(origin));
                    }
                    last_function_origin = Some(origin);
                }
                next_subject
            }
            PostfixStep::Index { node, id } => {
                if node.node.safe {
                    subject = safe_subject(&subject, node.node.target.node.id, node.span, tc);
                    optional_chain = true;
                }
                let origin = index_function_origin(&subject);
                let subject = apply_index(&subject, node, *id, optional_chain, tc);
                let ty = set_postfix_step_type(*id, &subject, optional_chain, node.span, tc);
                tc.record_function_value_expr(*id, &ty, FunctionValueKind::Storage(origin));
                last_function_origin = Some(origin);
                subject
            }
            PostfixStep::TupleIndex { node, id } => {
                let origin = FunctionValueOrigin::TupleField;
                let subject = apply_tuple_index(&subject, node, *id, optional_chain, tc);
                let ty = set_postfix_step_type(*id, &subject, optional_chain, node.span, tc);
                tc.record_function_value_expr(*id, &ty, FunctionValueKind::Storage(origin));
                last_function_origin = Some(origin);
                subject
            }
        };
    }

    match &subject {
        Subject::EnumVariant {
            resolved,
            explicit_args,
            ..
        } => {
            let expected_ty = expected_for_chain(expected, optional_chain, tc)
                .as_ref()
                .map(|handle| tc.handle_type(handle));
            let ty = if enum_variant::expect_unit(tc, resolved, expr.span) {
                enum_variant::solve_unit_owner_ty(
                    tc,
                    resolved,
                    explicit_args.as_ref(),
                    expected_ty.as_ref(),
                    expr.span,
                )
                .unwrap_or(Type::Infer)
            } else {
                Type::Infer
            };
            let ty = wrap_optional(ty, optional_chain, expr.span, tc);
            let checked = checked_from_type(expr, ty, tc);
            return PlaceValue::not_place(checked);
        }
        Subject::Callable { callee, .. } => {
            if callee.def.sig.ret.is_infer() {
                tc.push_error(TypeError::InferReturnValue {
                    span: tc.error_span(expr.span),
                });
            }
            let ty = func_type(&callee.def.sig.params, &callee.def.sig.ret);
            tc.record_function_value_expr(
                expr.node.id,
                &ty,
                TypeChecker::function_value_kind_for_callee(callee),
            );
        }
        Subject::Value(value) => {
            tc.check_mut_alias_root_use(value.root_name, &value.identity, expr.span);
            if record_final_read {
                place::record_value_read(expr.node.id, value, tc);
            }
        }
        Subject::Type(ty) => {
            tc.push_error(TypeError::TypeUsedAsValue {
                ty: ty.clone(),
                span: tc.error_span(expr.span),
            });
            return PlaceValue::not_place(checked_from_type(expr, Type::Infer, tc));
        }
        _ => {}
    }

    let ty = chain_type(&subject, optional_chain, expr.span, tc);
    let checked = CheckedType {
        handle: tc.set_type(expr.node.id, ty.clone(), expr.span),
        ty,
        contains_extern_any: subject_contains_extern_any(&subject),
    };
    let value = if optional_chain {
        PlaceValue::not_place(checked)
    } else {
        match subject {
            Subject::Value(mut value) => {
                let origin = last_function_origin
                    .unwrap_or_else(|| value_function_origin(expr.node.id, &value));
                tc.record_function_value_expr(
                    expr.node.id,
                    &checked.ty,
                    FunctionValueKind::Storage(origin),
                );
                value.checked = checked;
                value
            }
            _ => PlaceValue::not_place(checked),
        }
    };
    tc.record_expr_place(expr.node.id, &value);
    value
}

fn set_postfix_step_type(
    id: ExprId,
    subject: &Subject,
    optional_chain: bool,
    span: Span,
    tc: &mut TypeChecker,
) -> Type {
    let ty = chain_type(subject, optional_chain, span, tc);
    tc.set_type(id, ty.clone(), span);
    ty
}

fn field_function_origin(subject: &Subject) -> FunctionValueOrigin {
    let Subject::Value(value) = subject else {
        return FunctionValueOrigin::UnknownProjection;
    };
    if matches!(&value.checked.ty, Type::Nominal(nominal) if nominal.kind == crate::ast::NominalKind::DataRef)
    {
        return FunctionValueOrigin::DataRefProjection;
    }
    if value.global.is_some() {
        FunctionValueOrigin::GlobalProjection
    } else {
        FunctionValueOrigin::AggregateField
    }
}

fn index_function_origin(subject: &Subject) -> FunctionValueOrigin {
    let Subject::Value(value) = subject else {
        return FunctionValueOrigin::UnknownProjection;
    };
    match &value.checked.ty {
        Type::Array {
            len: ArrayLen::Fixed(_),
            ..
        } => FunctionValueOrigin::FixedArrayElement,
        Type::List { .. } => FunctionValueOrigin::ListElement,
        Type::Map { .. } => FunctionValueOrigin::MapValue,
        _ => FunctionValueOrigin::UnknownProjection,
    }
}

fn value_function_origin(expr_id: ExprId, value: &PlaceValue) -> FunctionValueOrigin {
    if let Some(global) = &value.global {
        if global.root_expr_id == expr_id {
            FunctionValueOrigin::GlobalRoot
        } else {
            FunctionValueOrigin::GlobalProjection
        }
    } else if value.root_local.is_some() {
        FunctionValueOrigin::KnownLocal
    } else {
        FunctionValueOrigin::UnknownProjection
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
            PostfixStep::Index { node, id } => {
                check_expr_checked(&node.node.index, tc);
                tc.set_type(*id, Type::Infer, node.span);
            }
            PostfixStep::TupleIndex { node, id } => {
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
        Subject::DynHoleMethod { .. } => Type::func(vec![], ReturnSpec::void()),
        Subject::Module(_) => Type::Void,
        Subject::Type(ty) => ty.clone(),
        Subject::QualifiedExtend { .. } | Subject::Error => Type::Infer,
    }
}

fn chain_type(subject: &Subject, optional: bool, span: Span, tc: &mut TypeChecker) -> Type {
    wrap_optional(subject_type(subject), optional, span, tc)
}

fn wrap_optional(ty: Type, optional: bool, span: Span, tc: &mut TypeChecker) -> Type {
    if optional {
        tc.optional_chain_result_type(ty, span)
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
        .semantic_option_inner(&ty)
        .map(|inner| tc.type_handle(inner))
}

fn safe_subject(subject: &Subject, expr_id: ExprId, span: Span, tc: &mut TypeChecker) -> Subject {
    let Subject::Value(value) = subject else {
        tc.push_error(TypeError::OptionalChainingOnNonOptional {
            span: tc.error_span(span),
        });
        return Subject::Error;
    };
    place::record_value_read(expr_id, value, tc);
    if matches!(value.checked.ty, Type::Infer) {
        return Subject::Value(value.clone());
    }
    let inner = tc.optional_chain_inner_type(&value.checked.ty, span);
    let origin = tc
        .semantic_facts
        .body(&tc.current_body())
        .and_then(|facts| facts.function_values.get(&expr_id))
        .and_then(|fact| match fact.kind {
            FunctionValueKind::Storage(origin) => Some(origin),
            _ => None,
        })
        .unwrap_or(FunctionValueOrigin::UnknownProjection);
    tc.record_function_value_expr(expr_id, &inner, FunctionValueKind::Storage(origin));
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

fn global_subject(sig: &super::GlobalSig, root_expr_id: ExprId, tc: &TypeChecker) -> Subject {
    Subject::Value(place::global_value(
        sig,
        root_expr_id,
        tc.global_checked(sig),
    ))
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
        Subject::Module(scope) => {
            apply_module_field(scope, field.node.field, field_id, field.span, kind, tc)
        }
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

fn apply_index(
    subject: &Subject,
    node: &IndexNode,
    id: ExprId,
    optional_chain: bool,
    tc: &mut TypeChecker,
) -> Subject {
    let Subject::Value(target) = subject else {
        let target = checked_type(subject_type(subject), tc);
        check_index_access_inner(node, &target, tc);
        return Subject::Error;
    };

    place::record_value_read(node.node.target.node.id, target, tc);
    let indexed = check_index_access_inner(node, &target.checked, tc);
    let mut checked = checked_type(indexed.read_ty, tc);
    checked.contains_extern_any = indexed.contains_extern_any;

    let value = if indexed.write_ty.is_some() {
        place::project_index_value(
            target,
            checked,
            node.node.target.node.id,
            &node.node.index,
            id,
            tc,
        )
    } else {
        PlaceValue::not_place(checked)
    };
    if let Some(inner) = tc.decls.semantic_option_inner(&value.checked.ty) {
        tc.record_function_value_expr(
            id,
            inner,
            FunctionValueKind::Storage(index_function_origin(subject)),
        );
    }
    if !optional_chain {
        tc.record_expr_place(id, &value);
    }
    Subject::Value(value)
}

fn apply_tuple_index(
    subject: &Subject,
    node: &TupleIndexNode,
    id: ExprId,
    optional_chain: bool,
    tc: &mut TypeChecker,
) -> Subject {
    let Subject::Value(target) = subject else {
        let target = subject_type(subject);
        if !matches!(target, Type::Infer) {
            tc.push_error(TypeError::TupleIndexOnNonTuple {
                ty: target,
                index: node.node.index,
                span: tc.error_span(node.span),
            });
        }
        return Subject::Error;
    };

    place::record_value_read(node.node.target.node.id, target, tc);
    tc.closure.copy_place_identity(node.node.target.node.id, id);
    let Type::Tuple(elems) = &target.checked.ty else {
        if !matches!(target.checked.ty, Type::Infer) {
            tc.push_error(TypeError::TupleIndexOnNonTuple {
                ty: target.checked.ty.clone(),
                index: node.node.index,
                span: tc.error_span(node.span),
            });
        }
        return Subject::Value(PlaceValue::not_place(checked_type(Type::Infer, tc)));
    };
    let Some(elem) = elems.get(node.node.index as usize).cloned() else {
        tc.push_error(TypeError::TupleIndexOutOfBounds {
            index: node.node.index,
            len: elems.len(),
            span: tc.error_span(node.span),
        });
        return Subject::Value(PlaceValue::not_place(checked_type(Type::Infer, tc)));
    };

    let mut checked = checked_type(elem, tc);
    checked.contains_extern_any = target.checked.contains_extern_any;
    let value = target.projected(
        checked,
        place::projected_field_access(target.access),
        target.facts.clone(),
        target.identity.clone().tuple(node.node.index as usize),
    );
    if !optional_chain {
        tc.record_expr_place(id, &value);
    }
    Subject::Value(value)
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
    module: &ModuleScope,
    name: Ident,
    value: &ValueDecl,
    root_expr_id: ExprId,
    span: Span,
) -> Subject {
    tc.warn_named_value_deprecated(value, name, span);
    if matches!(value, ValueDecl::Const(_)) {
        let value = tc.eval_top_const(module, name, tc.error_span(span));
        return value_subject(tc.record_const_value_result(root_expr_id, value), tc);
    }
    let resolved = ResolvedValue {
        module: (*module).clone(),
        name,
        decl: value.clone(),
    };
    let local_dyn_infer_ty = (module == &tc.current_module
        && super::dyn_infer::DynInference::has_raw_hole(value.ty()))
    .then(|| tc.lookup(name))
    .flatten()
    .map(|info| tc.solver.local_type_to_type(info.type_id));
    if let ValueDecl::Global(sig) = value {
        return global_subject(sig, root_expr_id, tc);
    }
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
    subject_ty: &Type,
    expected: Option<&TypeHandle>,
    tc: &TypeChecker,
) -> Subject {
    let expected_ty = expected.map(|handle| tc.handle_type(handle));
    let explicit_args = resolved.owner_args_from_type(subject_ty, tc);
    let surface_ty = expected_ty
        .as_ref()
        .filter(|ty| tc.decls.key_for_type(ty).as_ref() == Some(&resolved.key))
        .cloned()
        .or_else(|| explicit_args.as_ref().map(|_| subject_ty.clone()))
        .unwrap_or_else(|| resolved.owner_ty());
    Subject::EnumVariant {
        resolved,
        explicit_args,
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
    let indexed = check_index_access(index, &target.value.checked, tc);
    let ty = indexed
        .write_ty
        .as_ref()
        .unwrap_or(&indexed.read_ty)
        .clone();
    let mut checked = checked_from_type(expr, ty, tc);
    checked.contains_extern_any = indexed.contains_extern_any;

    if indexed.write_ty.is_some() {
        return place::project_index_value(
            &target.value,
            checked,
            index.node.target.node.id,
            &index.node.index,
            expr.node.id,
            tc,
        );
    }

    place::record_value_read(index.node.target.node.id, &target.value, tc);
    PlaceValue::not_place(checked)
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
    use_: ReceiverUse,
    name: Ident,
    collection_effect: Option<CollectionStructuralEffect>,
) -> SourceReceiver {
    SourceReceiver {
        mutable: matches!(mode, MethodMode::Instance { mutable: true }),
        use_,
        name,
        collection_effect,
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
    if tc.checked_is_poison(&receiver.checked) {
        return Subject::Error;
    }
    let receiver_ty = &receiver.checked.ty;
    let receiver_access = receiver.access;
    let receiver_place = &receiver.facts;
    let receiver_identity = receiver.identity.clone();

    tc.closure.copy_place_identity(receiver_id, field_id);

    match kind {
        MemberAccessKind::Field => {
            match place::field_value(None, receiver, field_id, name, span, tc) {
                place::FieldValueResult::Value(value, _) => {
                    if place::has_computed_extern_target(&value, tc) {
                        place::record_value_read(receiver_id, receiver, tc);
                    }
                    Subject::Value(*value)
                }
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
                    ReceiverUse {
                        access: receiver_access,
                        facts: receiver_place.clone(),
                        identity: receiver_identity,
                        root_local: receiver.root_local,
                        root_binding: receiver.root_binding,
                        root_source_depth: receiver.root_source_depth,
                        root_name: receiver.root_name,
                        global: receiver.global.clone(),
                        id: receiver_id,
                    },
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
                            receiver_use(
                                receiver,
                                receiver_access,
                                receiver_place,
                                receiver_identity,
                                receiver.global.clone(),
                                receiver_id,
                            ),
                            name,
                            collection_loan::classify_method_effect(
                                receiver_ty,
                                name,
                                &method.origin,
                            ),
                        )),
                    )
                }
                member::MethodResolution::Extend(method) => {
                    tc.mark_activation_imports_used(&method.extend.origin);
                    check_extend_method_access(
                        &mut AccessPolicyOutput {
                            source: tc.source_id(),
                            current_module: &tc.current_module,
                            lint_events: &mut tc.lint_events,
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
                            receiver_use(
                                receiver,
                                receiver_access,
                                receiver_place,
                                receiver_identity,
                                receiver.global.clone(),
                                receiver_id,
                            ),
                            name,
                            collection_loan::classify_method_effect(
                                receiver_ty,
                                name,
                                &method.extend.origin,
                            ),
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
                                    receiver_use(
                                        receiver,
                                        promoted_access,
                                        receiver_place,
                                        promoted_identity,
                                        receiver.global.as_ref().map(place::GlobalPlace::projected),
                                        field_id,
                                    ),
                                    name,
                                    None,
                                )),
                            )
                        }
                        member::PromotedMethodTarget::Extern(method) => Subject::ExternMethod {
                            method_ref: method.method_ref,
                            receiver: method.receiver,
                            receiver_use: ReceiverUse {
                                access: promoted_access,
                                facts: receiver_place.clone(),
                                identity: promoted_identity,
                                root_local: receiver.root_local,
                                root_binding: receiver.root_binding,
                                root_source_depth: receiver.root_source_depth,
                                root_name: receiver.root_name,
                                global: receiver.global.as_ref().map(place::GlobalPlace::projected),
                                id: field_id,
                            },
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
                    receiver_use: ReceiverUse {
                        access: receiver_access,
                        facts: receiver_place.clone(),
                        identity: receiver_identity,
                        root_local: receiver.root_local,
                        root_binding: receiver.root_binding,
                        root_source_depth: receiver.root_source_depth,
                        root_name: receiver.root_name,
                        global: receiver.global.clone(),
                        id: receiver_id,
                    },
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
    receiver_use: ReceiverUse,
    name: Ident,
    span: Span,
    tc: &mut TypeChecker,
) -> Subject {
    if let Some(hole) = super::dyn_infer::hole_id(contract) {
        return Subject::DynHoleMethod {
            hole,
            receiver_use,
            name,
        };
    }

    match contracts::resolve_dynamic_method(tc, contract, name) {
        Ok((contract, requirement)) => Subject::DynMethod {
            contract,
            requirement,
            receiver_use,
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
    out: &mut AccessPolicyOutput<'_>,
    extend: &ExtendSchema,
    method: &ExtendMethodSchema,
    owner_ty: &Type,
    name: Ident,
    span: Span,
) {
    emit_access_policy(
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
    field_id: ExprId,
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
            return named_value_subject(tc, &module, value_name, &decl, field_id, span);
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
            let has_variant = schema.variants.contains_key(name);
            if has_variant && has_static_extend {
                return static_extend_conflict(target, name, span, tc);
            }
            if has_variant {
                let resolved = enum_variant::resolve_use(tc, &key, name, span)
                    .expect("variant exists in enum schema");
                return enum_variant_subject(resolved, target, expected, tc);
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
    let matched = decls.find_extend_method(MethodSurface::Static, target, name, |ext| {
        TypeChecker::extend_visible_in(decls, current_module, ext)
    })?;
    let mut activation_origin = None;
    let subject = match member::extend_method_parts(target.clone(), name, &matched) {
        Ok((extend, method, _, owner_args)) => {
            activation_origin = Some(extend.origin.clone());
            check_extend_method_access(
                &mut AccessPolicyOutput {
                    source: tc.source_id(),
                    current_module: &tc.current_module,
                    lint_events: &mut tc.lint_events,
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
    if let Some(origin) = activation_origin {
        tc.mark_activation_imports_used(&origin);
    }
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
        Subject::EnumVariant {
            resolved,
            explicit_args,
            ..
        } => PlaceValue::not_place(check_enum_variant_call(
            resolved,
            explicit_args.as_ref(),
            call,
            call_id,
            expected,
            tc,
        )),
        Subject::ExternMethod {
            method_ref,
            receiver,
            receiver_use,
            name,
            signature,
        } => PlaceValue::not_place(check_extern_method_call(
            ExternMethodCall {
                method_ref: *method_ref,
                receiver: *receiver,
                receiver_use,
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
            receiver_use,
            name,
        } => PlaceValue::not_place(check_dyn_method_call(
            contract,
            requirement,
            receiver_use,
            *name,
            call,
            call_id,
            expected,
            tc,
        )),
        Subject::DynHoleMethod {
            hole,
            receiver_use,
            name,
        } => PlaceValue::not_place(check_dyn_hole_method_call(
            *hole,
            receiver_use,
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
            tc.check_mut_alias_root_use(value.root_name, &value.identity, call.node.func.span);
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
    receiver.use_.check_root(span, tc);
    if let Some(error) = receiver.collection_effect.and_then(|effect| {
        collection_loan::structural_method_error(
            &tc.active_collection_loans,
            effect,
            &receiver.use_.identity,
            tc.error_span(span),
        )
    }) {
        tc.push_error(error);
        return None;
    }
    if receiver.mutable {
        if let Some(error) = receiver.use_.access.error_for(
            MutableUseKind::MutatingReceiver(receiver.name),
            tc.error_span(span),
        ) {
            tc.push_error(error);
            return None;
        }
        receiver.use_.record_mut(tc);
        return Some(MutableArg {
            identity: receiver.use_.identity.clone(),
            span,
            source: receiver.use_.return_source(),
        });
    }
    receiver.use_.record_read(tc);
    None
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
                    escape: param.escape,
                })
                .collect::<Vec<_>>();
            let args_check = if check_arg_count(&call.node.args, params.len(), call.span, tc) {
                check_source_args(&call.node.args, &params, None, tc)
            } else {
                SourceArgsCheck {
                    failed: true,
                    place_source: None,
                }
            };
            let ret_handle = tc.type_handle(&ret.ty);
            let _ = constrain_expected_return(call.span, ret_handle.clone(), expected, tc);
            if !args_check.failed {
                let arg_facts = call
                    .node
                    .args
                    .iter()
                    .zip(&params)
                    .map(|(arg, param)| FunctionValueArgFact {
                        expr: arg.node.id,
                        param_ty: tc.handle_type(&param.ty),
                        mutable: param.mutable,
                        escape: param.escape,
                    })
                    .collect();
                tc.record_function_value_call(
                    call_id,
                    FunctionValueCallFact {
                        expr: call_id,
                        callee: call.node.func.node.id,
                        sig: callee_ty.clone(),
                        args: arg_facts,
                    },
                );
            }
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
    receiver: &ReceiverUse,
    name: Ident,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let method_receiver = if matches!(receiver.access, PlaceAccess::Mutable | PlaceAccess::DynView)
    {
        MethodReceiver::Ref
    } else {
        MethodReceiver::Value
    };
    let requires_mutable = matches!(method_receiver, MethodReceiver::Ref);
    receiver.check_root(call.span, tc);
    if requires_mutable {
        match receiver.access.error_for(
            MutableUseKind::MutatingReceiver(name),
            tc.error_span(call.span),
        ) {
            Some(error) => {
                tc.push_error(error);
            }
            None => {
                receiver.record_mut(tc);
            }
        }
    } else {
        receiver.record_read(tc);
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
        params.push(FuncParam::new(ty, false, false, EscapeMode::NonEscaping));
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
                tc.current_expr_site(call_id),
                tc.current_expr_site(receiver.id),
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
    receiver: &ReceiverUse,
    name: Ident,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let method_receiver = requirement
        .receiver
        .expect("contract requirements are finalized with receivers");
    let requires_mutable = matches!(method_receiver, MethodReceiver::Ref);
    receiver.check_root(call.span, tc);
    let mut failed = false;
    if requires_mutable {
        if let Some(error) = receiver.access.error_for(
            MutableUseKind::MutatingReceiver(name),
            tc.error_span(call.span),
        ) {
            tc.push_error(error);
            failed = true;
        } else {
            receiver.record_mut(tc);
        }
    } else {
        receiver.record_read(tc);
    }

    failed |= check_args(&call.node.args, &requirement.params, call.span, tc);
    let ret = tc.type_handle(&requirement.ret.ty);
    let _ = constrain_expected_return(call.span, ret.clone(), expected, tc);
    if !failed {
        tc.record_resolved_dyn_call(
            tc.current_expr_site(call_id),
            tc.current_expr_site(receiver.id),
            contract.clone(),
            name,
            call.node.args.len(),
            requires_mutable,
            tc.source_span(call.span),
        );
    }
    checked_type(requirement.ret.ty.clone(), tc)
}

struct ExternMethodCall<'a> {
    method_ref: ExternMethodRef,
    receiver: ReceiverMode,
    receiver_use: &'a ReceiverUse,
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
    escape: EscapeMode,
}

struct GenericCallSource<'a> {
    args: &'a [ExprNode],
    call_span: Span,
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

    let session = GenericSolveSession::new(tc, generics, seeds, call_span);
    add_constraints(session.vars(), tc);
    let mut failed = tc.solve_constraints();

    let params = instantiate_call_params(template_params, session.vars(), tc);
    let args_check = check_source_args(args, &params, receiver_arg, tc);
    failed |= args_check.failed;
    if !inferred_ret {
        let ret_handle = tc
            .solver
            .instantiate_generic_type(&template_ret.ty, session.vars());
        failed |= constrain_expected_return(call_span, ret_handle, expected, tc).failed();
    }

    if failed {
        return None;
    }

    let args = session.finish(tc)?;

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
            escape: param.escape,
        })
        .collect()
}

fn check_source_args(
    args: &[ExprNode],
    params: &[CallParam],
    receiver_arg: Option<MutableArg>,
    tc: &mut TypeChecker,
) -> SourceArgsCheck {
    let mut failed = false;
    let mut mutable_args = receiver_arg.into_iter().collect::<Vec<_>>();
    for (arg, param) in args.iter().zip(params) {
        let checked = check_source_arg(arg, param, tc);
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
    root_local: Option<SemanticLocalId>,
    root_binding: Option<super::BindingId>,
    root_source_depth: Option<usize>,
    root_name: Option<Ident>,
    global: Option<place::GlobalPlace>,
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
            root_local: source.root_local,
            root_binding: source.root_binding,
            root_source_depth: source.root_source_depth,
            root_name: source.root_name,
            global: source.global.as_ref().map(place::GlobalPlace::projected),
            map_entry_alias: false,
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
            root_local: value.root_local,
            root_binding: value.root_binding,
            root_source_depth: value.root_source_depth,
            root_name: value.root_name,
            global: value.global.clone(),
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

fn check_source_arg(arg: &ExprNode, param: &CallParam, tc: &mut TypeChecker) -> SourceArgCheck {
    let checked = match (param.mutable, param.cast_accept) {
        (true, _) => check_mutable_arg(arg, param, tc),
        (false, true) => check_cast_accept_arg(arg, param, tc),
        (false, false) => check_value_arg(arg, param, tc),
    };
    tc.record_argument_escape(arg, param.escape);
    checked
}

fn check_mutable_arg(arg: &ExprNode, param: &CallParam, tc: &mut TypeChecker) -> SourceArgCheck {
    let place = place::check_place(arg, tc);
    let name = super::assignment_target_name(arg);
    let mutability_error = place
        .value
        .access
        .error_for(MutableUseKind::RefArg(name), tc.error_span(arg.span));
    let target = tc.handle_type(&param.ty);
    if let Some(error) = mutability_error {
        tc.push_error(error);
        return reject_ref_arg(arg, param, &place.value.checked, tc);
    }

    match expected_place_projection(tc, arg, &place.value, &target) {
        ExpectedPlaceProjection::SourceAccepted => finish_ref_arg(arg, param, &place.value, tc),
        ExpectedPlaceProjection::Projected(projected) => finish_ref_arg(arg, param, &projected, tc),
        ExpectedPlaceProjection::Failed => SourceArgCheck {
            failed: true,
            mutable_arg: None,
        },
        ExpectedPlaceProjection::NotNeeded => reject_ref_arg(arg, param, &place.into_checked(), tc),
    }
}

fn reject_ref_arg(
    arg: &ExprNode,
    param: &CallParam,
    checked: &CheckedType,
    tc: &mut TypeChecker,
) -> SourceArgCheck {
    tc.reject_extern_any_escape(checked, arg.span);
    tc.expect_assignable_expr(
        arg.span,
        arg.node.id,
        checked.handle.clone(),
        param.ty.clone(),
    );
    SourceArgCheck {
        failed: tc.solve_constraints(),
        mutable_arg: None,
    }
}

fn finish_ref_arg(
    arg: &ExprNode,
    param: &CallParam,
    value: &PlaceValue,
    tc: &mut TypeChecker,
) -> SourceArgCheck {
    if let Some(error) = collection_loan::mutable_collection_arg_error(
        &tc.active_collection_loans,
        &value.identity,
        &value.checked.ty,
        &tc.handle_type(&param.ty),
        tc.error_span(arg.span),
    ) {
        tc.push_error(error);
        return SourceArgCheck {
            failed: true,
            mutable_arg: None,
        };
    }
    place::record_ref_argument(arg.node.id, value, tc);
    tc.reject_extern_any_escape(&value.checked, arg.span);
    tc.expect_assignable_expr(
        arg.span,
        arg.node.id,
        value.checked.handle.clone(),
        param.ty.clone(),
    );
    SourceArgCheck {
        failed: tc.solve_constraints(),
        mutable_arg: Some(mutable_arg(arg.span, value)),
    }
}

fn check_cast_accept_arg(
    arg: &ExprNode,
    param: &CallParam,
    tc: &mut TypeChecker,
) -> SourceArgCheck {
    let checked = check_value_expr_checked_with_hint(arg, None, tc);
    let target = tc.handle_type(&param.ty);
    match classify_expected_fit(
        tc,
        arg.span,
        &checked.ty,
        &target,
        ExpectedProjectionMode::CastAcceptingParam,
    ) {
        ExpectedFit::SourceAccepted(acceptance) => {
            finish_cast_accept_arg(arg, param, checked, acceptance, tc)
        }
        ExpectedFit::Project {
            projection,
            acceptance,
        } => {
            let source_ty = checked.ty.clone();
            let checked = apply_value_projection(tc, arg, &checked, &source_ty, projection);
            finish_cast_accept_arg(arg, param, checked, acceptance, tc)
        }
        fit @ (ExpectedFit::Ambiguous(_) | ExpectedFit::MissingProjection { .. }) => {
            match super::projection::expected_projection_decision(
                tc,
                arg.span,
                &checked.ty,
                &target,
                fit,
            ) {
                ExpectedProjectionDecision::Failed => {}
                ExpectedProjectionDecision::SourceAccepted
                | ExpectedProjectionDecision::NotNeeded
                | ExpectedProjectionDecision::Project(_) => unreachable!(),
            }
            SourceArgCheck {
                failed: true,
                mutable_arg: None,
            }
        }
        ExpectedFit::Deferred | ExpectedFit::Mismatch => {
            finish_unaccepted_cast_arg(arg, param, checked, tc)
        }
    }
}

fn finish_cast_accept_arg(
    arg: &ExprNode,
    param: &CallParam,
    checked: CheckedType,
    acceptance: SourceAcceptance,
    tc: &mut TypeChecker,
) -> SourceArgCheck {
    tc.reject_extern_any_escape(&checked, arg.span);
    match acceptance {
        SourceAcceptance::Assignable => {
            tc.expect_assignable_expr(arg.span, arg.node.id, checked.handle, param.ty.clone());
        }
        SourceAcceptance::Dyn { plan } => {
            super::convert::apply_expected_dyn_plan(tc, arg.span, Some(arg.node.id), plan);
        }
        SourceAcceptance::CastFrom(conversion) => {
            let target = tc.handle_type(&param.ty);
            super::body::check_cast_from_conversion_body(&conversion, &checked.ty, &target, tc);
            tc.mark_activation_imports_used(&conversion.origin);
            tc.record_conversion_escape(arg, conversion.escape);
        }
        SourceAcceptance::ExplicitCast { .. } => unreachable!(),
    }
    SourceArgCheck {
        failed: tc.solve_constraints(),
        mutable_arg: None,
    }
}

fn finish_unaccepted_cast_arg(
    arg: &ExprNode,
    param: &CallParam,
    checked: CheckedType,
    tc: &mut TypeChecker,
) -> SourceArgCheck {
    tc.reject_extern_any_escape(&checked, arg.span);
    let target = tc.handle_type(&param.ty);
    if tc.cast_from_ambiguous(&checked.ty, &target) {
        tc.push_error(TypeError::AmbiguousCast {
            from: checked.ty,
            to: target,
            span: tc.error_span(arg.span),
        });
        return SourceArgCheck {
            failed: true,
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
    let checked = check_expected_value_expr(arg, param.ty.clone(), tc);
    finish_value_arg(arg, param, checked, tc)
}

fn finish_value_arg(
    arg: &ExprNode,
    param: &CallParam,
    checked: CheckedType,
    tc: &mut TypeChecker,
) -> SourceArgCheck {
    tc.reject_extern_any_escape(&checked, arg.span);
    let dyn_format = matches!(tc.handle_type(&param.ty), Type::Any)
        && tc.reject_dyn_format(&checked.ty, arg.span);
    SourceArgCheck {
        failed: tc.solve_constraints() || dyn_format,
        mutable_arg: None,
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
        .map(|param| param.map_ty(|ty| tc.substitute_checked(ty, type_subst, const_subst, span)))
        .collect()
}

fn check_enum_variant_call(
    resolved: &ResolvedEnumVariant,
    explicit_args: Option<&GenericArgs>,
    call: &CallNode,
    call_id: ExprId,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let Some(params) = enum_variant::expect_tuple(tc, resolved, call.span) else {
        return check_unhinted_args(&call.node.args, tc);
    };
    if explicit_args.is_some() && !call.node.generic_args.is_empty() {
        tc.push_error(TypeError::GenericArity(super::ArityError::TypeArgs {
            expected: 0,
            found: call.node.generic_args.len(),
        }));
        return check_unhinted_args(&call.node.args, tc);
    }
    if params.len() != call.node.args.len() {
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
    let owner_args = explicit_args.cloned().unwrap_or_default();
    let callee = enum_variant::tuple_callable_ref(resolved, params, owner_args);
    let checked = check_callable_call_with_args(
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
    .checked;
    for arg in &call.node.args {
        tc.record_aggregate_elem_escape(call_id, arg);
    }
    checked
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
    record_collection_storage_args(callee, &call.node.args, tc);
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

fn record_collection_storage_args(callee: &CallableRef, args: &[ExprNode], tc: &mut TypeChecker) {
    let kind = match callee.receiver_ty.as_ref() {
        Some(Type::List { .. }) => CollectionKind::Sequence,
        Some(Type::Map { .. }) => CollectionKind::Map,
        _ => return,
    };
    for index in collection_effect::storage_value_arg_indices(kind, callee.def.id.name) {
        if let Some(arg) = args.get(*index) {
            tc.record_collection_storage_escape(arg);
        }
    }
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
            receiver_arg,
            expected,
        },
        tc,
        |vars, tc| constrain_callable_owner(callee, vars, call_span, tc),
    ) else {
        return CheckedCall::value(checked_type(Type::Infer, tc));
    };

    let provided_args = args.len();
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
    if let Some(inferred_ret) = check_specialized_callable_body(
        callee,
        &concrete_params,
        &ret,
        &args,
        type_subst,
        const_subst,
        &const_bindings,
        tc,
    ) {
        ret.ty = inferred_ret;
    }
    let id = callee.def.id.clone();
    let target = match form {
        CallForm::Normal => CallTarget::new(id, args),
        CallForm::QualifiedExtend { receiver } => CallTarget::qualified_extend(id, args, receiver),
    };
    tc.record_call(call_id, target.clone());
    tc.record_default_args(
        call_id,
        &target,
        provided_args,
        &concrete_params,
        &callee.def.sig.default_sites,
    );
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
        receiver_use(
            &receiver,
            receiver.access,
            &receiver.facts,
            receiver.identity.clone(),
            receiver.global.clone(),
            receiver_expr.node.id,
        ),
        name,
        collection_loan::classify_method_effect(&receiver.checked.ty, name, &extend.origin),
    );
    check_extend_method_access(
        &mut AccessPolicyOutput {
            source: tc.source_id(),
            current_module: &tc.current_module,
            lint_events: &mut tc.lint_events,
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
    method.receiver_use.check_root(call.span, tc);
    match method.receiver {
        ReceiverMode::Mutable => {
            if let Some(error) = method
                .receiver_use
                .access
                .mut_borrow_error(method.name, tc.error_span(call.span))
            {
                tc.push_error(error);
            } else {
                method.receiver_use.record_mut(tc);
            }
        }
        ReceiverMode::Value | ReceiverMode::Shared => {
            method.receiver_use.record_read(tc);
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
    Type::func(params.to_vec(), ret.clone())
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

fn check_args(
    args: &[ExprNode],
    params: &[FuncParam],
    call_span: Span,
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
            escape: param.escape,
        })
        .collect::<Vec<_>>();
    check_source_args(args, &params, None, tc).failed
}

pub(super) fn check_tuple_index_access(
    expr: &ExprNode,
    node: &TupleIndexNode,
    target: &CheckedType,
    tc: &mut TypeChecker,
) -> CheckedType {
    if tc.checked_is_poison(target) {
        return checked_from_type(expr, Type::Infer, tc);
    }
    let Type::Tuple(elems) = &target.ty else {
        tc.push_error(TypeError::TupleIndexOnNonTuple {
            ty: target.ty.clone(),
            index: node.node.index,
            span: tc.error_span(node.span),
        });
        return checked_from_type(expr, Type::Infer, tc);
    };

    let Some(elem_ty) = elems.get(node.node.index as usize).cloned() else {
        tc.push_error(TypeError::TupleIndexOutOfBounds {
            index: node.node.index,
            len: elems.len(),
            span: tc.error_span(node.span),
        });
        return checked_from_type(expr, Type::Infer, tc);
    };

    let mut checked = checked_from_type(expr, elem_ty, tc);
    checked.contains_extern_any = target.contains_extern_any;
    if let Some(target_value) = tc.expr_place(node.node.target.node.id) {
        let value = target_value.projected(
            checked.clone(),
            place::projected_field_access(target_value.access),
            target_value.facts.clone(),
            target_value
                .identity
                .clone()
                .tuple(node.node.index as usize),
        );
        tc.record_expr_place(expr.node.id, &value);
    }
    checked
}

pub(super) struct CheckedIndex {
    pub(super) read_ty: Type,
    pub(super) write_ty: Option<Type>,
    pub(super) contains_extern_any: bool,
}

impl CheckedIndex {
    fn new(
        read_ty: Type,
        write_ty: Option<Type>,
        target: &CheckedType,
        index: &CheckedType,
    ) -> Self {
        Self {
            read_ty,
            write_ty,
            contains_extern_any: target.contains_extern_any || index.contains_extern_any,
        }
    }

    fn projected(read_ty: Type, write_ty: Type, target: &CheckedType, index: &CheckedType) -> Self {
        Self::new(read_ty, Some(write_ty), target, index)
    }

    fn same_projected(ty: Type, target: &CheckedType, index: &CheckedType) -> Self {
        Self::projected(ty.clone(), ty, target, index)
    }

    fn value(read_ty: Type, target: &CheckedType, index: &CheckedType) -> Self {
        Self::new(read_ty, None, target, index)
    }

    fn infer(target: &CheckedType, index: &CheckedType) -> Self {
        Self::value(Type::Infer, target, index)
    }
}

pub(super) fn check_index_access(
    node: &IndexNode,
    target: &CheckedType,
    tc: &mut TypeChecker,
) -> CheckedIndex {
    check_index_access_inner(node, target, tc)
}

pub(super) fn check_map_key(node: &IndexNode, key: &Type, tc: &mut TypeChecker) -> CheckedType {
    let key_handle = tc.type_handle(key);
    let index = check_value_expr_checked_with_hint(&node.node.index, Some(key_handle.clone()), tc);
    tc.record_function_value_expr(
        node.node.index.node.id,
        key,
        FunctionValueKind::Storage(FunctionValueOrigin::MapKey),
    );
    tc.expect_assignable(node.node.index.span, index.handle.clone(), key_handle);
    tc.solve_constraints();
    index
}

fn check_index_access_inner(
    node: &IndexNode,
    target: &CheckedType,
    tc: &mut TypeChecker,
) -> CheckedIndex {
    if matches!(node.node.index.node.kind, ExprKind::Range(_)) {
        return check_range_index_access(node, target, tc);
    }

    match &target.ty {
        Type::Array {
            elem,
            len: ArrayLen::Fixed(len),
        } => check_sequence_scalar_index(node, target, elem, Some(*len), tc),
        Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem } => {
            check_sequence_scalar_index(node, target, elem, None, tc)
        }
        Type::Map { key, value } => {
            let index = check_map_key(node, key, tc);
            let read_ty = tc.core_option_or_infer((**value).clone(), node.span);
            CheckedIndex::value(read_ty, target, &index)
        }
        Type::Infer => {
            let index = check_expr_checked(&node.node.index, tc);
            CheckedIndex::infer(target, &index)
        }
        found => {
            let index = check_expr_checked(&node.node.index, tc);
            if !found.is_void() {
                tc.push_error(TypeError::IndexOnNonIndexable {
                    found: found.clone(),
                    span: tc.error_span(node.span),
                });
            }
            CheckedIndex::infer(target, &index)
        }
    }
}

fn check_sequence_scalar_index(
    node: &IndexNode,
    target: &CheckedType,
    elem: &Type,
    fixed_len: Option<usize>,
    tc: &mut TypeChecker,
) -> CheckedIndex {
    let index =
        check_value_expr_checked_with_hint(&node.node.index, Some(tc.type_handle(&Type::Int)), tc);
    if !matches!(index.ty, Type::Infer | Type::Int) {
        tc.push_error(TypeError::IndexNotInt {
            found: index.ty.clone(),
            span: tc.error_span(node.node.index.span),
        });
    }
    if let (Type::Int, Some(len)) = (&index.ty, fixed_len) {
        check_static_array_index_bounds(&node.node.index, len, tc);
    }
    CheckedIndex::same_projected(elem.clone(), target, &index)
}

fn check_static_array_index_bounds(index_expr: &ExprNode, len: usize, tc: &mut TypeChecker) {
    let Ok(ConstValue::Int(index)) = tc.eval_const_expr(index_expr, false) else {
        return;
    };
    if index >= 0 && usize::try_from(index).is_ok_and(|index| index < len) {
        return;
    }
    tc.push_error(TypeError::ArrayIndexOutOfBounds {
        index,
        len,
        span: tc.error_span(index_expr.span),
    });
}

fn check_range_index_access(
    node: &IndexNode,
    target: &CheckedType,
    tc: &mut TypeChecker,
) -> CheckedIndex {
    let index = check_expr_checked(&node.node.index, tc);
    match &target.ty {
        Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem } => {
            if !matches!(
                tc.decls.core_range_inner(&index.ty),
                Some(Type::Int | Type::Infer)
            ) {
                tc.push_error(TypeError::RangeIndexNotInt {
                    found: index.ty.clone(),
                    span: tc.error_span(node.node.index.span),
                });
            }
            CheckedIndex::value(Type::List { elem: elem.clone() }, target, &index)
        }
        Type::Infer => CheckedIndex::infer(target, &index),
        found => {
            tc.push_error(TypeError::RangeIndexUnsupported {
                found: found.clone(),
                span: tc.error_span(node.span),
            });
            CheckedIndex::infer(target, &index)
        }
    }
}
