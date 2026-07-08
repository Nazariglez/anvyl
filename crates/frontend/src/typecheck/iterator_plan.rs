use super::{
    CoreRangeKind, PatternBindMode, PatternRoot, PatternRootInput, PlaceAccess, TypeChecker,
    TypeError, collection_loan, pattern, place,
    semantic_use::{IterRuntimeCheckFact, IterRuntimeCheckKind},
};
use crate::{
    ast::{
        BlockNode, ConstValue, ExprId, ExprKind, ExprNode, ForBinding, Ident, Pattern, Stmt, Type,
    },
    span::Span,
};

pub(super) struct ForSourcePlan<'a> {
    pub(super) roots: Vec<PatternRoot<'a>>,
    pub(super) loans: Vec<collection_loan::ActiveCollectionLoan>,
}

enum IterSlot {
    Owned(Type),
    Item(Type),
}

pub(super) fn check_for_iterable<'a>(
    bindings: &'a [ForBinding],
    iterable: &ExprNode,
    tc: &mut TypeChecker,
) -> ForSourcePlan<'a> {
    let plan = peel_iter_plan(iterable, IterPlanContext::For, tc);
    let Some(source_expr) = plan
        .as_ref()
        .and_then(|plan| plan.source)
        .or_else(|| plan.is_none().then_some(iterable))
    else {
        if let Some(plan) = &plan {
            validate_sourceless_projection(plan.projection, iterable.span, tc);
        }
        let slots = helper_slots(
            bindings.len(),
            plan.as_ref().and_then(|plan| plan.sig.clone()),
        );
        for binding in bindings.iter().filter(|binding| binding.access.is_ref()) {
            tc.push_error(TypeError::ForRefRequiresMutableIterable {
                span: tc.error_span(binding.pattern.span),
            });
        }
        let roots = bindings
            .iter()
            .zip(slots)
            .map(|(binding, slot)| owned_helper_root(binding, slot))
            .collect();
        return ForSourcePlan {
            roots,
            loans: vec![],
        };
    };
    let source = super::check_place(source_expr, tc);
    place::record_value_read(source_expr.node.id, &source.value, tc);
    let iterable_ty = source.value.checked.ty.clone();
    let projection = plan
        .as_ref()
        .map_or(IterProjection::Default, |plan| plan.projection);
    validate_projection(projection, &iterable_ty, iterable.span, tc);
    let slots = for_source_slots(bindings, &iterable_ty, projection, source_expr.span, tc);
    let roots = bindings
        .iter()
        .zip(slots)
        .map(|(binding, slot)| for_slot_root(binding, slot, &source, source_expr, tc))
        .collect();
    let mut loans = vec![];
    if let Some(loan) =
        collection_loan::classify_for_loan(&iterable_ty, source.value.identity.clone())
    {
        loans.push(loan);
    }
    if let Some(plan) = &plan {
        if plan.has_unmapped_sources {
            for binding in bindings.iter().filter(|binding| binding.access.is_ref()) {
                tc.push_error(TypeError::ForRefRequiresMutableIterable {
                    span: tc.error_span(binding.pattern.span),
                });
            }
        }
        for loan_source in plan
            .loan_sources
            .iter()
            .filter(|loan_source| loan_source.node.id != source_expr.node.id)
        {
            let source = super::check_place(loan_source, tc);
            place::record_value_read(loan_source.node.id, &source.value, tc);
            validate_alternate_source(bindings, projection, loan_source, &source, tc);
            if let Some(loan) = collection_loan::classify_for_loan(
                &source.value.checked.ty,
                source.value.identity.clone(),
            ) {
                loans.push(loan);
            }
        }
    }
    ForSourcePlan { roots, loans }
}

fn validate_alternate_source(
    bindings: &[ForBinding],
    projection: IterProjection,
    source_expr: &ExprNode,
    source: &place::CheckedPlace,
    tc: &mut TypeChecker,
) {
    let slots = for_source_slots(
        bindings,
        &source.value.checked.ty,
        projection,
        source_expr.span,
        tc,
    );
    for (binding, slot) in bindings.iter().zip(slots) {
        if matches!(slot, IterSlot::Item(_)) && binding.access.is_ref() {
            let access = place::projected_field_access(source.value.access);
            if !access.can_assign() {
                tc.push_error(TypeError::ForRefRequiresMutableIterable {
                    span: tc.error_span(source_expr.span),
                });
            }
        }
    }
}

pub(super) fn validate_iter_helper_block(block: &BlockNode, tc: &mut TypeChecker) {
    for (index, stmt) in block.node.stmts.iter().enumerate() {
        if let Stmt::Binding(binding) = &stmt.node
            && matches!(binding.node.pattern.node, Pattern::Ident(_))
            && iter_return_adapter_arg_can_inline(&binding.node.value)
        {
            continue;
        }
        if index + 1 == block.node.stmts.len()
            && let Stmt::Return(ret) = &stmt.node
            && ret
                .node
                .value
                .as_ref()
                .is_some_and(|expr| may_be_iter_plan(expr, tc))
        {
            continue;
        }
        push_for_source_error(
            tc,
            "iterator helper statements must be simple aliases",
            stmt.span,
        );
    }
}

pub(super) fn check_iter_return_expr(expr: &ExprNode, tc: &mut TypeChecker) -> super::CheckedType {
    let Some(plan) = peel_iter_plan(expr, IterPlanContext::IterReturn, tc) else {
        super::check_expr_checked(expr, tc);
        tc.push_error(TypeError::IteratorPlanAsValue {
            span: tc.error_span(expr.span),
        });
        return super::checked_type(Type::Infer);
    };
    let ty = check_iter_plan_return(expr, &plan, tc);
    tc.record_current_iter_return_sig(ty.clone());
    super::checked_type(ty)
}

fn check_iter_plan_return(
    expr: &ExprNode,
    plan: &PeeledIterPlan<'_>,
    tc: &mut TypeChecker,
) -> Type {
    let Some(source_expr) = plan.source else {
        validate_sourceless_projection(plan.projection, expr.span, tc);
        return plan.sig.clone().unwrap_or(Type::Infer);
    };
    let source = super::check_place(source_expr, tc);
    place::record_value_read(source_expr.node.id, &source.value, tc);
    let source_ty = source.value.checked.ty.clone();
    validate_projection(plan.projection, &source_ty, expr.span, tc);
    iter_signature_ty(&source_ty, plan.projection, source_expr.span, tc)
}

pub(super) fn check_inferred_iter_return_expr(
    expr: &ExprNode,
    tc: &mut TypeChecker,
) -> Option<super::CheckedType> {
    if matches!(expr.node.kind, ExprKind::Block(_) | ExprKind::If(_)) {
        let checked =
            super::control_flow::check_return_expr(expr, &crate::ast::ReturnSpec::iter(), None, tc);
        tc.push_inferred_iter_return(expr.span);
        return Some(checked);
    }
    let plan = peel_iter_plan(expr, IterPlanContext::IterReturn, tc)?;
    let ty = check_iter_plan_return(expr, &plan, tc);
    tc.record_current_iter_return_sig(ty.clone());
    tc.push_inferred_iter_return(expr.span);
    Some(super::checked_type(ty))
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum IterPlanContext {
    For,
    IterReturn,
}

struct AdapterCall<'a> {
    name: Ident,
    args: &'a [ExprNode],
    span: Span,
}

struct PeeledIterPlan<'a> {
    source: Option<&'a ExprNode>,
    loan_sources: Vec<&'a ExprNode>,
    has_unmapped_sources: bool,
    projection: IterProjection,
    sig: Option<Type>,
}

struct HelperIterBase<'a> {
    source: Option<&'a ExprNode>,
    loan_sources: Vec<&'a ExprNode>,
    has_unmapped_sources: bool,
    sig: Option<Type>,
    projection: IterProjection,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum IterProjection {
    Default,
    MapKeys,
    MapValues,
}

pub(super) fn may_be_iter_plan(expr: &ExprNode, tc: &mut TypeChecker) -> bool {
    iter_plan_candidate(expr, tc)
}

fn iter_plan_candidate(expr: &ExprNode, tc: &mut TypeChecker) -> bool {
    if let ExprKind::If(if_) = &expr.node.kind {
        let Some(then_expr) = template_body_expr(&if_.node.then_block) else {
            return false;
        };
        let Some(else_expr) = if_.node.else_block.as_ref().and_then(template_body_expr) else {
            return false;
        };
        return iter_plan_candidate(then_expr, tc) && iter_plan_candidate(else_expr, tc);
    }

    let mut current = expr;
    while let ExprKind::Call(call) = &current.node.kind {
        if call.node.safe || !call.node.generic_args.is_empty() {
            return false;
        }
        let ExprKind::Field(field) = &call.node.func.node.kind else {
            break;
        };
        if field.node.safe {
            return false;
        }
        current = &field.node.target;
    }

    match &current.node.kind {
        ExprKind::IterSource(_) => true,
        ExprKind::Call(call) => iter_helper_call_candidate(call, tc),
        _ => false,
    }
}

fn iter_helper_call_candidate(call: &crate::ast::CallNode, tc: &mut TypeChecker) -> bool {
    iter_helper_callee(call, false, tc)
        .is_some_and(|callee| callable_returns_iter_plan(&callee, tc))
}

fn callable_returns_iter_plan(callee: &super::decls::CallableRef, tc: &mut TypeChecker) -> bool {
    callee.def.sig.ret.is_iter()
        || (callee.def.sig.ret.is_infer()
            && super::body::callable_template_returns_iter_plan_syntax(callee, tc))
}

fn iter_helper_callee(
    call: &crate::ast::CallNode,
    report_blocked: bool,
    tc: &mut TypeChecker,
) -> Option<super::decls::CallableRef> {
    if call.node.safe {
        return None;
    }
    let ExprKind::Ident(name) = call.node.func.node.kind else {
        return None;
    };
    match tc.resolve_ident_subject(name, call.node.func.span, super::NameSubjectMode::Value) {
        super::ResolvedIdentSubject::Local(super::LocalSymbol::Callable(info), _) => {
            Some(info.callee.clone())
        }
        super::ResolvedIdentSubject::Named(module, value_name, value) => {
            let super::ValueDecl::Func(sig) = value.as_ref() else {
                return None;
            };
            tc.decls.callable_for_value(&super::ResolvedValue {
                module,
                name: value_name,
                decl: super::ValueDecl::Func(sig.clone()),
            })
        }
        super::ResolvedIdentSubject::Blocked(error) => {
            if report_blocked {
                tc.push_error(*error);
            }
            None
        }
        super::ResolvedIdentSubject::Local(_, _)
        | super::ResolvedIdentSubject::Missing
        | super::ResolvedIdentSubject::Module(_)
        | super::ResolvedIdentSubject::Type(_) => None,
    }
}

fn peel_iter_plan<'a>(
    expr: &'a ExprNode,
    context: IterPlanContext,
    tc: &mut TypeChecker,
) -> Option<PeeledIterPlan<'a>> {
    let mut current = expr;
    let mut adapters = vec![];
    while let ExprKind::Call(call) = &current.node.kind {
        if call.node.safe || !call.node.generic_args.is_empty() {
            break;
        }
        let ExprKind::Field(field) = &call.node.func.node.kind else {
            break;
        };
        if field.node.safe {
            break;
        }
        adapters.push(AdapterCall {
            name: field.node.field,
            args: &call.node.args,
            span: call.span,
        });
        current = &field.node.target;
    }

    let base = match &current.node.kind {
        ExprKind::IterSource(iter) => HelperIterBase {
            source: Some(iter.node.source.as_ref()),
            loan_sources: vec![iter.node.source.as_ref()],
            has_unmapped_sources: false,
            sig: None,
            projection: IterProjection::Default,
        },
        _ => check_iter_helper_call(current, tc)?,
    };
    let mut projection = base.projection;
    for adapter in adapters.into_iter().rev() {
        check_adapter(&adapter, &mut projection, context, tc);
    }
    Some(PeeledIterPlan {
        source: base.source,
        loan_sources: base.loan_sources,
        has_unmapped_sources: base.has_unmapped_sources,
        projection,
        sig: base.sig,
    })
}

fn check_adapter(
    adapter: &AdapterCall<'_>,
    projection: &mut IterProjection,
    context: IterPlanContext,
    tc: &mut TypeChecker,
) {
    match adapter.name.as_str() {
        "rev" => {
            check_adapter_arity(adapter, 0, tc);
        }
        "skip" => check_count_adapter(adapter, CountAdapter::Skip, context, tc),
        "take" => check_count_adapter(adapter, CountAdapter::Take, context, tc),
        "step_by" => check_count_adapter(adapter, CountAdapter::StepBy, context, tc),
        "keys" => check_projection_adapter(adapter, IterProjection::MapKeys, projection, tc),
        "values" => check_projection_adapter(adapter, IterProjection::MapValues, projection, tc),
        _ => {
            for arg in adapter.args {
                super::check_expr_checked(arg, tc);
            }
            push_for_source_error(tc, "unknown iterator adapter", adapter.span);
        }
    }
}

#[derive(Clone, Copy)]
enum CountAdapter {
    Skip,
    Take,
    StepBy,
}

impl CountAdapter {
    fn runtime_check_kind(self) -> IterRuntimeCheckKind {
        match self {
            Self::Skip => IterRuntimeCheckKind::SkipNonNegative,
            Self::Take => IterRuntimeCheckKind::TakeNonNegative,
            Self::StepBy => IterRuntimeCheckKind::StepByPositive,
        }
    }

    fn invalid_message(self) -> &'static str {
        match self {
            Self::Skip => "skip requires a non-negative count",
            Self::Take => "take requires a non-negative count",
            Self::StepBy => "step_by requires a positive count",
        }
    }

    fn accepts(self, value: i64) -> bool {
        match self {
            Self::Skip | Self::Take => value >= 0,
            Self::StepBy => value > 0,
        }
    }
}

fn check_count_adapter(
    adapter: &AdapterCall<'_>,
    kind: CountAdapter,
    context: IterPlanContext,
    tc: &mut TypeChecker,
) {
    if !check_adapter_arity(adapter, 1, tc) {
        return;
    }
    let arg = &adapter.args[0];
    if context == IterPlanContext::IterReturn && !iter_return_adapter_arg_can_inline(arg) {
        push_for_source_error(
            tc,
            "iterator helper adapter arguments must be literals or simple names",
            arg.span,
        );
    }
    let checked = super::check_expr_checked(arg, tc);
    let int = TypeChecker::type_handle(&Type::Int);
    tc.expect_assignable(arg.span, checked.handle, int);
    if matches!(checked.ty, Type::Int | Type::Infer) {
        match known_int(arg, tc) {
            Some(value) if !kind.accepts(value) => {
                push_for_source_error(tc, kind.invalid_message(), arg.span);
            }
            Some(_) => {}
            None => {
                let site = tc.current_expr_site(arg.node.id);
                tc.semantic_facts.record_iter_runtime_check(
                    site,
                    IterRuntimeCheckFact {
                        expr: arg.node.id,
                        kind: kind.runtime_check_kind(),
                    },
                );
            }
        }
    }
}

fn check_projection_adapter(
    adapter: &AdapterCall<'_>,
    next: IterProjection,
    projection: &mut IterProjection,
    tc: &mut TypeChecker,
) {
    if !check_adapter_arity(adapter, 0, tc) {
        return;
    }
    if *projection != IterProjection::Default {
        push_for_source_error(
            tc,
            "map projection adapters cannot be chained",
            adapter.span,
        );
    }
    *projection = next;
}

fn check_adapter_arity(adapter: &AdapterCall<'_>, expected: usize, tc: &mut TypeChecker) -> bool {
    if adapter.args.len() == expected {
        return true;
    }
    for arg in adapter.args {
        super::check_expr_checked(arg, tc);
    }
    tc.push_error(TypeError::WrongArgCount {
        expected,
        found: adapter.args.len(),
        span: tc.error_span(adapter.span),
    });
    false
}

fn iter_return_adapter_arg_can_inline(expr: &ExprNode) -> bool {
    matches!(expr.node.kind, ExprKind::Ident(_) | ExprKind::Lit(_))
}

fn known_int(expr: &ExprNode, tc: &mut TypeChecker) -> Option<i64> {
    match tc.eval_const_expr(expr, false) {
        Ok(ConstValue::Int(value)) => Some(value),
        Ok(_) | Err(_) => None,
    }
}

fn check_iter_helper_call<'a>(
    expr: &'a ExprNode,
    tc: &mut TypeChecker,
) -> Option<HelperIterBase<'a>> {
    let ExprKind::Call(call) = &expr.node.kind else {
        return None;
    };
    let callee = iter_helper_callee(call, true, tc)?;
    callable_returns_iter_plan(&callee, tc)
        .then(|| iter_helper_base(&callee, call, expr.node.id, tc))
}

fn iter_helper_base<'a>(
    callee: &super::decls::CallableRef,
    call: &'a crate::ast::CallNode,
    call_id: ExprId,
    tc: &mut TypeChecker,
) -> HelperIterBase<'a> {
    let target = super::postfix::check_iter_helper_call_target(callee, call, call_id, tc);
    let sources = iter_helper_source_args(&callee.def.id, call, tc);
    let (source, projection) = sources
        .args
        .first()
        .map_or((None, IterProjection::Default), |source| {
            (Some(source.expr), source.projection)
        });
    let loan_sources = sources.args.iter().map(|source| source.expr).collect();
    let sig = target
        .as_ref()
        .and_then(|target| {
            let body = super::BodyInstanceKey::Callable(super::CallableInstanceKey {
                target: target.id.clone(),
                args: target.args.clone(),
            });
            tc.iter_return_sig(&body)
                .filter(|ty| !matches!(ty, Type::Infer))
                .cloned()
        })
        .or_else(|| iter_helper_template_sig(&callee.def.id, call, tc));
    HelperIterBase {
        source,
        loan_sources,
        has_unmapped_sources: sources.has_unmapped,
        sig,
        projection,
    }
}

fn iter_helper_template_sig(
    id: &super::decls::CallableId,
    call: &crate::ast::CallNode,
    tc: &mut TypeChecker,
) -> Option<Type> {
    let (source, projection, params) = {
        let template = tc.callable_template(id)?;
        let aliases = template_iter_aliases(&template.body);
        let source =
            template_iter_source_with_aliases(template_body_expr(&template.body)?, &aliases)?;
        (
            source.expr.clone(),
            source.projection,
            template
                .params
                .iter()
                .map(|param| param.name)
                .collect::<Vec<_>>(),
        )
    };
    let source = if let ExprKind::Ident(name) = source.node.kind {
        params
            .iter()
            .position(|param| *param == name)
            .and_then(|index| call.node.args.get(index))
            .unwrap_or(&source)
    } else {
        &source
    };
    if matches!(source.node.kind, ExprKind::Range(_)) && projection == IterProjection::Default {
        return Some(Type::Int);
    }
    let checked = super::check_expr_checked(source, tc);
    Some(iter_signature_ty(&checked.ty, projection, source.span, tc))
}

#[derive(Clone, Copy)]
struct TemplateIterSource<'a> {
    expr: &'a ExprNode,
    projection: IterProjection,
}

struct HelperSources<'a> {
    args: Vec<TemplateIterSource<'a>>,
    has_unmapped: bool,
}

fn iter_helper_source_args<'a>(
    id: &super::decls::CallableId,
    call: &'a crate::ast::CallNode,
    tc: &TypeChecker,
) -> HelperSources<'a> {
    let Some(template) = tc.callable_template(id) else {
        return HelperSources {
            args: vec![],
            has_unmapped: false,
        };
    };
    let Some(expr) = template_body_expr(&template.body) else {
        return HelperSources {
            args: vec![],
            has_unmapped: false,
        };
    };
    let aliases = template_iter_aliases(&template.body);
    let mut has_unmapped = false;
    let args = template_iter_sources_with_aliases(expr, &aliases)
        .unwrap_or_default()
        .into_iter()
        .filter_map(|source| {
            let expr = source.expr;
            let ExprKind::Ident(name) = expr.node.kind else {
                has_unmapped = true;
                return None;
            };
            let Some(index) = template.params.iter().position(|param| param.name == name) else {
                has_unmapped = true;
                return None;
            };
            Some(TemplateIterSource {
                expr: call.node.args.get(index)?,
                projection: source.projection,
            })
        })
        .collect();
    HelperSources { args, has_unmapped }
}

fn template_body_expr(body: &BlockNode) -> Option<&ExprNode> {
    body.node.tail.as_deref().or_else(|| {
        body.node.stmts.last().and_then(|stmt| match &stmt.node {
            Stmt::Expr(expr) => Some(expr),
            Stmt::Return(ret) => ret.node.value.as_ref(),
            _ => None,
        })
    })
}

fn template_iter_aliases(body: &BlockNode) -> Vec<(Ident, &ExprNode)> {
    body.node
        .stmts
        .iter()
        .filter_map(|stmt| {
            let Stmt::Binding(binding) = &stmt.node else {
                return None;
            };
            let Pattern::Ident(name) = binding.node.pattern.node else {
                return None;
            };
            Some((name, &binding.node.value))
        })
        .collect()
}

fn extended_template_aliases<'a>(
    aliases: &[(Ident, &'a ExprNode)],
    block: &'a BlockNode,
) -> Vec<(Ident, &'a ExprNode)> {
    let mut extended = aliases.to_vec();
    extended.extend(template_iter_aliases(block));
    extended
}

fn resolve_template_expr<'a>(
    expr: &'a ExprNode,
    aliases: &[(Ident, &'a ExprNode)],
) -> &'a ExprNode {
    let mut current = expr;
    for _ in 0..=aliases.len() {
        let ExprKind::Ident(name) = current.node.kind else {
            return current;
        };
        let Some(next) = aliases
            .iter()
            .rev()
            .find_map(|(alias, value)| (*alias == name).then_some(*value))
        else {
            return current;
        };
        if next.node.id == current.node.id {
            return current;
        }
        current = next;
    }
    current
}

fn template_iter_source_with_aliases<'a>(
    expr: &'a ExprNode,
    aliases: &[(Ident, &'a ExprNode)],
) -> Option<TemplateIterSource<'a>> {
    template_iter_sources_with_aliases(expr, aliases)?
        .into_iter()
        .next()
}

fn template_iter_sources(expr: &ExprNode) -> Option<Vec<TemplateIterSource<'_>>> {
    template_iter_sources_with_aliases(expr, &[])
}

fn template_iter_sources_with_aliases<'a>(
    expr: &'a ExprNode,
    aliases: &[(Ident, &'a ExprNode)],
) -> Option<Vec<TemplateIterSource<'a>>> {
    if let ExprKind::If(if_) = &expr.node.kind {
        let then_aliases = extended_template_aliases(aliases, &if_.node.then_block);
        let else_block = if_.node.else_block.as_ref()?;
        let else_aliases = extended_template_aliases(aliases, else_block);
        let then_sources = template_iter_sources_with_aliases(
            template_body_expr(&if_.node.then_block)?,
            &then_aliases,
        )?;
        let else_sources =
            template_iter_sources_with_aliases(template_body_expr(else_block)?, &else_aliases)?;
        let projection = then_sources.first()?.projection;
        if then_sources
            .iter()
            .chain(&else_sources)
            .any(|source| source.projection != projection)
        {
            return None;
        }
        return Some(then_sources.into_iter().chain(else_sources).collect());
    }
    let mut current = expr;
    let mut projection = IterProjection::Default;
    while let ExprKind::Call(call) = &current.node.kind {
        if call.node.safe || !call.node.generic_args.is_empty() {
            break;
        }
        let ExprKind::Field(field) = &call.node.func.node.kind else {
            break;
        };
        if field.node.safe {
            break;
        }
        match (field.node.field.as_str(), call.node.args.as_slice()) {
            ("rev" | "skip" | "take" | "step_by", _) => {}
            ("keys", []) => projection = IterProjection::MapKeys,
            ("values", []) => projection = IterProjection::MapValues,
            _ => break,
        }
        current = &field.node.target;
    }
    let ExprKind::IterSource(iter) = &current.node.kind else {
        return None;
    };
    Some(vec![TemplateIterSource {
        expr: resolve_template_expr(&iter.node.source, aliases),
        projection,
    }])
}

pub(super) fn validate_iter_branch_shapes(expr: &ExprNode, tc: &mut TypeChecker) {
    let ExprKind::If(if_) = &expr.node.kind else {
        return;
    };
    let Some(then_expr) = template_body_expr(&if_.node.then_block) else {
        return;
    };
    let Some(else_expr) = if_.node.else_block.as_ref().and_then(template_body_expr) else {
        return;
    };
    let Some(then_sources) = template_iter_sources(then_expr) else {
        return;
    };
    let Some(else_sources) = template_iter_sources(else_expr) else {
        return;
    };
    let Some(projection) = then_sources.first().map(|source| source.projection) else {
        return;
    };
    if else_sources
        .iter()
        .any(|source| source.projection != projection)
    {
        push_for_source_error(tc, "mismatched iterator projections", expr.span);
    }
}

fn iter_signature_ty(
    source_ty: &Type,
    projection: IterProjection,
    source_span: Span,
    tc: &mut TypeChecker,
) -> Type {
    match (
        projection,
        source_ty,
        tc.decls.core_range_inner(source_ty).cloned(),
    ) {
        (IterProjection::MapKeys, Type::Map { key, .. }, _) => (**key).clone(),
        (IterProjection::MapValues, Type::Map { value, .. }, _) => (**value).clone(),
        (IterProjection::MapKeys | IterProjection::MapValues, _, _)
        | (IterProjection::Default, Type::Infer, _) => Type::Infer,
        (
            IterProjection::Default,
            Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem },
            _,
        ) => (**elem).clone(),
        (IterProjection::Default, Type::Map { key, value }, _) => {
            Type::Tuple(vec![(**key).clone(), (**value).clone()])
        }
        (IterProjection::Default, _, Some(inner)) => inner,
        (IterProjection::Default, _, None) => {
            unsupported_slots(1, source_ty, source_span, tc);
            Type::Infer
        }
    }
}

fn validate_projection(
    projection: IterProjection,
    iterable_ty: &Type,
    iterable_span: Span,
    tc: &mut TypeChecker,
) {
    if projection == IterProjection::Default
        || matches!(iterable_ty, Type::Map { .. } | Type::Infer)
    {
        return;
    }
    push_for_source_error(tc, projection_error(projection), iterable_span);
}

fn validate_sourceless_projection(projection: IterProjection, span: Span, tc: &mut TypeChecker) {
    if projection != IterProjection::Default {
        push_for_source_error(tc, projection_error(projection), span);
    }
}

fn projection_error(projection: IterProjection) -> &'static str {
    match projection {
        IterProjection::Default => unreachable!("default projection has no error"),
        IterProjection::MapKeys => "keys() requires a map iterator",
        IterProjection::MapValues => "values() requires a map iterator",
    }
}

fn for_source_slots(
    bindings: &[ForBinding],
    iterable_ty: &Type,
    projection: IterProjection,
    iterable_span: Span,
    tc: &mut TypeChecker,
) -> Vec<IterSlot> {
    let range_inner = tc.decls.core_range_inner(iterable_ty).cloned();
    validate_range_source(
        iterable_ty,
        iterable_span,
        tc.decls.core_range_kind(iterable_ty),
        tc,
    );
    match (projection, bindings, iterable_ty, range_inner) {
        (IterProjection::MapKeys, [binding], Type::Map { key, .. }, _) => {
            reject_ref_binding(binding, tc);
            vec![IterSlot::Owned((**key).clone())]
        }
        (IterProjection::MapValues, [_], Type::Map { value, .. }, _) => {
            vec![IterSlot::Item((**value).clone())]
        }
        (IterProjection::MapKeys | IterProjection::MapValues, bindings, _, _) => {
            unsupported_slots(bindings.len(), iterable_ty, iterable_span, tc)
        }
        (
            IterProjection::Default,
            [_],
            Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem },
            _,
        ) => {
            vec![IterSlot::Item((**elem).clone())]
        }
        (IterProjection::Default, [binding], Type::Map { key, value }, _) => {
            if binding.access.is_ref() {
                tc.push_error(TypeError::ForMutableMapEntry {
                    span: tc.error_span(binding.pattern.span),
                });
            }
            vec![IterSlot::Owned(Type::Tuple(vec![
                (**key).clone(),
                (**value).clone(),
            ]))]
        }
        (IterProjection::Default, [_], Type::Infer, _) => infer_slots(1),
        (IterProjection::Default, [binding], _, Some(inner)) => {
            reject_range_ref_binding(binding, tc);
            vec![IterSlot::Owned(inner)]
        }
        (IterProjection::Default, [_], _, None) => {
            unsupported_slots(1, iterable_ty, iterable_span, tc)
        }

        (
            IterProjection::Default,
            [index, _],
            Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem },
            _,
        ) => {
            reject_ref_binding(index, tc);
            vec![IterSlot::Owned(Type::Int), IterSlot::Item((**elem).clone())]
        }
        (IterProjection::Default, [first, _], Type::Map { key, value }, _) => {
            if first.access.is_ref() {
                tc.push_error(TypeError::ForMutableMapKey {
                    span: tc.error_span(first.pattern.span),
                });
            }
            vec![
                IterSlot::Owned((**key).clone()),
                IterSlot::Item((**value).clone()),
            ]
        }
        (IterProjection::Default, [_, _], Type::Infer, _) => infer_slots(2),
        (IterProjection::Default, [index, binding], _, Some(inner)) => {
            for binding in [index, binding] {
                reject_range_ref_binding(binding, tc);
            }
            vec![IterSlot::Owned(Type::Int), IterSlot::Owned(inner)]
        }
        (IterProjection::Default, [_, _], _, None) => {
            unsupported_slots(2, iterable_ty, iterable_span, tc)
        }

        (IterProjection::Default, bindings, _, _) => infer_slots(bindings.len()),
    }
}

fn helper_slots(count: usize, sig: Option<Type>) -> Vec<IterSlot> {
    match (count, sig) {
        (1, Some(ty)) => vec![IterSlot::Owned(ty)],
        (2, Some(Type::Tuple(types))) if types.len() == 2 => {
            types.into_iter().map(IterSlot::Owned).collect()
        }
        _ => infer_slots(count),
    }
}

fn owned_helper_root(binding: &ForBinding, slot: IterSlot) -> PatternRoot<'_> {
    match slot {
        IterSlot::Owned(ty) | IterSlot::Item(ty) => owned_for_root(binding, ty),
    }
}

fn for_slot_root<'a>(
    binding: &'a ForBinding,
    slot: IterSlot,
    source: &place::CheckedPlace,
    source_expr: &ExprNode,
    tc: &mut TypeChecker,
) -> PatternRoot<'a> {
    match slot {
        IterSlot::Item(ty) if binding.access.is_ref() => {
            alias_for_root(binding, ty, source, source_expr, tc)
        }
        IterSlot::Owned(ty) | IterSlot::Item(ty) => owned_for_root(binding, ty),
    }
}

fn owned_for_root(binding: &ForBinding, ty: Type) -> PatternRoot<'_> {
    PatternRoot {
        pattern: &binding.pattern,
        input: PatternRootInput::Owned(ty),
        mode: PatternBindMode::Owned {
            mutable: binding.access.is_ref(),
        },
    }
}

fn alias_for_root<'a>(
    binding: &'a ForBinding,
    ty: Type,
    source: &place::CheckedPlace,
    source_expr: &ExprNode,
    tc: &mut TypeChecker,
) -> PatternRoot<'a> {
    let access = place::projected_field_access(source.value.access);
    let access = if access.can_assign() {
        access
    } else {
        tc.push_error(TypeError::ForRefRequiresMutableIterable {
            span: tc.error_span(source_expr.span),
        });
        PlaceAccess::Mutable
    };
    let place = pattern::PatternPlace {
        expected_handle: TypeChecker::type_handle(&ty),
        expected_ty: ty,
        access,
        facts: source.value.facts.clone(),
        identity: source.value.identity.clone().index(),
        accepts_extern_any: source.accepts_extern_any(),
        map_entry_alias: false,
    };
    PatternRoot {
        pattern: &binding.pattern,
        input: PatternRootInput::Place(Box::new(place), source_expr.node.id),
        mode: PatternBindMode::Alias,
    }
}

fn infer_slots(count: usize) -> Vec<IterSlot> {
    (0..count).map(|_| IterSlot::Owned(Type::Infer)).collect()
}

fn unsupported_slots(
    count: usize,
    iterable_ty: &Type,
    iterable_span: Span,
    tc: &mut TypeChecker,
) -> Vec<IterSlot> {
    tc.push_error(TypeError::ForIterableNotSupported {
        found: iterable_ty.clone(),
        span: tc.error_span(iterable_span),
    });
    infer_slots(count)
}

fn reject_ref_binding(binding: &ForBinding, tc: &mut TypeChecker) {
    if binding.access.is_ref() {
        tc.push_error(TypeError::ForRefRequiresMutableIterable {
            span: tc.error_span(binding.pattern.span),
        });
    }
}

fn reject_range_ref_binding(binding: &ForBinding, tc: &mut TypeChecker) {
    if binding.access.is_ref() {
        push_for_source_error(
            tc,
            "range loop bindings cannot be ref",
            binding.pattern.span,
        );
    }
}

fn validate_range_source(
    iterable_ty: &Type,
    iterable_span: Span,
    range_kind: Option<CoreRangeKind>,
    tc: &mut TypeChecker,
) {
    let Some(range_kind) = range_kind else {
        return;
    };

    if matches!(
        range_kind,
        CoreRangeKind::From | CoreRangeKind::To | CoreRangeKind::ToInclusive
    ) {
        push_for_source_error(tc, "range for-loops require bounded ranges", iterable_span);
    }

    if !matches!(
        tc.decls.core_range_inner(iterable_ty),
        Some(Type::Int | Type::Infer)
    ) {
        push_for_source_error(tc, "range for-loops require int bounds", iterable_span);
    }
}

fn push_for_source_error(tc: &mut TypeChecker, message: &'static str, span: Span) {
    tc.push_error(TypeError::ForIterationModifier {
        message,
        span: tc.error_span(span),
    });
}
