use super::{
    CallableBody, CheckedBranch, CheckedType, ConditionKind, CoreRangeKind, PatternBindMode,
    PatternContext, PatternRoot, PatternRootInput, PlaceAccess, PlaceIdentity, ReturnMode,
    ReturnSpec, TypeChecker, TypeError, TypeHandle,
    body::check_callable_body_place_return,
    check_block_checked, check_bool_condition, check_expr_checked, check_place,
    check_value_expr_checked_with_hint, checked_branch_against_expected, checked_from_checked,
    checked_type, checked_void, join_branches_with_hint, match_check, match_coverage,
    pattern::{self, check_pattern_scrutinee, mode_for_head},
    place,
};
use crate::{
    ast::{
        BlockNode, ConstValue, DeferBody, DeferNode, ExprKind, ExprNode, For, ForBinding, ForNode,
        MatchArmNode, MatchNode, Return, ReturnAccess, ReturnNode, Stmt, StmtNode, Type, WhileNode,
    },
    span::Span,
};

#[derive(Clone, Copy)]
pub(super) struct ControlFlowFrame {
    loops: usize,
    defers: usize,
    global_initializers: usize,
}

enum ReturnTarget {
    Explicit {
        ret: ReturnSpec,
        source: Option<PlaceIdentity>,
    },
    Infer {
        access: ReturnAccess,
        source: Option<PlaceIdentity>,
    },
}

impl TypeChecker {
    pub(super) fn enter_loop(&mut self) {
        self.loop_depth += 1;
    }

    pub(super) fn exit_loop(&mut self) {
        self.loop_depth = self
            .loop_depth
            .checked_sub(1)
            .expect("loop depth underflow");
    }

    pub(super) fn in_loop(&self) -> bool {
        self.loop_depth > 0
    }

    pub(super) fn enter_defer(&mut self) {
        self.defer_depth += 1;
    }

    pub(super) fn exit_defer(&mut self) {
        self.defer_depth = self
            .defer_depth
            .checked_sub(1)
            .expect("defer depth underflow");
    }

    pub(super) fn in_defer(&self) -> bool {
        self.defer_depth > 0
    }

    pub(super) fn enter_function_control_flow(&mut self) -> ControlFlowFrame {
        let frame = ControlFlowFrame {
            loops: self.loop_depth,
            defers: self.defer_depth,
            global_initializers: self.global_initializer_depth,
        };
        self.loop_depth = 0;
        self.defer_depth = 0;
        self.global_initializer_depth = 0;
        frame
    }

    pub(super) fn exit_function_control_flow(&mut self, frame: ControlFlowFrame) {
        self.loop_depth = frame.loops;
        self.defer_depth = frame.defers;
        self.global_initializer_depth = frame.global_initializers;
    }
}

pub(super) fn check_defer(defer_node: &DeferNode, tc: &mut TypeChecker) {
    if tc.in_global_initializer() {
        tc.push_error(TypeError::CompileError {
            message: "defer is not allowed in runtime global initializers".to_string(),
            span: tc.error_span(defer_node.span),
        });
        check_defer_body(&defer_node.node.body, tc);
        return;
    }

    tc.enter_defer();
    check_defer_body(&defer_node.node.body, tc);
    tc.exit_defer();
}

fn check_defer_body(body: &DeferBody, tc: &mut TypeChecker) {
    match body {
        DeferBody::Expr(expr) => {
            check_expr_checked(expr, tc);
        }
        DeferBody::Block(block) => {
            check_block_checked(block, tc);
        }
    }
}

pub(super) fn check_break(span: Span, tc: &mut TypeChecker) {
    if tc.in_defer() {
        tc.push_error(TypeError::BreakInsideDefer {
            span: tc.error_span(span),
        });
    } else if !tc.in_loop() {
        tc.push_error(TypeError::BreakOutsideLoop {
            span: tc.error_span(span),
        });
    }
}

pub(super) fn check_continue(span: Span, tc: &mut TypeChecker) {
    if tc.in_defer() {
        tc.push_error(TypeError::ContinueInsideDefer {
            span: tc.error_span(span),
        });
    } else if !tc.in_loop() {
        tc.push_error(TypeError::ContinueOutsideLoop {
            span: tc.error_span(span),
        });
    }
}

pub(super) fn check_while(while_node: &WhileNode, tc: &mut TypeChecker) {
    let cond = check_expr_checked(&while_node.node.cond, tc);
    check_bool_condition(ConditionKind::While, cond, while_node.node.cond.span, tc);
    check_loop_body(&while_node.node.body, tc);
}

pub(super) fn check_loop_body(body: &BlockNode, tc: &mut TypeChecker) {
    tc.closure.enter_loop_flow();
    tc.enter_loop();
    check_block_checked(body, tc);
    tc.exit_loop();
    tc.closure.exit_loop_flow();
}

pub(super) fn check_for(for_node: &ForNode, tc: &mut TypeChecker) {
    let node = &for_node.node;
    let source = check_place(&node.iterable, tc);
    place::record_value_read(node.iterable.node.id, &source.value, tc);
    let iterable_ty = source.value.checked.ty.clone();
    check_for_modifiers(node, &iterable_ty, tc);

    let slots = for_slots(&node.bindings, &iterable_ty, node.iterable.span, tc);
    let roots = node
        .bindings
        .iter()
        .zip(slots)
        .map(|(binding, slot)| for_slot_root(binding, slot, &source, &node.iterable, tc))
        .collect();

    tc.push_scope();
    pattern::check_roots(roots, PatternContext::For, tc);
    check_loop_body(&node.body, tc);
    tc.pop_scope();
}

enum ForSlot {
    Owned(Type),
    Item(Type),
}

fn for_slots(
    bindings: &[ForBinding],
    iterable_ty: &Type,
    iterable_span: Span,
    tc: &mut TypeChecker,
) -> Vec<ForSlot> {
    let range_inner = tc.decls.core_range_inner(iterable_ty).cloned();
    match (bindings, iterable_ty, range_inner) {
        ([_], Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem }, _) => {
            vec![ForSlot::Item((**elem).clone())]
        }
        ([binding], Type::Map { key, value }, _) => {
            if binding.mutable {
                tc.push_error(TypeError::ForMutableMapEntry {
                    span: tc.error_span(binding.pattern.span),
                });
            }
            vec![ForSlot::Owned(Type::Tuple(vec![
                (**key).clone(),
                (**value).clone(),
            ]))]
        }
        ([_], Type::Infer, _) => infer_for_slots(1),
        ([_], _, Some(inner)) => vec![ForSlot::Owned(inner)],
        ([_], _, None) => unsupported_for_slots(1, iterable_ty, iterable_span, tc),

        ([_, _], Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem }, _) => {
            vec![ForSlot::Owned(Type::Int), ForSlot::Item((**elem).clone())]
        }
        ([first, _], Type::Map { key, value }, _) => {
            if first.mutable {
                tc.push_error(TypeError::ForMutableMapKey {
                    span: tc.error_span(first.pattern.span),
                });
            }
            vec![
                ForSlot::Owned((**key).clone()),
                ForSlot::Item((**value).clone()),
            ]
        }
        ([_, _], Type::Infer, _) => infer_for_slots(2),
        ([_, _], _, Some(inner)) => vec![ForSlot::Owned(Type::Int), ForSlot::Owned(inner)],
        ([_, _], _, None) => unsupported_for_slots(2, iterable_ty, iterable_span, tc),

        (bindings, _, _) => infer_for_slots(bindings.len()),
    }
}

fn infer_for_slots(count: usize) -> Vec<ForSlot> {
    (0..count).map(|_| ForSlot::Owned(Type::Infer)).collect()
}

fn unsupported_for_slots(
    count: usize,
    iterable_ty: &Type,
    iterable_span: Span,
    tc: &mut TypeChecker,
) -> Vec<ForSlot> {
    tc.push_error(TypeError::ForIterableNotSupported {
        found: iterable_ty.clone(),
        span: tc.error_span(iterable_span),
    });
    infer_for_slots(count)
}

fn for_slot_root<'a>(
    binding: &'a ForBinding,
    slot: ForSlot,
    source: &place::CheckedPlace,
    iterable: &ExprNode,
    tc: &mut TypeChecker,
) -> PatternRoot<'a> {
    match slot {
        ForSlot::Owned(ty) => owned_for_root(binding, ty),
        ForSlot::Item(ty) if binding.mutable => alias_for_root(binding, ty, source, iterable, tc),
        ForSlot::Item(ty) => owned_for_root(binding, ty),
    }
}

fn owned_for_root(binding: &ForBinding, ty: Type) -> PatternRoot<'_> {
    PatternRoot {
        pattern: &binding.pattern,
        input: PatternRootInput::Owned(ty),
        mode: PatternBindMode::Owned {
            mutable: binding.mutable,
        },
    }
}

fn alias_for_root<'a>(
    binding: &'a ForBinding,
    ty: Type,
    source: &place::CheckedPlace,
    iterable: &ExprNode,
    tc: &mut TypeChecker,
) -> PatternRoot<'a> {
    let access = place::projected_field_access(source.value.access);
    let access = if access.can_assign() {
        access
    } else {
        tc.push_error(TypeError::ForVarRequiresMutableIterable {
            span: tc.error_span(iterable.span),
        });
        PlaceAccess::Mutable
    };
    let place = pattern::PatternPlace {
        expected_handle: tc.type_handle(&ty),
        expected_ty: ty,
        access,
        facts: source.value.facts.clone(),
        identity: source.value.identity.clone().index(),
        accepts_extern_any: source.accepts_extern_any(),
    };
    PatternRoot {
        pattern: &binding.pattern,
        input: PatternRootInput::Place(Box::new(place), iterable.node.id),
        mode: PatternBindMode::Alias,
    }
}

fn check_for_modifiers(node: &For, iterable_ty: &Type, tc: &mut TypeChecker) {
    let range_kind = tc.decls.core_range_kind(iterable_ty);
    check_for_rev(node, iterable_ty, range_kind, tc);
    check_for_step(node, iterable_ty, range_kind, tc);
}

fn check_for_rev(
    node: &For,
    iterable_ty: &Type,
    range_kind: Option<CoreRangeKind>,
    tc: &mut TypeChecker,
) {
    if !node.reversed {
        return;
    }

    if matches!(iterable_ty, Type::Map { .. }) {
        push_for_modifier_error(
            tc,
            "rev is not supported for map iteration",
            node.iterable.span,
        );
    } else if matches!(
        range_kind,
        Some(CoreRangeKind::From | CoreRangeKind::To | CoreRangeKind::ToInclusive)
    ) {
        push_for_modifier_error(
            tc,
            "reverse is not supported for open-ended ranges",
            node.iterable.span,
        );
    }
}

fn check_for_step(
    node: &For,
    iterable_ty: &Type,
    range_kind: Option<CoreRangeKind>,
    tc: &mut TypeChecker,
) {
    let Some(step) = &node.step else {
        return;
    };

    if matches!(iterable_ty, Type::Map { .. }) {
        push_for_modifier_error(tc, "step is not supported for map iteration", step.span);
        check_expr_checked(step, tc);
        return;
    }

    let step_checked = check_expr_checked(step, tc);
    let step_is_int = matches!(step_checked.ty, Type::Int | Type::Infer);
    let range_is_int = matches!(
        tc.decls.core_range_inner(iterable_ty),
        Some(Type::Int | Type::Infer)
    );
    if range_kind.is_some() && (!range_is_int || !step_is_int) {
        push_for_modifier_error(tc, "step is only supported for integer ranges", step.span);
    }
    let int = tc.type_handle(&Type::Int);
    tc.expect_assignable(step.span, step_checked.handle, int);

    if step_is_int {
        check_positive_step(step, tc);
    }
}

fn check_positive_step(step: &ExprNode, tc: &mut TypeChecker) {
    match known_step_int(step, tc) {
        Some(value) if value <= 0 => {
            push_for_modifier_error(tc, "for-loop step must be positive", step.span);
        }
        Some(_) => {}
        None => record_for_step_runtime_check(step, tc),
    }
}

fn record_for_step_runtime_check(step: &ExprNode, tc: &mut TypeChecker) {
    let span = tc.source_span(step.span);
    tc.for_step_runtime_checks
        .entry(step.node.id)
        .or_insert(span);
}

fn known_step_int(step: &ExprNode, tc: &mut TypeChecker) -> Option<i64> {
    match tc.eval_const_expr(step, false) {
        Ok(ConstValue::Int(value)) => Some(value),
        Ok(_) | Err(_) => None,
    }
}

fn push_for_modifier_error(tc: &mut TypeChecker, message: &'static str, span: Span) {
    tc.push_error(TypeError::ForIterationModifier {
        message,
        span: tc.error_span(span),
    });
}

pub(super) fn check_return(ret_node: &ReturnNode, tc: &mut TypeChecker) {
    let ret = &ret_node.node;
    if tc.in_global_initializer() {
        tc.push_error(TypeError::CompileError {
            message: "return is not allowed in runtime global initializers".to_string(),
            span: tc.error_span(ret_node.span),
        });
        check_discarded_return_value(ret, tc);
        return;
    }

    if tc.in_defer() {
        tc.push_error(TypeError::ReturnInsideDefer {
            span: tc.error_span(ret_node.span),
        });
        check_discarded_return_value(ret, tc);
        return;
    }

    let target = tc.return_mode().map(|mode| match mode {
        ReturnMode::Explicit { ret, source } => ReturnTarget::Explicit {
            ret: ret.clone(),
            source: source.clone(),
        },
        ReturnMode::Infer { access, source, .. } => ReturnTarget::Infer {
            access: *access,
            source: source.clone(),
        },
    });

    match (&ret.value, target) {
        (Some(expr), Some(ReturnTarget::Explicit { ret, source })) => {
            check_return_expr(expr, &ret, source.as_ref(), tc);
        }
        (Some(expr), Some(ReturnTarget::Infer { access, source })) => {
            let ret = ReturnSpec {
                access,
                ty: Type::InferReturn,
            };
            let actual = check_return_expr(expr, &ret, source.as_ref(), tc);
            tc.push_inferred_return(expr.span, actual.handle);
        }
        (Some(expr), None) => {
            let actual = check_value_expr_checked_with_hint(expr, None, tc);
            tc.record_escaping_use(expr);
            tc.reject_extern_any_escape(&actual, expr.span);
        }
        (None, Some(ReturnTarget::Explicit { ret, .. })) if !ret.ty.is_void() => {
            tc.push_error(TypeError::MissingReturn {
                expected: ret.ty,
                span: tc.error_span(ret_node.span),
            });
        }
        (None, Some(ReturnTarget::Infer { .. })) => {
            tc.push_inferred_return(ret_node.span, tc.type_handle(&Type::Void));
        }
        (None, _) => {}
    }
}

fn check_discarded_return_value(ret: &Return, tc: &mut TypeChecker) {
    if let Some(expr) = &ret.value {
        let actual = check_value_expr_checked_with_hint(expr, None, tc);
        tc.record_escaping_use(expr);
        tc.reject_extern_any_escape(&actual, expr.span);
    }
}

pub(super) fn check_return_expr(
    expr: &ExprNode,
    ret: &ReturnSpec,
    source: Option<&PlaceIdentity>,
    tc: &mut TypeChecker,
) -> CheckedType {
    if ret.is_place() {
        if let Some(checked) = check_branch_place_return_expr(expr, ret, source, tc) {
            return checked_from_checked(expr, checked, tc);
        }
        let place = check_place(expr, tc);
        validate_place_return_expr(&place.value, source, expr.span, tc);
        let checked = place.into_checked();
        tc.reject_extern_any_escape(&checked, expr.span);
        if !matches!(ret.ty, Type::InferReturn) {
            let expected = tc.type_handle(&ret.ty);
            tc.expect_assignable_expr(expr.span, expr.node.id, checked.handle.clone(), expected);
        }
        return checked;
    }

    let expected = (!matches!(ret.ty, Type::InferReturn)).then(|| tc.type_handle(&ret.ty));
    let actual = check_value_expr_checked_with_hint(expr, expected.clone(), tc);
    tc.record_escaping_use(expr);
    tc.reject_extern_any_escape(&actual, expr.span);
    if let Some(expected) = expected {
        tc.expect_assignable_expr(expr.span, expr.node.id, actual.handle.clone(), expected);
    }
    actual
}

fn check_branch_place_return_expr(
    expr: &ExprNode,
    ret: &ReturnSpec,
    source: Option<&PlaceIdentity>,
    tc: &mut TypeChecker,
) -> Option<CheckedType> {
    let expected = place_return_expected_handle(ret, tc);
    match &expr.node.kind {
        ExprKind::Block(block) => Some(check_callable_body_place_return(
            CallableBody::Block(block),
            Some(ret),
            source,
            block.span,
            tc,
        )),
        ExprKind::If(if_node) => {
            let cond = check_expr_checked(&if_node.node.cond, tc);
            check_bool_condition(ConditionKind::If, cond, if_node.node.cond.span, tc);
            let then_checked = check_callable_body_place_return(
                CallableBody::Block(&if_node.node.then_block),
                Some(ret),
                source,
                if_node.node.then_block.span,
                tc,
            );
            let Some(else_block) = &if_node.node.else_block else {
                tc.push_error(TypeError::MissingReturn {
                    expected: ret.ty.clone(),
                    span: tc.error_span(expr.span.to_end()),
                });
                return Some(if then_checked.ty.is_void() {
                    diverged_place_return(ret, tc)
                } else {
                    then_checked
                });
            };
            let else_checked = check_callable_body_place_return(
                CallableBody::Block(else_block),
                Some(ret),
                source,
                else_block.span,
                tc,
            );
            Some(join_place_return_branches(
                ret,
                expected,
                place_return_branch(
                    then_checked,
                    if_node.node.then_block.span,
                    block_diverges(&if_node.node.then_block),
                ),
                place_return_branch(else_checked, else_block.span, block_diverges(else_block)),
                tc,
            ))
        }
        ExprKind::Ternary(ternary) => {
            let cond = check_expr_checked(&ternary.node.cond, tc);
            check_bool_condition(ConditionKind::Ternary, cond, ternary.node.cond.span, tc);
            let then_checked = check_return_expr(&ternary.node.then_expr, ret, source, tc);
            let else_checked = check_return_expr(&ternary.node.else_expr, ret, source, tc);
            Some(join_place_return_branches(
                ret,
                expected,
                place_return_branch(
                    then_checked,
                    ternary.node.then_expr.span,
                    expr_diverges(&ternary.node.then_expr),
                ),
                place_return_branch(
                    else_checked,
                    ternary.node.else_expr.span,
                    expr_diverges(&ternary.node.else_expr),
                ),
                tc,
            ))
        }
        ExprKind::Match(match_node) => {
            let node = &match_node.node;
            if matches!(node.mode, crate::ast::MatchMode::Dynamic) {
                return Some(check_dynamic_match_return(
                    match_node, ret, source, expected, tc,
                ));
            }
            let mode = mode_for_head(node.head);
            let scrutinee = check_pattern_scrutinee(&node.scrutinee, mode, tc);
            if node.arms.is_empty() {
                tc.push_error(TypeError::EmptyMatch {
                    span: tc.error_span(match_node.span),
                });
                return Some(checked_void(tc));
            }

            let mut outcomes = Vec::with_capacity(node.arms.len());
            let joined =
                check_match_return_branches(&node.arms, ret, expected.as_ref(), tc, |arm, tc| {
                    tc.push_scope();
                    let outcome = match_check::check_arm_head(
                        &arm.node.head,
                        scrutinee.pattern_place(
                            scrutinee.checked.handle.clone(),
                            scrutinee.checked.ty.clone(),
                        ),
                        mode,
                        node.scrutinee.node.id,
                        tc,
                    );
                    let checked = check_return_expr(&arm.node.body, ret, source, tc);
                    tc.pop_scope();
                    outcomes.push(outcome);
                    checked
                });
            match_coverage::check(&scrutinee.checked.ty, &outcomes, match_node.span, tc);
            Some(finish_match_return_branches(ret, expected, joined, tc))
        }
        _ => None,
    }
}

fn place_return_expected_handle(ret: &ReturnSpec, tc: &TypeChecker) -> Option<TypeHandle> {
    (!matches!(ret.ty, Type::InferReturn)).then(|| tc.type_handle(&ret.ty))
}

fn check_dynamic_match_return(
    match_node: &MatchNode,
    ret: &ReturnSpec,
    source: Option<&PlaceIdentity>,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let node = &match_node.node;
    if node.arms.is_empty() {
        tc.push_error(TypeError::EmptyMatch {
            span: tc.error_span(match_node.span),
        });
        return checked_void(tc);
    }
    let valid_arms = match_check::validate_dynamic_arms(&node.arms, tc);
    let match_source = match_check::check_dynamic_source(node, tc);
    let mut targets = vec![];
    let joined = check_match_return_branches(&node.arms, ret, expected.as_ref(), tc, |arm, tc| {
        if valid_arms {
            match_check::with_dynamic_arm(
                arm,
                &match_source,
                node.scrutinee.node.id,
                &mut targets,
                tc,
                |tc| check_return_expr(&arm.node.body, ret, source, tc),
            )
        } else {
            match_check::with_dynamic_arm_recovery(arm, tc, |tc| {
                check_return_expr(&arm.node.body, ret, source, tc)
            })
        }
    });
    finish_match_return_branches(ret, expected, joined, tc)
}

fn check_match_return_branches(
    arms: &[MatchArmNode],
    ret: &ReturnSpec,
    expected: Option<&TypeHandle>,
    tc: &mut TypeChecker,
    mut check_arm: impl FnMut(&MatchArmNode, &mut TypeChecker) -> CheckedType,
) -> Option<CheckedBranch> {
    let mut joined = None;
    for arm in arms {
        let checked = check_arm(arm, tc);
        let branch =
            place_return_branch(checked, arm.node.body.span, expr_diverges(&arm.node.body));
        joined = Some(match joined {
            Some(previous) => {
                join_place_return_branch(ret, expected.cloned(), previous, branch, tc)
            }
            None => branch,
        });
    }
    joined
}

fn finish_match_return_branches(
    ret: &ReturnSpec,
    expected: Option<TypeHandle>,
    joined: Option<CheckedBranch>,
    tc: &mut TypeChecker,
) -> CheckedType {
    match joined {
        Some(branch) => finish_place_return_branch(ret, expected, branch, tc),
        None => checked_void(tc),
    }
}

fn place_return_branch(checked: CheckedType, span: Span, diverges: bool) -> CheckedBranch {
    CheckedBranch {
        diverges: diverges && checked.ty.is_void(),
        checked,
        span,
    }
}

fn join_place_return_branch(
    ret: &ReturnSpec,
    expected: Option<TypeHandle>,
    left: CheckedBranch,
    right: CheckedBranch,
    tc: &mut TypeChecker,
) -> CheckedBranch {
    let diverges = left.diverges && right.diverges;
    let span = right.span;
    let checked = join_branches_with_hint(expected, left, right, tc);
    let checked = if diverges {
        diverged_place_return(ret, tc)
    } else {
        checked
    };
    CheckedBranch {
        checked,
        span,
        diverges,
    }
}

fn join_place_return_branches(
    ret: &ReturnSpec,
    expected: Option<TypeHandle>,
    left: CheckedBranch,
    right: CheckedBranch,
    tc: &mut TypeChecker,
) -> CheckedType {
    let branch = join_place_return_branch(ret, expected, left, right, tc);
    if branch.diverges {
        diverged_place_return(ret, tc)
    } else {
        branch.checked
    }
}

fn finish_place_return_branch(
    ret: &ReturnSpec,
    expected: Option<TypeHandle>,
    branch: CheckedBranch,
    tc: &mut TypeChecker,
) -> CheckedType {
    if branch.diverges {
        diverged_place_return(ret, tc)
    } else {
        checked_branch_against_expected(branch, expected, tc)
    }
}

fn diverged_place_return(ret: &ReturnSpec, tc: &TypeChecker) -> CheckedType {
    let ty = if matches!(ret.ty, Type::InferReturn) {
        Type::Infer
    } else {
        ret.ty.clone()
    };
    checked_type(ty, tc)
}

fn validate_place_return_expr(
    value: &place::PlaceValue,
    source: Option<&PlaceIdentity>,
    span: Span,
    tc: &mut TypeChecker,
) {
    if !value.access.can_mut_borrow() {
        tc.push_error(TypeError::UnsupportedPlaceReturn {
            message: "mutable place return requires a mutable place",
            span: tc.error_span(span),
        });
    }
    if value.identity.is_indexed_derived() {
        tc.push_error(TypeError::UnsupportedPlaceReturn {
            message: "mutable place return cannot return an indexed place",
            span: tc.error_span(span),
        });
    }
    let Some(source) = source else {
        tc.push_error(TypeError::UnsupportedPlaceReturn {
            message: "mutable place return must derive from the first mutable input",
            span: tc.error_span(span),
        });
        return;
    };
    if !value.identity.derives_from(source) {
        tc.push_error(TypeError::UnsupportedPlaceReturn {
            message: "mutable place return must derive from the first mutable input",
            span: tc.error_span(span),
        });
    }
}

pub(super) fn block_diverges(block: &BlockNode) -> bool {
    block.node.stmts.iter().any(stmt_diverges)
        || block
            .node
            .tail
            .as_ref()
            .is_some_and(|expr| expr_diverges(expr))
}

fn stmt_diverges(stmt: &StmtNode) -> bool {
    match &stmt.node {
        Stmt::Return(_) | Stmt::Break | Stmt::Continue => true,
        Stmt::Expr(expr) => expr_diverges(expr),
        _ => false,
    }
}

pub(super) fn expr_diverges(expr: &ExprNode) -> bool {
    match &expr.node.kind {
        ExprKind::If(if_node) => {
            let Some(else_block) = &if_node.node.else_block else {
                return false;
            };
            block_diverges(&if_node.node.then_block) && block_diverges(else_block)
        }
        ExprKind::Block(block) => block_diverges(block),
        _ => false,
    }
}
