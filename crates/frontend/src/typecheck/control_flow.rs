use super::{
    CallableBody, CheckedType, CoreRangeKind, PatternBindMode, PatternContext, PatternRoot,
    PatternRootInput, PlaceAccess, PlaceIdentity, ReturnMode, ReturnSpec, TypeChecker, TypeError,
    TypeHandle,
    body::check_callable_body_place_return,
    check_block_checked, check_block_checked_with_hint, check_expected_value_expr,
    check_expected_value_expr_deferred, check_expr_checked, check_place,
    check_value_expr_checked_with_hint, checked_from_checked, checked_type, checked_void, closure,
    collection_loan, intrinsic_bool_value, join_checked, match_check, match_coverage,
    pattern::{self, check_pattern_scrutinee},
    place, projection,
};
use crate::{
    ast::{
        BlockNode, ConstValue, DeferBody, DeferNode, ExprId, ExprKind, ExprNode, For, ForBinding,
        ForNode, IfNode, LetElseFallback, LetElseFallbackNode, MatchArmNode, MatchMode, MatchNode,
        Return, ReturnAccess, ReturnNode, Stmt, StmtNode, TernaryNode, Type, WhileNode,
    },
    span::{SourceSpan, Span},
};

#[derive(Clone, Copy)]
pub(super) struct ControlFlowFrame {
    loops: usize,
    defers: usize,
    global_initializers: usize,
    deferred_expected_returns: usize,
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
            deferred_expected_returns: self.deferred_expected_return_depth,
        };
        self.loop_depth = 0;
        self.defer_depth = 0;
        self.global_initializer_depth = 0;
        self.deferred_expected_return_depth = 0;
        frame
    }

    pub(super) fn exit_function_control_flow(&mut self, frame: ControlFlowFrame) {
        self.loop_depth = frame.loops;
        self.defer_depth = frame.defers;
        self.global_initializer_depth = frame.global_initializers;
        self.deferred_expected_return_depth = frame.deferred_expected_returns;
    }
}

#[derive(Clone, Copy)]
enum ConditionKind {
    If,
    Ternary,
    While,
}

fn condition_not_bool(kind: ConditionKind, found: Type, span: Option<SourceSpan>) -> TypeError {
    match kind {
        ConditionKind::If => TypeError::IfConditionNotBool { found, span },
        ConditionKind::Ternary => TypeError::TernaryConditionNotBool { found, span },
        ConditionKind::While => TypeError::WhileConditionNotBool { found, span },
    }
}

fn check_bool_condition(
    kind: ConditionKind,
    expr: &ExprNode,
    cond: CheckedType,
    tc: &mut TypeChecker,
) {
    if cond.ty.is_bool() {
        return;
    }
    if cond.ty == Type::Infer {
        let bool_handle = tc.type_handle(&Type::Bool);
        tc.expect_assignable(expr.span, cond.handle, bool_handle);
        return;
    }
    let target = Type::Bool;
    match projection::expected_projection(
        tc,
        expr.span,
        &cond.ty,
        &target,
        projection::ExpectedProjectionMode::Assignable,
    ) {
        projection::ExpectedProjectionDecision::Project(projection) => {
            let source_ty = cond.ty.clone();
            projection::apply_value_projection(tc, expr, &cond, &source_ty, projection);
        }
        projection::ExpectedProjectionDecision::Failed => {}
        projection::ExpectedProjectionDecision::SourceAccepted
        | projection::ExpectedProjectionDecision::NotNeeded => {
            tc.push_error(condition_not_bool(kind, cond.ty, tc.error_span(expr.span)));
        }
    }
}

struct CheckedBranch {
    checked: CheckedType,
    span: Span,
    diverges: bool,
}

fn checked_branch_against_expected(
    branch: CheckedBranch,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let Some(expected) = expected else {
        return branch.checked;
    };
    CheckedType {
        ty: tc.handle_type(&expected),
        handle: expected,
        contains_extern_any: branch.checked.contains_extern_any,
    }
}

fn join_branches_with_hint(
    expected: Option<TypeHandle>,
    left: CheckedBranch,
    right: CheckedBranch,
    tc: &mut TypeChecker,
) -> CheckedType {
    match (left.diverges, right.diverges) {
        (true, true) => checked_void(tc),
        (true, false) => checked_branch_against_expected(right, expected, tc),
        (false, true) => checked_branch_against_expected(left, expected, tc),
        (false, false) => {
            if let Some(expected) = expected {
                let contains_extern_any =
                    left.checked.contains_extern_any || right.checked.contains_extern_any;
                return CheckedType {
                    ty: tc.handle_type(&expected),
                    handle: expected,
                    contains_extern_any,
                };
            }

            join_checked(left.checked, left.span, right.checked, right.span, tc)
        }
    }
}

enum BranchBody<'a> {
    Value,
    PlaceReturn {
        ret: &'a ReturnSpec,
        source: Option<&'a PlaceIdentity>,
    },
}

struct BranchPolicy<'a> {
    expected: Option<TypeHandle>,
    body: BranchBody<'a>,
}

impl<'a> BranchPolicy<'a> {
    fn value(expected: Option<TypeHandle>) -> Self {
        Self {
            expected,
            body: BranchBody::Value,
        }
    }

    fn place_return(
        ret: &'a ReturnSpec,
        source: Option<&'a PlaceIdentity>,
        tc: &mut TypeChecker,
    ) -> Self {
        Self {
            expected: (!matches!(ret.ty, Type::InferReturn)).then(|| tc.type_handle(&ret.ty)),
            body: BranchBody::PlaceReturn { ret, source },
        }
    }

    fn check_block(&self, block: &BlockNode, tc: &mut TypeChecker) -> CheckedType {
        match self.body {
            BranchBody::Value => check_block_checked_with_hint(block, self.expected.clone(), tc),
            BranchBody::PlaceReturn { ret, source } => {
                check_place_return_block(block, ret, source, tc)
            }
        }
    }

    fn check_expr(&self, expr: &ExprNode, tc: &mut TypeChecker) -> CheckedType {
        match self.body {
            BranchBody::Value => match self.expected.clone() {
                Some(expected) => check_expected_value_expr(expr, expected, tc),
                None => check_value_expr_checked_with_hint(expr, None, tc),
            },
            BranchBody::PlaceReturn { ret, source } => check_return_expr(expr, ret, source, tc),
        }
    }

    fn check_match_arm_body(
        &self,
        expr: &ExprNode,
        expected: Option<TypeHandle>,
        tc: &mut TypeChecker,
    ) -> CheckedType {
        match self.body {
            BranchBody::Value => match expected {
                Some(expected) => check_expected_value_expr_deferred(expr, expected, tc),
                None => check_value_expr_checked_with_hint(expr, None, tc),
            },
            BranchBody::PlaceReturn { ret, source } => check_return_expr(expr, ret, source, tc),
        }
    }

    fn match_expected(&self) -> Option<&TypeHandle> {
        match self.body {
            BranchBody::Value => self.expected.as_ref(),
            BranchBody::PlaceReturn { .. } => None,
        }
    }

    fn use_known_condition(&self) -> bool {
        matches!(self.body, BranchBody::Value)
    }

    fn branch(&self, checked: CheckedType, span: Span, diverges: bool) -> CheckedBranch {
        CheckedBranch {
            diverges: match self.body {
                BranchBody::Value => diverges,
                BranchBody::PlaceReturn { .. } => diverges && checked.ty.is_void(),
            },
            checked,
            span,
        }
    }

    fn join(
        &self,
        left: CheckedBranch,
        right: CheckedBranch,
        tc: &mut TypeChecker,
    ) -> CheckedBranch {
        let diverges = left.diverges && right.diverges;
        let span = right.span;
        let checked = join_branches_with_hint(self.expected.clone(), left, right, tc);
        CheckedBranch {
            checked,
            span,
            diverges,
        }
    }

    fn finish(&self, branch: CheckedBranch, tc: &mut TypeChecker) -> CheckedType {
        match self.body {
            BranchBody::Value => branch.checked,
            BranchBody::PlaceReturn { ret, .. } if branch.diverges => {
                diverged_place_return(ret, tc)
            }
            BranchBody::PlaceReturn { .. } => {
                checked_branch_against_expected(branch, self.expected.clone(), tc)
            }
        }
    }

    fn finish_match(&self, arms: Vec<CheckedBranch>, tc: &mut TypeChecker) -> CheckedType {
        match self.body {
            BranchBody::Value => {
                if let Some(expected) = self.expected.clone() {
                    let contains_extern_any =
                        arms.iter().any(|arm| arm.checked.contains_extern_any);
                    return CheckedType {
                        ty: tc.handle_type(&expected),
                        handle: expected,
                        contains_extern_any,
                    };
                }
                finish_match_arms(arms, tc)
            }
            BranchBody::PlaceReturn { .. } => match arms
                .into_iter()
                .reduce(|left, right| self.join(left, right, tc))
            {
                Some(branch) => self.finish(branch, tc),
                None => checked_void(tc),
            },
        }
    }
}

fn copy_if_branch_flow(if_node: &IfNode, parent: ExprId, tc: &mut TypeChecker) {
    if let Some(tail) = &if_node.node.then_block.node.tail {
        tc.closure.copy_expr_flow(tail.node.id, parent);
    }
    if let Some(else_block) = &if_node.node.else_block
        && let Some(tail) = &else_block.node.tail
    {
        tc.closure.copy_expr_flow(tail.node.id, parent);
    }
}

fn copy_ternary_branch_flow(ternary_node: &TernaryNode, parent: ExprId, tc: &mut TypeChecker) {
    tc.closure
        .copy_expr_flow(ternary_node.node.then_expr.node.id, parent);
    tc.closure
        .copy_expr_flow(ternary_node.node.else_expr.node.id, parent);
}

fn copy_match_branch_flow(match_node: &MatchNode, parent: ExprId, tc: &mut TypeChecker) {
    for arm in &match_node.node.arms {
        tc.closure.copy_expr_flow(arm.node.body.node.id, parent);
    }
}

pub(super) fn check_if_checked_with_hint(
    if_node: &IfNode,
    expected: Option<TypeHandle>,
    parent: ExprId,
    tc: &mut TypeChecker,
) -> CheckedType {
    check_if_with_policy(
        if_node,
        if_node.span,
        Some(parent),
        &BranchPolicy::value(expected),
        tc,
    )
}

pub(super) fn check_ternary_checked_with_hint(
    ternary_node: &TernaryNode,
    expected: Option<TypeHandle>,
    parent: ExprId,
    tc: &mut TypeChecker,
) -> CheckedType {
    check_ternary_with_policy(
        ternary_node,
        Some(parent),
        &BranchPolicy::value(expected),
        tc,
    )
}

fn check_if_with_policy(
    if_node: &IfNode,
    span: Span,
    parent: Option<ExprId>,
    policy: &BranchPolicy<'_>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let node = &if_node.node;
    let cond = check_expr_checked(&node.cond, tc);
    check_bool_condition(ConditionKind::If, &node.cond, cond, tc);
    let known_cond = policy
        .use_known_condition()
        .then(|| intrinsic_bool_value(&node.cond, tc))
        .flatten();

    let Some(else_block) = &node.else_block else {
        let then = if known_cond == Some(false) {
            None
        } else {
            let checked = closure::check_closure_flow_branch(tc, |tc| {
                policy.check_block(&node.then_block, tc)
            });
            if let Some(parent) = parent {
                copy_if_branch_flow(if_node, parent, tc);
            }
            Some(policy.branch(
                checked,
                node.then_block.span,
                block_diverges(&node.then_block),
            ))
        };
        return match &policy.body {
            BranchBody::Value => checked_void(tc),
            BranchBody::PlaceReturn { ret, .. } => {
                tc.push_error(TypeError::MissingReturn {
                    expected: ret.ty.clone(),
                    span: tc.error_span(span.to_end()),
                });
                match then {
                    Some(branch) if !branch.checked.ty.is_void() => branch.checked,
                    _ => diverged_place_return(ret, tc),
                }
            }
        };
    };

    if known_cond == Some(true) {
        let checked = policy.check_block(&node.then_block, tc);
        if let Some(parent) = parent {
            copy_if_branch_flow(if_node, parent, tc);
        }
        return policy.finish(
            policy.branch(
                checked,
                node.then_block.span,
                block_diverges(&node.then_block),
            ),
            tc,
        );
    }
    if known_cond == Some(false) {
        let checked = policy.check_block(else_block, tc);
        if let Some(parent) = parent {
            copy_if_branch_flow(if_node, parent, tc);
        }
        return policy.finish(
            policy.branch(checked, else_block.span, block_diverges(else_block)),
            tc,
        );
    }

    let (then, else_checked) = closure::check_closure_flow_branches(
        tc,
        |tc| policy.check_block(&node.then_block, tc),
        |tc| policy.check_block(else_block, tc),
    );
    if let Some(parent) = parent {
        copy_if_branch_flow(if_node, parent, tc);
    }
    let then = policy.branch(then, node.then_block.span, block_diverges(&node.then_block));
    let else_checked = policy.branch(else_checked, else_block.span, block_diverges(else_block));
    policy.finish(policy.join(then, else_checked, tc), tc)
}

fn check_ternary_with_policy(
    ternary_node: &TernaryNode,
    parent: Option<ExprId>,
    policy: &BranchPolicy<'_>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let node = &ternary_node.node;
    let cond = check_expr_checked(&node.cond, tc);
    check_bool_condition(ConditionKind::Ternary, &node.cond, cond, tc);
    let known_cond = policy
        .use_known_condition()
        .then(|| intrinsic_bool_value(&node.cond, tc))
        .flatten();

    if known_cond == Some(true) {
        let checked = policy.check_expr(&node.then_expr, tc);
        if let Some(parent) = parent {
            tc.closure.copy_expr_flow(node.then_expr.node.id, parent);
        }
        return policy.finish(
            policy.branch(checked, node.then_expr.span, expr_diverges(&node.then_expr)),
            tc,
        );
    }
    if known_cond == Some(false) {
        let checked = policy.check_expr(&node.else_expr, tc);
        if let Some(parent) = parent {
            tc.closure.copy_expr_flow(node.else_expr.node.id, parent);
        }
        return policy.finish(
            policy.branch(checked, node.else_expr.span, expr_diverges(&node.else_expr)),
            tc,
        );
    }

    let (then, else_checked) = closure::check_closure_flow_branches(
        tc,
        |tc| policy.check_expr(&node.then_expr, tc),
        |tc| policy.check_expr(&node.else_expr, tc),
    );
    if let Some(parent) = parent {
        copy_ternary_branch_flow(ternary_node, parent, tc);
    }
    let then = policy.branch(then, node.then_expr.span, expr_diverges(&node.then_expr));
    let else_checked = policy.branch(
        else_checked,
        node.else_expr.span,
        expr_diverges(&node.else_expr),
    );
    policy.finish(policy.join(then, else_checked, tc), tc)
}

pub(super) fn check_match_checked_with_hint(
    match_node: &MatchNode,
    expected: Option<TypeHandle>,
    parent: ExprId,
    tc: &mut TypeChecker,
) -> CheckedType {
    check_match_with_policy(match_node, Some(parent), &BranchPolicy::value(expected), tc)
}

fn check_match_with_policy(
    match_node: &MatchNode,
    parent: Option<ExprId>,
    policy: &BranchPolicy<'_>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let node = &match_node.node;
    if node.arms.is_empty() {
        tc.push_error(TypeError::EmptyMatch {
            span: tc.error_span(match_node.span),
        });
        return checked_void(tc);
    }

    let arms = if matches!(node.mode, MatchMode::Dynamic) {
        let valid_arms = match_check::validate_dynamic_arms(&node.arms, tc);
        let source = match_check::check_dynamic_source(node, tc);
        let mut targets = vec![];
        check_match_arm_bodies(
            &node.arms,
            policy.match_expected(),
            tc,
            |arm, expected, tc| {
                if valid_arms {
                    match_check::with_dynamic_arm(
                        arm,
                        &source,
                        node.scrutinee.node.id,
                        &mut targets,
                        tc,
                        |tc| policy.check_match_arm_body(&arm.node.body, expected, tc),
                    )
                } else {
                    match_check::with_dynamic_arm_recovery(arm, tc, |tc| {
                        policy.check_match_arm_body(&arm.node.body, expected, tc)
                    })
                }
            },
        )
    } else {
        let mode = if node.access.is_ref() {
            PatternBindMode::Alias
        } else {
            PatternBindMode::Owned { mutable: false }
        };
        let scrutinee = check_pattern_scrutinee(&node.scrutinee, mode, tc);
        let mut outcomes = Vec::with_capacity(node.arms.len());
        let arms = check_match_arm_bodies(
            &node.arms,
            policy.match_expected(),
            tc,
            |arm, expected, tc| {
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
                let body = policy.check_match_arm_body(&arm.node.body, expected, tc);
                tc.pop_scope();
                outcomes.push(outcome);
                body
            },
        );
        match_coverage::check(&scrutinee.checked.ty, &outcomes, match_node.span, tc);
        arms
    };

    if let Some(parent) = parent {
        copy_match_branch_flow(match_node, parent, tc);
    }
    policy.finish_match(arms, tc)
}

fn check_match_arm_bodies(
    arms: &[MatchArmNode],
    expected: Option<&TypeHandle>,
    tc: &mut TypeChecker,
    mut check_arm: impl FnMut(&MatchArmNode, Option<TypeHandle>, &mut TypeChecker) -> CheckedType,
) -> Vec<CheckedBranch> {
    let flow = tc.closure.closure_flow_snapshot();
    let mut arm_flows = Vec::with_capacity(arms.len());
    let mut checked = Vec::with_capacity(arms.len());
    for arm in arms {
        tc.closure.restore_closure_flow(&flow);
        let error_count = tc.errors.len();
        let body = check_arm(arm, expected.cloned(), tc);
        if tc.errors.len() == error_count {
            check_match_arm_expected(
                &body,
                arm.node.body.node.id,
                expected,
                arm.node.body.span,
                tc,
            );
        }
        arm_flows.push(tc.closure.closure_flow_snapshot());
        checked.push(CheckedBranch {
            checked: body,
            span: arm.node.body.span,
            diverges: expr_diverges(&arm.node.body),
        });
    }
    tc.closure.restore_closure_flow(&flow);
    for flow in arm_flows {
        let current = tc.closure.closure_flow_snapshot();
        tc.closure.join_closure_flow_snapshots(&current, &flow);
    }
    checked
}

fn check_match_arm_expected(
    body: &CheckedType,
    expr_id: ExprId,
    expected: Option<&TypeHandle>,
    span: Span,
    tc: &mut TypeChecker,
) {
    let Some(expected) = expected else {
        return;
    };
    let expected_ty = tc.handle_type(expected);
    if body.ty.is_void() || matches!(body.ty, Type::Infer) {
        return;
    }
    if projection::satisfies_without_effects(
        tc,
        span,
        &body.ty,
        &expected_ty,
        projection::ExpectedProjectionMode::Assignable,
    ) {
        tc.expect_assignable_expr(span, expr_id, body.handle.clone(), expected.clone());
        return;
    }
    tc.push_error(TypeError::MatchArmTypeMismatch {
        expected: expected_ty.clone(),
        found: body.ty.clone(),
        span: tc.error_span(span),
    });
}

fn finish_match_arms(arms: Vec<CheckedBranch>, tc: &mut TypeChecker) -> CheckedType {
    if arms[0].checked.ty.is_void() {
        return checked_void(tc);
    }
    let result = tc.fresh_temp_handle(arms[0].span);
    let contains_extern_any = arms
        .iter()
        .any(|arm| !arm.checked.ty.is_void() && arm.checked.contains_extern_any);
    for arm in arms {
        if !arm.checked.ty.is_void() {
            tc.expect_assignable(arm.span, arm.checked.handle, result.clone());
        }
    }
    tc.solve_constraints();
    CheckedType {
        ty: tc.handle_type(&result),
        handle: result,
        contains_extern_any,
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

pub(super) fn check_let_else_fallback(fallback: &LetElseFallbackNode, tc: &mut TypeChecker) {
    match &fallback.node {
        LetElseFallback::Block(block) => {
            tc.push_scope();
            check_block_checked(block, tc);
            tc.pop_scope();
            if !block_diverges(block) {
                tc.push_error(TypeError::LetElseMustDiverge {
                    span: tc.error_span(fallback.span),
                });
            }
        }
        LetElseFallback::Return(ret) => check_return(ret, tc),
        LetElseFallback::Break => check_break(fallback.span, tc),
        LetElseFallback::Continue => check_continue(fallback.span, tc),
    }
}

pub(super) fn check_while(while_node: &WhileNode, tc: &mut TypeChecker) {
    let cond = check_expr_checked(&while_node.node.cond, tc);
    check_bool_condition(ConditionKind::While, &while_node.node.cond, cond, tc);
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
    let saved_loan_len = tc.active_collection_loans.len();
    if let Some(loan) =
        collection_loan::classify_for_loan(&iterable_ty, source.value.identity.clone())
    {
        tc.active_collection_loans.push(loan);
    }
    check_loop_body(&node.body, tc);
    tc.active_collection_loans.truncate(saved_loan_len);
    debug_assert_eq!(tc.active_collection_loans.len(), saved_loan_len);
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
            if binding.access.is_ref() {
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
        ([binding], _, Some(inner)) => {
            reject_for_ref_binding(binding, tc);
            vec![ForSlot::Owned(inner)]
        }
        ([_], _, None) => unsupported_for_slots(1, iterable_ty, iterable_span, tc),

        ([index, _], Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem }, _) => {
            reject_for_ref_binding(index, tc);
            vec![ForSlot::Owned(Type::Int), ForSlot::Item((**elem).clone())]
        }
        ([first, _], Type::Map { key, value }, _) => {
            if first.access.is_ref() {
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
        ([index, binding], _, Some(inner)) => {
            for binding in [index, binding] {
                reject_for_ref_binding(binding, tc);
            }
            vec![ForSlot::Owned(Type::Int), ForSlot::Owned(inner)]
        }
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

fn reject_for_ref_binding(binding: &ForBinding, tc: &mut TypeChecker) {
    if binding.access.is_ref() {
        tc.push_error(TypeError::ForRefRequiresMutableIterable {
            span: tc.error_span(binding.pattern.span),
        });
    }
}

fn for_slot_root<'a>(
    binding: &'a ForBinding,
    slot: ForSlot,
    source: &place::CheckedPlace,
    iterable: &ExprNode,
    tc: &mut TypeChecker,
) -> PatternRoot<'a> {
    match slot {
        ForSlot::Item(ty) if binding.access.is_ref() => {
            alias_for_root(binding, ty, source, iterable, tc)
        }
        ForSlot::Owned(ty) | ForSlot::Item(ty) => owned_for_root(binding, ty),
    }
}

fn owned_for_root(binding: &ForBinding, ty: Type) -> PatternRoot<'_> {
    PatternRoot {
        pattern: &binding.pattern,
        input: PatternRootInput::Owned(ty),
        mode: PatternBindMode::Owned { mutable: false },
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
        tc.push_error(TypeError::ForRefRequiresMutableIterable {
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
        map_entry_alias: false,
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
    let site = tc.current_expr_site(step.node.id);
    tc.semantic_facts.record_for_step_runtime_check(site, span);
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
    let actual = match expected {
        Some(expected) => check_expected_value_expr(expr, expected, tc),
        None => check_value_expr_checked_with_hint(expr, None, tc),
    };
    tc.record_escaping_use(expr);
    tc.reject_extern_any_escape(&actual, expr.span);
    actual
}

fn check_branch_place_return_expr(
    expr: &ExprNode,
    ret: &ReturnSpec,
    source: Option<&PlaceIdentity>,
    tc: &mut TypeChecker,
) -> Option<CheckedType> {
    match &expr.node.kind {
        ExprKind::Block(block) => Some(check_place_return_block(block, ret, source, tc)),
        ExprKind::If(if_node) => {
            let policy = BranchPolicy::place_return(ret, source, tc);
            Some(check_if_with_policy(if_node, expr.span, None, &policy, tc))
        }
        ExprKind::Ternary(ternary) => {
            let policy = BranchPolicy::place_return(ret, source, tc);
            Some(check_ternary_with_policy(ternary, None, &policy, tc))
        }
        ExprKind::Match(match_node) => {
            let policy = BranchPolicy::place_return(ret, source, tc);
            Some(check_match_with_policy(match_node, None, &policy, tc))
        }
        _ => None,
    }
}

fn check_place_return_block(
    block: &BlockNode,
    ret: &ReturnSpec,
    source: Option<&PlaceIdentity>,
    tc: &mut TypeChecker,
) -> CheckedType {
    check_callable_body_place_return(
        CallableBody::Block(block),
        Some(ret),
        source,
        block.span,
        tc,
    )
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
