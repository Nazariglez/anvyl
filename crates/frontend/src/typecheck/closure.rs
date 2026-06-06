use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::{Hash, Hasher},
};

use super::{
    BindingId, BindingMutability, BindingPromotionFact, BodyInstanceKey, CaptureAccess,
    CaptureStorage, CaptureStorageOrigin, CheckedType, FunctionValueKind, LambdaBodyKey,
    LambdaCaptureFact, LambdaEscapeFact, LambdaEscapeKind, LambdaEscapeMap, LocalBindingKind,
    LocalValue, ReturnAccess, ReturnSpec, TypeChecker, TypeError, TypecheckFacts,
    body::{
        CallableBody, CallableParamBinding, check_callable_body_frame, with_callable_body_scope,
    },
    checked_from_type,
    decl_validate::{has_mutable_func_param, validate_return_spec},
    expected_assignable_type,
    infer::{SemanticLocalId, TypeHandle},
};
use crate::{
    ast::{EscapeMode, ExprId, ExprNode, FuncParam, Ident, LambdaNode, Type},
    span::Span,
};

#[derive(Default)]
pub(super) struct ClosureClassifier {
    lambda_escapes: LambdaEscapeMap,
    sealed_lambdas: Vec<SealedLambda>,
    replayed_facts: TypecheckFacts,
    escape_queue: VecDeque<usize>,
    escape_events: Vec<EscapeEvent>,
    expr_flows: HashMap<ExprId, EscapeFlow>,
    expr_sources: HashMap<ExprId, HashSet<BindingId>>,
    place_captures: HashMap<ExprId, BindingId>,
    bindings: HashMap<BindingId, BindingInfo>,
    local_flows: HashMap<BindingId, EscapeFlow>,
    active_loops: Vec<LoopFlow>,
    reported_non_escaping_callback_escapes: HashSet<SemanticLocalId>,
    reported_borrowed_escaping_captures: HashSet<BindingId>,
    active_lambdas: Vec<ActiveLambda>,
}

impl TypeChecker {
    pub(super) fn record_escaping_use(&mut self, expr: &ExprNode) {
        self.closure.record_escaping_use(expr.node.id, expr.span);
    }

    pub(super) fn push_escape_events(&mut self, events: Vec<EscapeEvent>) {
        for event in events {
            match event {
                EscapeEvent::Callback { origin, span } => {
                    self.push_non_escaping_callback_escape(&origin, span);
                }
                EscapeEvent::Borrowed { capture, span } => {
                    self.push_borrowed_escaping_capture(&capture, span);
                }
            }
        }
    }

    pub(super) fn check_argument_escape(&mut self, arg: &ExprNode, escape: EscapeMode) {
        if escape.is_escaping() {
            self.record_escaping_use(arg);
        }
    }

    fn mark_non_escaping_callback_binding(&mut self, name: Ident, origin: NonEscapingCallback) {
        let Some(binding_id) = self.local_binding_id(name) else {
            return;
        };
        self.closure.add_binding_callback(binding_id, origin);
    }

    pub(super) fn record_aggregate_elem_flow(&mut self, aggregate: ExprId, elem: &ExprNode) {
        self.record_escaping_use(elem);
        self.closure.copy_expr_flow(elem.node.id, aggregate);
    }

    pub(super) fn mark_non_escaping_callback_param(
        &mut self,
        name: Ident,
        type_id: SemanticLocalId,
        param: &FuncParam,
        source_ty: Option<&Type>,
    ) {
        if param.escape.is_escaping() || !matches!(param.ty, Type::Func { .. }) {
            return;
        }
        let ty = source_ty.unwrap_or(&param.ty);
        let help = Some(format!("mark the parameter as `{name}: escaping {ty}`"));
        self.mark_non_escaping_callback_binding(
            name,
            NonEscapingCallback {
                id: type_id,
                name,
                help,
            },
        );
    }

    fn push_non_escaping_callback_escape(&mut self, origin: &NonEscapingCallback, span: Span) {
        if !self.closure.record_non_escaping_callback_escape(origin.id) {
            return;
        }
        let help = origin.help.clone().or_else(|| {
            let ty = self.solver.local_type_to_type(origin.id);
            matches!(ty, Type::Func { .. })
                .then(|| format!("mark the parameter as `{}: escaping {ty}`", origin.name))
        });
        self.push_error(TypeError::NonEscapingCallbackEscapes {
            name: origin.name,
            help,
            span: self.error_span(span),
        });
    }

    pub(super) fn record_local_read(&mut self, expr: ExprId, value: &LocalValue) {
        self.closure
            .record_local_read(expr, value.info.binding_id, value.source_depth);
    }

    fn push_borrowed_escaping_capture(&mut self, capture: &BorrowedCapture, span: Span) {
        if !self.closure.record_borrowed_escaping_capture(capture.id) {
            return;
        }
        self.push_error(TypeError::BorrowedCaptureEscapes {
            name: capture.name,
            origin: capture.origin,
            span: self.error_span(span),
        });
    }
}

pub(super) fn check_closure_flow_branch<R>(
    tc: &mut TypeChecker,
    check: impl FnOnce(&mut TypeChecker) -> R,
) -> R {
    let flow = tc.closure.closure_flow_snapshot();
    let ret = check(tc);
    let branch_flow = tc.closure.closure_flow_snapshot();
    tc.closure.join_closure_flow_snapshots(&flow, &branch_flow);
    ret
}

pub(super) fn check_closure_flow_branches<R>(
    tc: &mut TypeChecker,
    left: impl FnOnce(&mut TypeChecker) -> R,
    right: impl FnOnce(&mut TypeChecker) -> R,
) -> (R, R) {
    let flow = tc.closure.closure_flow_snapshot();
    let left_ret = left(tc);
    let left_flow = tc.closure.closure_flow_snapshot();
    tc.closure.restore_closure_flow(&flow);
    let right_ret = right(tc);
    let right_flow = tc.closure.closure_flow_snapshot();
    tc.closure
        .join_closure_flow_snapshots(&left_flow, &right_flow);
    (left_ret, right_ret)
}

impl ClosureClassifier {
    pub(super) fn define_binding(
        &mut self,
        binding_id: BindingId,
        name: Ident,
        type_id: SemanticLocalId,
        kind: LocalBindingKind,
        scope_depth: usize,
    ) {
        self.bindings.insert(
            binding_id,
            BindingInfo {
                name,
                type_id,
                mutability: kind.mutability,
                storage: kind.storage,
                scope_depth,
            },
        );
        self.local_flows.entry(binding_id).or_default();
    }

    pub(super) fn scope_state_for_bindings(
        &self,
        bindings: impl IntoIterator<Item = (BindingId, Ident, SemanticLocalId, LocalBindingKind, usize)>,
    ) -> ClosureScopeState {
        let mut state = ClosureScopeState::default();
        for (binding_id, name, type_id, kind, scope_depth) in bindings {
            state.bindings.insert(
                binding_id,
                BindingInfo {
                    name,
                    type_id,
                    mutability: kind.mutability,
                    storage: kind.storage,
                    scope_depth,
                },
            );
            state.local_flows.insert(
                binding_id,
                self.local_flows
                    .get(&binding_id)
                    .cloned()
                    .unwrap_or_default(),
            );
        }
        state
    }

    pub(super) fn restore_scope_state(&mut self, state: ClosureScopeState) {
        self.bindings = state.bindings;
        self.local_flows = state.local_flows;
    }

    pub(super) fn replace_scope_state(&mut self, state: ClosureScopeState) -> ClosureScopeState {
        ClosureScopeState {
            bindings: std::mem::replace(&mut self.bindings, state.bindings),
            local_flows: std::mem::replace(&mut self.local_flows, state.local_flows),
        }
    }

    pub(super) fn exit_scope(&mut self, live_depth: usize) {
        self.bindings
            .retain(|_, binding| binding.scope_depth < live_depth);
        self.local_flows
            .retain(|id, _| self.bindings.contains_key(id));
    }

    fn binding_flow(&self, binding_id: BindingId) -> EscapeFlow {
        self.local_flows
            .get(&binding_id)
            .cloned()
            .unwrap_or_default()
    }

    fn borrowed_capture(&self, binding_id: BindingId) -> Option<BorrowedCapture> {
        let binding = self.bindings.get(&binding_id)?;
        binding
            .storage
            .is_borrowed_runtime()
            .then_some(BorrowedCapture {
                id: binding_id,
                name: binding.name,
                origin: binding.storage,
            })
    }

    pub(super) fn bind_local(
        &mut self,
        binding_id: Option<BindingId>,
        value: ExprId,
        function_value: bool,
        span: Span,
    ) {
        if let Some(binding_id) = binding_id
            && function_value
            && self.assign_local(binding_id, value, function_value, span)
        {
            return;
        }
        self.record_expr_escaping_flow(value, span);
    }

    pub(super) fn assign_local_or_use(
        &mut self,
        binding_id: BindingId,
        value: ExprId,
        function_value: bool,
        span: Span,
    ) {
        if !self.assign_local(binding_id, value, function_value, span) {
            self.record_expr_escaping_flow(value, span);
        }
    }

    fn assign_local(
        &mut self,
        binding_id: BindingId,
        value: ExprId,
        function_value: bool,
        span: Span,
    ) -> bool {
        let flow = self.storable_expr_flow(value, function_value);
        if function_value {
            self.record_assigned_capture_flow(binding_id, &flow, span);
            self.record_loop_assignment(binding_id, value, flow.clone(), span);
        }
        let has_flow = !flow.is_empty();
        self.local_flows.insert(binding_id, flow);
        has_flow
    }

    fn record_expr_escaping_flow(&mut self, expr: ExprId, span: Span) {
        let Some(flow) = self.expr_flows.get(&expr).cloned() else {
            return;
        };
        self.record_escaping_flow(&flow, span);
    }

    fn storable_expr_flow(&self, expr: ExprId, function_value: bool) -> EscapeFlow {
        let mut flow = EscapeFlow::default();
        let Some(expr_flow) = self.expr_flows.get(&expr) else {
            return flow;
        };
        for origin in expr_flow.callbacks() {
            flow.insert_callback(origin.clone());
        }
        if function_value {
            for capture in expr_flow.borrowed() {
                flow.insert_borrowed(capture.clone());
            }
            for lambda in expr_flow.lambdas() {
                flow.insert_lambda(lambda);
            }
        }
        flow
    }

    pub(super) fn closure_flow_snapshot(&self) -> ClosureFlowSnapshot {
        ClosureFlowSnapshot {
            local_flows: self.local_flows.clone(),
            active_lambdas: self.active_lambdas.clone(),
            loop_steps: self
                .active_loops
                .iter()
                .map(|loop_flow| loop_flow.steps.clone())
                .collect(),
        }
    }

    pub(super) fn enter_loop_flow(&mut self) {
        self.active_loops.push(LoopFlow {
            initial: self.closure_flow_snapshot(),
            steps: vec![],
        });
    }

    pub(super) fn exit_loop_flow(&mut self) {
        let body = self.closure_flow_snapshot();
        let loop_flow = self
            .active_loops
            .pop()
            .expect("active loop flow must exist on exit");
        self.join_closure_flow_snapshots(&loop_flow.initial, &body);
        self.close_loop_flow(&loop_flow.steps);
    }

    pub(super) fn restore_closure_flow(&mut self, snapshot: &ClosureFlowSnapshot) {
        self.local_flows.clone_from(&snapshot.local_flows);
        self.local_flows
            .retain(|binding_id, _| self.bindings.contains_key(binding_id));
        self.active_lambdas.clone_from(&snapshot.active_lambdas);
        self.restore_loop_steps(snapshot);
    }

    pub(super) fn join_closure_flow_snapshots(
        &mut self,
        left: &ClosureFlowSnapshot,
        right: &ClosureFlowSnapshot,
    ) {
        let binding_ids = self.bindings.keys().copied().collect::<Vec<_>>();
        for binding_id in binding_ids {
            let mut flow = left
                .local_flows
                .get(&binding_id)
                .cloned()
                .unwrap_or_default();
            if let Some(right_flow) = right.local_flows.get(&binding_id) {
                flow.union(right_flow);
            }
            self.local_flows.insert(binding_id, flow);
        }
        self.active_lambdas = merge_active_lambdas(&left.active_lambdas, &right.active_lambdas);
        self.join_loop_steps(left, right);
    }

    fn restore_loop_steps(&mut self, snapshot: &ClosureFlowSnapshot) {
        debug_assert_eq!(self.active_loops.len(), snapshot.loop_steps.len());
        for (loop_flow, steps) in self.active_loops.iter_mut().zip(&snapshot.loop_steps) {
            loop_flow.steps.clone_from(steps);
        }
    }

    fn join_loop_steps(&mut self, left: &ClosureFlowSnapshot, right: &ClosureFlowSnapshot) {
        debug_assert!(left.loop_steps.len() >= self.active_loops.len());
        debug_assert!(right.loop_steps.len() >= self.active_loops.len());
        for (index, loop_flow) in self.active_loops.iter_mut().enumerate() {
            loop_flow.steps =
                join_loop_step_lists(&left.loop_steps[index], &right.loop_steps[index]);
        }
    }

    fn record_loop_assignment(
        &mut self,
        target: BindingId,
        value: ExprId,
        flow: EscapeFlow,
        span: Span,
    ) {
        if self.active_loops.is_empty() {
            return;
        }
        let assignment = LoopAssignment {
            target,
            flow,
            sources: self.expr_sources.get(&value).cloned().unwrap_or_default(),
            span,
        };
        for loop_flow in &mut self.active_loops {
            loop_flow.steps.push(LoopStep::Assign(assignment.clone()));
        }
    }

    fn close_loop_flow(&mut self, steps: &[LoopStep]) {
        if steps.is_empty() {
            return;
        }
        let mut assigned_spans = HashMap::new();
        collect_loop_assignment_spans(steps, &mut assigned_spans);
        let live = self.bindings.keys().copied().collect::<Vec<_>>();
        loop {
            let state = self.apply_loop_steps(&self.local_flows, steps);
            let mut changed = false;
            for binding_id in &live {
                let mut flow = self
                    .local_flows
                    .get(binding_id)
                    .cloned()
                    .unwrap_or_default();
                if let Some(step_flow) = state.get(binding_id) {
                    flow.union(step_flow);
                }
                if self.local_flows.get(binding_id) != Some(&flow) {
                    self.local_flows.insert(*binding_id, flow);
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }

        let assigned_flows = assigned_spans
            .into_iter()
            .filter_map(|(binding_id, span)| {
                self.local_flows
                    .get(&binding_id)
                    .cloned()
                    .map(|flow| (binding_id, flow, span))
            })
            .collect::<Vec<_>>();
        for (binding_id, flow, span) in assigned_flows {
            self.record_assigned_capture_flow(binding_id, &flow, span);
        }
    }

    fn apply_loop_steps(
        &self,
        initial: &HashMap<BindingId, EscapeFlow>,
        steps: &[LoopStep],
    ) -> HashMap<BindingId, EscapeFlow> {
        let mut state = initial.clone();
        for step in steps {
            match step {
                LoopStep::Assign(assignment) => self.apply_loop_assignment(&mut state, assignment),
                LoopStep::Choice(arms) => {
                    let mut joined = HashMap::new();
                    for arm in arms {
                        union_loop_state(&mut joined, self.apply_loop_steps(&state, arm));
                    }
                    state = joined;
                }
            }
        }
        state
    }

    fn apply_loop_assignment(
        &self,
        state: &mut HashMap<BindingId, EscapeFlow>,
        assignment: &LoopAssignment,
    ) {
        if !self.bindings.contains_key(&assignment.target) {
            return;
        }
        let mut flow = assignment.flow.clone();
        for source in &assignment.sources {
            if let Some(source_flow) = state.get(source) {
                flow.union(source_flow);
            }
        }
        state.insert(assignment.target, flow);
    }

    pub(super) fn add_binding_callback(
        &mut self,
        binding_id: BindingId,
        origin: NonEscapingCallback,
    ) {
        self.local_flows
            .entry(binding_id)
            .or_default()
            .insert_callback(origin);
    }

    pub(super) fn record_non_escaping_callback_escape(&mut self, id: SemanticLocalId) -> bool {
        self.reported_non_escaping_callback_escapes.insert(id)
    }

    pub(super) fn record_borrowed_escaping_capture(&mut self, id: BindingId) -> bool {
        self.reported_borrowed_escaping_captures.insert(id)
    }

    fn record_non_escaping_callback_expr(&mut self, expr: ExprId, origin: NonEscapingCallback) {
        self.expr_flows
            .entry(expr)
            .or_default()
            .insert_callback(origin);
    }

    fn record_borrowed_function_expr(&mut self, expr: ExprId, capture: BorrowedCapture) {
        self.expr_flows
            .entry(expr)
            .or_default()
            .insert_borrowed(capture);
    }

    pub(super) fn record_local_read(&mut self, expr: ExprId, binding_id: BindingId, depth: usize) {
        self.expr_sources
            .entry(expr)
            .or_default()
            .insert(binding_id);
        let crosses_lambda = self.crosses_capture_boundary(depth);
        if crosses_lambda {
            self.capture_local_read(expr, binding_id, depth);
        }

        let flow = self.binding_flow(binding_id);
        if crosses_lambda {
            self.record_captured_flow(&flow, depth);
        }
        for origin in flow.callbacks() {
            self.record_non_escaping_callback_expr(expr, origin.clone());
        }
        if crosses_lambda && let Some(capture) = self.borrowed_capture(binding_id) {
            let mut flow = EscapeFlow::default();
            flow.insert_borrowed(capture);
            self.record_captured_flow(&flow, depth);
        }
        for capture in flow.borrowed() {
            self.record_borrowed_function_expr(expr, capture.clone());
        }
        for lambda in flow.lambdas() {
            self.record_lambda_origin(expr, lambda);
        }
    }

    pub(super) fn lambda_value(&mut self, expr: ExprId) {
        self.expr_flows.entry(expr).or_default().insert_lambda(expr);
    }

    fn record_lambda_origin(&mut self, expr: ExprId, origin: ExprId) {
        self.expr_flows
            .entry(expr)
            .or_default()
            .insert_lambda(origin);
    }

    pub(super) fn copy_expr_flow(&mut self, from: ExprId, to: ExprId) {
        if let Some(flow) = self.expr_flows.get(&from).cloned() {
            self.expr_flows.entry(to).or_default().union(&flow);
        }
        if let Some(sources) = self.expr_sources.get(&from).cloned() {
            self.expr_sources.entry(to).or_default().extend(sources);
        }
    }

    pub(super) fn read_place(&mut self, expr: ExprId) {
        self.mark_place_access(expr, CaptureAccess::Read);
    }

    pub(super) fn mutably_use_place(&mut self, expr: ExprId) {
        self.mark_place_access(expr, CaptureAccess::Mutable);
    }

    pub(super) fn copy_place_identity(&mut self, from: ExprId, to: ExprId) {
        let Some(binding_id) = self.place_captures.get(&from).copied() else {
            return;
        };
        self.place_captures.insert(to, binding_id);
    }

    pub(super) fn mark_lambda_non_escaping(&mut self, expr_id: ExprId) {
        self.lambda_escapes
            .entry(expr_id)
            .or_insert(LambdaEscapeFact {
                expr_id,
                escape: LambdaEscapeKind::NonEscaping,
            });
    }

    pub(super) fn record_escaping_use(&mut self, expr: ExprId, span: Span) {
        self.record_expr_escaping_flow(expr, span);
    }

    fn record_escaping_flow(&mut self, flow: &EscapeFlow, span: Span) {
        self.collect_escaping_flow_events(flow, span);
        self.drain_escape_queue(span);
    }

    pub(super) fn drain_escape_events(&mut self, span: Span) {
        self.drain_escape_queue(span);
    }

    pub(super) fn take_escape_events(&mut self) -> Vec<EscapeEvent> {
        std::mem::take(&mut self.escape_events)
    }

    fn collect_escaping_flow_events(&mut self, flow: &EscapeFlow, span: Span) {
        for origin in flow.callbacks() {
            self.escape_events.push(EscapeEvent::Callback {
                origin: origin.clone(),
                span,
            });
        }
        for capture in flow.borrowed() {
            self.escape_events.push(EscapeEvent::Borrowed {
                capture: capture.clone(),
                span,
            });
        }
        for lambda in flow.lambdas() {
            self.record_lambda_escape(lambda);
        }
    }

    fn drain_escape_queue(&mut self, span: Span) {
        while let Some(index) = self.escape_queue.pop_front() {
            let flow = self
                .sealed_lambdas
                .get(index)
                .expect("escaped lambda must be sealed before queueing")
                .captured_flow
                .clone();
            self.collect_escaping_flow_events(&flow, span);
        }
    }

    fn record_lambda_escape(&mut self, expr_id: ExprId) {
        if self.lambda_escapes(expr_id) {
            return;
        }
        self.lambda_escapes.insert(
            expr_id,
            LambdaEscapeFact {
                expr_id,
                escape: LambdaEscapeKind::Escaping,
            },
        );
        self.escape_queue.extend(
            self.sealed_lambdas
                .iter()
                .enumerate()
                .filter_map(|(index, lambda)| (lambda.expr_id == expr_id).then_some(index)),
        );
    }

    fn lambda_escapes(&self, expr_id: ExprId) -> bool {
        self.lambda_escapes
            .get(&expr_id)
            .is_some_and(|fact| matches!(fact.escape, LambdaEscapeKind::Escaping))
    }

    pub(super) fn finish(
        &self,
        mut type_of: impl FnMut(SemanticLocalId) -> Type,
    ) -> TypecheckFacts {
        debug_assert!(
            self.escape_queue.is_empty(),
            "closure escape queue must be drained before finalization"
        );
        self.build_final_facts(&mut type_of)
    }

    pub(super) fn fact_snapshot(
        &self,
        mut type_of: impl FnMut(SemanticLocalId) -> Type,
    ) -> TypecheckFacts {
        self.build_final_facts(&mut type_of)
    }

    fn build_final_facts(
        &self,
        type_of: &mut impl FnMut(SemanticLocalId) -> Type,
    ) -> TypecheckFacts {
        let mut facts = self.replayed_facts.clone();
        facts.lambda_escapes.extend(self.lambda_escapes.clone());
        for lambda in &self.sealed_lambdas {
            self.add_lambda_capture_facts(lambda, &mut facts, type_of);
        }
        facts
    }

    fn add_lambda_capture_facts(
        &self,
        lambda: &SealedLambda,
        facts: &mut TypecheckFacts,
        type_of: &mut impl FnMut(SemanticLocalId) -> Type,
    ) {
        let escaping = self.lambda_escapes(lambda.expr_id);
        for capture in &lambda.captures {
            let ty = type_of(capture.type_id);
            let source_mutable = matches!(capture.kind.mutability, BindingMutability::Mutable);
            let storage = capture
                .kind
                .storage
                .capture_storage(source_mutable, escaping);
            facts.lambda_captures.insert(
                (lambda.expr_id, capture.binding_id),
                LambdaCaptureFact {
                    lambda_id: lambda.expr_id,
                    binding_id: capture.binding_id,
                    name: capture.name,
                    ty: ty.clone(),
                    origin: capture.kind.storage,
                    source_mutable,
                    access: capture.access,
                    storage,
                },
            );
            if escaping && storage == CaptureStorage::OwnedMutableUpvalue {
                facts.binding_promotions.insert(
                    capture.binding_id,
                    BindingPromotionFact {
                        binding_id: capture.binding_id,
                        name: capture.name,
                        ty,
                    },
                );
            }
        }
    }

    pub(super) fn extend_facts(&mut self, facts: TypecheckFacts) {
        self.replayed_facts
            .lambda_escapes
            .extend(facts.lambda_escapes);
        self.replayed_facts
            .lambda_captures
            .extend(facts.lambda_captures);
        self.replayed_facts
            .binding_promotions
            .extend(facts.binding_promotions);
    }

    fn crosses_capture_boundary(&self, depth: usize) -> bool {
        self.active_lambdas
            .last()
            .is_some_and(|frame| depth > 0 && depth < frame.start_scope)
    }

    fn record_captured_flow(&mut self, flow: &EscapeFlow, source_depth: usize) {
        if flow.is_empty() {
            return;
        }
        self.with_capturing_active_lambdas(source_depth, |frame| {
            frame.captured_flow.union(flow);
        });
    }

    fn record_assigned_capture_flow(
        &mut self,
        binding_id: BindingId,
        flow: &EscapeFlow,
        span: Span,
    ) {
        if flow.is_empty() {
            return;
        }
        for frame in &mut self.active_lambdas {
            if frame.captures.contains_key(&binding_id) {
                frame.captured_flow.union(flow);
            }
        }

        let mut escaped = false;
        let lambda_escapes = &self.lambda_escapes;
        for lambda in &mut self.sealed_lambdas {
            if lambda
                .captures
                .iter()
                .any(|capture| capture.binding_id == binding_id)
            {
                lambda.captured_flow.union(flow);
                escaped |= lambda_escapes
                    .get(&lambda.expr_id)
                    .is_some_and(|fact| matches!(fact.escape, LambdaEscapeKind::Escaping));
            }
        }
        if escaped {
            self.record_escaping_flow(flow, span);
        }
    }

    fn capture_local_read(&mut self, expr: ExprId, binding_id: BindingId, source_depth: usize) {
        let Some(binding) = self.bindings.get(&binding_id).copied() else {
            return;
        };
        self.place_captures.insert(expr, binding_id);
        let capture = CaptureUse {
            binding_id,
            name: binding.name,
            type_id: binding.type_id,
            kind: LocalBindingKind {
                mutability: binding.mutability,
                storage: binding.storage,
            },
            access: CaptureAccess::Read,
        };
        self.with_capturing_active_lambdas(source_depth, |frame| {
            frame
                .captures
                .entry(capture.binding_id)
                .or_insert(capture.clone());
        });
    }

    pub(super) fn enter_lambda(&mut self, expr_id: ExprId, start_scope: usize) {
        self.active_lambdas.push(ActiveLambda {
            expr_id,
            start_scope,
            captured_flow: EscapeFlow::default(),
            captures: HashMap::new(),
        });
    }

    pub(super) fn exit_lambda(&mut self) {
        let frame = self
            .active_lambdas
            .pop()
            .expect("active lambda must exist on exit");
        let lambda = SealedLambda {
            expr_id: frame.expr_id,
            captured_flow: frame.captured_flow,
            captures: frame.captures.into_values().collect(),
        };
        let escaped = self.lambda_escapes(lambda.expr_id);
        self.sealed_lambdas.push(lambda);
        if escaped {
            self.escape_queue.push_back(self.sealed_lambdas.len() - 1);
        }
    }

    fn mark_place_access(&mut self, expr: ExprId, access: CaptureAccess) {
        let Some(binding_id) = self.place_captures.get(&expr).copied() else {
            return;
        };
        for frame in &mut self.active_lambdas {
            if let Some(capture) = frame.captures.get_mut(&binding_id) {
                capture.access = strongest_capture_access(capture.access, access);
            }
        }
    }

    fn with_capturing_active_lambdas(
        &mut self,
        source_depth: usize,
        mut f: impl FnMut(&mut ActiveLambda),
    ) {
        if source_depth == 0 {
            return;
        }
        for frame in &mut self.active_lambdas {
            if source_depth < frame.start_scope {
                f(frame);
            }
        }
    }
}

#[derive(Clone, Default)]
pub(super) struct ClosureFlowSnapshot {
    local_flows: HashMap<BindingId, EscapeFlow>,
    active_lambdas: Vec<ActiveLambda>,
    loop_steps: Vec<Vec<LoopStep>>,
}

struct LoopFlow {
    initial: ClosureFlowSnapshot,
    steps: Vec<LoopStep>,
}

#[derive(Clone, PartialEq, Eq)]
enum LoopStep {
    Assign(LoopAssignment),
    Choice(Vec<Vec<LoopStep>>),
}

#[derive(Clone, PartialEq, Eq)]
struct LoopAssignment {
    target: BindingId,
    flow: EscapeFlow,
    sources: HashSet<BindingId>,
    span: Span,
}

#[derive(Clone, Default)]
pub(super) struct ClosureScopeState {
    bindings: HashMap<BindingId, BindingInfo>,
    local_flows: HashMap<BindingId, EscapeFlow>,
}

#[derive(Clone, Copy)]
struct BindingInfo {
    name: Ident,
    type_id: SemanticLocalId,
    mutability: BindingMutability,
    storage: CaptureStorageOrigin,
    scope_depth: usize,
}

#[derive(Clone, Default, PartialEq, Eq)]
struct EscapeFlow {
    origins: HashSet<FlowOrigin>,
}

impl EscapeFlow {
    fn is_empty(&self) -> bool {
        self.origins.is_empty()
    }

    fn insert_callback(&mut self, origin: NonEscapingCallback) {
        self.origins.insert(FlowOrigin::Callback(origin));
    }

    fn insert_borrowed(&mut self, capture: BorrowedCapture) {
        self.origins.insert(FlowOrigin::Borrowed(capture));
    }

    fn insert_lambda(&mut self, expr: ExprId) {
        self.origins.insert(FlowOrigin::Lambda(expr));
    }

    fn union(&mut self, other: &Self) {
        self.origins.extend(other.origins.iter().cloned());
    }

    fn callbacks(&self) -> impl Iterator<Item = &NonEscapingCallback> {
        self.origins.iter().filter_map(|origin| match origin {
            FlowOrigin::Callback(callback) => Some(callback),
            FlowOrigin::Borrowed(_) | FlowOrigin::Lambda(_) => None,
        })
    }

    fn borrowed(&self) -> impl Iterator<Item = &BorrowedCapture> {
        self.origins.iter().filter_map(|origin| match origin {
            FlowOrigin::Borrowed(capture) => Some(capture),
            FlowOrigin::Callback(_) | FlowOrigin::Lambda(_) => None,
        })
    }

    fn lambdas(&self) -> impl Iterator<Item = ExprId> + '_ {
        self.origins.iter().filter_map(|origin| match origin {
            FlowOrigin::Lambda(expr) => Some(*expr),
            FlowOrigin::Callback(_) | FlowOrigin::Borrowed(_) => None,
        })
    }
}

#[derive(Clone, Eq)]
enum FlowOrigin {
    Callback(NonEscapingCallback),
    Borrowed(BorrowedCapture),
    Lambda(ExprId),
}

impl PartialEq for FlowOrigin {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Callback(left), Self::Callback(right)) => left.id == right.id,
            (Self::Borrowed(left), Self::Borrowed(right)) => left.id == right.id,
            (Self::Lambda(left), Self::Lambda(right)) => left == right,
            _ => false,
        }
    }
}

impl Hash for FlowOrigin {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Self::Callback(origin) => origin.id.hash(state),
            Self::Borrowed(capture) => capture.id.hash(state),
            Self::Lambda(expr) => expr.hash(state),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct NonEscapingCallback {
    pub(super) id: SemanticLocalId,
    pub(super) name: Ident,
    pub(super) help: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct BorrowedCapture {
    pub(super) id: BindingId,
    pub(super) name: Ident,
    pub(super) origin: CaptureStorageOrigin,
}

#[derive(Clone, PartialEq)]
struct CaptureUse {
    binding_id: BindingId,
    name: Ident,
    type_id: SemanticLocalId,
    kind: LocalBindingKind,
    access: CaptureAccess,
}

#[derive(Clone)]
pub(super) enum EscapeEvent {
    Callback {
        origin: NonEscapingCallback,
        span: Span,
    },
    Borrowed {
        capture: BorrowedCapture,
        span: Span,
    },
}

#[derive(Clone, PartialEq)]
struct SealedLambda {
    expr_id: ExprId,
    captured_flow: EscapeFlow,
    captures: Vec<CaptureUse>,
}

#[derive(Clone, PartialEq)]
struct ActiveLambda {
    expr_id: ExprId,
    start_scope: usize,
    captured_flow: EscapeFlow,
    captures: HashMap<BindingId, CaptureUse>,
}

fn join_loop_step_lists(left: &[LoopStep], right: &[LoopStep]) -> Vec<LoopStep> {
    let prefix_len = left
        .iter()
        .zip(right)
        .take_while(|(left, right)| left == right)
        .count();
    let mut steps = left[..prefix_len].to_vec();
    let left_suffix = left[prefix_len..].to_vec();
    let right_suffix = right[prefix_len..].to_vec();
    if !left_suffix.is_empty() || !right_suffix.is_empty() {
        steps.push(LoopStep::Choice(vec![left_suffix, right_suffix]));
    }
    steps
}

fn collect_loop_assignment_spans(steps: &[LoopStep], spans: &mut HashMap<BindingId, Span>) {
    for step in steps {
        match step {
            LoopStep::Assign(assignment) => {
                spans.entry(assignment.target).or_insert(assignment.span);
            }
            LoopStep::Choice(arms) => {
                for arm in arms {
                    collect_loop_assignment_spans(arm, spans);
                }
            }
        }
    }
}

fn union_loop_state(
    target: &mut HashMap<BindingId, EscapeFlow>,
    source: HashMap<BindingId, EscapeFlow>,
) {
    for (binding_id, flow) in source {
        target.entry(binding_id).or_default().union(&flow);
    }
}

fn merge_active_lambdas(left: &[ActiveLambda], right: &[ActiveLambda]) -> Vec<ActiveLambda> {
    debug_assert_eq!(left.len(), right.len());
    left.iter()
        .zip(right)
        .map(|(left, right)| {
            debug_assert_eq!(left.expr_id, right.expr_id);
            debug_assert_eq!(left.start_scope, right.start_scope);
            let mut frame = left.clone();
            frame.captured_flow.union(&right.captured_flow);
            for capture in right.captures.values() {
                frame
                    .captures
                    .entry(capture.binding_id)
                    .and_modify(|saved| {
                        saved.access = strongest_capture_access(saved.access, capture.access);
                    })
                    .or_insert_with(|| capture.clone());
            }
            frame
        })
        .collect()
}

fn strongest_capture_access(current: CaptureAccess, new: CaptureAccess) -> CaptureAccess {
    match (current, new) {
        (CaptureAccess::Mutable, _) | (_, CaptureAccess::Mutable) => CaptureAccess::Mutable,
        _ => CaptureAccess::Read,
    }
}

pub(super) fn check_lambda_expr(
    expr: &ExprNode,
    lambda: &LambdaNode,
    expected: Option<&TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let expected_func = expected_assignable_type(expected, tc).and_then(|ty| match ty {
        Type::Func { params, ret } => Some((params, *ret)),
        _ => None,
    });
    if let Some((params, _)) = &expected_func
        && params.len() != lambda.node.params.len()
    {
        tc.push_error(TypeError::LambdaParamCountMismatch {
            expected: params.len(),
            found: lambda.node.params.len(),
            span: tc.error_span(lambda.span),
        });
        return checked_from_type(expr, Type::Infer, tc);
    }

    let params = lambda
        .node
        .params
        .iter()
        .enumerate()
        .map(|(index, param)| {
            let ty = match &param.ty {
                Some(ty) => tc.resolve_callable_param_type(ty, lambda.span, false),
                None => expected_func
                    .as_ref()
                    .and_then(|(params, _)| params.get(index))
                    .map_or_else(
                        || {
                            tc.push_error(TypeError::CannotInferType {
                                span: tc.error_span(lambda.span),
                            });
                            Type::Infer
                        },
                        |param| param.ty.clone(),
                    ),
            };
            tc.validate_func_param_escape(
                param.escape,
                param.mutable,
                param.cast_accept,
                &ty,
                lambda.span,
            );
            FuncParam::new(ty, param.mutable, param.cast_accept, param.escape)
        })
        .collect::<Vec<_>>();

    let explicit_ret = lambda
        .node
        .ret_type
        .as_ref()
        .map(|ret| ret.with_ty(tc.resolve_type_for_tc_at(&ret.ty, lambda.span)));
    if let Some(ret) = &explicit_ret {
        validate_return_spec(ret, false, has_mutable_func_param(&params), lambda.span, tc);
    }
    let expected_ret = explicit_ret.or_else(|| expected_func.as_ref().map(|(_, ret)| ret.clone()));

    let inferred_ret = with_callable_body_scope(
        tc,
        |tc| {
            tc.closure.mark_lambda_non_escaping(expr.node.id);
            tc.closure.enter_lambda(expr.node.id, tc.scopes.len());
        },
        |tc| {
            let bindings = lambda
                .node
                .params
                .iter()
                .zip(&params)
                .map(|(param, ty)| CallableParamBinding {
                    name: param.name,
                    source_ty: param.ty.as_ref(),
                    ty,
                })
                .collect::<Vec<_>>();
            tc.with_body_instance(
                BodyInstanceKey::Lambda(LambdaBodyKey {
                    expr: expr.node.id,
                    specialization: tc.visible_generic_owner().args,
                }),
                |tc| {
                    check_callable_body_frame(
                        &bindings,
                        expected_ret.as_ref(),
                        ReturnAccess::Value,
                        None,
                        0,
                        CallableBody::Expr(&lambda.node.body),
                        lambda.span,
                        tc,
                    )
                },
            )
        },
        |tc| {
            tc.closure.exit_lambda();
            tc.closure.drain_escape_events(expr.span);
        },
    );
    tc.closure.lambda_value(expr.node.id);

    let ret = expected_ret
        .or_else(|| inferred_ret.map(ReturnSpec::value))
        .unwrap_or_else(|| ReturnSpec::value(Type::Infer));
    let ty = Type::Func {
        params,
        ret: Box::new(ret),
    };
    tc.record_function_value_expr(
        expr.node.id,
        &ty,
        FunctionValueKind::Lambda {
            lambda_expr: expr.node.id,
        },
    );
    checked_from_type(expr, ty, tc)
}
