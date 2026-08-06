use super::rir::{
    RirDynCarrierId, RirDynReceiver, RirDynVariantId, RirDynWeakening, RirOperand, RirPlace,
    RirPlaceRoot, RirProgram, RirRValue, RirStmt, RirStructuredBlock, stmt_child_blocks_any,
};

#[derive(Clone, Copy, PartialEq, Eq)]
struct ExactWitness {
    carrier: RirDynCarrierId,
    variant: RirDynVariantId,
}

pub(super) fn propagate(program: &mut RirProgram) {
    let (functions, weakenings, dispatches) = (
        &mut program.functions,
        &program.dyn_weakenings,
        &program.dyn_dispatches,
    );
    for function in functions {
        let mut state = vec![None; function.locals.len()];
        visit_block(&mut function.body, &mut state, weakenings, dispatches);
    }
}

pub(super) fn is_canonical(program: &RirProgram) -> bool {
    let found = annotations(program);
    let mut expected = program.clone();
    clear(&mut expected);
    propagate(&mut expected);
    found == annotations(&expected)
}

fn annotations(program: &RirProgram) -> Vec<Option<RirDynVariantId>> {
    let mut annotations = vec![];
    for function in &program.functions {
        collect_annotations(&function.body, &mut annotations);
    }
    annotations
}

fn collect_annotations(block: &RirStructuredBlock, annotations: &mut Vec<Option<RirDynVariantId>>) {
    for stmt in &block.stmts {
        if let RirStmt::Init { value, .. }
        | RirStmt::GlobalSetRoot { value, .. }
        | RirStmt::GlobalUpdateRoot { value, .. }
        | RirStmt::MutPlaceSet { value, .. }
        | RirStmt::Assign { value, .. }
        | RirStmt::CellInit { value, .. }
        | RirStmt::CellSet { value, .. }
        | RirStmt::ScopedPlaceCellSet { value, .. }
        | RirStmt::DataRefSet { value, .. }
        | RirStmt::Eval(value) = stmt
            && let RirRValue::DynCall { exact_variant, .. } = value
        {
            annotations.push(*exact_variant);
        }
        stmt_child_blocks_any(stmt, |block| {
            collect_annotations(block, annotations);
            false
        });
    }
}

fn clear(program: &mut RirProgram) {
    for function in &mut program.functions {
        clear_block(&mut function.body);
    }
}

fn clear_block(block: &mut RirStructuredBlock) {
    for stmt in &mut block.stmts {
        if let RirStmt::Init { value, .. }
        | RirStmt::GlobalSetRoot { value, .. }
        | RirStmt::GlobalUpdateRoot { value, .. }
        | RirStmt::MutPlaceSet { value, .. }
        | RirStmt::Assign { value, .. }
        | RirStmt::CellInit { value, .. }
        | RirStmt::CellSet { value, .. }
        | RirStmt::ScopedPlaceCellSet { value, .. }
        | RirStmt::DataRefSet { value, .. }
        | RirStmt::Eval(value) = stmt
            && let RirRValue::DynCall { exact_variant, .. } = value
        {
            *exact_variant = None;
        }
        match stmt {
            RirStmt::If(branch) => {
                clear_block(&mut branch.then_block);
                if let Some(block) = &mut branch.else_block {
                    clear_block(block);
                }
            }
            RirStmt::Loop(loop_) => clear_block(&mut loop_.body),
            RirStmt::RangeFor(range) => clear_block(&mut range.body),
            RirStmt::CollectionFor(for_) => clear_block(&mut for_.body),
            RirStmt::CollectionLoanScope(scope) => clear_block(&mut scope.body),
            RirStmt::CollectionSlotScope(block) => clear_block(block),
            RirStmt::PatternMatch(match_) => {
                for arm in &mut match_.arms {
                    clear_block(&mut arm.block);
                }
            }
            RirStmt::DynMatch(match_) => {
                for arm in &mut match_.arms {
                    clear_block(&mut arm.block);
                }
                clear_block(&mut match_.fallback);
            }
            RirStmt::OptionMatch(match_) => {
                clear_block(&mut match_.some_block);
                clear_block(&mut match_.none_block);
            }
            RirStmt::MapEntryMatch(match_) => {
                clear_block(&mut match_.some_block);
                clear_block(&mut match_.none_block);
            }
            _ => {}
        }
    }
}

fn visit_block(
    block: &mut RirStructuredBlock,
    state: &mut [Option<ExactWitness>],
    weakenings: &[RirDynWeakening],
    dispatches: &[super::rir::RirDynDispatch],
) {
    for stmt in &mut block.stmts {
        visit_stmt(stmt, state, weakenings, dispatches);
    }
}

fn visit_stmt(
    stmt: &mut RirStmt,
    state: &mut [Option<ExactWitness>],
    weakenings: &[RirDynWeakening],
    dispatches: &[super::rir::RirDynDispatch],
) {
    match stmt {
        RirStmt::Init { local, value } => {
            visit_rvalue(value, state, dispatches);
            let exact = rvalue_exact(value, state, weakenings);
            for source in transferred_locals(value) {
                if source != *local {
                    state[source.index()] = None;
                }
            }
            state[local.index()] = exact;
            invalidate_after_call(value, state);
        }
        RirStmt::Assign { dst, value } => {
            visit_rvalue(value, state, dispatches);
            let exact = rvalue_exact(value, state, weakenings);
            let destination = direct_local(dst);
            for source in transferred_locals(value) {
                if Some(source) != destination {
                    state[source.index()] = None;
                }
            }
            if let Some(local) = destination {
                state[local.index()] = exact;
            } else {
                invalidate_place(dst, state);
            }
            invalidate_after_call(value, state);
        }
        RirStmt::Eval(value) => {
            visit_rvalue(value, state, dispatches);
            for source in transferred_locals(value) {
                state[source.index()] = None;
            }
            invalidate_after_call(value, state);
        }
        RirStmt::If(branch) => {
            visit_nested(&mut branch.then_block, state, weakenings, dispatches);
            if let Some(block) = &mut branch.else_block {
                visit_nested(block, state, weakenings, dispatches);
            }
            state.fill(None);
        }
        RirStmt::Loop(loop_) => {
            visit_loop(&mut loop_.body, state.len(), weakenings, dispatches);
            state.fill(None);
        }
        RirStmt::RangeFor(range) => {
            visit_loop(&mut range.body, state.len(), weakenings, dispatches);
            state.fill(None);
        }
        RirStmt::CollectionFor(for_) => {
            visit_loop(&mut for_.body, state.len(), weakenings, dispatches);
            state.fill(None);
        }
        RirStmt::CollectionLoanScope(scope) => {
            visit_nested(&mut scope.body, state, weakenings, dispatches);
            state.fill(None);
        }
        RirStmt::CollectionSlotScope(block) => {
            visit_nested(block, state, weakenings, dispatches);
            state.fill(None);
        }
        RirStmt::PatternMatch(match_) => {
            let exact = direct_local(&match_.subject).and_then(|local| state[local.index()]);
            for arm in &mut match_.arms {
                let mut nested = state.to_vec();
                if let Some(exact) = exact {
                    for alternative in &arm.alternatives {
                        for binding in &alternative.bindings {
                            if binding.path.steps.is_empty() {
                                nested[binding.local.index()] = Some(exact);
                            }
                        }
                    }
                }
                visit_block(&mut arm.block, &mut nested, weakenings, dispatches);
            }
            state.fill(None);
        }
        RirStmt::DynMatch(match_) => {
            let exact = match &match_.source {
                super::rir::RirDynMatchSource::Owned(value) => owned_exact(value, state),
                super::rir::RirDynMatchSource::MutPlace(_)
                | super::rir::RirDynMatchSource::Borrowed(_) => None,
            };
            for arm in &mut match_.arms {
                visit_nested(&mut arm.block, state, weakenings, dispatches);
            }
            let mut fallback = state.to_vec();
            if let (Some(exact), Some(local)) = (exact, match_.fallback_binding.local()) {
                fallback[local.index()] = Some(exact);
            }
            visit_block(&mut match_.fallback, &mut fallback, weakenings, dispatches);
            state.fill(None);
        }
        RirStmt::OptionMatch(match_) => {
            visit_nested(&mut match_.some_block, state, weakenings, dispatches);
            visit_nested(&mut match_.none_block, state, weakenings, dispatches);
            state.fill(None);
        }
        RirStmt::MapEntryMatch(match_) => {
            visit_nested(&mut match_.some_block, state, weakenings, dispatches);
            visit_nested(&mut match_.none_block, state, weakenings, dispatches);
            state.fill(None);
        }
        RirStmt::GlobalSetRoot { value, .. }
        | RirStmt::GlobalUpdateRoot { value, .. }
        | RirStmt::MutPlaceSet { value, .. }
        | RirStmt::CellInit { value, .. }
        | RirStmt::CellSet { value, .. }
        | RirStmt::ScopedPlaceCellSet { value, .. }
        | RirStmt::DataRefSet { value, .. } => {
            visit_rvalue(value, state, dispatches);
            state.fill(None);
        }
        RirStmt::GlobalEnsure { .. }
        | RirStmt::ScopedPlaceCellInit { .. }
        | RirStmt::SequenceSlotSet { .. }
        | RirStmt::MapValueSet { .. } => state.fill(None),
    }
}

fn visit_nested(
    block: &mut RirStructuredBlock,
    state: &[Option<ExactWitness>],
    weakenings: &[RirDynWeakening],
    dispatches: &[super::rir::RirDynDispatch],
) {
    let mut nested = state.to_vec();
    visit_block(block, &mut nested, weakenings, dispatches);
}

fn visit_loop(
    block: &mut RirStructuredBlock,
    locals: usize,
    weakenings: &[RirDynWeakening],
    dispatches: &[super::rir::RirDynDispatch],
) {
    let mut state = vec![None; locals];
    visit_block(block, &mut state, weakenings, dispatches);
}

fn visit_rvalue(
    value: &mut RirRValue,
    state: &[Option<ExactWitness>],
    dispatches: &[super::rir::RirDynDispatch],
) {
    let RirRValue::DynCall {
        dispatch,
        exact_variant,
        receiver: RirDynReceiver::Owned(value),
        ..
    } = value
    else {
        return;
    };
    let Some(exact) = owned_exact(value, state) else {
        return;
    };
    let Some(dispatch) = dispatches.get(dispatch.index()) else {
        return;
    };
    *exact_variant = (exact.carrier == dispatch.carrier).then_some(exact.variant);
}

fn rvalue_exact(
    value: &RirRValue,
    state: &[Option<ExactWitness>],
    weakenings: &[RirDynWeakening],
) -> Option<ExactWitness> {
    match value {
        RirRValue::DynPack { variant, .. } => Some(ExactWitness {
            carrier: variant.carrier(),
            variant: *variant,
        }),
        RirRValue::Use(value) => operand_exact(value, state),
        RirRValue::Materialize(owned) => owned_exact(owned, state),
        RirRValue::DynWeaken {
            weakening, value, ..
        } => {
            let source = owned_exact(value, state)?;
            let weakening = weakenings.get(weakening.index())?;
            (weakening.source == source.carrier)
                .then(|| weakening.arms.get(source.variant.index()))?
                .map(|arm| ExactWitness {
                    carrier: weakening.target,
                    variant: arm.target,
                })
        }
        _ => None,
    }
}

fn owned_exact(
    owned: &super::rir::RirOwnedValue,
    state: &[Option<ExactWitness>],
) -> Option<ExactWitness> {
    match &owned.value {
        super::rir::RirOwnedOperand::Value(value) => operand_exact(value, state),
        super::rir::RirOwnedOperand::Access(_) | super::rir::RirOwnedOperand::DynBorrow(_) => None,
    }
}

fn operand_exact(operand: &RirOperand, state: &[Option<ExactWitness>]) -> Option<ExactWitness> {
    let RirOperand::Place(place) = operand else {
        return None;
    };
    direct_local(place).and_then(|local| state[local.index()])
}

fn direct_local(place: &RirPlace) -> Option<super::rir::RirLocalId> {
    match (&place.root, place.projections.is_empty()) {
        (RirPlaceRoot::Local(local), true) => Some(*local),
        _ => None,
    }
}

fn invalidate_place(place: &RirPlace, state: &mut [Option<ExactWitness>]) {
    if let RirPlaceRoot::Local(local) = place.root {
        state[local.index()] = None;
    }
}

fn transferred_locals(value: &RirRValue) -> Vec<super::rir::RirLocalId> {
    let mut locals = vec![];
    value.for_each_owned_value(&mut |owned| {
        if matches!(owned.source, super::rir::RirOwnedSource::Transfer { .. })
            && let super::rir::RirOwnedOperand::Value(RirOperand::Place(place)) = &owned.value
            && let Some(local) = direct_local(place)
        {
            locals.push(local);
        }
    });
    locals
}

fn invalidate_after_call(value: &RirRValue, state: &mut [Option<ExactWitness>]) {
    if matches!(value, RirRValue::Call { .. } | RirRValue::DynCall { .. }) {
        state.fill(None);
    }
}
