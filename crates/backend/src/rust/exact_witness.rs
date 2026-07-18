use super::rir::{
    RirDynCarrierId, RirDynReceiver, RirDynVariantId, RirOperand, RirPlace, RirPlaceRoot,
    RirProgram, RirRValue, RirStmt, RirStructuredBlock, stmt_child_blocks_any,
};

#[derive(Clone, Copy, PartialEq, Eq)]
struct ExactWitness {
    carrier: RirDynCarrierId,
    variant: RirDynVariantId,
}

pub(super) fn propagate(program: &mut RirProgram) {
    for function in &mut program.functions {
        let mut state = vec![None; function.locals.len()];
        visit_block(&mut function.body, &mut state);
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

fn visit_block(block: &mut RirStructuredBlock, state: &mut [Option<ExactWitness>]) {
    for stmt in &mut block.stmts {
        visit_stmt(stmt, state);
    }
}

fn visit_stmt(stmt: &mut RirStmt, state: &mut [Option<ExactWitness>]) {
    match stmt {
        RirStmt::Init { local, value } => {
            visit_rvalue(value, state);
            let exact = rvalue_exact(value, state);
            for source in transferred_locals(value) {
                if source != *local {
                    state[source.index()] = None;
                }
            }
            state[local.index()] = exact;
            invalidate_after_call(value, state);
        }
        RirStmt::Assign { dst, value } => {
            visit_rvalue(value, state);
            let exact = rvalue_exact(value, state);
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
            visit_rvalue(value, state);
            for source in transferred_locals(value) {
                state[source.index()] = None;
            }
            invalidate_after_call(value, state);
        }
        RirStmt::If(branch) => {
            visit_nested(&mut branch.then_block, state);
            if let Some(block) = &mut branch.else_block {
                visit_nested(block, state);
            }
            state.fill(None);
        }
        RirStmt::Loop(loop_) => {
            visit_loop(&mut loop_.body, state.len());
            state.fill(None);
        }
        RirStmt::RangeFor(range) => {
            visit_loop(&mut range.body, state.len());
            state.fill(None);
        }
        RirStmt::CollectionFor(for_) => {
            visit_loop(&mut for_.body, state.len());
            state.fill(None);
        }
        RirStmt::CollectionLoanScope(scope) => {
            visit_nested(&mut scope.body, state);
            state.fill(None);
        }
        RirStmt::CollectionSlotScope(block) => {
            visit_nested(block, state);
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
                visit_block(&mut arm.block, &mut nested);
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
                visit_nested(&mut arm.block, state);
            }
            let mut fallback = state.to_vec();
            if let (Some(exact), Some(local)) = (exact, match_.fallback_binding.local()) {
                fallback[local.index()] = Some(exact);
            }
            visit_block(&mut match_.fallback, &mut fallback);
            state.fill(None);
        }
        RirStmt::OptionMatch(match_) => {
            visit_nested(&mut match_.some_block, state);
            visit_nested(&mut match_.none_block, state);
            state.fill(None);
        }
        RirStmt::MapEntryMatch(match_) => {
            visit_nested(&mut match_.some_block, state);
            visit_nested(&mut match_.none_block, state);
            state.fill(None);
        }
        RirStmt::GlobalSetRoot { value, .. }
        | RirStmt::GlobalUpdateRoot { value, .. }
        | RirStmt::MutPlaceSet { value, .. }
        | RirStmt::CellInit { value, .. }
        | RirStmt::CellSet { value, .. }
        | RirStmt::ScopedPlaceCellSet { value, .. }
        | RirStmt::DataRefSet { value, .. } => {
            visit_rvalue(value, state);
            state.fill(None);
        }
        RirStmt::GlobalEnsure { .. }
        | RirStmt::ScopedPlaceCellInit { .. }
        | RirStmt::SequenceSlotSet { .. }
        | RirStmt::MapValueSet { .. } => state.fill(None),
    }
}

fn visit_nested(block: &mut RirStructuredBlock, state: &[Option<ExactWitness>]) {
    let mut nested = state.to_vec();
    visit_block(block, &mut nested);
}

fn visit_loop(block: &mut RirStructuredBlock, locals: usize) {
    let mut state = vec![None; locals];
    visit_block(block, &mut state);
}

fn visit_rvalue(value: &mut RirRValue, state: &[Option<ExactWitness>]) {
    let RirRValue::DynCall {
        carrier,
        exact_variant,
        receiver: RirDynReceiver::Owned(value),
        ..
    } = value
    else {
        return;
    };
    *exact_variant = owned_exact(value, state)
        .filter(|exact| exact.carrier == *carrier)
        .map(|exact| exact.variant);
}

fn rvalue_exact(value: &RirRValue, state: &[Option<ExactWitness>]) -> Option<ExactWitness> {
    match value {
        RirRValue::DynPack {
            carrier, variant, ..
        } => Some(ExactWitness {
            carrier: *carrier,
            variant: *variant,
        }),
        RirRValue::Use(value) => operand_exact(value, state),
        RirRValue::Materialize(owned) => owned_exact(owned, state),
        RirRValue::DynWeaken {
            target,
            value,
            arms,
            ..
        } => {
            let source = owned_exact(value, state)?;
            arms.iter()
                .find(|arm| arm.source == source.variant)
                .map(|arm| ExactWitness {
                    carrier: *target,
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

#[cfg(test)]
mod tests {
    use anvyx_frontend::air;

    use super::*;
    use crate::rust::rir::{
        RirDataRefId, RirDynMatch, RirDynMatchFallbackBinding, RirDynMatchSource, RirFieldId,
        RirLocalId, RirLoop, RirLoopId, RirMaterializerId, RirOwnedOperand, RirOwnedSource,
        RirOwnedValue, RirPatternAlternative, RirPatternArm, RirPatternBinding,
        RirPatternBindingMode, RirPatternMatch, RirPatternPath, RirProjection, RirRValue, RirTerm,
        RirTypeId,
    };

    fn local(id: usize, ty: RirTypeId) -> RirOperand {
        RirOperand::Place(RirPlace::local(RirLocalId::from_index(id), vec![], ty))
    }

    fn owned(value: RirOperand) -> RirOwnedValue {
        RirOwnedValue {
            value: RirOwnedOperand::Value(value),
            source: RirOwnedSource::Reuse(RirMaterializerId::from_index(0)),
        }
    }

    fn call(value: RirOperand, carrier: RirDynCarrierId, ty: RirTypeId) -> RirRValue {
        RirRValue::DynCall {
            carrier,
            air_slot: air::ContractSlotId::from_index(0),
            exact_variant: None,
            receiver: RirDynReceiver::Owned(owned(value)),
            args: vec![],
            arms: vec![],
            ty,
        }
    }

    #[test]
    fn propagates_pack_through_move() {
        let ty = RirTypeId::from_index(0);
        let carrier = RirDynCarrierId::from_index(0);
        let variant = RirDynVariantId::from_index(1);
        let mut block = RirStructuredBlock {
            stmts: vec![
                RirStmt::Init {
                    local: RirLocalId::from_index(0),
                    value: RirRValue::DynPack {
                        carrier,
                        variant,
                        air_witness: air::ContractWitnessId::from_index(0),
                        value: owned(local(2, ty)),
                        ty,
                    },
                },
                RirStmt::Init {
                    local: RirLocalId::from_index(1),
                    value: RirRValue::Use(local(0, ty)),
                },
                RirStmt::Eval(call(local(1, ty), carrier, ty)),
            ],
            term: RirTerm::Unreachable,
        };
        visit_block(&mut block, &mut [None; 3]);
        let RirStmt::Eval(RirRValue::DynCall { exact_variant, .. }) = &block.stmts[2] else {
            panic!("expected dynamic call");
        };
        assert_eq!(*exact_variant, Some(variant));
    }

    #[test]
    fn loop_does_not_inherit_exact_witness() {
        let ty = RirTypeId::from_index(0);
        let carrier = RirDynCarrierId::from_index(0);
        let mut block = RirStructuredBlock {
            stmts: vec![
                RirStmt::Init {
                    local: RirLocalId::from_index(0),
                    value: RirRValue::DynPack {
                        carrier,
                        variant: RirDynVariantId::from_index(0),
                        air_witness: air::ContractWitnessId::from_index(0),
                        value: owned(local(1, ty)),
                        ty,
                    },
                },
                RirStmt::Loop(RirLoop {
                    id: RirLoopId::from_index(0),
                    body: RirStructuredBlock {
                        stmts: vec![RirStmt::Eval(call(local(0, ty), carrier, ty))],
                        term: RirTerm::Continue(RirLoopId::from_index(0)),
                    },
                }),
            ],
            term: RirTerm::Unreachable,
        };
        visit_block(&mut block, &mut [None; 2]);
        let RirStmt::Loop(loop_) = &block.stmts[1] else {
            panic!("expected loop");
        };
        let RirStmt::Eval(RirRValue::DynCall { exact_variant, .. }) = &loop_.body.stmts[0] else {
            panic!("expected dynamic call");
        };
        assert_eq!(*exact_variant, None);
    }

    #[test]
    fn opaque_call_invalidates_exact_witness() {
        let ty = RirTypeId::from_index(0);
        let carrier = RirDynCarrierId::from_index(0);
        let variant = RirDynVariantId::from_index(0);
        let mut block = RirStructuredBlock {
            stmts: vec![
                RirStmt::Init {
                    local: RirLocalId::from_index(0),
                    value: RirRValue::DynPack {
                        carrier,
                        variant,
                        air_witness: air::ContractWitnessId::from_index(0),
                        value: owned(local(1, ty)),
                        ty,
                    },
                },
                RirStmt::Eval(call(local(0, ty), carrier, ty)),
                RirStmt::Eval(call(local(0, ty), carrier, ty)),
            ],
            term: RirTerm::Unreachable,
        };
        visit_block(&mut block, &mut [None; 2]);
        let RirStmt::Eval(RirRValue::DynCall { exact_variant, .. }) = &block.stmts[2] else {
            panic!("expected dynamic call");
        };
        assert_eq!(*exact_variant, None);
    }

    #[test]
    fn pattern_binding_preserves_exact_witness() {
        let ty = RirTypeId::from_index(0);
        let carrier = RirDynCarrierId::from_index(0);
        let variant = RirDynVariantId::from_index(0);
        let mut block = RirStructuredBlock {
            stmts: vec![
                RirStmt::Init {
                    local: RirLocalId::from_index(0),
                    value: RirRValue::DynPack {
                        carrier,
                        variant,
                        air_witness: air::ContractWitnessId::from_index(0),
                        value: owned(local(2, ty)),
                        ty,
                    },
                },
                RirStmt::PatternMatch(RirPatternMatch {
                    subject: RirPlace::local(RirLocalId::from_index(0), vec![], ty),
                    arms: vec![RirPatternArm {
                        alternatives: vec![RirPatternAlternative {
                            tests: vec![],
                            bindings: vec![RirPatternBinding {
                                local: RirLocalId::from_index(1),
                                path: RirPatternPath::default(),
                                ty,
                                mode: RirPatternBindingMode::Owned {
                                    materializer: RirMaterializerId::from_index(0),
                                },
                            }],
                        }],
                        block: RirStructuredBlock {
                            stmts: vec![RirStmt::Eval(call(local(1, ty), carrier, ty))],
                            term: RirTerm::Unreachable,
                        },
                    }],
                }),
            ],
            term: RirTerm::Unreachable,
        };

        visit_block(&mut block, &mut [None; 3]);

        let RirStmt::PatternMatch(match_) = &block.stmts[1] else {
            panic!("expected pattern match");
        };
        let RirStmt::Eval(RirRValue::DynCall { exact_variant, .. }) =
            &match_.arms[0].block.stmts[0]
        else {
            panic!("expected dynamic call");
        };
        assert_eq!(*exact_variant, Some(variant));
    }

    #[test]
    fn dynamic_fallback_binding_preserves_exact_witness() {
        let ty = RirTypeId::from_index(0);
        let carrier = RirDynCarrierId::from_index(0);
        let variant = RirDynVariantId::from_index(0);
        let mut block = RirStructuredBlock {
            stmts: vec![
                RirStmt::Init {
                    local: RirLocalId::from_index(0),
                    value: RirRValue::DynPack {
                        carrier,
                        variant,
                        air_witness: air::ContractWitnessId::from_index(0),
                        value: owned(local(2, ty)),
                        ty,
                    },
                },
                RirStmt::DynMatch(RirDynMatch {
                    carrier,
                    source: RirDynMatchSource::Owned(owned(local(0, ty))),
                    arms: vec![],
                    fallback_binding: RirDynMatchFallbackBinding::Take(RirLocalId::from_index(1)),
                    fallback: RirStructuredBlock {
                        stmts: vec![RirStmt::Eval(call(local(1, ty), carrier, ty))],
                        term: RirTerm::Unreachable,
                    },
                }),
            ],
            term: RirTerm::Unreachable,
        };

        visit_block(&mut block, &mut [None; 3]);

        let RirStmt::DynMatch(match_) = &block.stmts[1] else {
            panic!("expected dynamic match");
        };
        let RirStmt::Eval(RirRValue::DynCall { exact_variant, .. }) = &match_.fallback.stmts[0]
        else {
            panic!("expected dynamic call");
        };
        assert_eq!(*exact_variant, Some(variant));
    }

    #[test]
    fn dataref_set_annotations_are_canonicalized() {
        let ty = RirTypeId::from_index(0);
        let carrier = RirDynCarrierId::from_index(0);
        let expected = RirDynVariantId::from_index(0);
        let forged = RirDynVariantId::from_index(1);
        let mut block = RirStructuredBlock {
            stmts: vec![
                RirStmt::Init {
                    local: RirLocalId::from_index(0),
                    value: RirRValue::DynPack {
                        carrier,
                        variant: expected,
                        air_witness: air::ContractWitnessId::from_index(0),
                        value: owned(local(2, ty)),
                        ty,
                    },
                },
                RirStmt::DataRefSet {
                    object: local(2, ty),
                    dataref: RirDataRefId::from_index(0),
                    projections: vec![RirProjection::Field(RirFieldId::from_index(0))],
                    value: call(local(0, ty), carrier, ty),
                    ty,
                },
            ],
            term: RirTerm::Unreachable,
        };
        let RirStmt::DataRefSet {
            value: RirRValue::DynCall { exact_variant, .. },
            ..
        } = &mut block.stmts[1]
        else {
            panic!("expected dynamic dataref write");
        };
        *exact_variant = Some(forged);

        let mut annotations = vec![];
        collect_annotations(&block, &mut annotations);
        assert_eq!(annotations, [Some(forged)]);

        clear_block(&mut block);
        let mut annotations = vec![];
        collect_annotations(&block, &mut annotations);
        assert_eq!(annotations, [None]);

        visit_block(&mut block, &mut [None; 3]);
        let mut annotations = vec![];
        collect_annotations(&block, &mut annotations);
        assert_eq!(annotations, [Some(expected)]);
    }
}
