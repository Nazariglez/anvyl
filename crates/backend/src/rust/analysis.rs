use std::collections::HashSet;

use super::{
    native_call::NativeArgAction,
    rir::{
        RirCallArg, RirCallTarget, RirChild, RirCollectionAccess, RirCollectionLoanScope,
        RirCollectionRootKind, RirDynReceiver, RirFunction, RirMutPlaceAccess, RirMutPlaceArg,
        RirMutPlaceHandle, RirOperand, RirOptionPayloadBinding, RirOptionSubject, RirOwnedOperand,
        RirPassMode, RirPlace, RirPlaceRoot, RirProgram, RirRValue, RirRangeFor, RirRawEnumValue,
        RirResolvedCallTarget, RirStmt, RirStringLiteralId, RirStringifyHelper,
        RirStringifyHelperKind, RirStringifyReqKind, RirStructuredBlock, RirType, RirTypeId,
        stmt_child_blocks_any,
    },
};

pub(super) fn owned_string_literals(program: &RirProgram) -> HashSet<RirStringLiteralId> {
    let mut owned = HashSet::new();
    for helper in &program.stringify_helpers {
        match &helper.kind {
            RirStringifyHelperKind::Struct(_) => {}
            RirStringifyHelperKind::Enum { variants, .. } => {
                for variant in variants {
                    owned.insert(variant.label);
                    owned.extend(&variant.field_labels);
                }
            }
            RirStringifyHelperKind::Flag { empty, members, .. } => {
                owned.insert(*empty);
                owned.extend(members);
            }
        }
    }
    for function in &program.functions {
        collect_block_string_literals(program, function, &function.body, &mut owned);
    }
    owned
}

fn collect_block_string_literals(
    program: &RirProgram,
    function: &RirFunction,
    block: &RirStructuredBlock,
    owned: &mut HashSet<RirStringLiteralId>,
) {
    for stmt in &block.stmts {
        match stmt {
            RirStmt::Init { value, .. }
            | RirStmt::GlobalSetRoot { value, .. }
            | RirStmt::GlobalUpdateRoot { value, .. }
            | RirStmt::MutPlaceSet { value, .. }
            | RirStmt::Assign { value, .. }
            | RirStmt::CellInit { value, .. }
            | RirStmt::CellSet { value, .. }
            | RirStmt::ScopedPlaceCellSet { value, .. }
            | RirStmt::Eval(value) => {
                collect_rvalue_string_literals(program, function, value, owned);
            }
            RirStmt::DataRefSet { object, value, .. } => {
                collect_operand_string_literal(program, object, owned);
                collect_rvalue_string_literals(program, function, value, owned);
            }
            RirStmt::SequenceSlotSet { value, .. } | RirStmt::MapValueSet { value, .. } => {
                collect_operand_string_literal(program, value, owned);
            }
            RirStmt::MapEntryMatch(match_) => {
                collect_operand_string_literal(program, &match_.key, owned);
            }
            RirStmt::GlobalEnsure { .. }
            | RirStmt::ScopedPlaceCellInit { .. }
            | RirStmt::If(_)
            | RirStmt::Loop(_)
            | RirStmt::RangeFor(_)
            | RirStmt::CollectionFor(_)
            | RirStmt::CollectionLoanScope(_)
            | RirStmt::CollectionSlotScope(_)
            | RirStmt::PatternMatch(_)
            | RirStmt::DynMatch(_)
            | RirStmt::OptionMatch(_) => {}
        }
        stmt.for_each_child(&mut |child| {
            if let RirChild::Block(child) = child {
                collect_block_string_literals(program, function, child, owned);
            }
        });
    }
    match &block.term {
        super::rir::RirTerm::Return(Some(value)) => {
            collect_operand_string_literal(program, value, owned);
        }
        super::rir::RirTerm::ReturnOwned(owned_value) => {
            if let RirOwnedOperand::Value(value) = &owned_value.value {
                collect_operand_string_literal(program, value, owned);
            }
        }
        _ => {}
    }
}

fn collect_rvalue_string_literals(
    program: &RirProgram,
    function: &RirFunction,
    value: &RirRValue,
    owned: &mut HashSet<RirStringLiteralId>,
) {
    match value {
        RirRValue::StringConcat { .. } | RirRValue::RawTryConstruct { .. } => return,
        RirRValue::Format { source_ty, .. }
            if matches!(program.types.get(source_ty.index()), Some(RirType::String)) =>
        {
            return;
        }
        RirRValue::RawProject { value, target }
            if raw_string_projection_uses_statics(program, function, value, *target) =>
        {
            let Some(source) = operand_ty(program, function, value) else {
                return;
            };
            let Some(RirType::Enum(id)) = program.types.get(source.index()) else {
                return;
            };
            let Some(enm) = program.enums.get(id.index()) else {
                return;
            };
            for variant in &enm.variants {
                if let Some(RirRawEnumValue::String(id)) = variant.raw_value {
                    owned.insert(id);
                }
            }
        }
        _ => {}
    }
    value.for_each_child(&mut |child| match child {
        RirChild::Operand { operand, .. } => {
            collect_operand_string_literal(program, operand, owned);
        }
        RirChild::CallArg(arg) => collect_call_arg_string_literals(program, arg, owned),
        RirChild::CaptureArg(super::rir::RirLambdaCaptureArg::Owned { value }) => {
            collect_owned_string_literal(program, value, owned);
        }
        RirChild::Place { .. }
        | RirChild::MutPlace { .. }
        | RirChild::Collection { .. }
        | RirChild::CaptureArg(_)
        | RirChild::LocalRead(_)
        | RirChild::Block(_) => {}
    });
}

fn collect_call_arg_string_literals(
    program: &RirProgram,
    arg: &RirCallArg,
    owned: &mut HashSet<RirStringLiteralId>,
) {
    match arg {
        RirCallArg::Value(value)
        | RirCallArg::InitFieldProvided(value)
        | RirCallArg::ScopedLambda { callee: value, .. }
        | RirCallArg::EscapingLambda { callee: value, .. }
        | RirCallArg::AnvCallback { callee: value, .. } => {
            collect_owned_string_literal(program, value, owned);
        }
        RirCallArg::InitFieldOmitted
        | RirCallArg::SharedBorrow(_)
        | RirCallArg::SharedStringConst(_)
        | RirCallArg::MutBorrow(_)
        | RirCallArg::MutPlace(_)
        | RirCallArg::DynBorrow(_) => {}
    }
}

fn collect_owned_string_literal(
    program: &RirProgram,
    value: &super::rir::RirOwnedValue,
    owned: &mut HashSet<RirStringLiteralId>,
) {
    if let RirOwnedOperand::Value(operand) = &value.value {
        collect_operand_string_literal(program, operand, owned);
    }
}

fn collect_operand_string_literal(
    program: &RirProgram,
    operand: &RirOperand,
    owned: &mut HashSet<RirStringLiteralId>,
) {
    if let RirOperand::Const(id) = operand {
        collect_const_string_literal(program, *id, owned);
    }
}

fn collect_const_string_literal(
    program: &RirProgram,
    id: super::rir::RirConstId,
    owned: &mut HashSet<RirStringLiteralId>,
) {
    if let Some(super::rir::RirConstValue::String(id)) =
        program.consts.get(id.index()).map(|konst| &konst.value)
    {
        owned.insert(*id);
    }
}

pub(super) fn fallible_functions(program: &RirProgram) -> Vec<bool> {
    let mut fallible = vec![false; program.functions.len()];
    for global in &program.globals {
        if let Some(slot) = fallible.get_mut(global.init.index()) {
            *slot = true;
        }
    }
    loop {
        let mut changed = false;
        for function in &program.functions {
            let is_fallible = block_calls_fallible(program, &fallible, function, &function.body);
            let slot = &mut fallible[function.id.index()];
            if is_fallible && !*slot {
                *slot = true;
                changed = true;
            }
        }
        if !changed {
            return fallible;
        }
    }
}

fn block_calls_fallible(
    program: &RirProgram,
    fallible: &[bool],
    function: &RirFunction,
    block: &RirStructuredBlock,
) -> bool {
    block
        .stmts
        .iter()
        .any(|stmt| stmt_calls_fallible(program, fallible, function, stmt))
        || term_calls_fallible(&block.term)
}

fn stmt_calls_fallible(
    program: &RirProgram,
    fallible: &[bool],
    function: &RirFunction,
    stmt: &RirStmt,
) -> bool {
    match stmt {
        RirStmt::Init { value, .. } | RirStmt::CellInit { value, .. } | RirStmt::Eval(value) => {
            rvalue_calls_fallible(program, fallible, function, value)
        }
        RirStmt::GlobalEnsure { .. }
        | RirStmt::GlobalSetRoot { .. }
        | RirStmt::GlobalUpdateRoot { .. }
        | RirStmt::MutPlaceSet { .. }
        | RirStmt::CellSet { .. }
        | RirStmt::ScopedPlaceCellSet { .. }
        | RirStmt::DataRefSet { .. }
        | RirStmt::SequenceSlotSet { .. }
        | RirStmt::MapValueSet { .. }
        | RirStmt::MapEntryMatch(_) => true,
        RirStmt::Assign { dst, value } => {
            place_is_mut_place_param(function, dst)
                || place_has_fallible_projection(dst)
                || program.collection_replace_ty(program.verified_place_ty(function, dst))
                || rvalue_calls_fallible(program, fallible, function, value)
        }
        RirStmt::CollectionLoanScope(scope) => {
            block_calls_fallible(program, fallible, function, &scope.body)
                || loan_scope_is_fallible(program, function, scope)
        }
        RirStmt::RangeFor(range) => {
            range_for_operands(range)
                .into_iter()
                .any(operand_has_fallible_place)
                || block_calls_fallible(program, fallible, function, &range.body)
        }
        RirStmt::CollectionFor(for_) => {
            for_.ordinal_plan.operands().any(operand_has_fallible_place)
                || block_calls_fallible(program, fallible, function, &for_.body)
        }
        RirStmt::PatternMatch(match_) => {
            place_has_fallible_projection(&match_.subject)
                || stmt_child_blocks_any(stmt, |block| {
                    block_calls_fallible(program, fallible, function, block)
                })
        }
        RirStmt::DynMatch(match_) => {
            let source = match &match_.source {
                super::rir::RirDynMatchSource::Owned(value) => {
                    owned_value_has_fallible_place(value)
                }
                super::rir::RirDynMatchSource::MutPlace(_)
                | super::rir::RirDynMatchSource::Borrowed(_) => true,
            };
            source
                || stmt_child_blocks_any(stmt, |block| {
                    block_calls_fallible(program, fallible, function, block)
                })
        }
        RirStmt::OptionMatch(match_) => {
            match_.payload.is_some_and(RirOptionPayloadBinding::is_ref)
                || option_subject_fallible(&match_.subject)
                || stmt_child_blocks_any(stmt, |block| {
                    block_calls_fallible(program, fallible, function, block)
                })
        }
        _ => stmt_child_blocks_any(stmt, |block| {
            block_calls_fallible(program, fallible, function, block)
        }),
    }
}

fn term_calls_fallible(term: &super::rir::RirTerm) -> bool {
    match term {
        super::rir::RirTerm::Return(Some(operand)) => operand_has_fallible_place(operand),
        super::rir::RirTerm::ReturnOwned(owned) => match &owned.value {
            RirOwnedOperand::Value(value) => operand_has_fallible_place(value),
            RirOwnedOperand::Access(_) | RirOwnedOperand::DynBorrow(_) => true,
        },
        super::rir::RirTerm::None
        | super::rir::RirTerm::Return(None)
        | super::rir::RirTerm::Break(_)
        | super::rir::RirTerm::Continue(_)
        | super::rir::RirTerm::Unreachable => false,
    }
}

fn dyn_receiver_uses_descriptor(receiver: &RirDynReceiver) -> bool {
    matches!(
        receiver,
        RirDynReceiver::MutPlace(_) | RirDynReceiver::Borrowed(_)
    )
}

fn rvalue_calls_fallible(
    program: &RirProgram,
    fallible: &[bool],
    function: &RirFunction,
    value: &RirRValue,
) -> bool {
    rvalue_uses_mut_place_param(function, value)
        || rvalue_uses_fallible_place(value)
        || match value {
            RirRValue::Cast { value, target } => {
                let Some(source) = operand_ty(program, function, value) else {
                    return true;
                };
                matches!(
                    (
                        &program.types[source.index()],
                        &program.types[target.index()]
                    ),
                    (RirType::Float, RirType::Int)
                )
            }
            RirRValue::Materialize(super::rir::RirOwnedValue {
                value: RirOwnedOperand::Access(_),
                ..
            })
            | RirRValue::ListPush { .. }
            | RirRValue::RangeListCopy { .. }
            | RirRValue::MapGet { .. }
            | RirRValue::MapInsert { .. }
            | RirRValue::MapRemove { .. }
            | RirRValue::MapEntryAt { .. }
            | RirRValue::MapKeyAt { .. }
            | RirRValue::MapValueAt { .. }
            | RirRValue::SequenceSlotAt { .. }
            | RirRValue::SliceView { .. }
            | RirRValue::CheckedIterCount { .. } => true,
            RirRValue::DynCall {
                dispatch,
                receiver,
                args,
                ..
            } => {
                let Some(dispatch) = program.dyn_dispatches.get(dispatch.index()) else {
                    return true;
                };
                dyn_receiver_uses_descriptor(receiver)
                    || args.iter().any(call_arg_preparation_fallible)
                    || dispatch.arms.iter().any(|arm| {
                        dynamic_receiver_preparation_fallible(receiver, arm.receiver)
                            .unwrap_or(true)
                            || dynamic_target_calls_fallible(
                                program,
                                fallible,
                                arm.target.base(),
                                args,
                            )
                    })
            }
            RirRValue::Call { callee, args, .. } => {
                args.iter().any(call_arg_preparation_fallible)
                    || call_target_calls_fallible(program, fallible, callee, args)
            }
            RirRValue::Stringify { source_ty, .. } => {
                stringify_calls_fallible(program, fallible, *source_ty)
            }
            _ => false,
        }
}

fn call_target_calls_fallible(
    program: &RirProgram,
    fallible: &[bool],
    callee: &RirCallTarget,
    args: &[RirCallArg],
) -> bool {
    match callee {
        RirCallTarget::Function(id) => fallible[id.index()],
        RirCallTarget::Extern(id) => {
            let ext = &program.externs[id.index()];
            ext.suspends_runtime_entry
                || ext.fallible
                || native_ref_borrow_conversion_fallible(program, *id, args)
        }
        RirCallTarget::LambdaValue { sig, .. } => program
            .lambdas_for_sig(*sig)
            .any(|lambda| fallible[lambda.function.index()]),
    }
}

fn dynamic_receiver_preparation_fallible(
    receiver: &RirDynReceiver,
    mode: RirPassMode,
) -> Option<bool> {
    match (receiver, mode) {
        (RirDynReceiver::Owned(_), RirPassMode::Value)
        | (
            RirDynReceiver::Owned(super::rir::RirOwnedValue {
                value: RirOwnedOperand::Value(RirOperand::Place(_)),
                source: super::rir::RirOwnedSource::Reuse(_),
            }),
            RirPassMode::SharedBorrow,
        )
        | (
            RirDynReceiver::Borrowed(_),
            RirPassMode::Value
            | RirPassMode::SharedBorrow
            | RirPassMode::MutBorrow
            | RirPassMode::MutPlace,
        ) => Some(false),
        (RirDynReceiver::MutPlace(place), RirPassMode::MutBorrow | RirPassMode::MutPlace) => {
            Some(mut_place_preparation_fallible(place))
        }
        _ => None,
    }
}

fn dynamic_target_calls_fallible(
    program: &RirProgram,
    fallible: &[bool],
    target: &RirResolvedCallTarget,
    args: &[RirCallArg],
) -> bool {
    match target {
        RirResolvedCallTarget::Function(id) => fallible[id.index()],
        RirResolvedCallTarget::Extern(id) => {
            let ext = &program.externs[id.index()];
            ext.suspends_runtime_entry
                || ext.fallible
                || matches!(
                    ext.params[0].action,
                    NativeArgAction::NativeRefBorrow { .. }
                )
                || args.iter().enumerate().any(|(index, arg)| {
                    matches!(
                        ext.arg_action(index + 1, arg),
                        NativeArgAction::NativeRefBorrow { .. }
                    )
                })
        }
        RirResolvedCallTarget::Promoted { .. } => unreachable!(),
    }
}

fn rvalue_uses_fallible_place(value: &RirRValue) -> bool {
    let mut fallible = false;
    value.for_each_child(&mut |child| {
        fallible |= match child {
            RirChild::Operand { operand, .. } => operand_has_fallible_place(operand),
            RirChild::Place { place, .. } => place_has_fallible_projection(place),
            RirChild::MutPlace { place, .. } => mut_place_preparation_fallible(place),
            RirChild::Collection { collection, .. } => collection_access_fallible(collection),
            RirChild::CallArg(arg) => call_arg_has_fallible_place(arg),
            RirChild::CaptureArg(capture) => match capture {
                super::rir::RirLambdaCaptureArg::Owned { value } => {
                    owned_value_has_fallible_place(value)
                }
                super::rir::RirLambdaCaptureArg::Shared { place } => {
                    place_has_fallible_projection(place)
                }
                super::rir::RirLambdaCaptureArg::StackCell { .. }
                | super::rir::RirLambdaCaptureArg::HeapCell { .. }
                | super::rir::RirLambdaCaptureArg::ScopedPlaceCell { .. } => false,
            },
            RirChild::LocalRead(_) | RirChild::Block(_) => false,
        };
    });
    fallible
}

fn operand_has_fallible_place(operand: &RirOperand) -> bool {
    match operand {
        RirOperand::Place(place) => {
            matches!(place.root, RirPlaceRoot::Global(_)) || place_has_fallible_projection(place)
        }
        RirOperand::Const(_) => false,
    }
}

fn owned_value_has_fallible_place(value: &super::rir::RirOwnedValue) -> bool {
    match &value.value {
        RirOwnedOperand::Value(operand) => operand_has_fallible_place(operand),
        RirOwnedOperand::Access(place) => mut_place_preparation_fallible(place),
        RirOwnedOperand::DynBorrow(_) => true,
    }
}

fn call_arg_has_fallible_place(arg: &RirCallArg) -> bool {
    match arg {
        RirCallArg::Value(operand)
        | RirCallArg::InitFieldProvided(operand)
        | RirCallArg::ScopedLambda {
            callee: operand, ..
        }
        | RirCallArg::EscapingLambda {
            callee: operand, ..
        }
        | RirCallArg::AnvCallback {
            callee: operand, ..
        } => owned_value_has_fallible_place(operand),
        RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
            place_has_fallible_projection(place)
        }
        RirCallArg::SharedStringConst(_)
        | RirCallArg::MutPlace(_)
        | RirCallArg::DynBorrow(_)
        | RirCallArg::InitFieldOmitted => false,
    }
}

fn place_has_fallible_projection(place: &RirPlace) -> bool {
    place.projections.iter().any(|step| {
        matches!(
            step.kind,
            super::rir::RirPlaceStepKind::ArrayIndex { .. }
                | super::rir::RirPlaceStepKind::ListIndex { .. }
                | super::rir::RirPlaceStepKind::SliceIndex { .. }
        )
    })
}

fn collection_access_fallible(access: &RirCollectionAccess) -> bool {
    match access {
        RirCollectionAccess::Direct(place) => {
            matches!(place.root, RirPlaceRoot::Global(_)) || place_has_fallible_projection(place)
        }
        RirCollectionAccess::MutPlace(_) => true,
    }
}

fn operand_ty(
    program: &RirProgram,
    function: &RirFunction,
    operand: &RirOperand,
) -> Option<RirTypeId> {
    match operand {
        RirOperand::Place(place) => Some(program.verified_place_ty(function, place)),
        RirOperand::Const(id) => program.consts.get(id.index()).map(|konst| konst.ty),
    }
}

fn loan_scope_is_fallible(
    program: &RirProgram,
    function: &RirFunction,
    scope: &RirCollectionLoanScope,
) -> bool {
    scope.root_kind.tracks_shape_loan()
        && matches!(
            (
                scope.root_kind,
                program.types[program
                    .verified_collection_ty(function, &scope.root)
                    .index()]
            ),
            (RirCollectionRootKind::List, RirType::List(_))
                | (RirCollectionRootKind::Map, RirType::Map { .. })
        )
}

fn stringify_calls_fallible(program: &RirProgram, fallible: &[bool], ty: RirTypeId) -> bool {
    let Some(req) = program.stringify_req(ty) else {
        return false;
    };
    match req.kind {
        RirStringifyReqKind::Override { function, .. } => fallible[function.index()],
        RirStringifyReqKind::Helper(helper) => stringify_helper_fallible(
            program,
            fallible,
            &program.stringify_helpers[helper.index()],
        ),
    }
}

pub(super) fn stringify_helper_fallible(
    program: &RirProgram,
    fallible: &[bool],
    helper: &RirStringifyHelper,
) -> bool {
    match &helper.kind {
        RirStringifyHelperKind::Struct(strukt) => program.structs[strukt.index()]
            .fields
            .iter()
            .any(|field| stringify_calls_fallible(program, fallible, field.ty)),
        RirStringifyHelperKind::Enum { enm, .. } => program.enums[enm.index()]
            .variants
            .iter()
            .flat_map(|variant| &variant.fields)
            .any(|field| stringify_calls_fallible(program, fallible, field.ty)),
        RirStringifyHelperKind::Flag { .. } => false,
    }
}

fn native_ref_borrow_conversion_fallible(
    program: &RirProgram,
    ext: super::rir::RirExternId,
    args: &[RirCallArg],
) -> bool {
    let ext = &program.externs[ext.index()];
    args.iter().enumerate().any(|(index, arg)| {
        matches!(
            ext.arg_action(index, arg),
            NativeArgAction::NativeRefBorrow { .. }
        )
    })
}

fn range_for_operands(range: &RirRangeFor) -> impl Iterator<Item = &RirOperand> {
    std::iter::once(&range.start)
        .chain(std::iter::once(&range.end))
        .chain(range.ordinal_plan.operands())
}

fn raw_string_projection_uses_statics(
    program: &RirProgram,
    function: &RirFunction,
    operand: &RirOperand,
    target: RirTypeId,
) -> bool {
    if !matches!(program.types.get(target.index()), Some(RirType::String)) {
        return false;
    }
    let Some(source) = operand_ty(program, function, operand) else {
        return false;
    };
    let Some(RirType::Enum(id)) = program.types.get(source.index()) else {
        return false;
    };
    program.enums.get(id.index()).is_some_and(|enm| {
        enm.repr == super::rir::RirEnumRepr::RawString && !enm.variants.is_empty()
    })
}

fn call_arg_preparation_fallible(arg: &RirCallArg) -> bool {
    let RirCallArg::MutPlace(arg) = arg else {
        return false;
    };
    mut_place_preparation_fallible(arg)
}

fn option_subject_fallible(subject: &RirOptionSubject) -> bool {
    match subject {
        RirOptionSubject::Place(place) => place_has_fallible_projection(place),
        RirOptionSubject::MutPlace(_) => true,
    }
}

fn mut_place_preparation_fallible(arg: &RirMutPlaceArg) -> bool {
    match arg.access {
        RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { .. })
        | RirMutPlaceAccess::DataRef { .. } => true,
        RirMutPlaceAccess::Handle(
            RirMutPlaceHandle::Local { .. }
            | RirMutPlaceHandle::Param { .. }
            | RirMutPlaceHandle::StackCell { .. }
            | RirMutPlaceHandle::HeapCell { .. }
            | RirMutPlaceHandle::ScopedPlaceCell { .. },
        ) => arg.projections.iter().any(|step| {
            matches!(
                step.kind,
                super::rir::RirPlaceStepKind::ArrayIndex { .. }
                    | super::rir::RirPlaceStepKind::ListIndex { .. }
                    | super::rir::RirPlaceStepKind::SliceIndex { .. }
            )
        }),
    }
}

fn mut_place_uses_mut_place_param(function: &RirFunction, arg: &RirMutPlaceArg) -> bool {
    match arg.access {
        RirMutPlaceAccess::Handle(RirMutPlaceHandle::Param { local }) => matches!(
            function.locals[local.index()].binding,
            super::rir::RirLocalBinding::Parameter {
                mode: RirPassMode::MutPlace,
                ..
            }
        ),
        _ => false,
    }
}

fn call_arg_uses_mut_place_param(function: &RirFunction, arg: &RirCallArg) -> bool {
    match arg {
        RirCallArg::Value(operand)
        | RirCallArg::InitFieldProvided(operand)
        | RirCallArg::ScopedLambda {
            callee: operand, ..
        }
        | RirCallArg::EscapingLambda {
            callee: operand, ..
        }
        | RirCallArg::AnvCallback {
            callee: operand, ..
        } => owned_value_uses_mut_place_param(function, operand),
        RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
            place_is_mut_place_param(function, place)
        }
        RirCallArg::SharedStringConst(_)
        | RirCallArg::MutPlace(_)
        | RirCallArg::DynBorrow(_)
        | RirCallArg::InitFieldOmitted => false,
    }
}

fn owned_value_uses_mut_place_param(
    function: &RirFunction,
    value: &super::rir::RirOwnedValue,
) -> bool {
    match &value.value {
        RirOwnedOperand::Value(operand) => operand_uses_mut_place_param(function, operand),
        RirOwnedOperand::Access(place) => mut_place_uses_mut_place_param(function, place),
        RirOwnedOperand::DynBorrow(_) => false,
    }
}

fn operand_uses_mut_place_param(function: &RirFunction, operand: &RirOperand) -> bool {
    match operand {
        RirOperand::Place(place) => place_is_mut_place_param(function, place),
        RirOperand::Const(_) => false,
    }
}

fn place_is_mut_place_param(function: &RirFunction, place: &RirPlace) -> bool {
    let RirPlaceRoot::Local(local) = place.root else {
        return false;
    };
    local_is_mut_place_param(function, local)
}

fn local_is_mut_place_param(function: &RirFunction, local: super::rir::RirLocalId) -> bool {
    matches!(
        function.locals[local.index()].binding,
        super::rir::RirLocalBinding::Parameter {
            mode: RirPassMode::MutPlace,
            ..
        }
    )
}

fn collection_access_uses_mut_place_param(
    function: &RirFunction,
    access: &RirCollectionAccess,
) -> bool {
    match access {
        RirCollectionAccess::Direct(place) => place_is_mut_place_param(function, place),
        RirCollectionAccess::MutPlace(place) => mut_place_uses_mut_place_param(function, place),
    }
}

fn rvalue_uses_mut_place_param(function: &RirFunction, value: &RirRValue) -> bool {
    let mut uses = false;
    value.for_each_child(&mut |child| {
        uses |= match child {
            RirChild::Operand { operand, .. } => operand_uses_mut_place_param(function, operand),
            RirChild::Place { place, .. } => place_is_mut_place_param(function, place),
            RirChild::MutPlace { place, .. } => mut_place_uses_mut_place_param(function, place),
            RirChild::Collection { collection, .. } => {
                collection_access_uses_mut_place_param(function, collection)
            }
            RirChild::CallArg(arg) => call_arg_uses_mut_place_param(function, arg),
            RirChild::CaptureArg(capture) => match capture {
                super::rir::RirLambdaCaptureArg::Owned { value } => {
                    owned_value_uses_mut_place_param(function, value)
                }
                super::rir::RirLambdaCaptureArg::Shared { place } => {
                    place_is_mut_place_param(function, place)
                }
                super::rir::RirLambdaCaptureArg::StackCell { .. }
                | super::rir::RirLambdaCaptureArg::HeapCell { .. }
                | super::rir::RirLambdaCaptureArg::ScopedPlaceCell { .. } => false,
            },
            RirChild::LocalRead(_) | RirChild::Block(_) => false,
        };
    });
    uses
}
