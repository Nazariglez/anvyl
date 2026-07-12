use super::{
    native_call::NativeArgAction,
    place::{mut_place_dynamic_facts, place_dynamic_facts},
    rir::{
        RirCallArg, RirCallTarget, RirCellRef, RirCellStorage, RirChild, RirCollectionAccess,
        RirCollectionLoanScope, RirCollectionRootKind, RirDynReceiver, RirExternKind, RirFunction,
        RirLambdaStorage, RirMutPlaceAccess, RirMutPlaceArg, RirMutPlaceHandle, RirOperand,
        RirOptionSubject, RirParamAbi, RirPlace, RirPlaceRoot, RirProgram, RirRValue, RirRangeFor,
        RirResolvedCallTarget, RirStmt, RirStringifyReqKind, RirStruct, RirStructuredBlock,
        RirType, RirTypeId, native_arg_facts, native_return_adopts_resource, stmt_child_blocks_any,
    },
};

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
        || term_calls_fallible(program, function, &block.term)
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
        | RirStmt::SequenceSlotSet { .. }
        | RirStmt::MapValueSet { .. }
        | RirStmt::MapEntryMatch(_) => true,
        RirStmt::Assign { dst, value } => {
            place_is_mut_place_param(function, dst)
                || place_has_fallible_projection(program, function, dst)
                || program.collection_replace_ty(dst.ty)
                || rvalue_calls_fallible(program, fallible, function, value)
        }
        RirStmt::CollectionLoanScope(scope) => {
            block_calls_fallible(program, fallible, function, &scope.body)
                || loan_scope_is_fallible(program, scope)
        }
        RirStmt::RangeFor(range) => {
            range_for_operands(range)
                .into_iter()
                .any(|operand| operand_has_fallible_place(program, function, operand))
                || block_calls_fallible(program, fallible, function, &range.body)
        }
        RirStmt::CollectionFor(for_) => {
            for_.ordinal_plan
                .operands()
                .any(|operand| operand_has_fallible_place(program, function, operand))
                || block_calls_fallible(program, fallible, function, &for_.body)
        }
        RirStmt::DataRefSet { object, value, .. } => {
            operand_uses_mut_place_param(function, object)
                || operand_uses_mut_place_param(function, value)
                || operand_has_fallible_place(program, function, object)
                || operand_has_fallible_place(program, function, value)
                || operand_ty(program, value).is_some_and(|ty| program.collection_replace_ty(ty))
        }
        RirStmt::PatternMatch(match_) => {
            place_has_fallible_projection(program, function, &match_.subject)
                || stmt_child_blocks_any(stmt, |block| {
                    block_calls_fallible(program, fallible, function, block)
                })
        }
        RirStmt::DynMatch(match_) => {
            let source = match &match_.source {
                super::rir::RirDynMatchSource::Owned { value, .. } => {
                    operand_has_fallible_place(program, function, value)
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
            match_.payload_ref
                || option_subject_fallible(program, function, &match_.subject)
                || stmt_child_blocks_any(stmt, |block| {
                    block_calls_fallible(program, fallible, function, block)
                })
        }
        _ => stmt_child_blocks_any(stmt, |block| {
            block_calls_fallible(program, fallible, function, block)
        }),
    }
}

fn term_calls_fallible(
    program: &RirProgram,
    function: &RirFunction,
    term: &super::rir::RirTerm,
) -> bool {
    match term {
        super::rir::RirTerm::Return(Some(operand)) => {
            operand_has_fallible_place(program, function, operand)
        }
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
        || rvalue_uses_fallible_place(program, function, value)
        || match value {
            RirRValue::ListPush { .. }
            | RirRValue::RangeListCopy { .. }
            | RirRValue::MapGet { .. }
            | RirRValue::MapInsert { .. }
            | RirRValue::MapRemove { .. }
            | RirRValue::MapEntryAt { .. }
            | RirRValue::MapKeyAt { .. }
            | RirRValue::MapValueAt { .. }
            | RirRValue::SequenceSlotAt { .. }
            | RirRValue::SliceView { .. }
            | RirRValue::CellGetCopy { .. }
            | RirRValue::ScopedPlaceCellGet { .. }
            | RirRValue::MutPlaceGetCopy { .. }
            | RirRValue::CheckedIterCount { .. } => true,
            RirRValue::DynCall {
                receiver,
                arms,
                args,
                ..
            } => {
                dyn_receiver_uses_descriptor(receiver)
                    || arms.iter().any(|arm| {
                        let Some(call_args) = dynamic_call_args(receiver, arm.receiver, args)
                        else {
                            return true;
                        };
                        call_args
                            .iter()
                            .any(|arg| call_arg_preparation_fallible(program, arg))
                            || {
                                let target = match arm.target.base() {
                                    RirResolvedCallTarget::Function(id) => {
                                        RirCallTarget::Function(*id)
                                    }
                                    RirResolvedCallTarget::Extern(id) => RirCallTarget::Extern(*id),
                                    RirResolvedCallTarget::Promoted { .. } => unreachable!(),
                                };
                                call_target_calls_fallible(program, fallible, &target, &call_args)
                            }
                    })
            }
            RirRValue::Call { callee, args, .. } => {
                args.iter()
                    .any(|arg| call_arg_preparation_fallible(program, arg))
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
        RirCallTarget::Extern(id) => match &program.externs[id.index()].kind {
            RirExternKind::Native(native) => {
                program
                    .native_call_plan(*id)
                    .provider_entry()
                    .suspends_runtime_entry()
                    || native.abi.fallible
                    || native_ref_borrow_conversion_fallible(program, *id, args)
            }
        },
        RirCallTarget::LambdaValue { sig, .. } => program
            .lambdas_for_sig(*sig)
            .any(|lambda| fallible[lambda.function.index()]),
    }
}

fn dynamic_call_args(
    receiver: &RirDynReceiver,
    abi: RirParamAbi,
    args: &[RirCallArg],
) -> Option<Vec<RirCallArg>> {
    let receiver = match (receiver, abi) {
        (RirDynReceiver::Owned { value, .. }, RirParamAbi::Value) => {
            RirCallArg::Value(value.clone())
        }
        (
            RirDynReceiver::Owned {
                value: RirOperand::Place(place),
                ..
            },
            RirParamAbi::SharedBorrow,
        ) => RirCallArg::SharedBorrow(place.clone()),
        (
            RirDynReceiver::Borrowed(borrow),
            RirParamAbi::Value
            | RirParamAbi::SharedBorrow
            | RirParamAbi::MutBorrow
            | RirParamAbi::MutPlace,
        ) => RirCallArg::DynBorrow(borrow.clone()),
        (RirDynReceiver::MutPlace(place), RirParamAbi::MutBorrow | RirParamAbi::MutPlace) => {
            RirCallArg::MutPlace(place.clone())
        }
        _ => return None,
    };
    Some(
        std::iter::once(receiver)
            .chain(args.iter().cloned())
            .collect(),
    )
}

fn rvalue_uses_fallible_place(
    program: &RirProgram,
    function: &RirFunction,
    value: &RirRValue,
) -> bool {
    let mut fallible = false;
    value.for_each_child(super::rir::RirValueUse::Read, &mut |child| {
        fallible |= match child {
            RirChild::Operand { operand, .. } => {
                operand_has_fallible_place(program, function, operand)
            }
            RirChild::Place { place, .. } => {
                place_has_fallible_projection(program, function, place)
            }
            RirChild::MutPlace { place, .. } => mut_place_preparation_fallible(program, place),
            RirChild::Collection { collection, .. } => {
                collection_access_fallible(program, function, collection)
            }
            RirChild::CallArg(arg) => call_arg_has_fallible_place(program, function, arg),
            RirChild::CaptureArg(capture) => match capture {
                super::rir::RirLambdaCaptureArg::Readonly { value } => {
                    operand_has_fallible_place(program, function, value)
                }
                super::rir::RirLambdaCaptureArg::Scoped { place } => {
                    place_has_fallible_projection(program, function, place)
                }
                super::rir::RirLambdaCaptureArg::StackCell { .. }
                | super::rir::RirLambdaCaptureArg::HeapCell { .. }
                | super::rir::RirLambdaCaptureArg::ScopedPlaceCell { .. } => false,
            },
            RirChild::LocalRead(_) | RirChild::Block(_) | RirChild::Tail(_) => false,
        };
    });
    fallible
}

fn operand_has_fallible_place(
    program: &RirProgram,
    function: &RirFunction,
    operand: &RirOperand,
) -> bool {
    match operand {
        RirOperand::Place(place) => {
            matches!(place.root, RirPlaceRoot::Global(_))
                || place_has_fallible_projection(program, function, place)
        }
        RirOperand::Const(_) => false,
    }
}

fn call_arg_has_fallible_place(
    program: &RirProgram,
    function: &RirFunction,
    arg: &RirCallArg,
) -> bool {
    match arg {
        RirCallArg::Value(operand)
        | RirCallArg::MovedValue { value: operand, .. }
        | RirCallArg::InitFieldProvided(operand)
        | RirCallArg::ScopedLambda {
            callee: operand, ..
        }
        | RirCallArg::EscapingLambda {
            callee: operand, ..
        }
        | RirCallArg::AnvCallback {
            callee: operand, ..
        } => operand_has_fallible_place(program, function, operand),
        RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
            place_has_fallible_projection(program, function, place)
        }
        RirCallArg::SharedStringConst(_)
        | RirCallArg::MutPlace(_)
        | RirCallArg::DynBorrow(_)
        | RirCallArg::InitFieldOmitted => false,
    }
}

fn place_has_fallible_projection(
    program: &RirProgram,
    function: &RirFunction,
    place: &RirPlace,
) -> bool {
    place_dynamic_facts(program, function, place).is_some_and(|facts| facts.fallible_projection)
}

fn collection_access_fallible(
    program: &RirProgram,
    function: &RirFunction,
    access: &RirCollectionAccess,
) -> bool {
    match access {
        RirCollectionAccess::Direct(place) => {
            matches!(place.root, RirPlaceRoot::Global(_))
                || place_has_fallible_projection(program, function, place)
        }
        RirCollectionAccess::MutPlace(_) => true,
    }
}

fn operand_ty(program: &RirProgram, operand: &RirOperand) -> Option<RirTypeId> {
    match operand {
        RirOperand::Place(place) => Some(place.ty),
        RirOperand::Const(id) => program.consts.get(id.index()).map(|konst| konst.ty),
    }
}

fn loan_scope_is_fallible(program: &RirProgram, scope: &RirCollectionLoanScope) -> bool {
    scope.root_kind.tracks_shape_loan()
        && matches!(
            (scope.root_kind, program.types[scope.root.ty().index()]),
            (RirCollectionRootKind::List, RirType::List(_))
                | (RirCollectionRootKind::Map, RirType::Map { .. })
        )
}

fn stringify_calls_fallible(program: &RirProgram, fallible: &[bool], ty: RirTypeId) -> bool {
    program
        .stringify_reqs
        .iter()
        .find(|req| req.ty == ty)
        .is_some_and(|req| match req.kind {
            RirStringifyReqKind::Override { function, .. } => fallible[function.index()],
            RirStringifyReqKind::Structural(_) => false,
        })
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct ContextUse {
    pub rt: bool,
    pub types: bool,
    pub globals: bool,
}

impl ContextUse {
    fn union(self, other: Self) -> Self {
        Self {
            rt: self.rt || other.rt,
            types: self.types || other.types,
            globals: self.globals || other.globals,
        }
    }

    fn rt() -> Self {
        Self {
            rt: true,
            ..Self::default()
        }
    }

    fn rt_types() -> Self {
        Self {
            rt: true,
            types: true,
            ..Self::default()
        }
    }

    fn globals() -> Self {
        Self {
            globals: true,
            ..Self::default()
        }
    }

    fn generated_call() -> Self {
        Self {
            rt: true,
            types: true,
            globals: true,
        }
    }
}

pub(super) fn function_context_use(program: &RirProgram, function: &RirFunction) -> ContextUse {
    block_context_use(program, function, &function.body)
}

pub(super) fn stringify_helper_context_use(program: &RirProgram, strukt: &RirStruct) -> ContextUse {
    if strukt
        .fields
        .iter()
        .any(|field| matches!(program.types[field.ty.index()], RirType::Struct(_)))
    {
        ContextUse::rt_types()
    } else {
        ContextUse::default()
    }
}

fn block_context_use(
    program: &RirProgram,
    function: &RirFunction,
    block: &RirStructuredBlock,
) -> ContextUse {
    let stmts = block
        .stmts
        .iter()
        .fold(ContextUse::default(), |uses, stmt| {
            uses.union(stmt_context_use(program, function, stmt))
        });
    stmts.union(term_context_use(program, function, &block.term))
}

fn stmt_context_use(program: &RirProgram, function: &RirFunction, stmt: &RirStmt) -> ContextUse {
    match stmt {
        RirStmt::Init { value, .. } | RirStmt::Eval(value) => {
            rvalue_context_use(program, function, value)
        }
        RirStmt::GlobalEnsure { .. } => ContextUse::generated_call(),
        RirStmt::GlobalSetRoot { value, .. } | RirStmt::GlobalUpdateRoot { value, .. } => {
            ContextUse::globals().union(rvalue_context_use(program, function, value))
        }
        RirStmt::MutPlaceSet { place, value } => ContextUse::rt()
            .union(mut_place_context_use(program, function, place))
            .union(rvalue_context_use(program, function, value)),
        RirStmt::CellInit { cell, value } | RirStmt::CellSet { cell, value } => {
            cell_context_use(program, *cell).union(rvalue_context_use(program, function, value))
        }
        RirStmt::Assign { dst, value } => place_context_use(program, function, dst)
            .union(rvalue_context_use(program, function, value)),
        RirStmt::ScopedPlaceCellSet { value, .. } => {
            ContextUse::rt().union(rvalue_context_use(program, function, value))
        }
        RirStmt::DataRefSet { object, value, .. } => ContextUse::rt_types()
            .union(operand_context_use(program, function, object))
            .union(operand_context_use(program, function, value)),
        RirStmt::SequenceSlotSet {
            collection, value, ..
        } => ContextUse::rt()
            .union(collection_access_context_use(program, function, collection))
            .union(operand_context_use(program, function, value)),
        RirStmt::MapValueSet { map, value, .. } => ContextUse::rt()
            .union(collection_access_context_use(program, function, map))
            .union(operand_context_use(program, function, value)),
        RirStmt::CollectionLoanScope(scope) => collection_loan_context_use(
            program, function, scope,
        )
        .union(block_context_use(program, function, &scope.body)),
        RirStmt::RangeFor(range) => {
            operands_context_use(program, function, range_for_operands(range))
                .union(block_context_use(program, function, &range.body))
        }
        RirStmt::CollectionFor(for_) => {
            operands_context_use(program, function, for_.ordinal_plan.operands())
                .union(block_context_use(program, function, &for_.body))
        }
        RirStmt::CollectionSlotScope(block) => block_context_use(program, function, block),
        RirStmt::PatternMatch(match_) => place_context_use(program, function, &match_.subject)
            .union(stmt_child_blocks_context_use(program, function, stmt)),
        RirStmt::DynMatch(match_) => {
            let source = match &match_.source {
                super::rir::RirDynMatchSource::Owned { value, .. } => {
                    operand_context_use(program, function, value)
                }
                super::rir::RirDynMatchSource::MutPlace(place) => {
                    mut_place_context_use(program, function, place).union(ContextUse::rt())
                }
                super::rir::RirDynMatchSource::Borrowed(_) => ContextUse::rt(),
            };
            source.union(stmt_child_blocks_context_use(program, function, stmt))
        }
        RirStmt::OptionMatch(match_) => {
            let subject = option_subject_context_use(program, function, &match_.subject);
            let payload = match_.payload_ref.then(ContextUse::rt).unwrap_or_default();
            subject
                .union(payload)
                .union(stmt_child_blocks_context_use(program, function, stmt))
        }
        RirStmt::MapEntryMatch(match_) => mut_place_context_use(program, function, &match_.map)
            .union(ContextUse::rt())
            .union(operand_context_use(program, function, &match_.key))
            .union(stmt_child_blocks_context_use(program, function, stmt)),
        _ => stmt_child_blocks_context_use(program, function, stmt),
    }
}

fn stmt_child_blocks_context_use(
    program: &RirProgram,
    function: &RirFunction,
    stmt: &RirStmt,
) -> ContextUse {
    let mut uses = ContextUse::default();
    let mut merge = |block: &RirStructuredBlock| {
        uses = uses.union(block_context_use(program, function, block));
    };
    stmt_child_blocks_any(stmt, |block| {
        merge(block);
        false
    });
    uses
}

fn collection_access_context_use(
    program: &RirProgram,
    function: &RirFunction,
    access: &RirCollectionAccess,
) -> ContextUse {
    match access {
        RirCollectionAccess::Direct(place) => place_context_use(program, function, place),
        RirCollectionAccess::MutPlace(place) => mut_place_context_use(program, function, place),
    }
}

fn collection_loan_context_use(
    program: &RirProgram,
    function: &RirFunction,
    scope: &RirCollectionLoanScope,
) -> ContextUse {
    match &scope.root {
        RirCollectionAccess::Direct(place) => match place.root {
            RirPlaceRoot::Global(_) => ContextUse::generated_call(),
            RirPlaceRoot::Local(_) if place_is_mut_place_param(function, place) => ContextUse::rt(),
            RirPlaceRoot::Local(_) => ContextUse::default(),
        },
        RirCollectionAccess::MutPlace(place) => mut_place_context_use(program, function, place),
    }
}

fn rvalue_context_use(
    program: &RirProgram,
    function: &RirFunction,
    value: &RirRValue,
) -> ContextUse {
    let mut uses = if rvalue_uses_mut_place_param(function, value) {
        ContextUse::rt()
    } else {
        ContextUse::default()
    };
    uses = uses.union(rvalue_operand_context_use(program, function, value));
    match value {
        RirRValue::DynCall {
            receiver,
            arms,
            args,
            ..
        } => {
            let uses = if dyn_receiver_uses_descriptor(receiver) {
                uses.union(ContextUse::rt())
            } else {
                uses
            };
            arms.iter().fold(uses, |uses, arm| {
                let Some(call_args) = dynamic_call_args(receiver, arm.receiver, args) else {
                    return uses.union(ContextUse::rt());
                };
                let call_args = adapt_dynamic_call_args(program, &arm.target, &call_args);
                let arg_uses = call_args.iter().fold(ContextUse::default(), |uses, arg| {
                    uses.union(call_arg_context_use(program, function, arg))
                });
                uses.union(arg_uses).union(resolved_target_context_use(
                    program,
                    &arm.target,
                    &call_args,
                ))
            })
        }
        RirRValue::Call { callee, args, .. } => {
            uses = uses.union(match callee {
                RirCallTarget::Function(_) | RirCallTarget::LambdaValue { .. } => {
                    ContextUse::generated_call()
                }
                RirCallTarget::Extern(id) => extern_context_use(program, *id)
                    .union(native_ref_borrow_context_use(program, *id, args)),
            });
            uses
        }
        RirRValue::DataRefAlloc { .. }
        | RirRValue::DataRefGet { .. }
        | RirRValue::List { .. }
        | RirRValue::Map { .. }
        | RirRValue::RangeListCopy { .. } => uses.union(ContextUse::rt_types()),
        RirRValue::ListPush { .. }
        | RirRValue::MapGet { .. }
        | RirRValue::MapInsert { .. }
        | RirRValue::MapRemove { .. }
        | RirRValue::MapEntryAt { .. }
        | RirRValue::MapKeyAt { .. }
        | RirRValue::MapValueAt { .. }
        | RirRValue::ScopedPlaceCellGet { .. } => uses.union(ContextUse::rt()),
        RirRValue::MutPlaceGetCopy { place, .. } => uses
            .union(ContextUse::rt())
            .union(mut_place_context_use(program, function, place)),
        RirRValue::CellGetCopy { cell, .. } => uses.union(cell_context_use(program, *cell)),
        RirRValue::Stringify { source_ty, .. }
            if matches!(program.types[source_ty.index()], RirType::Struct(_)) =>
        {
            uses.union(stringify_context_use(program, *source_ty))
        }
        RirRValue::Lambda { lambda, .. }
            if program.lambdas.get(lambda.index()).is_some_and(|lambda| {
                matches!(lambda.storage, RirLambdaStorage::HeapEnv { .. })
            }) =>
        {
            uses.union(ContextUse::rt_types())
        }
        _ => uses,
    }
}

fn adapt_dynamic_call_args(
    program: &RirProgram,
    target: &RirResolvedCallTarget,
    args: &[RirCallArg],
) -> Vec<RirCallArg> {
    let semantics = match target.base() {
        RirResolvedCallTarget::Function(id) => program.functions[id.index()]
            .params
            .iter()
            .map(|param| param.semantic)
            .collect::<Vec<_>>(),
        RirResolvedCallTarget::Extern(id) => program.externs[id.index()]
            .params
            .iter()
            .map(|param| param.semantic)
            .collect::<Vec<_>>(),
        RirResolvedCallTarget::Promoted { .. } => unreachable!(),
    };
    semantics
        .into_iter()
        .zip(args)
        .map(|(semantic, arg)| match arg {
            RirCallArg::MutPlace(_) | RirCallArg::DynBorrow(_) => arg.clone(),
            _ => arg
                .adapted_to(semantic, program)
                .expect("verified dynamic call argument adaptation"),
        })
        .collect()
}

fn resolved_target_context_use(
    program: &RirProgram,
    target: &RirResolvedCallTarget,
    args: &[RirCallArg],
) -> ContextUse {
    match target.base() {
        RirResolvedCallTarget::Function(_) => ContextUse::generated_call(),
        RirResolvedCallTarget::Extern(id) => extern_context_use(program, *id)
            .union(native_ref_borrow_context_use(program, *id, args)),
        RirResolvedCallTarget::Promoted { .. } => unreachable!(),
    }
}

fn rvalue_operand_context_use(
    program: &RirProgram,
    function: &RirFunction,
    value: &RirRValue,
) -> ContextUse {
    let mut uses = ContextUse::default();
    value.for_each_child(super::rir::RirValueUse::Read, &mut |child| {
        let child = match child {
            RirChild::Operand { operand, .. } => operand_context_use(program, function, operand),
            RirChild::Place { place, .. } => place_context_use(program, function, place),
            RirChild::MutPlace { place, .. } => mut_place_context_use(program, function, place),
            RirChild::Collection { collection, .. } => {
                collection_access_context_use(program, function, collection)
            }
            RirChild::CallArg(arg) => call_arg_context_use(program, function, arg),
            RirChild::CaptureArg(capture) => match capture {
                super::rir::RirLambdaCaptureArg::Readonly { value } => {
                    operand_context_use(program, function, value)
                }
                super::rir::RirLambdaCaptureArg::Scoped { place } => {
                    place_context_use(program, function, place)
                }
                super::rir::RirLambdaCaptureArg::StackCell { cell }
                | super::rir::RirLambdaCaptureArg::HeapCell { cell } => {
                    cell_context_use(program, *cell)
                }
                super::rir::RirLambdaCaptureArg::ScopedPlaceCell { .. } => ContextUse::rt(),
            },
            RirChild::LocalRead(_) | RirChild::Block(_) | RirChild::Tail(_) => {
                ContextUse::default()
            }
        };
        uses = uses.union(child);
    });
    uses
}

fn term_context_use(
    program: &RirProgram,
    function: &RirFunction,
    term: &super::rir::RirTerm,
) -> ContextUse {
    match term {
        super::rir::RirTerm::Return(Some(operand)) => {
            operand_context_use(program, function, operand)
        }
        super::rir::RirTerm::None
        | super::rir::RirTerm::Return(None)
        | super::rir::RirTerm::Break(_)
        | super::rir::RirTerm::Continue(_)
        | super::rir::RirTerm::Unreachable => ContextUse::default(),
    }
}

fn extern_context_use(program: &RirProgram, id: super::rir::RirExternId) -> ContextUse {
    match &program.externs[id.index()].kind {
        RirExternKind::Native(native) => {
            let base = match native.abi.ctx {
                anvyx_runtime::RustWrapperCtx::HiddenRuntime => ContextUse::rt(),
                anvyx_runtime::RustWrapperCtx::None => ContextUse::default(),
            };
            if native_return_adopts_resource(
                program,
                program.externs[id.index()].ret,
                &native.abi.ret,
            ) {
                base.union(ContextUse::rt())
            } else {
                base
            }
        }
    }
}

fn native_ref_borrow_context_use(
    program: &RirProgram,
    ext: super::rir::RirExternId,
    args: &[RirCallArg],
) -> ContextUse {
    if native_ref_borrow_conversion_fallible(program, ext, args) {
        ContextUse::rt()
    } else {
        ContextUse::default()
    }
}

fn native_ref_borrow_conversion_fallible(
    program: &RirProgram,
    ext: super::rir::RirExternId,
    args: &[RirCallArg],
) -> bool {
    let plan = program.native_call_plan(ext);
    program.externs[ext.index()]
        .params
        .iter()
        .zip(args)
        .enumerate()
        .any(|(index, (param, arg))| {
            matches!(
                plan.arg_action(index, native_arg_facts(program, param.ty, arg)),
                NativeArgAction::NativeRefBorrow { .. }
            )
        })
}

fn stringify_context_use(program: &RirProgram, ty: RirTypeId) -> ContextUse {
    let Some(req) = program.stringify_reqs.iter().find(|req| req.ty == ty) else {
        return ContextUse::generated_call();
    };
    match req.kind {
        RirStringifyReqKind::Structural(_) => ContextUse::rt_types(),
        RirStringifyReqKind::Override { .. } => ContextUse::generated_call(),
    }
}

fn call_arg_context_use(
    program: &RirProgram,
    function: &RirFunction,
    arg: &RirCallArg,
) -> ContextUse {
    match arg {
        RirCallArg::Value(operand)
        | RirCallArg::MovedValue { value: operand, .. }
        | RirCallArg::InitFieldProvided(operand) => operand_context_use(program, function, operand),
        RirCallArg::ScopedLambda { callee, .. }
        | RirCallArg::EscapingLambda { callee, .. }
        | RirCallArg::AnvCallback { callee, .. } => {
            ContextUse::generated_call().union(operand_context_use(program, function, callee))
        }
        RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
            place_context_use(program, function, place)
        }
        RirCallArg::SharedStringConst(_) | RirCallArg::InitFieldOmitted => ContextUse::default(),
        RirCallArg::MutPlace(arg) => mut_place_context_use(program, function, arg),
        RirCallArg::DynBorrow(borrow) => match &borrow.source {
            super::rir::RirDynBorrowSource::Concrete { place, .. }
            | super::rir::RirDynBorrowSource::Owned { place, .. } => {
                mut_place_context_use(program, function, place)
            }
            super::rir::RirDynBorrowSource::Borrowed { .. }
            | super::rir::RirDynBorrowSource::Reborrowed { .. } => ContextUse::default(),
        },
    }
}

fn mut_place_context_use(
    program: &RirProgram,
    _function: &RirFunction,
    arg: &RirMutPlaceArg,
) -> ContextUse {
    let root = mut_place_root_context_use(&arg.access);
    let Some(facts) = mut_place_dynamic_facts(program, arg) else {
        return root.union(ContextUse::rt_types());
    };
    if facts.fallible_projection {
        root.union(ContextUse::rt())
    } else {
        root
    }
}

fn mut_place_root_context_use(access: &RirMutPlaceAccess) -> ContextUse {
    match access {
        RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { .. }) => ContextUse::generated_call(),
        RirMutPlaceAccess::DataRef { .. } => ContextUse::rt_types(),
        RirMutPlaceAccess::Handle(
            RirMutPlaceHandle::Local { .. }
            | RirMutPlaceHandle::Param { .. }
            | RirMutPlaceHandle::StackCell { .. }
            | RirMutPlaceHandle::HeapCell { .. }
            | RirMutPlaceHandle::ScopedPlaceCell { .. },
        ) => ContextUse::default(),
    }
}

fn option_subject_context_use(
    program: &RirProgram,
    function: &RirFunction,
    subject: &RirOptionSubject,
) -> ContextUse {
    match subject {
        RirOptionSubject::Place(place) => place_context_use(program, function, place),
        RirOptionSubject::MutPlace(place) => {
            ContextUse::rt().union(mut_place_context_use(program, function, place))
        }
    }
}

fn range_for_operands(range: &RirRangeFor) -> impl Iterator<Item = &RirOperand> {
    std::iter::once(&range.start)
        .chain(std::iter::once(&range.end))
        .chain(range.ordinal_plan.operands())
}

fn operands_context_use<'a>(
    program: &RirProgram,
    function: &RirFunction,
    operands: impl IntoIterator<Item = &'a RirOperand>,
) -> ContextUse {
    operands
        .into_iter()
        .fold(ContextUse::default(), |uses, operand| {
            uses.union(operand_context_use(program, function, operand))
        })
}

fn operand_context_use(
    program: &RirProgram,
    function: &RirFunction,
    operand: &RirOperand,
) -> ContextUse {
    match operand {
        RirOperand::Place(place) => place_context_use(program, function, place),
        RirOperand::Const(_) => ContextUse::default(),
    }
}

fn place_context_use(program: &RirProgram, function: &RirFunction, place: &RirPlace) -> ContextUse {
    if matches!(place.root, RirPlaceRoot::Global(_)) {
        return ContextUse::generated_call();
    }
    if place_is_mut_place_param(function, place)
        || place_dynamic_facts(program, function, place)
            .is_some_and(|facts| facts.fallible_projection)
    {
        ContextUse::rt()
    } else {
        ContextUse::default()
    }
}

fn cell_context_use(program: &RirProgram, cell: RirCellRef) -> ContextUse {
    if cell_uses_ctx(program, cell) {
        ContextUse::rt_types()
    } else {
        ContextUse::rt()
    }
}

fn call_arg_preparation_fallible(program: &RirProgram, arg: &RirCallArg) -> bool {
    let RirCallArg::MutPlace(arg) = arg else {
        return false;
    };
    mut_place_preparation_fallible(program, arg)
}

fn option_subject_fallible(
    program: &RirProgram,
    function: &RirFunction,
    subject: &RirOptionSubject,
) -> bool {
    match subject {
        RirOptionSubject::Place(place) => place_has_fallible_projection(program, function, place),
        RirOptionSubject::MutPlace(_) => true,
    }
}

fn mut_place_preparation_fallible(program: &RirProgram, arg: &RirMutPlaceArg) -> bool {
    match arg.access {
        RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { .. })
        | RirMutPlaceAccess::DataRef { .. } => true,
        RirMutPlaceAccess::Handle(
            RirMutPlaceHandle::Local { .. }
            | RirMutPlaceHandle::Param { .. }
            | RirMutPlaceHandle::StackCell { .. }
            | RirMutPlaceHandle::HeapCell { .. }
            | RirMutPlaceHandle::ScopedPlaceCell { .. },
        ) => mut_place_dynamic_facts(program, arg).is_none_or(|facts| facts.fallible_projection),
    }
}

fn mut_place_uses_mut_place_param(function: &RirFunction, arg: &RirMutPlaceArg) -> bool {
    match arg.access {
        RirMutPlaceAccess::Handle(RirMutPlaceHandle::Param { local, .. }) => function
            .params
            .iter()
            .any(|param| param.local == local && param.abi == RirParamAbi::MutPlace),
        _ => false,
    }
}

fn call_arg_uses_mut_place_param(function: &RirFunction, arg: &RirCallArg) -> bool {
    match arg {
        RirCallArg::Value(operand)
        | RirCallArg::MovedValue { value: operand, .. }
        | RirCallArg::InitFieldProvided(operand)
        | RirCallArg::ScopedLambda {
            callee: operand, ..
        }
        | RirCallArg::EscapingLambda {
            callee: operand, ..
        }
        | RirCallArg::AnvCallback {
            callee: operand, ..
        } => operand_uses_mut_place_param(function, operand),
        RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
            place_is_mut_place_param(function, place)
        }
        RirCallArg::SharedStringConst(_)
        | RirCallArg::MutPlace(_)
        | RirCallArg::DynBorrow(_)
        | RirCallArg::InitFieldOmitted => false,
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
    function
        .params
        .iter()
        .any(|param| param.local == local && param.abi == RirParamAbi::MutPlace)
}

fn cell_uses_ctx(program: &RirProgram, cell: RirCellRef) -> bool {
    let id = match cell {
        RirCellRef::Owner(id) | RirCellRef::Capture { cell: id, .. } => id,
    };
    program
        .cells
        .get(id.index())
        .is_some_and(|cell| cell.storage == RirCellStorage::Heap)
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
    value.for_each_child(super::rir::RirValueUse::Read, &mut |child| {
        uses |= match child {
            RirChild::Operand { operand, .. } => operand_uses_mut_place_param(function, operand),
            RirChild::Place { place, .. } => place_is_mut_place_param(function, place),
            RirChild::MutPlace { place, .. } => mut_place_uses_mut_place_param(function, place),
            RirChild::Collection { collection, .. } => {
                collection_access_uses_mut_place_param(function, collection)
            }
            RirChild::CallArg(arg) => call_arg_uses_mut_place_param(function, arg),
            RirChild::CaptureArg(capture) => match capture {
                super::rir::RirLambdaCaptureArg::Readonly { value } => {
                    operand_uses_mut_place_param(function, value)
                }
                super::rir::RirLambdaCaptureArg::Scoped { place } => {
                    place_is_mut_place_param(function, place)
                }
                super::rir::RirLambdaCaptureArg::StackCell { .. }
                | super::rir::RirLambdaCaptureArg::HeapCell { .. }
                | super::rir::RirLambdaCaptureArg::ScopedPlaceCell { .. } => false,
            },
            RirChild::LocalRead(_) | RirChild::Block(_) | RirChild::Tail(_) => false,
        };
    });
    uses
}
