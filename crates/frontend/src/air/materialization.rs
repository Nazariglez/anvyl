use std::collections::HashSet;

use super::{
    AirBlock, AirChild, AirStmt, LambdaCaptureDecl, LocalId, LocalKind, Operand, Place, Program,
    RValue, ValueSource,
};

pub fn finalize(program: &mut Program) {
    let slice_types = program
        .type_arena
        .iter()
        .enumerate()
        .filter_map(|(index, ty)| {
            matches!(ty, super::TypeData::Slice(_)).then_some(super::TypeId::from_index(index))
        })
        .collect();
    for index in 0..program.functions.len() {
        let transferable = transferable_locals(program, index);
        let disposition = Disposition {
            transferable: &transferable,
            slice_types: &slice_types,
        };
        let function = &mut program.functions[index];
        let owned_return = matches!(function.signature.return_mode, super::ReturnMode::Value(_));
        finalize_block(&mut function.body.block, &disposition, owned_return);
    }
}

struct Disposition<'a> {
    transferable: &'a HashSet<LocalId>,
    slice_types: &'a HashSet<super::TypeId>,
}

impl Disposition<'_> {
    fn source(&self, value: &Operand, allow_slice: bool) -> ValueSource {
        match value {
            Operand::Place(place)
                if place.projection.is_empty()
                    && (allow_slice || !self.slice_types.contains(&place.ty)) =>
            {
                match place.root.local() {
                    Some(local) if self.transferable.contains(&local) => {
                        ValueSource::TransferTemp { local }
                    }
                    _ => ValueSource::Reusable,
                }
            }
            Operand::Place(_) | Operand::Const(_) => ValueSource::Reusable,
        }
    }
}

pub(super) fn transferable_locals(program: &Program, function_index: usize) -> HashSet<LocalId> {
    let function = &program.functions[function_index];
    let counts = local_uses(program, function_index);
    let repeated = repeated_locals(&function.body.block);
    function
        .locals
        .iter()
        .enumerate()
        .filter_map(|(index, local)| {
            let id = LocalId::from_index(index);
            (local.kind == LocalKind::Temp
                && local.binding.is_none()
                && counts[index] == 1
                && !repeated.contains(&id))
            .then_some(id)
        })
        .collect()
}

fn local_uses(program: &Program, function_index: usize) -> Vec<usize> {
    let function = &program.functions[function_index];
    let mut counts = vec![0; function.locals.len()];
    function
        .body
        .walk_children(&mut |child| count_child(child, &mut counts));
    for lambda in &program.lambdas {
        for capture in &lambda.captures {
            match capture {
                LambdaCaptureDecl::ReadonlyLocal { source, .. }
                | LambdaCaptureDecl::ScopedLocal { source, .. }
                    if source.owner.index() == function_index =>
                {
                    increment(&mut counts, source.local);
                }
                _ => {}
            }
        }
    }
    for borrow in &program.dyn_borrow_params {
        if borrow.owner.index() == function_index {
            increment(&mut counts, borrow.source);
        }
    }
    counts
}

fn increment(counts: &mut [usize], local: LocalId) {
    if let Some(count) = counts.get_mut(local.index()) {
        *count += 1;
    }
}

fn repeated_locals(block: &AirBlock) -> HashSet<LocalId> {
    let mut repeated = HashSet::new();
    collect_repeated(block, &mut repeated);
    repeated
}

fn collect_repeated(block: &AirBlock, repeated: &mut HashSet<LocalId>) {
    for stmt in &block.stmts {
        match stmt {
            AirStmt::Loop(loop_) => collect_block_locals(&loop_.body, repeated),
            AirStmt::RangeFor(for_) => collect_block_locals(&for_.body, repeated),
            AirStmt::CollectionFor(for_) => collect_block_locals(&for_.body, repeated),
            _ => stmt.for_each_child(&mut |child| {
                if let AirChild::Block(block) = child {
                    collect_repeated(block, repeated);
                }
            }),
        }
    }
}

fn collect_block_locals(block: &AirBlock, locals: &mut HashSet<LocalId>) {
    block.walk_children(&mut |child| collect_child(child, locals));
}

fn for_each_child_local(child: AirChild<'_>, add: &mut impl FnMut(LocalId)) {
    match child {
        AirChild::Operand { operand, .. } => collect_operand(operand, add),
        AirChild::Place { place, .. } => collect_place(place, add),
        AirChild::LocalRead(local) => add(local),
        AirChild::CallArg { arg, .. } => {
            if let Some(place) = arg.place() {
                collect_place(place, add);
            }
        }
        AirChild::LambdaCapture(capture) => match capture {
            super::LambdaCaptureArg::ReadonlyLocal { value } => collect_operand(&value.value, add),
            super::LambdaCaptureArg::ScopedLocal { place }
            | super::LambdaCaptureArg::ScopedBorrow { place } => collect_place(place, add),
            super::LambdaCaptureArg::NoRuntime | super::LambdaCaptureArg::CaptureCell { .. } => {}
        },
        AirChild::DynBorrow(borrow) => collect_place(borrow.place(), add),
        AirChild::RValue { .. } | AirChild::Block(_) => {}
    }
}

fn count_child(child: AirChild<'_>, counts: &mut [usize]) {
    for_each_child_local(child, &mut |local| increment(counts, local));
}

fn collect_child(child: AirChild<'_>, locals: &mut HashSet<LocalId>) {
    for_each_child_local(child, &mut |local| {
        locals.insert(local);
    });
}

fn collect_operand(operand: &Operand, add: &mut impl FnMut(LocalId)) {
    if let Operand::Place(place) = operand {
        collect_place(place, add);
    }
}

fn collect_place(place: &Place, add: &mut impl FnMut(LocalId)) {
    place.for_each_read_local(&mut |read| match read {
        super::PlaceReadLocal::Root(local) | super::PlaceReadLocal::Index(local) => add(local),
    });
}

fn finalize_block(block: &mut AirBlock, disposition: &Disposition<'_>, owned_return: bool) {
    for stmt in &mut block.stmts {
        match stmt {
            AirStmt::Init { value, .. }
            | AirStmt::GlobalSetRoot { value, .. }
            | AirStmt::GlobalUpdateRoot { value, .. }
            | AirStmt::Assign { value, .. } => finalize_rvalue(value, disposition),
            AirStmt::Eval(value) => finalize_owned_children(value, disposition),
            AirStmt::GlobalEnsure { .. } => {}
            AirStmt::If(branch) => {
                finalize_block(&mut branch.then_block, disposition, owned_return);
                if let Some(block) = &mut branch.else_block {
                    finalize_block(block, disposition, owned_return);
                }
            }
            AirStmt::Loop(loop_) => finalize_block(&mut loop_.body, disposition, owned_return),
            AirStmt::RangeFor(for_) => finalize_block(&mut for_.body, disposition, owned_return),
            AirStmt::CollectionFor(for_) => {
                finalize_block(&mut for_.body, disposition, owned_return);
            }
            AirStmt::CollectionLoan(loan) => {
                finalize_block(&mut loan.body, disposition, owned_return);
            }
            AirStmt::CollectionSlotScope(scope) => {
                finalize_block(&mut scope.body, disposition, owned_return);
            }
            AirStmt::PatternMatch(match_) => {
                for arm in &mut match_.arms {
                    finalize_block(&mut arm.block, disposition, owned_return);
                }
            }
            AirStmt::DynMatch(match_) => {
                if let super::AirDynMatchSource::Owned(owned) = &mut match_.source {
                    finalize_owned(owned, disposition, false);
                    let takes = matches!(owned.source, ValueSource::TransferTemp { .. });
                    for arm in &mut match_.arms {
                        arm.binding = match arm.binding {
                            super::AirDynMatchTargetBinding::Take(local)
                            | super::AirDynMatchTargetBinding::Materialize(local)
                                if takes =>
                            {
                                super::AirDynMatchTargetBinding::Take(local)
                            }
                            super::AirDynMatchTargetBinding::Take(local)
                            | super::AirDynMatchTargetBinding::Materialize(local) => {
                                super::AirDynMatchTargetBinding::Materialize(local)
                            }
                            binding => binding,
                        };
                    }
                }
                for arm in &mut match_.arms {
                    finalize_block(&mut arm.block, disposition, owned_return);
                }
                finalize_block(&mut match_.fallback.block, disposition, owned_return);
            }
            AirStmt::OptionalMatch(match_) => {
                finalize_block(&mut match_.some_block, disposition, owned_return);
                finalize_block(&mut match_.none_block, disposition, owned_return);
            }
            AirStmt::MapEntryMatch(match_) => {
                finalize_block(&mut match_.some_block, disposition, owned_return);
                finalize_block(&mut match_.none_block, disposition, owned_return);
            }
        }
    }
    match &mut block.tail {
        super::AirTail::Return(Some(value)) if owned_return => {
            let value = value.clone();
            block.tail = super::AirTail::ReturnOwned(super::OwnedValue {
                source: disposition.source(&value, false),
                value,
            });
        }
        super::AirTail::ReturnOwned(owned) if owned_return => {
            owned.source = disposition.source(&owned.value, false);
        }
        super::AirTail::ReturnOwned(owned) => {
            block.tail = super::AirTail::Return(Some(owned.value.clone()));
        }
        super::AirTail::None
        | super::AirTail::Return(None | Some(_))
        | super::AirTail::Break(_)
        | super::AirTail::Continue(_)
        | super::AirTail::Unreachable => {}
    }
}

fn finalize_rvalue(rvalue: &mut RValue, disposition: &Disposition<'_>) {
    finalize_owned_children(rvalue, disposition);
    if let RValue::Use(value) = rvalue {
        *rvalue = RValue::Materialize(super::OwnedValue {
            source: disposition.source(value, false),
            value: value.clone(),
        });
    }
}

fn finalize_owned_children(rvalue: &mut RValue, disposition: &Disposition<'_>) {
    match rvalue {
        RValue::DynPack { value, .. }
        | RValue::DynWeaken { value, .. }
        | RValue::DynDowncast { value, .. }
        | RValue::FunctionValue { value, .. }
        | RValue::OptionalSome { value, .. }
        | RValue::ListPush { value, .. } => finalize_owned(value, disposition, false),
        RValue::Aggregate { kind, fields, .. } => {
            for field in fields {
                if matches!(kind, super::AggregateCtor::ArrayFill) {
                    field.source = ValueSource::Reusable;
                } else {
                    finalize_owned(field, disposition, false);
                }
            }
        }
        RValue::MapInsert { key, value, .. } => {
            finalize_owned(key, disposition, false);
            finalize_owned(value, disposition, false);
        }
        RValue::Call { args, .. } => finalize_args(args, disposition),
        RValue::DynCall { receiver, args, .. } => {
            if let super::DynReceiver::Owned(owned) = receiver {
                finalize_owned(owned, disposition, false);
            }
            finalize_args(args, disposition);
        }
        RValue::MakeLambda { captures, .. } => {
            for capture in captures {
                if let super::LambdaCaptureArg::ReadonlyLocal { value } = capture {
                    finalize_owned(value, disposition, false);
                }
            }
        }
        _ => {}
    }
}

fn finalize_args(args: &mut [super::CallArg], disposition: &Disposition<'_>) {
    for arg in args {
        if let super::CallArg::Value(owned) | super::CallArg::InitFieldProvided(owned) = arg {
            finalize_owned(owned, disposition, true);
        }
    }
}

fn finalize_owned(
    owned: &mut super::OwnedValue<Operand>,
    disposition: &Disposition<'_>,
    allow_slice: bool,
) {
    owned.source = disposition.source(&owned.value, allow_slice);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::air::{
        AirTail, CallArg, Callee, ConstData, ConstValue, FunctionId, FunctionKind, Mutability,
        OwnedValue, PlaceRoot, Projection, TypeId,
        tests::{FunctionBuilder, ProgramBuilder, test_module},
    };

    fn use_local(local: LocalId, projection: Vec<Projection>) -> RValue {
        RValue::Use(Operand::Place(Place {
            root: PlaceRoot::Local(local),
            projection,
            ty: TypeId::from_index(0),
        }))
    }

    fn source(value: &RValue) -> ValueSource {
        let RValue::Materialize(owned) = value else {
            panic!("expected materialized value")
        };
        owned.source
    }

    fn finalize_test(value: &mut RValue, transferable: &HashSet<LocalId>) {
        let slices = HashSet::new();
        finalize_rvalue(
            value,
            &Disposition {
                transferable,
                slice_types: &slices,
            },
        );
    }

    fn forwarding_program(extra: Option<RValue>) -> (Program, LocalId) {
        let mut builder = ProgramBuilder::default();
        let int = builder.int_ty();
        let void = builder.void_ty();
        let module = test_module(&mut builder);
        let one = builder.alloc_const(ConstData {
            ty: int,
            value: ConstValue::Int(1),
        });
        let mut function = FunctionBuilder::new("forward", module, FunctionKind::Normal, void);
        let source = function.push_local(None, int, Mutability::Immutable, LocalKind::Temp);
        let dest = function.push_local(None, int, Mutability::Immutable, LocalKind::Temp);
        let block = function.push_block(AirTail::Return(None));
        function.add_statement(
            block,
            AirStmt::Init {
                local: source,
                value: RValue::Use(Operand::Const(one)),
            },
        );
        if let Some(extra) = extra {
            function.add_statement(block, AirStmt::Eval(extra));
        }
        function.add_statement(
            block,
            AirStmt::Init {
                local: dest,
                value: use_local(source, vec![]),
            },
        );
        builder.alloc_function(function.finish());
        (builder.finish(), source)
    }

    #[test]
    fn transfers_only_available_whole_temp() {
        let id = LocalId::from_index(0);
        let mut value = use_local(id, vec![]);
        finalize_test(&mut value, &HashSet::from([id]));
        assert_eq!(source(&value), ValueSource::TransferTemp { local: id });

        let mut projected = use_local(id, vec![Projection::TupleField(0)]);
        finalize_test(&mut projected, &HashSet::from([id]));
        assert_eq!(source(&projected), ValueSource::Reusable);

        let mut unavailable = use_local(id, vec![]);
        finalize_test(&mut unavailable, &HashSet::new());
        assert_eq!(source(&unavailable), ValueSource::Reusable);
    }

    #[test]
    fn slice_transfer_requires_call_role() {
        let id = LocalId::from_index(0);
        let slice = TypeId::from_index(0);
        let transferable = HashSet::from([id]);
        let slices = HashSet::from([slice]);
        let disposition = Disposition {
            transferable: &transferable,
            slice_types: &slices,
        };
        let mut owned = OwnedValue::reusable(Operand::Place(Place {
            root: PlaceRoot::Local(id),
            projection: vec![],
            ty: slice,
        }));
        finalize_owned(&mut owned, &disposition, false);
        assert_eq!(owned.source, ValueSource::Reusable);
        finalize_owned(&mut owned, &disposition, true);
        assert_eq!(owned.source, ValueSource::TransferTemp { local: id });
    }

    #[test]
    fn array_fill_never_transfers_its_source() {
        let id = LocalId::from_index(0);
        let mut value = RValue::Aggregate {
            kind: super::super::AggregateCtor::ArrayFill,
            fields: vec![OwnedValue::reusable(match use_local(id, vec![]) {
                RValue::Use(value) => value,
                _ => unreachable!(),
            })],
            ty: TypeId::from_index(0),
        };
        finalize_test(&mut value, &HashSet::from([id]));
        let RValue::Aggregate { fields, .. } = value else {
            unreachable!()
        };
        assert_eq!(fields[0].source, ValueSource::Reusable);
    }

    #[test]
    fn finalizer_marks_owned_aggregate_children() {
        let id = LocalId::from_index(0);
        let mut value = RValue::Aggregate {
            kind: super::super::AggregateCtor::Tuple,
            fields: vec![OwnedValue::reusable(match use_local(id, vec![]) {
                RValue::Use(value) => value,
                _ => unreachable!(),
            })],
            ty: TypeId::from_index(0),
        };
        finalize_test(&mut value, &HashSet::from([id]));
        let RValue::Aggregate { fields, .. } = value else {
            unreachable!()
        };
        assert_eq!(fields[0].source, ValueSource::TransferTemp { local: id });
    }

    #[test]
    fn finalizer_proves_transfer_and_counts_owned_uses() {
        let (mut program, source_id) = forwarding_program(None);
        finalize(&mut program);
        let AirStmt::Init { value, .. } = &program.functions[0].body.block.stmts[1] else {
            unreachable!()
        };
        assert_eq!(
            source(value),
            ValueSource::TransferTemp { local: source_id }
        );

        let call = RValue::Call {
            callee: Callee::Function(FunctionId::from_index(0)),
            args: vec![CallArg::Value(OwnedValue::reusable(Operand::Place(
                Place {
                    root: PlaceRoot::Local(source_id),
                    projection: vec![],
                    ty: TypeId::from_index(0),
                },
            )))],
        };
        let (mut program, _) = forwarding_program(Some(call));
        finalize(&mut program);
        let AirStmt::Init { value, .. } = &program.functions[0].body.block.stmts[2] else {
            unreachable!()
        };
        assert_eq!(source(value), ValueSource::Reusable);

        let capture = RValue::MakeLambda {
            lambda: super::super::LambdaId::from_index(0),
            captures: vec![super::super::LambdaCaptureArg::ReadonlyLocal {
                value: OwnedValue::reusable(Operand::Place(Place {
                    root: PlaceRoot::Local(source_id),
                    projection: vec![],
                    ty: TypeId::from_index(0),
                })),
            }],
            ty: TypeId::from_index(0),
        };
        let (mut program, _) = forwarding_program(Some(capture));
        finalize(&mut program);
        let AirStmt::Init { value, .. } = &program.functions[0].body.block.stmts[2] else {
            unreachable!()
        };
        assert_eq!(source(value), ValueSource::Reusable);

        let (mut program, source_id) = forwarding_program(None);
        let slice = program.alloc_type(super::super::TypeData::Slice(TypeId::from_index(0)));
        program.functions[0].locals[source_id.index()].ty = slice;
        assert!(transferable_locals(&program, 0).contains(&source_id));
    }
}
