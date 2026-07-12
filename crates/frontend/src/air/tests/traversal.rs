use crate::{
    air::*,
    ast::{BinaryOp, FormatSpec, UnaryOp},
};

fn local(index: usize) -> LocalId {
    LocalId::from_index(index)
}

fn ty(index: usize) -> TypeId {
    TypeId::from_index(index)
}

fn place(index: usize) -> Place {
    Place {
        root: PlaceRoot::Local(local(index)),
        projection: vec![],
        ty: ty(0),
    }
}

fn operand(index: usize) -> Operand {
    Operand::Place(place(index))
}

fn labels(stmt: &AirStmt) -> Vec<String> {
    let mut labels = vec![];
    stmt.for_each_child(&mut |child| labels.push(label(child)));
    labels
}

fn rvalue_labels(value: &RValue, use_: ValueUse) -> Vec<String> {
    let mut labels = vec![];
    value.for_each_child(use_, &mut |child| labels.push(label(child)));
    labels
}

fn label(child: AirChild<'_>) -> String {
    match child {
        AirChild::RValue { use_, .. } => format!("r:{use_:?}"),
        AirChild::Operand { use_, .. } => format!("o:{use_:?}"),
        AirChild::Place { use_, .. } => format!("p:{use_:?}"),
        AirChild::CallArg { mode, .. } => format!("a:{mode:?}"),
        AirChild::LambdaCapture(_) => "capture".into(),
        AirChild::DynBorrow(_) => "dyn-borrow".into(),
        AirChild::LocalRead(_) => "local".into(),
        AirChild::Block(_) => "block".into(),
    }
}

#[test]
fn statement_children_have_roles() {
    let block = AirBlock::default();
    let ordinal_plan = AirOrdinalPlan {
        adapters: vec![AirOrdinalAdapter::Skip { count: operand(4) }],
    };
    let stmts = vec![
        (
            AirStmt::Init {
                local: local(0),
                value: RValue::Use(operand(0)),
            },
            vec!["r:Store"],
        ),
        (
            AirStmt::Assign {
                dst: place(0),
                value: RValue::Use(operand(1)),
            },
            vec!["p:Mutate", "r:Store"],
        ),
        (AirStmt::Eval(RValue::Use(operand(0))), vec!["r:Read"]),
        (
            AirStmt::GlobalEnsure {
                global: GlobalId::from_index(0),
            },
            vec![],
        ),
        (
            AirStmt::GlobalSetRoot {
                global: GlobalId::from_index(0),
                value: RValue::Use(operand(0)),
                init: GlobalInitEffect::InitializeFirst,
            },
            vec!["r:Store"],
        ),
        (
            AirStmt::GlobalUpdateRoot {
                global: GlobalId::from_index(0),
                value: RValue::Use(operand(0)),
            },
            vec!["r:Store"],
        ),
        (
            AirStmt::If(AirIf {
                cond: operand(0),
                then_block: block.clone(),
                else_block: Some(block.clone()),
            }),
            vec!["o:Read", "block", "block"],
        ),
        (
            AirStmt::Loop(AirLoop {
                id: AirLoopId::from_index(0),
                body: block.clone(),
            }),
            vec!["block"],
        ),
        (
            AirStmt::RangeFor(AirRangeFor {
                id: AirLoopId::from_index(0),
                start: operand(0),
                end: operand(1),
                ordinal_plan: ordinal_plan.clone(),
                inclusive: false,
                ordinal: Some(local(2)),
                item: local(3),
                body: block.clone(),
            }),
            vec!["o:Read", "o:Read", "o:Read", "local", "local", "block"],
        ),
        (
            AirStmt::CollectionFor(AirCollectionFor {
                id: AirLoopId::from_index(0),
                len: local(0),
                ordinal_plan,
                index: local(1),
                ordinal: Some(local(2)),
                body: block.clone(),
            }),
            vec!["local", "o:Read", "local", "local", "block"],
        ),
        (
            AirStmt::CollectionLoan(AirCollectionLoan {
                root: place(0),
                root_kind: AirCollectionRootKind::List,
                mode: AirCollectionLoanMode::MutableSequenceElement,
                body: block.clone(),
            }),
            vec!["p:Borrow(MutBorrow)", "block"],
        ),
        (
            AirStmt::CollectionSlotScope(AirCollectionSlotScope {
                root: place(0),
                index: local(1),
                slots: vec![],
                body: block.clone(),
            }),
            vec!["p:Read", "local", "block"],
        ),
        (
            AirStmt::PatternMatch(AirPatternMatch {
                subject: place(0),
                arms: vec![AirPatternArm {
                    alternatives: vec![AirPatternAlternative {
                        tests: vec![],
                        bindings: vec![AirPatternBinding {
                            local: local(1),
                            path: AirPatternPath::default(),
                            ty: ty(0),
                            mode: AirPatternBindingMode::Alias,
                        }],
                    }],
                    block: block.clone(),
                }],
            }),
            vec!["p:Borrow(MutBorrow)", "block"],
        ),
        (
            AirStmt::DynMatch(AirDynMatch {
                source: AirDynMatchSource::Mutable(place(0)),
                surface: ContractSurfaceId::from_index(0),
                arms: vec![AirDynMatchArm {
                    target: ty(1),
                    binding: AirDynMatchTargetBinding::Alias(local(1)),
                    block: block.clone(),
                }],
                fallback: AirDynMatchFallback {
                    binding: AirDynMatchFallbackBinding::Alias(local(2)),
                    block: block.clone(),
                },
            }),
            vec!["p:Borrow(MutBorrow)", "block", "block"],
        ),
        (
            AirStmt::OptionalMatch(AirOptionalMatch {
                discr: place(0),
                payload: Some(local(1)),
                payload_ref: true,
                payload_escapes: false,
                some_block: block.clone(),
                none_block: block.clone(),
            }),
            vec!["p:Borrow(MutBorrow)", "block", "block"],
        ),
        (
            AirStmt::MapEntryMatch(AirMapEntryMatch {
                map: place(0),
                key: operand(1),
                payload: Some(local(2)),
                payload_escapes: false,
                some_block: block.clone(),
                none_block: block,
            }),
            vec!["p:Mutate", "o:Read", "block", "block"],
        ),
    ];

    for (stmt, expected) in stmts {
        assert_eq!(labels(&stmt), expected);
    }
}

#[test]
fn rvalue_children_have_roles() {
    let unary = UnaryOp::Neg;
    let borrow = DynBorrow {
        source: DynBorrowSource::Borrowed(place(0)),
        ty: ty(1),
        surface: ContractSurfaceId::from_index(0),
        weakening: None,
    };
    let values = vec![
        (RValue::Use(operand(0)), vec!["o:Consume"]),
        (
            RValue::DynPack {
                value: operand(0),
                use_: DynOwnedUse::ConsumeTemporary,
                witness: ContractWitnessId::from_index(0),
                ty: ty(1),
            },
            vec!["o:Consume"],
        ),
        (
            RValue::DynWeaken {
                value: operand(0),
                use_: DynOwnedUse::ConsumeTemporary,
                weakening: ContractWeakeningId::from_index(0),
                ty: ty(1),
            },
            vec!["o:Consume"],
        ),
        (
            RValue::DynDowncast {
                value: operand(0),
                use_: DynOwnedUse::ReusableRead,
                surface: ContractSurfaceId::from_index(0),
                target: ty(1),
                ty: ty(2),
            },
            vec!["o:Read"],
        ),
        (
            RValue::DynCall {
                receiver: DynReceiver::Borrowed(borrow.clone()),
                surface: ContractSurfaceId::from_index(0),
                slot: ContractSlotId::from_index(0),
                args: vec![CallArg::DynBorrow(borrow)],
            },
            vec!["dyn-borrow", "a:MutBorrow"],
        ),
        (
            RValue::FunctionValue {
                value: operand(0),
                capability: FunctionValueCapability::Unknown,
            },
            vec!["o:Consume"],
        ),
        (
            RValue::Unary {
                op: unary,
                value: operand(0),
                ty: ty(0),
            },
            vec!["o:Read"],
        ),
        (
            RValue::Binary {
                op: BinaryOp::Add,
                lhs: operand(0),
                rhs: operand(1),
                ty: ty(0),
            },
            vec!["o:Read", "o:Read"],
        ),
        (
            RValue::SharedRefEq {
                lhs: operand(0),
                rhs: operand(1),
                negated: false,
            },
            vec!["o:Read", "o:Read"],
        ),
        (
            RValue::OptionalSome {
                value: operand(0),
                ty: ty(0),
            },
            vec!["o:Read"],
        ),
        (
            RValue::Cast {
                value: operand(0),
                target: ty(0),
            },
            vec!["o:Read"],
        ),
        (
            RValue::Aggregate {
                kind: AggregateCtor::Tuple,
                fields: vec![operand(0), operand(1)],
                ty: ty(0),
            },
            vec!["o:Store", "o:Store"],
        ),
        (
            RValue::Call {
                callee: Callee::Lambda(operand(0)),
                args: vec![CallArg::Value(operand(1)), CallArg::MutBorrow(place(2))],
            },
            vec!["o:Read", "a:Value", "a:MutBorrow"],
        ),
        (
            RValue::Stringify {
                value: operand(0),
                source_ty: ty(0),
            },
            vec!["o:Read"],
        ),
        (
            RValue::StringConcat {
                parts: vec![operand(0), operand(1)],
            },
            vec!["o:Read", "o:Read"],
        ),
        (
            RValue::Format {
                value: operand(0),
                spec: FormatSpec::default(),
            },
            vec!["o:Read"],
        ),
        (RValue::Len { source: place(0) }, vec!["p:Read"]),
        (
            RValue::ListPush {
                list: place(0),
                value: operand(1),
            },
            vec!["p:Mutate", "o:Store"],
        ),
        (
            RValue::ListPop {
                list: place(0),
                ty: ty(0),
            },
            vec!["p:Mutate"],
        ),
        (
            RValue::RangeListCopy {
                source: place(0),
                start: local(1),
                end: local(2),
                inclusive: false,
                ty: ty(0),
            },
            vec!["p:Read", "local", "local"],
        ),
        (
            RValue::MapGet {
                map: place(0),
                key: operand(1),
                ty: ty(0),
            },
            vec!["p:Read", "o:Read"],
        ),
        (
            RValue::MapInsert {
                map: place(0),
                key: operand(1),
                value: operand(2),
                kind: MapWriteKind::StructuralInsert,
            },
            vec!["p:Mutate", "o:Store", "o:Store"],
        ),
        (
            RValue::MapRemove {
                map: place(0),
                key: operand(1),
                ty: ty(0),
            },
            vec!["p:Mutate", "o:Read"],
        ),
        (
            RValue::CheckedIterCount {
                count: operand(0),
                check: IterCountCheck::SkipNonNegative,
            },
            vec!["o:Read"],
        ),
        (
            RValue::MapEntryAt {
                map: place(0),
                index: local(1),
                ty: ty(0),
            },
            vec!["p:Read", "local"],
        ),
        (
            RValue::MapKeyAt {
                map: place(0),
                index: local(1),
                ty: ty(0),
            },
            vec!["p:Read", "local"],
        ),
        (
            RValue::MapValueAt {
                map: place(0),
                index: local(1),
                ty: ty(0),
            },
            vec!["p:Read", "local"],
        ),
        (
            RValue::SliceView {
                source: place(0),
                start: local(1),
                end: local(2),
                inclusive: false,
                ty: ty(0),
            },
            vec!["p:Borrow(SharedBorrow)", "local", "local"],
        ),
        (
            RValue::FunctionRef {
                function: FunctionId::from_index(0),
                ty: ty(0),
            },
            vec![],
        ),
        (
            RValue::MakeLambda {
                lambda: LambdaId::from_index(0),
                captures: vec![LambdaCaptureArg::ReadonlyLocal { value: operand(0) }],
                ty: ty(0),
            },
            vec!["capture"],
        ),
    ];

    for (value, expected) in values {
        assert_eq!(rvalue_labels(&value, ValueUse::Consume), expected);
    }
}

#[test]
fn children_preserve_identity_and_order() {
    let binary = RValue::Binary {
        op: BinaryOp::Add,
        lhs: operand(3),
        rhs: operand(7),
        ty: ty(0),
    };
    let mut operand_roots = vec![];
    binary.for_each_child(ValueUse::Read, &mut |child| {
        if let AirChild::Operand {
            operand: Operand::Place(place),
            ..
        } = child
        {
            operand_roots.push(place.root);
        }
    });
    assert_eq!(
        operand_roots,
        [PlaceRoot::Local(local(3)), PlaceRoot::Local(local(7))]
    );

    let call = RValue::Call {
        callee: Callee::Function(FunctionId::from_index(0)),
        args: vec![CallArg::Value(operand(4)), CallArg::SharedBorrow(place(8))],
    };
    let mut args = vec![];
    call.for_each_child(ValueUse::Read, &mut |child| {
        if let AirChild::CallArg {
            index, mode, arg, ..
        } = child
        {
            let root = arg.place().map(|place| place.root);
            args.push((index, mode, root));
        }
    });
    assert_eq!(
        args,
        [
            (0, ParamMode::Value, Some(PlaceRoot::Local(local(4)))),
            (1, ParamMode::SharedBorrow, Some(PlaceRoot::Local(local(8)))),
        ]
    );
}

#[test]
fn tails_expose_only_returned_results() {
    let tails = [
        (AirTail::None, vec![]),
        (AirTail::Return(None), vec![]),
        (AirTail::Return(Some(operand(0))), vec!["o:Consume"]),
        (AirTail::Break(AirLoopId::from_index(0)), vec![]),
        (AirTail::Continue(AirLoopId::from_index(0)), vec![]),
        (AirTail::Unreachable, vec![]),
    ];
    for (tail, expected) in tails {
        let block = AirBlock {
            stmts: vec![],
            tail,
        };
        let mut labels = vec![];
        block.for_each_child(&mut |child| labels.push(label(child)));
        assert_eq!(labels, expected);
    }
}

#[test]
fn recursive_walk_includes_nested_rvalues_and_tail() {
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::If(AirIf {
                cond: operand(0),
                then_block: AirBlock {
                    stmts: vec![AirStmt::Eval(RValue::Use(operand(1)))],
                    tail: AirTail::Return(Some(operand(2))),
                },
                else_block: None,
            })],
            tail: AirTail::Return(Some(operand(3))),
        },
    };
    let mut labels = vec![];
    body.walk_children(&mut |child| labels.push(label(child)));
    assert_eq!(
        labels,
        [
            "o:Read",
            "block",
            "r:Read",
            "o:Read",
            "o:Consume",
            "o:Consume",
        ]
    );
}
