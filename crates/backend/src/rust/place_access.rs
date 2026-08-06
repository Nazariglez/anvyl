use std::ops::Range;

use anvyx_frontend::air::{
    self, FunctionId, GlobalId, LocalId, Mutability, ParamMode, Place, PlaceRoot, Program,
    TypeData, TypeId, place_model as air_place,
};

use super::mut_place::direct_native_mut_borrow_supported;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlaceAccessIntent {
    ReadValue,
    OwnedRead,
    Assign,
    MutPlaceArg,
    StructuralMutation,
    CollectionLoan,
    SliceView,
    PayloadAlias,
    NativeMutBorrow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlaceAccessPlan {
    pub root: PlaceAccessRoot,
    path: air_place::PlacePath,
    dataref: Option<DataRefProjectionPlan>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CollectionLoanPlan {
    pub place: PlaceAccessPlan,
    pub root_kind: air::AirCollectionRootKind,
    pub mode: air::AirCollectionLoanMode,
}

impl PlaceAccessPlan {
    pub fn dataref_plan(&self) -> Option<&DataRefProjectionPlan> {
        self.dataref.as_ref()
    }

    pub fn path(&self) -> &air_place::PlacePath {
        &self.path
    }

    pub fn steps(&self) -> &[air_place::ProjectionStep] {
        self.path.steps()
    }

    pub fn payload_alias_direct_place(&self) -> bool {
        matches!(
            self.root,
            PlaceAccessRoot::Local {
                source_mut_param: false,
                ..
            }
        ) && self.dataref.is_none()
            && self.steps().iter().all(|step| {
                matches!(
                    step.kind(),
                    air_place::ProjectionKind::Field(_)
                        | air_place::ProjectionKind::TupleField(_)
                        | air_place::ProjectionKind::ArrayIndex(_)
                )
            })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataRefProjectionPlan {
    pub(super) object_prefix: Range<usize>,
    pub(super) object_prefix_can_fail: bool,
    pub(super) segments: Vec<DataRefSegmentPlan>,
    pub(super) remaining: Range<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataRefSegmentPlan {
    pub(super) object_prefix: Range<usize>,
    pub(super) dataref_ty: TypeId,
    pub(super) dataref: air::AggregateId,
    pub(super) storage: Range<usize>,
    pub(super) storage_ty: TypeId,
}

impl DataRefProjectionPlan {
    fn build(
        program: &Program,
        path: &air_place::PlacePath,
        intent: PlaceAccessIntent,
    ) -> Result<Option<Self>, PlaceAccessGapKind> {
        if !path.crosses_dataref() {
            return Ok(None);
        }
        let steps = path.steps();
        let mut object_prefix_end = 0;
        let mut object_prefix_can_fail = false;
        let mut segments = vec![];
        let mut current = None;
        let mut remaining_start = None;
        let mut materialized = false;
        for (index, step) in steps.iter().enumerate() {
            if intent == PlaceAccessIntent::MutPlaceArg
                && matches!(step.kind(), air_place::ProjectionKind::ExternField(_))
            {
                return Err(Self::projection_gap(intent));
            }
            if materialized {
                if let TypeData::DataRef(dataref) = program.type_arena.data(step.source_ty())
                    && matches!(step.kind(), air_place::ProjectionKind::DataRefField(_))
                {
                    let object_prefix = remaining_start.take().expect("materialized range");
                    current = Some(DataRefSegmentPlan {
                        object_prefix: object_prefix..index,
                        dataref_ty: step.source_ty(),
                        dataref: *dataref,
                        storage: index..index + 1,
                        storage_ty: step.ty(),
                    });
                    materialized = false;
                    continue;
                }
                if ordinary_projection_supported(step.kind()) {
                    continue;
                }
                return Err(Self::projection_gap(intent));
            }
            match program.type_arena.data(step.source_ty()) {
                TypeData::DataRef(dataref)
                    if matches!(step.kind(), air_place::ProjectionKind::DataRefField(_)) =>
                {
                    let segment = DataRefSegmentPlan {
                        object_prefix: 0..0,
                        dataref_ty: step.source_ty(),
                        dataref: *dataref,
                        storage: index..index + 1,
                        storage_ty: step.ty(),
                    };
                    if let Some(segment) = current.replace(segment) {
                        segments.push(segment);
                    }
                }
                TypeData::DataRef(_) => return Err(Self::projection_gap(intent)),
                TypeData::Aggregate(_) | TypeData::Extern(_) if current.is_some() => {
                    if !matches!(
                        step.kind(),
                        air_place::ProjectionKind::Field(_)
                            | air_place::ProjectionKind::ExternField(_)
                    ) {
                        return Err(Self::projection_gap(intent));
                    }
                    let segment = current.as_mut().expect("checked above");
                    segment.storage.end = index + 1;
                    segment.storage_ty = step.ty();
                }
                TypeData::Tuple(_) if current.is_some() => {
                    if !matches!(step.kind(), air_place::ProjectionKind::TupleField(_)) {
                        return Err(Self::projection_gap(intent));
                    }
                    let segment = current.as_mut().expect("checked above");
                    segment.storage.end = index + 1;
                    segment.storage_ty = step.ty();
                }
                _ if current.is_some() && ordinary_projection_supported(step.kind()) => {
                    segments.push(current.take().expect("checked above"));
                    remaining_start = Some(index);
                    materialized = true;
                }
                _ if current.is_some() => return Err(Self::projection_gap(intent)),
                _ if ordinary_projection_supported(step.kind()) => {
                    object_prefix_can_fail |= projection_is_dynamic(step.kind());
                    object_prefix_end = index + 1;
                }
                _ => return Err(Self::projection_gap(intent)),
            }
        }
        if let Some(segment) = current {
            segments.push(segment);
        }
        let end = steps.len();
        Ok(Some(Self {
            object_prefix: 0..object_prefix_end,
            object_prefix_can_fail,
            segments,
            remaining: remaining_start.map_or(end..end, |start| start..end),
        }))
    }

    fn projection_gap(intent: PlaceAccessIntent) -> PlaceAccessGapKind {
        match intent {
            PlaceAccessIntent::MutPlaceArg => PlaceAccessGapKind::MutablePlaceDataRef,
            _ => PlaceAccessGapKind::PlaceProjection,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlaceAccessRoot {
    Local {
        local: LocalId,
        source_mut_param: bool,
    },
    CaptureCell(air::CaptureCellId),
    ScopedPlaceCell(air::ScopedBorrowId),
    Global(GlobalId),
    LambdaCapture(air::LambdaCaptureSlotId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlaceAccessGapKind {
    PlaceProjection,
    GlobalAccess,
    GlobalProjection,
    GlobalValueRead,
    GlobalRooting,
    MutablePlace,
    MutablePlaceProjection,
    MutablePlaceDataRef,
    MutablePlaceNativeBoundary,
    SliceView,
}

pub struct PlaceAccessCx<'a> {
    program: &'a Program,
}

impl<'a> PlaceAccessCx<'a> {
    pub fn new(program: &'a Program) -> Self {
        Self { program }
    }

    pub fn plan(
        &self,
        function: FunctionId,
        intent: PlaceAccessIntent,
        place: &Place,
    ) -> Result<PlaceAccessPlan, PlaceAccessGapKind> {
        if intent == PlaceAccessIntent::NativeMutBorrow
            && !direct_native_mut_borrow_supported(self.program, function, place)
        {
            return Err(PlaceAccessGapKind::MutablePlaceNativeBoundary);
        }
        let path = air_place::walk_place(self.program, function, place)
            .ok_or(PlaceAccessGapKind::PlaceProjection)?;
        if intent == PlaceAccessIntent::SliceView
            && (matches!(
                place.root,
                PlaceRoot::CaptureCell(_) | PlaceRoot::ScopedBorrow(_)
            ) || path.crosses_dataref())
        {
            return Err(PlaceAccessGapKind::SliceView);
        }
        let dataref = DataRefProjectionPlan::build(self.program, &path, intent)?;
        if intent == PlaceAccessIntent::PayloadAlias {
            self.check_payload_alias(function, &path, dataref.as_ref())?;
        }
        let root = self.root(function, path.root());
        self.check_root_support(function, intent, place, &path, dataref.as_ref())?;
        Ok(PlaceAccessPlan {
            root,
            path,
            dataref,
        })
    }

    pub fn collection_loan_plan(
        &self,
        function: FunctionId,
        loan: &air::AirCollectionLoan,
    ) -> Result<CollectionLoanPlan, PlaceAccessGapKind> {
        let place = self.plan(function, PlaceAccessIntent::CollectionLoan, &loan.root)?;
        self.check_collection_loan_kind(loan.root.ty, loan.root_kind, loan.mode)?;
        self.check_collection_loan_mutability(function, &loan.root, loan.mode)?;
        if matches!(
            loan.mode,
            air::AirCollectionLoanMode::MutableSequenceElement
                | air::AirCollectionLoanMode::MutableMapValue
        ) && place
            .steps()
            .iter()
            .any(|step| matches!(step.kind(), air_place::ProjectionKind::ExternField(_)))
        {
            return Err(PlaceAccessGapKind::MutablePlaceProjection);
        }
        Ok(CollectionLoanPlan {
            place,
            root_kind: loan.root_kind,
            mode: loan.mode,
        })
    }

    fn check_collection_loan_kind(
        &self,
        ty: TypeId,
        root_kind: air::AirCollectionRootKind,
        mode: air::AirCollectionLoanMode,
    ) -> Result<(), PlaceAccessGapKind> {
        let root_matches = matches!(
            (root_kind, self.program.type_arena.data(ty)),
            (air::AirCollectionRootKind::List, TypeData::List(_))
                | (
                    air::AirCollectionRootKind::FixedArray,
                    TypeData::Array { .. }
                )
                | (air::AirCollectionRootKind::Slice, TypeData::Slice(_))
                | (air::AirCollectionRootKind::Map, TypeData::Map { .. })
        );
        let mode_matches = matches!(
            (root_kind, mode),
            (
                air::AirCollectionRootKind::List
                    | air::AirCollectionRootKind::FixedArray
                    | air::AirCollectionRootKind::Slice,
                air::AirCollectionLoanMode::ReadonlySequence
                    | air::AirCollectionLoanMode::MutableSequenceElement,
            ) | (
                air::AirCollectionRootKind::Map,
                air::AirCollectionLoanMode::ReadonlyMap
                    | air::AirCollectionLoanMode::MutableMapValue,
            )
        );
        if root_matches && mode_matches {
            Ok(())
        } else {
            Err(PlaceAccessGapKind::PlaceProjection)
        }
    }

    fn check_collection_loan_mutability(
        &self,
        function: FunctionId,
        root: &Place,
        mode: air::AirCollectionLoanMode,
    ) -> Result<(), PlaceAccessGapKind> {
        if !matches!(
            mode,
            air::AirCollectionLoanMode::MutableSequenceElement
                | air::AirCollectionLoanMode::MutableMapValue
        ) {
            return Ok(());
        }
        match root.root {
            PlaceRoot::Local(local) => self
                .program
                .functions
                .get(function.index())
                .and_then(|function| function.locals.get(local.index()))
                .is_some_and(|local| local.mutability == Mutability::Mutable)
                .then_some(())
                .ok_or(PlaceAccessGapKind::MutablePlace),
            PlaceRoot::Global(global) => self
                .program
                .globals
                .get(global.index())
                .is_some_and(|global| global.mutability == Mutability::Mutable)
                .then_some(())
                .ok_or(PlaceAccessGapKind::GlobalRooting),
            _ => Ok(()),
        }
    }

    fn root(&self, function: FunctionId, root: air_place::PlaceRootInfo) -> PlaceAccessRoot {
        match root.root {
            PlaceRoot::Local(local) => PlaceAccessRoot::Local {
                local,
                source_mut_param: self.local_is_source_mut_place_param(function, local),
            },
            PlaceRoot::DynBorrowParam(id) => PlaceAccessRoot::Local {
                local: self.program.dyn_borrow_params[id.index()].source,
                source_mut_param: true,
            },
            PlaceRoot::CaptureCell(cell) => PlaceAccessRoot::CaptureCell(cell),
            PlaceRoot::ScopedBorrow(borrow) => PlaceAccessRoot::ScopedPlaceCell(borrow),
            PlaceRoot::Global(global) => PlaceAccessRoot::Global(global),
            PlaceRoot::LambdaCapture(slot) => match root.storage {
                Some(air_place::PlaceStorage::CaptureCell(cell)) => {
                    PlaceAccessRoot::CaptureCell(cell)
                }
                Some(air_place::PlaceStorage::ScopedBorrow(borrow)) => {
                    PlaceAccessRoot::ScopedPlaceCell(borrow)
                }
                Some(air_place::PlaceStorage::Local(local)) => PlaceAccessRoot::Local {
                    local,
                    source_mut_param: self.local_is_source_mut_place_param(function, local),
                },
                Some(air_place::PlaceStorage::Global(global)) => PlaceAccessRoot::Global(global),
                None => PlaceAccessRoot::LambdaCapture(slot),
            },
        }
    }

    fn check_root_support(
        &self,
        function: FunctionId,
        intent: PlaceAccessIntent,
        place: &Place,
        path: &air_place::PlacePath,
        dataref: Option<&DataRefProjectionPlan>,
    ) -> Result<(), PlaceAccessGapKind> {
        let root = path.root();
        if let Some(air_place::PlaceStorage::Global(global)) = root.storage {
            return self.check_global(intent, global, place, path, dataref);
        }
        if intent == PlaceAccessIntent::MutPlaceArg && root.storage.is_none() {
            return Err(PlaceAccessGapKind::MutablePlace);
        }
        if intent == PlaceAccessIntent::CollectionLoan {
            if dataref.is_none() && !collection_loan_projection_supported(self.program, path) {
                return Err(PlaceAccessGapKind::PlaceProjection);
            }
            match root.storage {
                Some(air_place::PlaceStorage::Local(local))
                    if self.local_is_source_mut_place_param(function, local)
                        && !place.projection.is_empty() =>
                {
                    return Err(PlaceAccessGapKind::MutablePlaceProjection);
                }
                Some(
                    air_place::PlaceStorage::CaptureCell(_)
                    | air_place::PlaceStorage::ScopedBorrow(_),
                ) if !place.projection.is_empty() => {
                    return Err(PlaceAccessGapKind::MutablePlaceProjection);
                }
                None => return Err(PlaceAccessGapKind::MutablePlace),
                Some(
                    air_place::PlaceStorage::Local(_)
                    | air_place::PlaceStorage::CaptureCell(_)
                    | air_place::PlaceStorage::ScopedBorrow(_)
                    | air_place::PlaceStorage::Global(_),
                ) => {}
            }
        }
        if matches!(
            intent,
            PlaceAccessIntent::ReadValue | PlaceAccessIntent::OwnedRead
        ) && path.crosses_dataref()
            && dataref.is_none()
        {
            return Err(PlaceAccessGapKind::PlaceProjection);
        }
        if matches!(root.storage, Some(air_place::PlaceStorage::Local(_)))
            && matches!(intent, PlaceAccessIntent::MutPlaceArg)
            && !place.projection.is_empty()
        {
            return self.check_projected_mut_place(path, dataref, true);
        }
        match root.storage {
            Some(air_place::PlaceStorage::Local(local))
                if self.local_is_source_mut_place_param(function, local)
                    && !place.projection.is_empty()
                    && !path.crosses_dataref()
                    && !ordinary_mut_place_supported(self.program, path, true) =>
            {
                Err(PlaceAccessGapKind::MutablePlaceProjection)
            }
            Some(
                air_place::PlaceStorage::CaptureCell(_) | air_place::PlaceStorage::ScopedBorrow(_),
            ) if !place.projection.is_empty() => {
                let allow_collections = matches!(
                    intent,
                    PlaceAccessIntent::ReadValue | PlaceAccessIntent::OwnedRead
                );
                if ordinary_mut_place_supported(self.program, path, allow_collections) {
                    Ok(())
                } else {
                    self.check_projected_mut_place(path, dataref, allow_collections)
                }
            }
            Some(
                air_place::PlaceStorage::Local(_)
                | air_place::PlaceStorage::CaptureCell(_)
                | air_place::PlaceStorage::ScopedBorrow(_),
            )
            | None => Ok(()),
            Some(air_place::PlaceStorage::Global(_)) => unreachable!("global roots return above"),
        }
    }

    fn check_payload_alias(
        &self,
        _function: FunctionId,
        path: &air_place::PlacePath,
        dataref: Option<&DataRefProjectionPlan>,
    ) -> Result<(), PlaceAccessGapKind> {
        let PlaceRoot::Local(_) = path.root().root else {
            return Err(PlaceAccessGapKind::PlaceProjection);
        };
        if path.crosses_dataref() {
            return Ok(());
        }
        self.check_projected_mut_place(path, dataref, true)
            .map_err(|_| PlaceAccessGapKind::PlaceProjection)
    }

    fn check_global(
        &self,
        intent: PlaceAccessIntent,
        global: GlobalId,
        place: &Place,
        path: &air_place::PlacePath,
        dataref: Option<&DataRefProjectionPlan>,
    ) -> Result<(), PlaceAccessGapKind> {
        let Some(decl) = self.program.globals.get(global.index()) else {
            return Err(PlaceAccessGapKind::GlobalAccess);
        };
        match intent {
            PlaceAccessIntent::Assign if place.projection.is_empty() => {
                if decl.mutability == Mutability::Mutable {
                    Ok(())
                } else {
                    Err(PlaceAccessGapKind::GlobalRooting)
                }
            }
            PlaceAccessIntent::StructuralMutation => {
                if decl.mutability != Mutability::Mutable {
                    return Err(PlaceAccessGapKind::GlobalRooting);
                }
                if place.projection.is_empty()
                    || (global_structural_mutation_projection_supported(self.program, path)
                        && self.value_read_supported(path.ty()))
                {
                    Ok(())
                } else {
                    Err(PlaceAccessGapKind::GlobalProjection)
                }
            }
            PlaceAccessIntent::CollectionLoan => {
                if place.projection.is_empty()
                    || (collection_loan_projection_supported(self.program, path)
                        && self.value_read_supported(path.ty()))
                {
                    Ok(())
                } else {
                    Err(PlaceAccessGapKind::GlobalProjection)
                }
            }
            PlaceAccessIntent::ReadValue | PlaceAccessIntent::OwnedRead
                if !place.projection.is_empty() =>
            {
                if !(global_read_projection_supported(path) || dataref.is_some()) {
                    return Err(PlaceAccessGapKind::GlobalProjection);
                }
                if self.value_read_supported(path.ty()) {
                    Ok(())
                } else {
                    Err(PlaceAccessGapKind::GlobalValueRead)
                }
            }
            PlaceAccessIntent::Assign | PlaceAccessIntent::MutPlaceArg
                if !place.projection.is_empty()
                    && decl.mutability == Mutability::Mutable
                    && self.check_projected_mut_place(path, dataref, true).is_ok() =>
            {
                Ok(())
            }
            PlaceAccessIntent::Assign | PlaceAccessIntent::MutPlaceArg
                if !place.projection.is_empty() =>
            {
                Err(PlaceAccessGapKind::GlobalProjection)
            }
            PlaceAccessIntent::SliceView if place.projection.is_empty() => Ok(()),
            PlaceAccessIntent::PayloadAlias | PlaceAccessIntent::SliceView => {
                Err(PlaceAccessGapKind::GlobalProjection)
            }
            PlaceAccessIntent::NativeMutBorrow => {
                Err(PlaceAccessGapKind::MutablePlaceNativeBoundary)
            }
            PlaceAccessIntent::ReadValue
            | PlaceAccessIntent::OwnedRead
            | PlaceAccessIntent::Assign
            | PlaceAccessIntent::MutPlaceArg => Ok(()),
        }
    }

    fn check_projected_mut_place(
        &self,
        path: &air_place::PlacePath,
        dataref: Option<&DataRefProjectionPlan>,
        allow_collections: bool,
    ) -> Result<(), PlaceAccessGapKind> {
        if dataref.is_some() || ordinary_mut_place_supported(self.program, path, allow_collections)
        {
            Ok(())
        } else {
            Err(PlaceAccessGapKind::MutablePlaceProjection)
        }
    }

    fn value_read_supported(&self, ty: TypeId) -> bool {
        !matches!(
            self.program.type_arena.data(ty),
            TypeData::Void | TypeData::Any | TypeData::Slice(_)
        )
    }

    fn local_is_source_mut_place_param(&self, function: FunctionId, local: LocalId) -> bool {
        let function = self.program.function(function);
        function.locals.get(local.index()).is_some_and(|decl| {
            decl.kind == air::LocalKind::Arg
                && function
                    .signature
                    .params
                    .iter()
                    .any(|param| param.local_id == local && param.mode == ParamMode::MutBorrow)
        })
    }
}

fn ordinary_projection_supported(kind: air_place::ProjectionKind) -> bool {
    matches!(
        kind,
        air_place::ProjectionKind::Field(_)
            | air_place::ProjectionKind::ExternField(_)
            | air_place::ProjectionKind::TupleField(_)
            | air_place::ProjectionKind::ArrayIndex(_)
            | air_place::ProjectionKind::ListIndex(_)
            | air_place::ProjectionKind::SliceIndex(_)
    )
}

fn projection_is_dynamic(kind: air_place::ProjectionKind) -> bool {
    matches!(
        kind,
        air_place::ProjectionKind::ArrayIndex(_)
            | air_place::ProjectionKind::ListIndex(_)
            | air_place::ProjectionKind::SliceIndex(_)
    )
}

fn global_read_projection_supported(path: &air_place::PlacePath) -> bool {
    path.steps()
        .iter()
        .all(|step| ordinary_projection_supported(step.kind()))
}

fn collection_loan_projection_supported(program: &Program, path: &air_place::PlacePath) -> bool {
    path.steps().iter().all(|step| {
        matches!(
            (program.type_arena.data(step.source_ty()), step.kind()),
            (TypeData::Aggregate(_), air_place::ProjectionKind::Field(_))
                | (
                    TypeData::Extern(_),
                    air_place::ProjectionKind::ExternField(_)
                )
                | (TypeData::Tuple(_), air_place::ProjectionKind::TupleField(_))
                | (
                    TypeData::Array { .. },
                    air_place::ProjectionKind::ArrayIndex(_)
                )
        )
    })
}

fn global_structural_mutation_projection_supported(
    program: &Program,
    path: &air_place::PlacePath,
) -> bool {
    let mut ty = path.root().ty;
    for step in path.steps() {
        let supported = matches!(
            (program.type_arena.data(ty), step.kind()),
            (
                TypeData::Aggregate(_) | TypeData::Tuple(_),
                air_place::ProjectionKind::Field(_) | air_place::ProjectionKind::TupleField(_),
            ) | (
                TypeData::Extern(_),
                air_place::ProjectionKind::ExternField(_),
            ) | (
                TypeData::Array { .. },
                air_place::ProjectionKind::ArrayIndex(_),
            ) | (TypeData::List(_), air_place::ProjectionKind::ListIndex(_))
        );
        if !supported {
            return false;
        }
        ty = step.ty();
    }
    true
}

fn ordinary_mut_place_supported(
    program: &Program,
    path: &air_place::PlacePath,
    allow_collections: bool,
) -> bool {
    let mut slice_dynamic = false;
    for step in path.steps() {
        let data = program.type_arena.data(step.source_ty());
        let direct = matches!(
            (data, step.kind()),
            (
                TypeData::Aggregate(_) | TypeData::Tuple(_),
                air_place::ProjectionKind::Field(_) | air_place::ProjectionKind::TupleField(_),
            ) | (
                TypeData::Array { .. },
                air_place::ProjectionKind::ArrayIndex(_)
            )
        );
        let collection = allow_collections
            && !slice_dynamic
            && matches!(
                (data, step.kind()),
                (TypeData::List(_), air_place::ProjectionKind::ListIndex(_))
            );
        let slice = allow_collections
            && matches!(
                (data, step.kind()),
                (TypeData::Slice(_), air_place::ProjectionKind::SliceIndex(_))
            );
        if !(direct || collection || slice) {
            return false;
        }
        slice_dynamic |= slice;
    }
    true
}
