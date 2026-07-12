use anvyx_frontend::air::{
    self, FunctionId, GlobalId, LocalId, Mutability, ParamMode, Place, PlaceRoot, Program,
    TypeData, TypeId, TypePassClasses, place_model as air_place,
};

use super::{
    mut_place::direct_native_mut_borrow_supported,
    rep_policy::{
        RustMaterialGap, RustMaterialIntent, RustMaterialSource, RustMaterialization,
        RustRepresentationPlan,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlaceAccessIntent {
    ReadValue,
    SharedBorrow,
    MutBorrow,
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
    pub ty: TypeId,
    pub projection: Vec<PlaceProjection>,
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

    pub fn payload_alias_direct_place(&self) -> bool {
        matches!(
            self.root,
            PlaceAccessRoot::Local {
                source_mut_param: false,
                ..
            }
        ) && self.dataref.is_none()
            && self.projection.iter().all(|projection| {
                matches!(
                    projection.kind,
                    PlaceProjectionKind::Field(_)
                        | PlaceProjectionKind::TupleField(_)
                        | PlaceProjectionKind::ArrayIndex(_)
                )
            })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataRefProjectionPlan {
    pub(super) root_ty: TypeId,
    pub(super) object_prefix: Vec<PlaceProjection>,
    pub(super) object_prefix_can_fail: bool,
    pub(super) segments: Vec<DataRefSegmentPlan>,
    pub(super) remaining: Vec<PlaceProjection>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataRefSegmentPlan {
    pub(super) dataref_ty: TypeId,
    pub(super) dataref: air::AggregateId,
    pub(super) storage: Vec<PlaceProjection>,
    pub(super) storage_ty: TypeId,
}

impl DataRefProjectionPlan {
    fn build(
        program: &Program,
        classes: &TypePassClasses,
        path: &air_place::PlacePath,
        intent: PlaceAccessIntent,
    ) -> Result<Option<Self>, PlaceAccessGapKind> {
        if !path.crosses_dataref() {
            return Ok(None);
        }

        let mut object_prefix = vec![];
        let mut object_prefix_can_fail = false;
        let mut segments = vec![];
        let mut current = None;
        let mut remaining = vec![];
        let mut materialized = false;

        for step in path.steps() {
            let projection = convert_projection(step);
            if materialized {
                if dataref_remaining_projection_supported(step.kind()) {
                    remaining.push(projection);
                    continue;
                }
                return Err(Self::projection_gap(intent));
            }
            match program.type_arena.data(step.source_ty()) {
                TypeData::DataRef(dataref) => {
                    let mut segment = DataRefSegmentPlan {
                        dataref_ty: step.source_ty(),
                        dataref: *dataref,
                        storage: vec![],
                        storage_ty: step.source_ty(),
                    };
                    if matches!(step.kind(), air_place::ProjectionKind::DataRefField(_)) {
                        segment.storage_ty = projection.ty;
                        segment.storage.push(projection);
                        if let Some(segment) = current.replace(segment) {
                            segments.push(segment);
                        }
                    } else {
                        return Err(Self::projection_gap(intent));
                    }
                }
                TypeData::Aggregate(_) if current.is_some() => {
                    if matches!(step.kind(), air_place::ProjectionKind::Field(_)) {
                        let segment = current.as_mut().expect("checked above");
                        segment.storage_ty = projection.ty;
                        segment.storage.push(projection);
                    } else {
                        return Err(Self::projection_gap(intent));
                    }
                }
                TypeData::Tuple(_) if current.is_some() => {
                    if matches!(step.kind(), air_place::ProjectionKind::TupleField(_)) {
                        let segment = current.as_mut().expect("checked above");
                        segment.storage_ty = projection.ty;
                        segment.storage.push(projection);
                    } else {
                        return Err(Self::projection_gap(intent));
                    }
                }
                _ if current.is_some()
                    && intent == PlaceAccessIntent::ReadValue
                    && dataref_remaining_projection_supported(step.kind()) =>
                {
                    segments.push(current.take().expect("checked above"));
                    remaining.push(projection);
                    materialized = true;
                }
                _ if current.is_some() => return Err(Self::projection_gap(intent)),
                _ if ordinary_projection_supported(step.kind()) => {
                    object_prefix_can_fail |= projection_is_dynamic(projection.kind);
                    object_prefix.push(projection);
                }
                _ => return Err(Self::projection_gap(intent)),
            }
        }
        if let Some(segment) = current {
            segments.push(segment);
        }

        if matches!(intent, PlaceAccessIntent::MutPlaceArg)
            && !dataref_mut_place_payload_supported(program, classes, path.ty())
        {
            return Err(PlaceAccessGapKind::MutablePlaceDataRef);
        }

        Ok(Some(Self {
            root_ty: path.root().ty,
            object_prefix,
            object_prefix_can_fail,
            segments,
            remaining,
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
pub struct PlaceProjection {
    pub source_ty: TypeId,
    pub ty: TypeId,
    pub kind: PlaceProjectionKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlaceProjectionKind {
    Field(air::FieldId),
    DataRefField(air::FieldId),
    ExternField(air::FieldId),
    TupleField(u32),
    ArrayIndex(LocalId),
    ListIndex(LocalId),
    SliceIndex(LocalId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlaceAccessGapKind {
    PlaceProjection,
    GlobalAccess,
    GlobalBorrow,
    GlobalProjection,
    GlobalValueRead,
    GlobalRooting,
    GlobalType,
    MutablePlace,
    MutablePlaceProjection,
    MutablePlaceDataRef,
    MutablePlaceNativeBoundary,
    SliceView,
    ReturnPlace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum CollectionLoanBase {
    Aggregate,
    Tuple,
    Array,
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum CollectionLoanProjection {
    Field,
    TupleField,
    Index,
    Other,
}

pub(super) fn collection_loan_step_supported(
    base: CollectionLoanBase,
    projection: CollectionLoanProjection,
) -> bool {
    matches!(
        (base, projection),
        (
            CollectionLoanBase::Aggregate,
            CollectionLoanProjection::Field
        ) | (
            CollectionLoanBase::Tuple,
            CollectionLoanProjection::TupleField
        ) | (CollectionLoanBase::Array, CollectionLoanProjection::Index)
    )
}

pub struct PlaceAccessCx<'a> {
    program: &'a Program,
    classes: &'a TypePassClasses,
}

impl<'a> PlaceAccessCx<'a> {
    pub fn new(program: &'a Program, classes: &'a TypePassClasses) -> Self {
        Self { program, classes }
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
                PlaceRoot::CaptureCell(_) | PlaceRoot::ScopedBorrow(_) | PlaceRoot::Global(_)
            ) || path.crosses_dataref())
        {
            return Err(PlaceAccessGapKind::SliceView);
        }
        if intent == PlaceAccessIntent::PayloadAlias {
            self.check_payload_alias(function, &path)?;
        }
        let dataref = DataRefProjectionPlan::build(self.program, self.classes, &path, intent)?;
        let root = self.root(function, path.root());
        self.check_root_support(function, intent, place, &path, dataref.as_ref())?;
        Ok(PlaceAccessPlan {
            root,
            ty: path.ty(),
            projection: path.steps().iter().map(convert_projection).collect(),
            dataref,
        })
    }

    pub fn global_payload_gap(&self, ty: TypeId) -> Option<PlaceAccessGapKind> {
        let plan = self.policy().materialization_plan_for(
            ty,
            RustMaterialSource::ExactGlobalRoot,
            RustMaterialIntent::Read,
        );
        match plan.materialization {
            RustMaterialization::Copy
            | RustMaterialization::Share
            | RustMaterialization::CloneHandle
            | RustMaterialization::CloneLambda => None,
            RustMaterialization::BorrowGuard | RustMaterialization::Gap => Some(match plan.gap {
                Some(RustMaterialGap::UnsupportedType) => PlaceAccessGapKind::GlobalType,
                Some(RustMaterialGap::UnsupportedRooting) | None => {
                    PlaceAccessGapKind::GlobalRooting
                }
            }),
        }
    }

    pub fn global_supported(&self, global: GlobalId) -> bool {
        self.program
            .globals
            .get(global.index())
            .is_some_and(|decl| self.global_payload_gap(decl.ty).is_none())
    }

    pub fn global_root_set_supported(&self, global: GlobalId) -> bool {
        self.program
            .globals
            .get(global.index())
            .is_some_and(|decl| {
                decl.mutability == Mutability::Mutable && self.global_payload_gap(decl.ty).is_none()
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
        if intent == PlaceAccessIntent::StructuralMutation && path.crosses_dataref() {
            return Err(PlaceAccessGapKind::PlaceProjection);
        }
        if intent == PlaceAccessIntent::MutPlaceArg && root.storage.is_none() {
            return Err(PlaceAccessGapKind::MutablePlace);
        }
        if intent == PlaceAccessIntent::CollectionLoan {
            if !collection_loan_projection_supported(self.program, path) {
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
            PlaceAccessIntent::ReadValue | PlaceAccessIntent::SharedBorrow
        ) && path.crosses_dataref()
            && dataref.is_none()
        {
            return Err(PlaceAccessGapKind::PlaceProjection);
        }
        if matches!(root.storage, Some(air_place::PlaceStorage::Local(_)))
            && matches!(intent, PlaceAccessIntent::MutPlaceArg)
            && !place.projection.is_empty()
        {
            return self.check_projected_mut_place(path, true);
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
                let allow_collections = intent == PlaceAccessIntent::ReadValue;
                if ordinary_mut_place_supported(self.program, path, allow_collections) {
                    Ok(())
                } else {
                    self.check_projected_mut_place(path, allow_collections)
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
    ) -> Result<(), PlaceAccessGapKind> {
        let PlaceRoot::Local(_) = path.root().root else {
            return Err(PlaceAccessGapKind::PlaceProjection);
        };
        if path.crosses_dataref() {
            return Err(PlaceAccessGapKind::PlaceProjection);
        }
        self.check_projected_mut_place(path, true)
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
        let payload_gap = self.global_payload_gap(decl.ty);
        match intent {
            PlaceAccessIntent::Assign if place.projection.is_empty() => {
                if decl.mutability == Mutability::Mutable && payload_gap.is_none() {
                    Ok(())
                } else {
                    Err(PlaceAccessGapKind::GlobalRooting)
                }
            }
            PlaceAccessIntent::StructuralMutation => {
                if decl.mutability != Mutability::Mutable || payload_gap.is_some() {
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
                if payload_gap.is_some() {
                    return Err(PlaceAccessGapKind::GlobalValueRead);
                }
                if place.projection.is_empty()
                    || (collection_loan_projection_supported(self.program, path)
                        && self.value_read_supported(path.ty()))
                {
                    Ok(())
                } else {
                    Err(PlaceAccessGapKind::GlobalProjection)
                }
            }
            PlaceAccessIntent::ReadValue if !place.projection.is_empty() => {
                if !(global_read_projection_supported(path) || dataref.is_some()) {
                    return Err(PlaceAccessGapKind::GlobalProjection);
                }
                if payload_gap.is_none() && self.value_read_supported(path.ty()) {
                    Ok(())
                } else {
                    Err(PlaceAccessGapKind::GlobalValueRead)
                }
            }
            PlaceAccessIntent::SharedBorrow if !place.projection.is_empty() => {
                if (global_read_projection_supported(path) || dataref.is_some())
                    && self.value_read_supported(path.ty())
                {
                    Ok(())
                } else {
                    Err(PlaceAccessGapKind::GlobalProjection)
                }
            }
            PlaceAccessIntent::MutBorrow if !place.projection.is_empty() => {
                Err(PlaceAccessGapKind::GlobalProjection)
            }
            PlaceAccessIntent::MutBorrow => Err(PlaceAccessGapKind::GlobalBorrow),
            PlaceAccessIntent::ReadValue | PlaceAccessIntent::SharedBorrow
                if payload_gap.is_some() =>
            {
                Err(PlaceAccessGapKind::GlobalValueRead)
            }
            PlaceAccessIntent::Assign
                if !place.projection.is_empty()
                    && decl.mutability == Mutability::Mutable
                    && payload_gap.is_none()
                    && (dataref.is_some()
                        || self.check_projected_mut_place(path, true).is_ok()) =>
            {
                Ok(())
            }
            PlaceAccessIntent::MutPlaceArg
                if !place.projection.is_empty()
                    && decl.mutability == Mutability::Mutable
                    && payload_gap.is_none()
                    && self.check_projected_mut_place(path, true).is_ok() =>
            {
                Ok(())
            }
            PlaceAccessIntent::Assign | PlaceAccessIntent::MutPlaceArg
                if !place.projection.is_empty() =>
            {
                Err(PlaceAccessGapKind::GlobalProjection)
            }
            PlaceAccessIntent::PayloadAlias | PlaceAccessIntent::SliceView => {
                Err(PlaceAccessGapKind::GlobalProjection)
            }
            PlaceAccessIntent::NativeMutBorrow => {
                Err(PlaceAccessGapKind::MutablePlaceNativeBoundary)
            }
            PlaceAccessIntent::ReadValue
            | PlaceAccessIntent::SharedBorrow
            | PlaceAccessIntent::Assign
            | PlaceAccessIntent::MutPlaceArg => Ok(()),
        }
    }

    fn check_projected_mut_place(
        &self,
        path: &air_place::PlacePath,
        allow_collections: bool,
    ) -> Result<(), PlaceAccessGapKind> {
        if dataref_mut_place_supported(self.program, self.classes, path)?
            || ordinary_mut_place_supported(self.program, path, allow_collections)
        {
            return Ok(());
        }
        Err(PlaceAccessGapKind::MutablePlaceProjection)
    }

    fn value_read_supported(&self, ty: TypeId) -> bool {
        !matches!(
            self.policy()
                .materialization_plan_for(ty, RustMaterialSource::Value, RustMaterialIntent::Read)
                .materialization,
            RustMaterialization::Gap
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

    fn policy(&self) -> RustRepresentationPlan<'_> {
        RustRepresentationPlan::new(self.program, self.classes)
    }
}

fn ordinary_projection_supported(kind: air_place::ProjectionKind) -> bool {
    matches!(
        kind,
        air_place::ProjectionKind::Field(_)
            | air_place::ProjectionKind::TupleField(_)
            | air_place::ProjectionKind::ArrayIndex(_)
            | air_place::ProjectionKind::ListIndex(_)
            | air_place::ProjectionKind::SliceIndex(_)
    )
}

fn projection_is_dynamic(kind: PlaceProjectionKind) -> bool {
    matches!(
        kind,
        PlaceProjectionKind::ArrayIndex(_)
            | PlaceProjectionKind::ListIndex(_)
            | PlaceProjectionKind::SliceIndex(_)
    )
}

fn dataref_remaining_projection_supported(kind: air_place::ProjectionKind) -> bool {
    matches!(
        kind,
        air_place::ProjectionKind::Field(_)
            | air_place::ProjectionKind::TupleField(_)
            | air_place::ProjectionKind::ListIndex(_)
    )
}

fn global_read_projection_supported(path: &air_place::PlacePath) -> bool {
    path.steps()
        .iter()
        .all(|step| ordinary_projection_supported(step.kind()))
}

fn collection_loan_projection_supported(program: &Program, path: &air_place::PlacePath) -> bool {
    path.steps().iter().all(|step| {
        collection_loan_step_supported(
            collection_loan_base(program.type_arena.data(step.source_ty())),
            collection_loan_projection(step.kind()),
        )
    })
}

fn collection_loan_base(data: &TypeData) -> CollectionLoanBase {
    match data {
        TypeData::Aggregate(_) => CollectionLoanBase::Aggregate,
        TypeData::Tuple(_) => CollectionLoanBase::Tuple,
        TypeData::Array { .. } => CollectionLoanBase::Array,
        _ => CollectionLoanBase::Other,
    }
}

fn collection_loan_projection(kind: air_place::ProjectionKind) -> CollectionLoanProjection {
    match kind {
        air_place::ProjectionKind::Field(_) => CollectionLoanProjection::Field,
        air_place::ProjectionKind::TupleField(_) => CollectionLoanProjection::TupleField,
        air_place::ProjectionKind::ArrayIndex(_) => CollectionLoanProjection::Index,
        _ => CollectionLoanProjection::Other,
    }
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

fn dataref_mut_place_supported(
    program: &Program,
    classes: &TypePassClasses,
    path: &air_place::PlacePath,
) -> Result<bool, PlaceAccessGapKind> {
    if !path.crosses_dataref() {
        return Ok(false);
    }
    DataRefProjectionPlan::build(program, classes, path, PlaceAccessIntent::MutPlaceArg)
        .map(|plan| plan.is_some())
}

fn dataref_mut_place_payload_supported(
    program: &Program,
    classes: &TypePassClasses,
    ty: TypeId,
) -> bool {
    !matches!(
        RustRepresentationPlan::new(program, classes).materialization_for(
            ty,
            RustMaterialSource::DataRefMutPlace,
            RustMaterialIntent::MutPlacePayload,
        ),
        RustMaterialization::Gap
    )
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

fn convert_projection(step: &air_place::ProjectionStep) -> PlaceProjection {
    PlaceProjection {
        source_ty: step.source_ty(),
        ty: step.ty(),
        kind: match step.kind() {
            air_place::ProjectionKind::Field(field) => PlaceProjectionKind::Field(field),
            air_place::ProjectionKind::DataRefField(field) => {
                PlaceProjectionKind::DataRefField(field)
            }
            air_place::ProjectionKind::ExternField(field) => {
                PlaceProjectionKind::ExternField(field)
            }
            air_place::ProjectionKind::TupleField(index) => PlaceProjectionKind::TupleField(index),
            air_place::ProjectionKind::ArrayIndex(local) => PlaceProjectionKind::ArrayIndex(local),
            air_place::ProjectionKind::ListIndex(local) => PlaceProjectionKind::ListIndex(local),
            air_place::ProjectionKind::SliceIndex(local) => PlaceProjectionKind::SliceIndex(local),
        },
    }
}

#[cfg(test)]
mod tests {
    use anvyx_frontend::{
        air::{
            AggregateDecl, AggregateKind, AirBody, BindingId, CaptureCellDecl, CaptureCellId,
            CaptureCellLifetime, FieldDecl, Function, FunctionKind, GlobalDecl, LambdaCaptureDecl,
            LambdaCaptureSlotId, LambdaDecl, LambdaEscape, LambdaId, Local, LocalKind, Mutability,
            Param, ParamEscape, ParamRole, ReturnMode, ScopedBorrowDecl, ScopedBorrowId,
            ScopedBorrowSource, Signature, SignatureType, TypeData, TypePassClasses,
        },
        ast::{ExprId, Ident},
    };

    use super::*;

    #[test]
    fn accepts_exact_global_read() {
        let (program, global, int) = global_program(TypeData::Int);
        let cx = cx(&program);
        let place = Place {
            root: PlaceRoot::Global(global),
            projection: vec![],
            ty: int,
        };

        let plan = cx
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::ReadValue,
                &place,
            )
            .expect("exact global read supported");

        assert_eq!(plan.root, PlaceAccessRoot::Global(global));
        assert_eq!(plan.ty, int);
    }

    #[test]
    fn accepts_projected_global_read() {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let aggregate = air::AggregateId::from_index(0);
        let aggregate_ty = program.type_arena.alloc(TypeData::Aggregate(aggregate));
        program.aggregates.push(aggregate_decl(int));
        let global = push_global(&mut program, aggregate_ty);
        program.functions.push(function(vec![]));
        let cx = cx(&program);
        let place = Place {
            root: PlaceRoot::Global(global),
            projection: vec![air::Projection::Field(air::FieldId::from_index(0))],
            ty: int,
        };

        let plan = cx
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::ReadValue,
                &place,
            )
            .expect("projected global reads are supported");

        assert_eq!(plan.root, PlaceAccessRoot::Global(global));
        assert_eq!(plan.ty, int);
        assert_eq!(plan.projection.len(), 1);
    }

    #[test]
    fn exposes_dynamic_index_operand() {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let list = program.type_arena.alloc(TypeData::List(int));
        program
            .functions
            .push(function(vec![local(list, true), local(int, false)]));
        let cx = cx(&program);
        let place = Place {
            root: PlaceRoot::Local(LocalId::from_index(0)),
            projection: vec![air::Projection::Index(LocalId::from_index(1))],
            ty: int,
        };

        let plan = cx
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::ReadValue,
                &place,
            )
            .expect("local list index read supported");

        assert_eq!(
            plan.projection[0].kind,
            PlaceProjectionKind::ListIndex(LocalId::from_index(1))
        );
    }

    #[test]
    fn accepts_global_structural_mutation() {
        let (program, global, place) = global_list_place();
        let cx = cx(&program);

        let plan = cx
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::StructuralMutation,
                &place,
            )
            .expect("global structural mutation is supported");

        assert_eq!(plan.root, PlaceAccessRoot::Global(global));
    }

    #[test]
    fn accepts_dynamic_global_structural_mutation() {
        let (program, place) = dynamic_global_list_place();
        let cx = cx(&program);

        let plan = cx
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::StructuralMutation,
                &place,
            )
            .expect("dynamic global prefixes use live structural targets");

        assert_eq!(plan.root, PlaceAccessRoot::Global(GlobalId::from_index(0)));
    }

    #[test]
    fn accepts_exact_global_collection_loan() {
        let (program, global, place) = global_list_place();
        let cx = cx(&program);

        let plan = cx
            .collection_loan_plan(
                FunctionId::from_index(0),
                &collection_loan(place, air::AirCollectionLoanMode::MutableSequenceElement),
            )
            .expect("global collection loans are centrally accepted");

        assert_eq!(plan.place.root, PlaceAccessRoot::Global(global));
    }

    #[test]
    fn rejects_dynamic_global_collection_loan() {
        let (program, place) = dynamic_global_list_place();
        let cx = cx(&program);

        let gap = cx
            .collection_loan_plan(
                FunctionId::from_index(0),
                &collection_loan(place, air::AirCollectionLoanMode::MutableSequenceElement),
            )
            .expect_err("dynamic global loans await live descriptors");

        assert_eq!(gap, PlaceAccessGapKind::GlobalProjection);
    }

    #[test]
    fn accepts_payload_alias_over_direct_local() {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let option = program.type_arena.alloc(TypeData::Optional(int));
        program.functions.push(function(vec![local(option, true)]));
        let cx = cx(&program);
        let place = Place {
            root: PlaceRoot::Local(LocalId::from_index(0)),
            projection: vec![],
            ty: option,
        };

        let plan = cx
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::PayloadAlias,
                &place,
            )
            .expect("direct local payload aliases are supported");

        assert_eq!(
            plan.root,
            PlaceAccessRoot::Local {
                local: LocalId::from_index(0),
                source_mut_param: false,
            }
        );
    }

    #[test]
    fn accepts_payload_alias_over_source_param() {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let option = program.type_arena.alloc(TypeData::Optional(int));
        program.functions.push(source_param_function(option));
        let cx = cx(&program);
        let place = Place {
            root: PlaceRoot::Local(LocalId::from_index(0)),
            projection: vec![],
            ty: option,
        };

        let plan = cx
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::PayloadAlias,
                &place,
            )
            .expect("source-param payload aliases use mutable-place subjects");

        assert!(!plan.payload_alias_direct_place());
    }

    #[test]
    fn accepts_scalar_dataref_field_read() {
        let (program, place) = dataref_field_program(TypeData::Int, true);
        let plan = cx(&program)
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::ReadValue,
                &place,
            )
            .expect("scalar dataref field reads are supported");

        assert!(plan.dataref_plan().is_some());
    }

    #[test]
    fn accepts_nested_inline_dataref_field_read() {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let point = air::AggregateId::from_index(0);
        let point_ty = program.type_arena.alloc(TypeData::Aggregate(point));
        let dataref = air::AggregateId::from_index(1);
        let dataref_ty = program.type_arena.alloc(TypeData::DataRef(dataref));
        program.aggregates.push(aggregate_decl(int));
        program.aggregates.push(dataref_decl(point_ty));
        program
            .functions
            .push(function(vec![local(dataref_ty, false)]));
        let place = Place {
            root: PlaceRoot::Local(LocalId::from_index(0)),
            projection: vec![
                air::Projection::Field(air::FieldId::from_index(0)),
                air::Projection::Field(air::FieldId::from_index(0)),
            ],
            ty: int,
        };

        let plan = cx(&program)
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::ReadValue,
                &place,
            )
            .expect("nested inline dataref reads are supported");

        let dataref = plan.dataref_plan().expect("dataref plan");
        assert_eq!(dataref.segments.len(), 1);
        assert_eq!(dataref.segments[0].storage.len(), 2);
    }

    #[test]
    fn splits_nested_dataref_handle_reads_into_segments() {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let inner = air::AggregateId::from_index(0);
        let inner_ty = program.type_arena.alloc(TypeData::DataRef(inner));
        let outer = air::AggregateId::from_index(1);
        let outer_ty = program.type_arena.alloc(TypeData::DataRef(outer));
        program.aggregates.push(dataref_decl(int));
        program.aggregates.push(dataref_decl(inner_ty));
        program
            .functions
            .push(function(vec![local(outer_ty, false)]));
        let place = Place {
            root: PlaceRoot::Local(LocalId::from_index(0)),
            projection: vec![
                air::Projection::Field(air::FieldId::from_index(0)),
                air::Projection::Field(air::FieldId::from_index(0)),
            ],
            ty: int,
        };

        let plan = cx(&program)
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::ReadValue,
                &place,
            )
            .expect("nested dataref handle reads are supported");
        let dataref = plan.dataref_plan().expect("dataref plan");

        assert_eq!(dataref.segments.len(), 2);
        assert_eq!(dataref.segments[0].storage_ty, inner_ty);
        assert_eq!(dataref.segments[1].storage_ty, int);
    }

    #[test]
    fn accepts_collection_value_projection_after_dataref_crossing() {
        let (program, place) = aggregate_collection_dataref_projection_program();

        let plan = cx(&program)
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::ReadValue,
                &place,
            )
            .expect("collection value projections after dataref crossings are supported");
        let dataref = plan.dataref_plan().expect("dataref plan");

        assert_eq!(dataref.segments.len(), 1);
        assert_eq!(dataref.remaining.len(), 1);
    }

    #[test]
    fn rejects_collection_mut_place_projection_after_dataref_crossing() {
        let (program, place) = aggregate_collection_dataref_projection_program();

        let gap = cx(&program)
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::MutPlaceArg,
                &place,
            )
            .expect_err(
                "collection mut-place projections after dataref crossings remain unsupported",
            );

        assert_eq!(gap, PlaceAccessGapKind::MutablePlaceDataRef);
    }

    #[test]
    fn records_dynamic_dataref_object_prefix() {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let dataref = air::AggregateId::from_index(0);
        let dataref_ty = program.type_arena.alloc(TypeData::DataRef(dataref));
        let list = program.type_arena.alloc(TypeData::List(dataref_ty));
        program.aggregates.push(dataref_decl(int));
        program
            .functions
            .push(function(vec![local(list, false), local(int, false)]));
        let place = Place {
            root: PlaceRoot::Local(LocalId::from_index(0)),
            projection: vec![
                air::Projection::Index(LocalId::from_index(1)),
                air::Projection::Field(air::FieldId::from_index(0)),
            ],
            ty: int,
        };

        let plan = cx(&program)
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::ReadValue,
                &place,
            )
            .expect("dynamic object prefix dataref reads keep current support");
        let dataref = plan.dataref_plan().expect("dataref plan");

        assert!(dataref.object_prefix_can_fail);
        assert_eq!(dataref.object_prefix.len(), 1);
        assert_eq!(dataref.segments.len(), 1);
    }

    #[test]
    fn accepts_capture_cell_field_assignment() {
        let (program, cell, place) =
            captured_field_program(PlaceRoot::CaptureCell(CaptureCellId::from_index(0)));

        let plan = cx(&program)
            .plan(FunctionId::from_index(0), PlaceAccessIntent::Assign, &place)
            .expect("capture-cell field assignment uses mutable-place support");

        assert_eq!(plan.root, PlaceAccessRoot::CaptureCell(cell));
        assert_eq!(plan.projection.len(), 1);
    }

    #[test]
    fn accepts_lambda_capture_field_assignment() {
        let (program, cell, place) =
            captured_field_program(PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0)));

        let plan = cx(&program)
            .plan(FunctionId::from_index(0), PlaceAccessIntent::Assign, &place)
            .expect("lambda capture field assignment resolves to the backing cell");

        assert_eq!(plan.root, PlaceAccessRoot::CaptureCell(cell));
        assert_eq!(plan.projection.len(), 1);
    }

    #[test]
    fn accepts_scoped_borrow_field_assignment() {
        let (program, _, place) =
            captured_field_program(PlaceRoot::ScopedBorrow(ScopedBorrowId::from_index(0)));

        let plan = cx(&program)
            .plan(FunctionId::from_index(0), PlaceAccessIntent::Assign, &place)
            .expect("scoped-borrow field assignment uses mutable-place support");

        assert_eq!(
            plan.root,
            PlaceAccessRoot::ScopedPlaceCell(ScopedBorrowId::from_index(0))
        );
        assert_eq!(plan.projection.len(), 1);
    }

    #[test]
    fn rejects_dynamic_capture_cell_assignment_projection() {
        let (program, place) =
            captured_list_index_program(PlaceRoot::CaptureCell(CaptureCellId::from_index(0)));

        let gap = cx(&program)
            .plan(FunctionId::from_index(0), PlaceAccessIntent::Assign, &place)
            .expect_err("dynamic collection projection through cells stays unsupported");

        assert_eq!(gap, PlaceAccessGapKind::MutablePlaceProjection);
    }

    #[test]
    fn rejects_dynamic_scoped_borrow_assignment_projection() {
        let (program, place) =
            captured_list_index_program(PlaceRoot::ScopedBorrow(ScopedBorrowId::from_index(0)));

        let gap = cx(&program)
            .plan(FunctionId::from_index(0), PlaceAccessIntent::Assign, &place)
            .expect_err("dynamic collection projection through scoped cells stays unsupported");

        assert_eq!(gap, PlaceAccessGapKind::MutablePlaceProjection);
    }

    #[test]
    fn keeps_projected_source_param_mut_place_arg_support() {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let aggregate = air::AggregateId::from_index(0);
        let aggregate_ty = program.type_arena.alloc(TypeData::Aggregate(aggregate));
        program.aggregates.push(aggregate_decl(int));
        program.functions.push(source_param_function(aggregate_ty));
        let place = Place {
            root: PlaceRoot::Local(LocalId::from_index(0)),
            projection: vec![air::Projection::Field(air::FieldId::from_index(0))],
            ty: int,
        };

        let plan = cx(&program)
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::MutPlaceArg,
                &place,
            )
            .expect("source-param projected mut-place args keep existing support");

        assert_eq!(plan.projection.len(), 1);
    }

    #[test]
    fn rejects_unsupported_dataref_mut_place_payload() {
        let (program, place) = dataref_field_program(TypeData::String, true);

        let gap = cx(&program)
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::MutPlaceArg,
                &place,
            )
            .expect_err("string dataref mut-place payloads stay unsupported");

        assert_eq!(gap, PlaceAccessGapKind::MutablePlaceDataRef);
    }

    fn captured_field_program(root: PlaceRoot) -> (Program, CaptureCellId, Place) {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let aggregate = air::AggregateId::from_index(0);
        let aggregate_ty = program.type_arena.alloc(TypeData::Aggregate(aggregate));
        let cell = CaptureCellId::from_index(0);
        program.aggregates.push(aggregate_decl(int));
        program.capture_cells.push(CaptureCellDecl {
            binding: BindingId::from_index(0),
            owner: FunctionId::from_index(0),
            source_local: LocalId::from_index(0),
            ty: aggregate_ty,
            lifetime: CaptureCellLifetime::Function,
        });
        program.scoped_borrows.push(ScopedBorrowDecl {
            owner: FunctionId::from_index(0),
            binding: BindingId::from_index(0),
            source: ScopedBorrowSource::SourceMutParam {
                local: LocalId::from_index(0),
            },
            ty: aggregate_ty,
            mutability: Mutability::Mutable,
        });
        push_lambda_capture_function(
            &mut program,
            LambdaCaptureDecl::CaptureCell {
                binding: BindingId::from_index(0),
                cell,
                ty: aggregate_ty,
            },
        );
        let place = Place {
            root,
            projection: vec![air::Projection::Field(air::FieldId::from_index(0))],
            ty: int,
        };
        (program, cell, place)
    }

    fn captured_list_index_program(root: PlaceRoot) -> (Program, Place) {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let list = program.type_arena.alloc(TypeData::List(int));
        program.capture_cells.push(CaptureCellDecl {
            binding: BindingId::from_index(0),
            owner: FunctionId::from_index(0),
            source_local: LocalId::from_index(0),
            ty: list,
            lifetime: CaptureCellLifetime::Function,
        });
        program.scoped_borrows.push(ScopedBorrowDecl {
            owner: FunctionId::from_index(0),
            binding: BindingId::from_index(0),
            source: ScopedBorrowSource::SourceMutParam {
                local: LocalId::from_index(0),
            },
            ty: list,
            mutability: Mutability::Mutable,
        });
        push_lambda_capture_function(
            &mut program,
            LambdaCaptureDecl::CaptureCell {
                binding: BindingId::from_index(0),
                cell: CaptureCellId::from_index(0),
                ty: list,
            },
        );
        program.functions[0].locals.push(local(int, false));
        let place = Place {
            root,
            projection: vec![air::Projection::Index(LocalId::from_index(0))],
            ty: int,
        };
        (program, place)
    }

    fn push_lambda_capture_function(program: &mut Program, capture: LambdaCaptureDecl) {
        let function_id = FunctionId::from_index(0);
        let lambda = LambdaId::from_index(0);
        program.lambdas.push(LambdaDecl {
            source: ExprId(0),
            module: air::ModuleId::from_index(0),
            owner: function_id,
            body: function_id,
            signature: SignatureType::new(vec![], ReturnMode::Value(TypeId::from_index(0))),
            escape: LambdaEscape::NonEscaping,
            captures: vec![capture],
        });
        program.functions.push(Function {
            kind: FunctionKind::Lambda(lambda),
            ..function(vec![])
        });
    }

    fn global_program(data: TypeData) -> (Program, GlobalId, TypeId) {
        let mut program = Program::default();
        let ty = program.type_arena.alloc(data);
        let global = push_global(&mut program, ty);
        program.functions.push(function(vec![]));
        (program, global, ty)
    }

    fn global_list_place() -> (Program, GlobalId, Place) {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let list = program.type_arena.alloc(TypeData::List(int));
        let global = push_global(&mut program, list);
        program.functions.push(function(vec![]));
        let place = Place {
            root: PlaceRoot::Global(global),
            projection: vec![],
            ty: list,
        };
        (program, global, place)
    }

    fn dynamic_global_list_place() -> (Program, Place) {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let row = program.type_arena.alloc(TypeData::List(int));
        let rows = program.type_arena.alloc(TypeData::List(row));
        let global = push_global(&mut program, rows);
        program.functions.push(function(vec![local(int, false)]));
        let place = Place {
            root: PlaceRoot::Global(global),
            projection: vec![air::Projection::Index(LocalId::from_index(0))],
            ty: row,
        };
        (program, place)
    }

    fn collection_loan(root: Place, mode: air::AirCollectionLoanMode) -> air::AirCollectionLoan {
        air::AirCollectionLoan {
            root,
            root_kind: air::AirCollectionRootKind::List,
            mode,
            body: air::AirBlock::default(),
        }
    }

    fn push_global(program: &mut Program, ty: TypeId) -> GlobalId {
        let global = GlobalId::from_index(program.globals.len());
        program.globals.push(GlobalDecl {
            name: Ident::new("state"),
            module: air::ModuleId::from_index(0),
            ty,
            mutability: Mutability::Mutable,
            init: FunctionId::from_index(0),
        });
        global
    }

    fn aggregate_decl(field_ty: TypeId) -> AggregateDecl {
        aggregate_decl_with_kind(field_ty, AggregateKind::Struct)
    }

    fn dataref_decl(field_ty: TypeId) -> AggregateDecl {
        aggregate_decl_with_kind(field_ty, AggregateKind::DataRef)
    }

    fn aggregate_decl_with_kind(field_ty: TypeId, kind: AggregateKind) -> AggregateDecl {
        AggregateDecl {
            name: Ident::new("Box"),
            module: air::ModuleId::from_index(0),
            kind,
            type_args: vec![],
            const_args: vec![],
            fields: vec![FieldDecl {
                name: Ident::new("value"),
                ty: field_ty,
            }],
            cycle_capable: false,
            stringify_override: None,
        }
    }

    fn dataref_field_program(field_data: TypeData, mutable: bool) -> (Program, Place) {
        let mut program = Program::default();
        let field_ty = program.type_arena.alloc(field_data);
        let dataref = air::AggregateId::from_index(0);
        let dataref_ty = program.type_arena.alloc(TypeData::DataRef(dataref));
        program.aggregates.push(dataref_decl(field_ty));
        program
            .functions
            .push(function(vec![local(dataref_ty, mutable)]));
        let place = Place {
            root: PlaceRoot::Local(LocalId::from_index(0)),
            projection: vec![air::Projection::Field(air::FieldId::from_index(0))],
            ty: field_ty,
        };
        (program, place)
    }

    fn aggregate_collection_dataref_projection_program() -> (Program, Place) {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let list = program.type_arena.alloc(TypeData::List(int));
        let payload = air::AggregateId::from_index(0);
        let payload_ty = program.type_arena.alloc(TypeData::Aggregate(payload));
        let dataref = air::AggregateId::from_index(1);
        let dataref_ty = program.type_arena.alloc(TypeData::DataRef(dataref));
        program.aggregates.push(aggregate_decl(list));
        program.aggregates.push(dataref_decl(payload_ty));
        program
            .functions
            .push(function(vec![local(dataref_ty, false), local(int, false)]));
        let place = Place {
            root: PlaceRoot::Local(LocalId::from_index(0)),
            projection: vec![
                air::Projection::Field(air::FieldId::from_index(0)),
                air::Projection::Field(air::FieldId::from_index(0)),
                air::Projection::Index(LocalId::from_index(1)),
            ],
            ty: int,
        };
        (program, place)
    }

    fn function(locals: Vec<Local>) -> Function {
        Function {
            name: Ident::new("main"),
            module: air::ModuleId::from_index(0),
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], TypeId::from_index(0)),
            locals,
            body: AirBody {
                block: air::AirBlock::default(),
            },
        }
    }

    fn source_param_function(ty: TypeId) -> Function {
        let local_id = LocalId::from_index(0);
        Function {
            name: Ident::new("touch"),
            module: air::ModuleId::from_index(0),
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(
                vec![Param {
                    name: None,
                    ty,
                    mode: ParamMode::MutBorrow,
                    escape: ParamEscape::NonEscaping,
                    role: ParamRole::Normal,
                    local_id,
                }],
                TypeId::from_index(0),
            ),
            locals: vec![Local {
                kind: LocalKind::Arg,
                ..local(ty, true)
            }],
            body: AirBody {
                block: air::AirBlock::default(),
            },
        }
    }

    fn local(ty: TypeId, mutable: bool) -> Local {
        Local {
            name: None,
            binding: None,
            ty,
            mutability: if mutable {
                Mutability::Mutable
            } else {
                Mutability::Immutable
            },
            kind: LocalKind::User,
        }
    }

    fn cx(program: &Program) -> PlaceAccessCx<'_> {
        let classes = Box::leak(Box::new(TypePassClasses::analyze(program)));
        PlaceAccessCx::new(program, classes)
    }
}
