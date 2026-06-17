use anvyx_frontend::air::{
    self, FunctionId, GlobalId, LocalId, Mutability, ParamMode, Place, PlaceRoot, Program,
    TypeData, TypeId, TypePassClasses, place_model as air_place,
};

use super::{
    mut_place::direct_native_mut_borrow_supported,
    rep_policy::{
        AirRustRepPolicy, RustMaterialGap, RustMaterialIntent, RustMaterialSource,
        RustMaterialization,
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
    pub crosses_dataref: bool,
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
    ExternField,
    TupleField(u32),
    VariantField,
    ArrayIndex(LocalId),
    ListIndex(LocalId),
    SliceIndex(LocalId),
    MapIndex(LocalId),
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
        let root = self.root(function, path.root());
        self.check_root_support(function, intent, place, &path)?;
        Ok(PlaceAccessPlan {
            root,
            ty: path.ty(),
            projection: path.steps().iter().map(convert_projection).collect(),
            crosses_dataref: path.crosses_dataref(),
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
            | RustMaterialization::CloneHandle => None,
            RustMaterialization::CloneLambda
            | RustMaterialization::BorrowGuard
            | RustMaterialization::Gap => Some(match plan.gap {
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

    fn root(&self, function: FunctionId, root: air_place::PlaceRootInfo) -> PlaceAccessRoot {
        match root.root {
            PlaceRoot::Local(local) => PlaceAccessRoot::Local {
                local,
                source_mut_param: self.local_is_source_mut_place_param(function, local),
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
    ) -> Result<(), PlaceAccessGapKind> {
        let root = path.root();
        if let Some(air_place::PlaceStorage::Global(global)) = root.storage {
            return self.check_global(intent, global, place, path);
        }
        if intent == PlaceAccessIntent::StructuralMutation && path.crosses_dataref() {
            return Err(PlaceAccessGapKind::PlaceProjection);
        }
        if intent == PlaceAccessIntent::MutPlaceArg && root.storage.is_none() {
            return Err(PlaceAccessGapKind::MutablePlace);
        }
        if matches!(
            intent,
            PlaceAccessIntent::ReadValue | PlaceAccessIntent::SharedBorrow
        ) && path.crosses_dataref()
            && !dataref_projection_supported(self.program, path)
        {
            return Err(PlaceAccessGapKind::PlaceProjection);
        }
        if matches!(intent, PlaceAccessIntent::MutPlaceArg) && !place.projection.is_empty() {
            return self.check_projected_mut_place(path, true);
        }
        if matches!(intent, PlaceAccessIntent::Assign)
            && !place.projection.is_empty()
            && !path.crosses_dataref()
            && matches!(
                root.storage,
                Some(
                    air_place::PlaceStorage::CaptureCell(_)
                        | air_place::PlaceStorage::ScopedBorrow(_)
                )
            )
        {
            return Err(PlaceAccessGapKind::MutablePlaceProjection);
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
            ) if !place.projection.is_empty()
                && !ordinary_mut_place_supported(self.program, path, false) =>
            {
                self.check_projected_mut_place(path, false)
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
        function: FunctionId,
        path: &air_place::PlacePath,
    ) -> Result<(), PlaceAccessGapKind> {
        let PlaceRoot::Local(local) = path.root().root else {
            return Err(PlaceAccessGapKind::PlaceProjection);
        };
        if self.local_is_source_mut_place_param(function, local) || path.crosses_dataref() {
            return Err(PlaceAccessGapKind::PlaceProjection);
        }
        if path.steps().iter().all(|step| {
            matches!(
                step.kind(),
                air_place::ProjectionKind::Field(_)
                    | air_place::ProjectionKind::TupleField(_)
                    | air_place::ProjectionKind::ArrayIndex(_)
            )
        }) {
            Ok(())
        } else {
            Err(PlaceAccessGapKind::PlaceProjection)
        }
    }

    fn check_global(
        &self,
        intent: PlaceAccessIntent,
        global: GlobalId,
        place: &Place,
        path: &air_place::PlacePath,
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
                    || (global_structural_mutation_projection_supported(path)
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
                    || (global_structural_mutation_projection_supported(path)
                        && self.value_read_supported(path.ty()))
                {
                    Ok(())
                } else {
                    Err(PlaceAccessGapKind::GlobalProjection)
                }
            }
            PlaceAccessIntent::ReadValue if !place.projection.is_empty() => {
                if !(global_read_projection_supported(path)
                    || dataref_projection_supported(self.program, path))
                {
                    return Err(PlaceAccessGapKind::GlobalProjection);
                }
                if payload_gap.is_none() && self.value_read_supported(path.ty()) {
                    Ok(())
                } else {
                    Err(PlaceAccessGapKind::GlobalValueRead)
                }
            }
            PlaceAccessIntent::SharedBorrow if !place.projection.is_empty() => {
                if (global_read_projection_supported(path)
                    || dataref_projection_supported(self.program, path))
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
                    && (dataref_projection_supported(self.program, path)
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

    fn policy(&self) -> AirRustRepPolicy<'_> {
        AirRustRepPolicy::new(self.program, self.classes)
    }
}

fn dataref_projection_supported(program: &Program, path: &air_place::PlacePath) -> bool {
    let mut crosses_dataref = false;
    for step in path.steps() {
        match program.type_arena.data(step.source_ty()) {
            TypeData::DataRef(_) => {
                if !matches!(step.kind(), air_place::ProjectionKind::DataRefField(_)) {
                    return false;
                }
                crosses_dataref = true;
            }
            TypeData::Aggregate(_) if crosses_dataref => {
                if !matches!(step.kind(), air_place::ProjectionKind::Field(_)) {
                    return false;
                }
            }
            TypeData::Tuple(_) if crosses_dataref => {
                if !matches!(step.kind(), air_place::ProjectionKind::TupleField(_)) {
                    return false;
                }
            }
            _ if crosses_dataref => return false,
            _ if !ordinary_projection_supported(step.kind()) => return false,
            _ => {}
        }
    }
    crosses_dataref
}

fn ordinary_projection_supported(kind: air_place::ProjectionKind) -> bool {
    matches!(
        kind,
        air_place::ProjectionKind::Field(_)
            | air_place::ProjectionKind::TupleField(_)
            | air_place::ProjectionKind::ArrayIndex(_)
            | air_place::ProjectionKind::ListIndex(_)
            | air_place::ProjectionKind::SliceIndex(_)
            | air_place::ProjectionKind::MapIndex { .. }
    )
}

fn global_read_projection_supported(path: &air_place::PlacePath) -> bool {
    path.steps()
        .iter()
        .all(|step| ordinary_projection_supported(step.kind()))
}

fn global_structural_mutation_projection_supported(path: &air_place::PlacePath) -> bool {
    path.steps().iter().all(|step| {
        matches!(
            step.kind(),
            air_place::ProjectionKind::Field(_)
                | air_place::ProjectionKind::TupleField(_)
                | air_place::ProjectionKind::ArrayIndex(_)
        )
    })
}

fn dataref_mut_place_supported(
    program: &Program,
    classes: &TypePassClasses,
    path: &air_place::PlacePath,
) -> Result<bool, PlaceAccessGapKind> {
    if !path.crosses_dataref() {
        return Ok(false);
    }
    if !dataref_projection_supported(program, path) {
        return Err(PlaceAccessGapKind::MutablePlaceDataRef);
    }
    if dataref_mut_place_payload_supported(program, classes, path.ty()) {
        Ok(true)
    } else {
        Err(PlaceAccessGapKind::MutablePlaceDataRef)
    }
}

fn dataref_mut_place_payload_supported(
    program: &Program,
    classes: &TypePassClasses,
    ty: TypeId,
) -> bool {
    !matches!(
        AirRustRepPolicy::new(program, classes).materialization_for(
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
                    | (
                        TypeData::Map { .. },
                        air_place::ProjectionKind::MapIndex { .. }
                    )
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
            air_place::ProjectionKind::ExternField(_) => PlaceProjectionKind::ExternField,
            air_place::ProjectionKind::TupleField(index) => PlaceProjectionKind::TupleField(index),
            air_place::ProjectionKind::VariantField { .. } => PlaceProjectionKind::VariantField,
            air_place::ProjectionKind::ArrayIndex(local) => PlaceProjectionKind::ArrayIndex(local),
            air_place::ProjectionKind::ListIndex(local) => PlaceProjectionKind::ListIndex(local),
            air_place::ProjectionKind::SliceIndex(local) => PlaceProjectionKind::SliceIndex(local),
            air_place::ProjectionKind::MapIndex { local, .. } => {
                PlaceProjectionKind::MapIndex(local)
            }
        },
    }
}

#[cfg(test)]
mod tests {
    use anvyx_frontend::{
        air::{
            AggregateDecl, AggregateKind, AirBody, FieldDecl, Function, FunctionKind, GlobalDecl,
            Local, LocalKind, Mutability, Signature, TypeData, TypePassClasses,
        },
        ast::Ident,
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
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let list = program.type_arena.alloc(TypeData::List(int));
        let global = push_global(&mut program, list);
        program.functions.push(function(vec![]));
        let cx = cx(&program);
        let place = Place {
            root: PlaceRoot::Global(global),
            projection: vec![],
            ty: list,
        };

        let plan = cx
            .plan(
                FunctionId::from_index(0),
                PlaceAccessIntent::StructuralMutation,
                &place,
            )
            .expect("global structural mutation is supported");

        assert_eq!(plan.root, PlaceAccessRoot::Global(global));
    }

    fn global_program(data: TypeData) -> (Program, GlobalId, TypeId) {
        let mut program = Program::default();
        let ty = program.type_arena.alloc(data);
        let global = push_global(&mut program, ty);
        program.functions.push(function(vec![]));
        (program, global, ty)
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
        AggregateDecl {
            name: Ident::new("Box"),
            module: air::ModuleId::from_index(0),
            kind: AggregateKind::Struct,
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
