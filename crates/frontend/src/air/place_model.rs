use super::{
    CaptureCellId, FieldId, FunctionId, GlobalId, LambdaCaptureDecl, LocalId, Mutability, Place,
    PlaceRoot, Program, Projection, ScopedBorrowId, TypeData, TypeId, typing,
    typing::PrimitiveTypes,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlaceStorage {
    Local(LocalId),
    ScopedBorrow(ScopedBorrowId),
    CaptureCell(CaptureCellId),
    Global(GlobalId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PlaceRootInfo {
    pub root: PlaceRoot,
    pub ty: TypeId,
    pub mutability: Mutability,
    pub storage: Option<PlaceStorage>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectionKind {
    Field(FieldId),
    DataRefField(FieldId),
    ExternField(FieldId),
    TupleField(u32),
    ArrayIndex(LocalId),
    ListIndex(LocalId),
    SliceIndex(LocalId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectionStep {
    kind: ProjectionKind,
    source_ty: TypeId,
    ty: TypeId,
}

impl ProjectionStep {
    pub fn kind(&self) -> ProjectionKind {
        self.kind
    }

    pub fn source_ty(&self) -> TypeId {
        self.source_ty
    }

    pub fn ty(&self) -> TypeId {
        self.ty
    }

    pub fn crosses_dataref(&self) -> bool {
        matches!(self.kind, ProjectionKind::DataRefField(_))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlacePath {
    root: PlaceRootInfo,
    steps: Vec<ProjectionStep>,
    ty: TypeId,
}

impl PlacePath {
    pub fn root(&self) -> PlaceRootInfo {
        self.root
    }

    pub fn steps(&self) -> &[ProjectionStep] {
        &self.steps
    }

    pub fn ty(&self) -> TypeId {
        self.ty
    }

    pub fn crosses_dataref(&self) -> bool {
        self.steps.iter().any(ProjectionStep::crosses_dataref)
    }
}

pub fn root_info(
    program: &Program,
    function: FunctionId,
    root: PlaceRoot,
) -> Option<PlaceRootInfo> {
    match root {
        PlaceRoot::Local(local) => {
            let decl = program.function(function).locals.get(local.index())?;
            Some(PlaceRootInfo {
                root,
                ty: decl.ty,
                mutability: decl.mutability,
                storage: Some(PlaceStorage::Local(local)),
            })
        }
        PlaceRoot::LambdaCapture(slot) => {
            let capture = program.lambda_capture(function, slot)?;
            Some(PlaceRootInfo {
                root,
                ty: capture.ty(),
                mutability: capture.mutability(),
                storage: lambda_capture_storage(capture),
            })
        }
        PlaceRoot::ScopedBorrow(borrow) => {
            let decl = program.scoped_borrows.get(borrow.index())?;
            Some(PlaceRootInfo {
                root,
                ty: decl.ty,
                mutability: decl.mutability,
                storage: Some(PlaceStorage::ScopedBorrow(borrow)),
            })
        }
        PlaceRoot::DynBorrowParam(id) => {
            let decl = program.dyn_borrow_params.get(id.index())?;
            (decl.owner == function).then_some(PlaceRootInfo {
                root,
                ty: decl.ty,
                mutability: Mutability::Mutable,
                storage: None,
            })
        }
        PlaceRoot::CaptureCell(cell) => {
            let decl = program.capture_cells.get(cell.index())?;
            Some(PlaceRootInfo {
                root,
                ty: decl.ty,
                mutability: Mutability::Mutable,
                storage: Some(PlaceStorage::CaptureCell(cell)),
            })
        }
        PlaceRoot::Global(global) => {
            let decl = program.globals.get(global.index())?;
            Some(PlaceRootInfo {
                root,
                ty: decl.ty,
                mutability: decl.mutability,
                storage: Some(PlaceStorage::Global(global)),
            })
        }
    }
}

pub fn project_ty(program: &Program, source_ty: TypeId, projection: &Projection) -> Option<TypeId> {
    match *projection {
        Projection::Field(field) => field_step(program, source_ty, field).map(|(_, ty)| ty),
        Projection::TupleField(index) => typing::tuple_field(program, source_ty, index),
        Projection::Index(_) => typing::index_elem(program, source_ty),
    }
}

pub fn walk_place(program: &Program, function: FunctionId, place: &Place) -> Option<PlacePath> {
    let root = root_info(program, function, place.root)?;
    let mut ty = root.ty;
    let mut steps = vec![];

    for projection in &place.projection {
        let step = project_step(program, function, ty, projection)?;
        ty = step.ty();
        steps.push(step);
    }

    (ty == place.ty).then_some(PlacePath { root, steps, ty })
}

pub fn place_crosses_dataref(program: &Program, function: FunctionId, place: &Place) -> bool {
    walk_place(program, function, place).is_some_and(|path| path.crosses_dataref())
}

fn lambda_capture_storage(capture: &LambdaCaptureDecl) -> Option<PlaceStorage> {
    match capture {
        LambdaCaptureDecl::ScopedBorrow { borrow, .. } => Some(PlaceStorage::ScopedBorrow(*borrow)),
        LambdaCaptureDecl::CaptureCell { cell, .. } => Some(PlaceStorage::CaptureCell(*cell)),
        LambdaCaptureDecl::NoRuntime { .. }
        | LambdaCaptureDecl::ReadonlyLocal { .. }
        | LambdaCaptureDecl::ScopedLocal { .. } => None,
    }
}

pub fn project_step(
    program: &Program,
    function: FunctionId,
    source_ty: TypeId,
    projection: &Projection,
) -> Option<ProjectionStep> {
    let (kind, ty) = match *projection {
        Projection::Field(field) => field_step(program, source_ty, field)?,
        Projection::TupleField(index) => (
            ProjectionKind::TupleField(index),
            typing::tuple_field(program, source_ty, index)?,
        ),
        Projection::Index(local) => {
            let (kind, ty, expected) = index_step(program, source_ty, local)?;
            local_has_type(program, function, local, expected)?;
            (kind, ty)
        }
    };
    Some(ProjectionStep {
        kind,
        source_ty,
        ty,
    })
}

fn field_step(
    program: &Program,
    source_ty: TypeId,
    field: FieldId,
) -> Option<(ProjectionKind, TypeId)> {
    match program.type_arena.get(source_ty)? {
        TypeData::Aggregate(_) => Some((
            ProjectionKind::Field(field),
            typing::field_by_id(program, source_ty, field)?,
        )),
        TypeData::DataRef(_) => Some((
            ProjectionKind::DataRefField(field),
            typing::field_by_id(program, source_ty, field)?,
        )),
        TypeData::Extern(extern_id) => {
            let decl = program.extern_type(*extern_id);
            let field_decl = decl.fields.get(field.index())?;
            if decl.rep != crate::air::ExternRep::Inline || field_decl.computed {
                return None;
            }
            Some((ProjectionKind::ExternField(field), field_decl.ty))
        }
        _ => None,
    }
}

fn index_step(
    program: &Program,
    source_ty: TypeId,
    local: LocalId,
) -> Option<(ProjectionKind, TypeId, TypeId)> {
    let int = PrimitiveTypes::scan(program).int()?;
    match program.type_arena.get(source_ty)? {
        TypeData::Array { elem, .. } => Some((ProjectionKind::ArrayIndex(local), *elem, int)),
        TypeData::List(elem) => Some((ProjectionKind::ListIndex(local), *elem, int)),
        TypeData::Slice(elem) => Some((ProjectionKind::SliceIndex(local), *elem, int)),
        _ => None,
    }
}

fn local_has_type(
    program: &Program,
    function: FunctionId,
    local: LocalId,
    expected: TypeId,
) -> Option<()> {
    (program.function(function).locals.get(local.index())?.ty == expected).then_some(())
}
