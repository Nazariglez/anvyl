use super::{
    CaptureCellId, FieldId, FunctionId, GlobalId, LambdaCaptureDecl, LocalId, Mutability, Place,
    PlaceRoot, Program, Projection, ScopedBorrowId, TypeData, TypeId, VariantId, typing,
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
    VariantField {
        enum_id: super::EnumId,
        variant: VariantId,
        field: u16,
    },
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
        Projection::VariantField {
            enum_id,
            variant,
            field,
        } => {
            let TypeData::Enum(source_enum) = program.type_arena.get(source_ty)? else {
                return None;
            };
            (*source_enum == enum_id)
                .then(|| typing::enum_variant_field(program, source_ty, variant, field))?
        }
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
        Projection::VariantField {
            enum_id,
            variant,
            field,
        } => {
            let TypeData::Enum(source_enum) = program.type_arena.get(source_ty)? else {
                return None;
            };
            if *source_enum != enum_id {
                return None;
            }
            (
                ProjectionKind::VariantField {
                    enum_id,
                    variant,
                    field,
                },
                typing::enum_variant_field(program, source_ty, variant, field)?,
            )
        }
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
        TypeData::Extern(_) => Some((
            ProjectionKind::ExternField(field),
            typing::field_by_id(program, source_ty, field)?,
        )),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        air::{
            AggregateDecl, AggregateKind, AirBlock, AirBody, EnumDecl, EnumRepr, FieldDecl,
            Function, FunctionKind, GlobalDecl, Local, LocalKind, ModuleId, Signature, VariantDecl,
            VariantShape,
        },
        ast::Ident,
    };

    #[test]
    fn walks_root_and_projection_types() {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let item_id = super::super::AggregateId::from_index(0);
        let item_ty = program.type_arena.alloc(TypeData::Aggregate(item_id));
        let list_ty = program.type_arena.alloc(TypeData::List(item_ty));
        program
            .aggregates
            .push(aggregate("Item", AggregateKind::Struct, int));
        program
            .functions
            .push(function(vec![local(list_ty), local(int)]));

        let list_place = Place {
            root: PlaceRoot::Local(LocalId::from_index(0)),
            projection: vec![
                Projection::Index(LocalId::from_index(1)),
                Projection::Field(FieldId::from_index(0)),
            ],
            ty: int,
        };
        let path = walk_place(&program, FunctionId::from_index(0), &list_place).unwrap();
        assert_eq!(path.ty(), int);
        assert_eq!(
            path.steps()[0].kind(),
            ProjectionKind::ListIndex(LocalId::from_index(1))
        );
        assert_eq!(
            path.steps()[1].kind(),
            ProjectionKind::Field(FieldId::from_index(0))
        );
    }

    #[test]
    fn rejects_dynamic_index_type_mismatch() {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let bool_ty = program.type_arena.alloc(TypeData::Bool);
        let list_ty = program.type_arena.alloc(TypeData::List(int));
        program
            .functions
            .push(function(vec![local(list_ty), local(bool_ty)]));
        let place = Place {
            root: PlaceRoot::Local(LocalId::from_index(0)),
            projection: vec![Projection::Index(LocalId::from_index(1))],
            ty: int,
        };
        assert!(walk_place(&program, FunctionId::from_index(0), &place).is_none());
    }

    #[test]
    fn rejects_wrong_variant_enum_id() {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let first_id = super::super::EnumId::from_index(0);
        let second_id = super::super::EnumId::from_index(1);
        let enum_ty = program.type_arena.alloc(TypeData::Enum(first_id));
        program.enums.push(enum_decl("First", int));
        program.enums.push(enum_decl("Second", int));
        program.functions.push(function(vec![local(enum_ty)]));
        let place = Place {
            root: PlaceRoot::Local(LocalId::from_index(0)),
            projection: vec![Projection::VariantField {
                enum_id: second_id,
                variant: VariantId::from_index(0),
                field: 0,
            }],
            ty: int,
        };
        assert!(walk_place(&program, FunctionId::from_index(0), &place).is_none());
    }

    #[test]
    fn marks_dataref_crossings() {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        let node_id = super::super::AggregateId::from_index(0);
        let node_ty = program.type_arena.alloc(TypeData::DataRef(node_id));
        program
            .aggregates
            .push(aggregate("Node", AggregateKind::DataRef, int));
        program.functions.push(function(vec![local(node_ty)]));

        let place = Place {
            root: PlaceRoot::Local(LocalId::from_index(0)),
            projection: vec![Projection::Field(FieldId::from_index(0))],
            ty: int,
        };
        let path = walk_place(&program, FunctionId::from_index(0), &place).unwrap();
        assert!(path.crosses_dataref());
        assert!(place_crosses_dataref(
            &program,
            FunctionId::from_index(0),
            &place
        ));
        assert_eq!(
            path.steps()[0].kind(),
            ProjectionKind::DataRefField(FieldId::from_index(0))
        );
    }

    #[test]
    fn reports_global_root_info() {
        let mut program = Program::default();
        let int = program.type_arena.alloc(TypeData::Int);
        program.globals.push(GlobalDecl {
            name: Ident::new("counter"),
            module: ModuleId::from_index(0),
            ty: int,
            mutability: Mutability::Mutable,
            init: FunctionId::from_index(0),
        });
        let info = root_info(
            &program,
            FunctionId::from_index(0),
            PlaceRoot::Global(GlobalId::from_index(0)),
        )
        .unwrap();
        assert_eq!(info.ty, int);
        assert_eq!(info.mutability, Mutability::Mutable);
        assert_eq!(
            info.storage,
            Some(PlaceStorage::Global(GlobalId::from_index(0)))
        );
    }

    fn aggregate(name: &str, kind: AggregateKind, field_ty: TypeId) -> AggregateDecl {
        AggregateDecl {
            name: Ident::new(name),
            module: ModuleId::from_index(0),
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

    fn enum_decl(name: &str, field_ty: TypeId) -> EnumDecl {
        EnumDecl {
            name: Ident::new(name),
            module: ModuleId::from_index(0),
            type_args: vec![],
            const_args: vec![],
            core: None,
            repr: EnumRepr::Adt,
            raw_type: None,
            variants: vec![VariantDecl {
                name: Ident::new("Some"),
                shape: VariantShape::Tuple(vec![field_ty]),
                raw_value: None,
            }],
        }
    }

    fn function(locals: Vec<Local>) -> Function {
        Function {
            name: Ident::new("test"),
            module: ModuleId::from_index(0),
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], TypeId::from_index(0)),
            locals,
            body: AirBody {
                block: AirBlock::default(),
            },
        }
    }

    fn local(ty: TypeId) -> Local {
        Local {
            name: None,
            binding: None,
            ty,
            mutability: Mutability::Mutable,
            kind: LocalKind::User,
        }
    }
}
