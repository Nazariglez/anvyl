use super::{
    ConstValue, Program, TypeData,
    body::{CallArg, Callee, Operand, RValue},
    ids::{ConstId, ExternId, FieldId, FunctionId, TypeId, VariantId},
};
use crate::ast::{Ident, ScalarKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveKind {
    Int,
    Float,
    Bool,
    String,
    Void,
    Any,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct DuplicatePrimitive {
    pub kind: PrimitiveKind,
    pub first: TypeId,
    pub duplicate: TypeId,
}

#[derive(Debug, Default)]
pub(crate) struct PrimitiveTypes {
    ids: [Option<TypeId>; PrimitiveKind::COUNT],
    duplicates: Vec<DuplicatePrimitive>,
}

impl PrimitiveTypes {
    pub(crate) fn scan(program: &Program) -> Self {
        let mut primitives = Self::default();
        for (index, ty) in program.type_arena.iter().enumerate() {
            let id = TypeId::from_index(index);
            let Some(kind) = PrimitiveKind::from_type(ty) else {
                continue;
            };
            let slot = &mut primitives.ids[kind.index()];
            match *slot {
                Some(first) => primitives.duplicates.push(DuplicatePrimitive {
                    kind,
                    first,
                    duplicate: id,
                }),
                None => *slot = Some(id),
            }
        }
        primitives
    }

    pub(crate) fn duplicates(&self) -> &[DuplicatePrimitive] {
        &self.duplicates
    }

    pub(crate) fn get(&self, kind: PrimitiveKind) -> Option<TypeId> {
        if self
            .duplicates
            .iter()
            .any(|duplicate| duplicate.kind == kind)
        {
            None
        } else {
            self.ids[kind.index()]
        }
    }

    pub(crate) fn kind_of(&self, ty: TypeId) -> Option<PrimitiveKind> {
        PrimitiveKind::ALL
            .into_iter()
            .find(|kind| self.get(*kind) == Some(ty))
    }

    pub(crate) fn int(&self) -> Option<TypeId> {
        self.get(PrimitiveKind::Int)
    }

    pub(crate) fn void(&self) -> Option<TypeId> {
        self.get(PrimitiveKind::Void)
    }

    pub(crate) fn float(&self) -> Option<TypeId> {
        self.get(PrimitiveKind::Float)
    }

    pub(crate) fn bool(&self) -> Option<TypeId> {
        self.get(PrimitiveKind::Bool)
    }

    pub(crate) fn string(&self) -> Option<TypeId> {
        self.get(PrimitiveKind::String)
    }

    pub(crate) fn is_int(&self, ty: TypeId) -> bool {
        self.int() == Some(ty)
    }

    pub(crate) fn is_float(&self, ty: TypeId) -> bool {
        self.float() == Some(ty)
    }

    pub(crate) fn is_bool(&self, ty: TypeId) -> bool {
        self.bool() == Some(ty)
    }

    pub(crate) fn is_string(&self, ty: TypeId) -> bool {
        self.string() == Some(ty)
    }

    pub(crate) fn scalar(&self, ty: TypeId) -> Option<ScalarKind> {
        match self.kind_of(ty)? {
            PrimitiveKind::Int => Some(ScalarKind::Int),
            PrimitiveKind::Float => Some(ScalarKind::Float),
            PrimitiveKind::Bool => Some(ScalarKind::Bool),
            PrimitiveKind::String => Some(ScalarKind::String),
            PrimitiveKind::Void | PrimitiveKind::Any => None,
        }
    }
}

impl PrimitiveKind {
    const ALL: [Self; Self::COUNT] = [
        Self::Int,
        Self::Float,
        Self::Bool,
        Self::String,
        Self::Void,
        Self::Any,
    ];
    const COUNT: usize = 6;

    fn index(self) -> usize {
        self as usize
    }

    fn from_type(ty: &TypeData) -> Option<Self> {
        match ty {
            TypeData::Int => Some(Self::Int),
            TypeData::Float => Some(Self::Float),
            TypeData::Bool => Some(Self::Bool),
            TypeData::String => Some(Self::String),
            TypeData::Void => Some(Self::Void),
            TypeData::Any => Some(Self::Any),
            _ => None,
        }
    }
}

pub(crate) fn call_arg_ty(program: &Program, arg: &CallArg) -> Option<TypeId> {
    call_arg_ty_with(arg, |id| {
        program.const_arena.get_checked(id).map(|data| data.ty)
    })
}

pub(crate) fn call_arg_ty_with(
    arg: &CallArg,
    mut const_lookup: impl FnMut(ConstId) -> Option<TypeId>,
) -> Option<TypeId> {
    match arg {
        CallArg::Value(op) => operand_ty_with(op, const_lookup),
        CallArg::SharedBorrow(place) | CallArg::MutBorrow(place) => Some(place.ty),
        CallArg::SharedStringConst(id) => const_lookup(*id),
    }
}

pub(crate) fn operand_ty(program: &Program, op: &Operand) -> Option<TypeId> {
    operand_ty_with(op, |id| {
        program.const_arena.get_checked(id).map(|data| data.ty)
    })
}

pub(crate) fn operand_ty_with(
    op: &Operand,
    mut const_lookup: impl FnMut(ConstId) -> Option<TypeId>,
) -> Option<TypeId> {
    match op {
        Operand::Place(place) => Some(place.ty),
        Operand::Const(id) => const_lookup(*id),
    }
}

pub(crate) fn const_is_string(program: &Program, primitives: &PrimitiveTypes, id: ConstId) -> bool {
    program.const_arena.get_checked(id).is_some_and(|konst| {
        primitives.kind_of(konst.ty) == Some(PrimitiveKind::String)
            && matches!(konst.value, ConstValue::String(_))
    })
}

pub(crate) fn valid_cast(
    program: &Program,
    primitives: &PrimitiveTypes,
    source: TypeId,
    target: TypeId,
) -> bool {
    if (primitives.is_int(source) && primitives.is_float(target))
        || (primitives.is_float(source) && primitives.is_int(target))
    {
        return true;
    }
    let Some(TypeData::Enum(enum_id)) = program.type_arena.get(source) else {
        return false;
    };
    program.enums.get(enum_id.index()).is_some()
        && program.raw_enum_raw_type(*enum_id) == Some(target)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CalleeParams {
    Function(FunctionId),
    Extern(ExternId),
    Lambda(TypeId),
}

impl CalleeParams {
    pub(crate) fn len(self, program: &Program) -> Option<usize> {
        match self {
            Self::Function(id) => program
                .functions
                .get(id.index())
                .map(|func| func.signature.params.len()),
            Self::Extern(id) => program
                .externs
                .get(id.index())
                .map(super::ExternDecl::call_arity),
            Self::Lambda(ty) => match program.type_arena.get(ty) {
                Some(TypeData::Function(sig)) => Some(sig.params.len()),
                _ => None,
            },
        }
    }

    pub(crate) fn get(self, program: &Program, index: usize) -> Option<super::ParamType> {
        match self {
            Self::Function(id) => program
                .functions
                .get(id.index())?
                .signature
                .params
                .get(index)
                .map(|param| super::ParamType {
                    ty: param.ty,
                    mode: param.mode,
                    escape: param.escape,
                }),
            Self::Extern(id) => program.externs.get(id.index())?.call_param(index),
            Self::Lambda(ty) => match program.type_arena.get(ty) {
                Some(TypeData::Function(sig)) => sig.params.get(index).copied(),
                _ => None,
            },
        }
    }
}

pub(crate) fn callee_params(program: &Program, callee: &Callee) -> Option<CalleeParams> {
    match callee {
        Callee::Function(id) => program
            .functions
            .get(id.index())
            .map(|_| CalleeParams::Function(*id)),
        Callee::Extern(id) => program
            .externs
            .get(id.index())
            .map(|_| CalleeParams::Extern(*id)),
        Callee::Lambda(op) => {
            let ty = operand_ty(program, op)?;
            matches!(program.type_arena.get(ty), Some(TypeData::Function(_)))
                .then_some(CalleeParams::Lambda(ty))
        }
    }
}

pub(crate) fn callee_return_ty(program: &Program, callee: &Callee) -> Option<TypeId> {
    match callee {
        Callee::Function(id) => program
            .functions
            .get(id.index())
            .map(|func| func.signature.return_type()),
        Callee::Extern(id) => program.externs.get(id.index()).map(|ext| ext.return_type),
        Callee::Lambda(op) => {
            let ty = operand_ty(program, op)?;
            match program.type_arena.get(ty) {
                Some(TypeData::Function(sig)) => Some(sig.ret.ty()),
                _ => None,
            }
        }
    }
}

pub(crate) fn rvalue_ty(
    program: &Program,
    primitives: &PrimitiveTypes,
    value: &RValue,
) -> Option<TypeId> {
    match value {
        RValue::Use(op) => operand_ty(program, op),
        RValue::Unary { ty, .. }
        | RValue::Binary { ty, .. }
        | RValue::Aggregate { ty, .. }
        | RValue::OptionalSome { ty, .. }
        | RValue::ListPop { ty, .. }
        | RValue::RangeListCopy { ty, .. }
        | RValue::MapGet { ty, .. }
        | RValue::MapRemove { ty, .. }
        | RValue::MapEntryAt { ty, .. }
        | RValue::SliceView { ty, .. }
        | RValue::FunctionRef { ty, .. }
        | RValue::MakeLambda { ty, .. } => Some(*ty),
        RValue::Cast { target, .. } => Some(*target),
        RValue::Call { callee, .. } => callee_return_ty(program, callee),
        RValue::SharedRefEq { .. } => primitives.bool(),
        RValue::Stringify { .. } | RValue::StringConcat { .. } | RValue::Format { .. } => {
            primitives.string()
        }
        RValue::Len { .. } => primitives.int(),
        RValue::ListPush { .. } | RValue::MapInsert { .. } => primitives.void(),
    }
}

pub(crate) fn optional_inner(program: &Program, ty: TypeId) -> Option<TypeId> {
    match program.type_arena.get(ty) {
        Some(TypeData::Optional(inner)) => Some(*inner),
        _ => None,
    }
}

pub(crate) fn list_elem(program: &Program, ty: TypeId) -> Option<TypeId> {
    match program.type_arena.get(ty) {
        Some(TypeData::List(elem)) => Some(*elem),
        _ => None,
    }
}

pub(crate) fn array_elem_len(program: &Program, ty: TypeId) -> Option<(TypeId, usize)> {
    match program.type_arena.get(ty) {
        Some(TypeData::Array { elem, len }) => Some((*elem, *len)),
        _ => None,
    }
}

pub(crate) fn sequence_elem(program: &Program, ty: TypeId) -> Option<TypeId> {
    match program.type_arena.get(ty) {
        Some(TypeData::List(elem) | TypeData::Array { elem, .. } | TypeData::Slice(elem)) => {
            Some(*elem)
        }
        _ => None,
    }
}

pub(crate) fn index_elem(program: &Program, ty: TypeId) -> Option<TypeId> {
    sequence_elem(program, ty)
}

pub(crate) fn map_slot(program: &Program, ty: TypeId) -> Option<(TypeId, TypeId)> {
    let (key, value) = map_kv(program, ty)?;
    let slot = program
        .type_arena
        .iter()
        .enumerate()
        .find_map(|(index, data)| {
            matches!(data, TypeData::Optional(inner) if *inner == value)
                .then(|| TypeId::from_index(index))
        })?;
    Some((key, slot))
}

pub(crate) fn map_kv(program: &Program, ty: TypeId) -> Option<(TypeId, TypeId)> {
    match program.type_arena.get(ty) {
        Some(TypeData::Map { key, value, .. }) => Some((*key, *value)),
        _ => None,
    }
}

pub(crate) fn is_countable(program: &Program, primitives: &PrimitiveTypes, ty: TypeId) -> bool {
    primitives.is_string(ty)
        || matches!(
            program.type_arena.get(ty),
            Some(
                TypeData::List(_)
                    | TypeData::Array { .. }
                    | TypeData::Slice(_)
                    | TypeData::Map { .. }
            )
        )
}

pub(crate) fn tuple_field(program: &Program, ty: TypeId, index: u32) -> Option<TypeId> {
    match program.type_arena.get(ty) {
        Some(TypeData::Tuple(fields)) => fields.get(index as usize).copied(),
        _ => None,
    }
}

pub(crate) fn field_by_name(
    program: &Program,
    ty: TypeId,
    name: Ident,
) -> Option<(FieldId, TypeId)> {
    match program.type_arena.get(ty) {
        Some(TypeData::Aggregate(id)) => {
            aggregate_field_by_name(program, *id, super::AggregateKind::Struct, name)
        }
        Some(TypeData::DataRef(id)) => {
            aggregate_field_by_name(program, *id, super::AggregateKind::DataRef, name)
        }
        Some(TypeData::Extern(id)) => program
            .extern_types
            .get(id.index())
            .filter(|decl| decl.rep == super::ExternRep::Inline)?
            .fields
            .iter()
            .enumerate()
            .find(|(_, field)| field.name == name && !field.computed)
            .map(|(index, field)| (FieldId::from_index(index), field.ty)),
        _ => None,
    }
}

pub(crate) fn field_by_id(program: &Program, ty: TypeId, field: FieldId) -> Option<TypeId> {
    match program.type_arena.get(ty) {
        Some(TypeData::Aggregate(id)) => {
            aggregate_field_by_id(program, *id, super::AggregateKind::Struct, field)
        }
        Some(TypeData::DataRef(id)) => {
            aggregate_field_by_id(program, *id, super::AggregateKind::DataRef, field)
        }
        Some(TypeData::Extern(id)) => program
            .extern_types
            .get(id.index())
            .filter(|decl| decl.rep == super::ExternRep::Inline)?
            .fields
            .get(field.index())
            .filter(|field| !field.computed)
            .map(|field| field.ty),
        _ => None,
    }
}

fn aggregate_field_by_name(
    program: &Program,
    id: super::AggregateId,
    kind: super::AggregateKind,
    name: Ident,
) -> Option<(FieldId, TypeId)> {
    program
        .aggregates
        .get(id.index())
        .filter(|decl| decl.kind == kind)?
        .fields
        .iter()
        .enumerate()
        .find(|(_, field)| field.name == name)
        .map(|(index, field)| (FieldId::from_index(index), field.ty))
}

fn aggregate_field_by_id(
    program: &Program,
    id: super::AggregateId,
    kind: super::AggregateKind,
    field: FieldId,
) -> Option<TypeId> {
    program
        .aggregates
        .get(id.index())
        .filter(|decl| decl.kind == kind)?
        .fields
        .get(field.index())
        .map(|field| field.ty)
}

pub(crate) fn variant_field_count(shape: &super::VariantShape) -> usize {
    match shape {
        super::VariantShape::Unit => 0,
        super::VariantShape::Tuple(fields) => fields.len(),
        super::VariantShape::Struct(fields) => fields.len(),
    }
}

pub(crate) fn variant_field_ty(shape: &super::VariantShape, index: usize) -> Option<TypeId> {
    match shape {
        super::VariantShape::Unit => None,
        super::VariantShape::Tuple(fields) => fields.get(index).copied(),
        super::VariantShape::Struct(fields) => fields.get(index).map(|field| field.ty),
    }
}

pub(crate) fn enum_variant_field(
    program: &Program,
    ty: TypeId,
    variant: VariantId,
    field: u16,
) -> Option<TypeId> {
    let TypeData::Enum(enum_id) = program.type_arena.get(ty)? else {
        return None;
    };
    let shape = &program
        .enums
        .get(enum_id.index())?
        .variants
        .get(variant.index())?
        .shape;
    variant_field_ty(shape, field as usize)
}
