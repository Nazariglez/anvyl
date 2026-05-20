use super::{
    Program, TypeData,
    body::{Callee, Operand, RValue},
    ids::TypeId,
};
use crate::ast::{BinaryOp, UnaryOp};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ScalarType {
    Int,
    Float,
    Bool,
    String,
}

pub(crate) fn supports_scalar_unary(op: UnaryOp, value: ScalarType, result: ScalarType) -> bool {
    match op {
        UnaryOp::Neg => matches!(
            (value, result),
            (ScalarType::Int, ScalarType::Int) | (ScalarType::Float, ScalarType::Float)
        ),
        UnaryOp::Not => matches!((value, result), (ScalarType::Bool, ScalarType::Bool)),
        UnaryOp::BitNot => matches!((value, result), (ScalarType::Int, ScalarType::Int)),
    }
}

pub(crate) fn supports_scalar_binary(
    op: BinaryOp,
    lhs: ScalarType,
    rhs: ScalarType,
    result: ScalarType,
) -> bool {
    match op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
            matches!(
                (lhs, rhs, result),
                (ScalarType::Int, ScalarType::Int, ScalarType::Int)
                    | (ScalarType::Float, ScalarType::Float, ScalarType::Float)
            )
        }
        BinaryOp::LessThan
        | BinaryOp::GreaterThan
        | BinaryOp::LessThanEq
        | BinaryOp::GreaterThanEq => {
            matches!(
                (lhs, rhs, result),
                (ScalarType::Int, ScalarType::Int, ScalarType::Bool)
                    | (ScalarType::Float, ScalarType::Float, ScalarType::Bool)
            )
        }
        BinaryOp::Eq | BinaryOp::NotEq => {
            matches!(
                (lhs, rhs, result),
                (ScalarType::Int, ScalarType::Int, ScalarType::Bool)
                    | (ScalarType::Float, ScalarType::Float, ScalarType::Bool)
                    | (ScalarType::Bool, ScalarType::Bool, ScalarType::Bool)
                    | (ScalarType::String, ScalarType::String, ScalarType::Bool)
            )
        }
        BinaryOp::Xor | BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::Shl | BinaryOp::Shr => {
            matches!(
                (lhs, rhs, result),
                (ScalarType::Int, ScalarType::Int, ScalarType::Int)
            )
        }
        BinaryOp::And | BinaryOp::Or | BinaryOp::Coalesce => false,
    }
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

    pub(crate) fn int(&self) -> Option<TypeId> {
        self.get(PrimitiveKind::Int)
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

    pub(crate) fn void(&self) -> Option<TypeId> {
        self.get(PrimitiveKind::Void)
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

    pub(crate) fn scalar(&self, ty: TypeId) -> Option<ScalarType> {
        // FIXME: this is better and more readable with a match statement
        if self.is_int(ty) {
            Some(ScalarType::Int)
        } else if self.is_float(ty) {
            Some(ScalarType::Float)
        } else if self.is_bool(ty) {
            Some(ScalarType::Bool)
        } else if self.is_string(ty) {
            Some(ScalarType::String)
        } else {
            None
        }
    }
}

impl PrimitiveKind {
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

pub(crate) fn operand_ty(program: &Program, op: &Operand) -> Option<TypeId> {
    match op {
        Operand::Place(place) => Some(place.ty),
        Operand::Const(id) => program.const_arena.get_checked(*id).map(|data| data.ty),
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
        | RValue::ListPop { ty, .. }
        | RValue::ListSlice { ty, .. }
        | RValue::MapGet { ty, .. }
        | RValue::MapRemove { ty, .. }
        | RValue::MapEntryAt { ty, .. }
        | RValue::SliceView { ty, .. }
        | RValue::MakeClosure { ty, .. } => Some(*ty),
        RValue::Cast { target, .. } => Some(*target),
        RValue::Call { callee, .. } => call_ty(program, callee),
        RValue::SharedRefEq { .. } => primitives.bool(),
        RValue::ToString { .. } | RValue::Format { .. } => primitives.string(),
        RValue::Len { .. } => primitives.int(),
        RValue::ListPush { .. } | RValue::MapInsert { .. } => primitives.void(),
    }
}

pub(crate) fn list_elem_ty(program: &Program, ty: TypeId) -> Option<TypeId> {
    match program.type_arena.get(ty) {
        Some(TypeData::List(elem)) => Some(*elem),
        _ => None,
    }
}

pub(crate) fn map_kv(program: &Program, ty: TypeId) -> Option<(TypeId, TypeId)> {
    match program.type_arena.get(ty) {
        Some(TypeData::Map { key, value }) => Some((*key, *value)),
        _ => None,
    }
}

fn call_ty(program: &Program, callee: &Callee) -> Option<TypeId> {
    match callee {
        Callee::Function(id) => program
            .functions
            .get(id.index())
            .map(|func| func.signature.return_type),
        Callee::Extern(id) => program.externs.get(id.index()).map(|ext| ext.return_type),
        Callee::Closure(op) => {
            let ty = operand_ty(program, op)?;
            match program.type_arena.get(ty) {
                Some(TypeData::Function(sig)) => Some(sig.ret),
                _ => None,
            }
        }
    }
}
