use super::ids::*;
pub use crate::ast::{BinaryOp, UnaryOp};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TypeArena {
    data: Vec<TypeData>,
}

impl TypeArena {
    pub fn alloc(&mut self, data: TypeData) -> TypeId {
        let id = TypeId::from_index(self.data.len());
        self.data.push(data);
        id
    }

    pub fn data(&self, id: TypeId) -> &TypeData {
        &self.data[id.index()]
    }

    pub fn data_mut(&mut self, id: TypeId) -> &mut TypeData {
        &mut self.data[id.index()]
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeData {
    Int,
    Float,
    Bool,
    String,
    Void,
    Any,
    Optional(TypeId),
    Tuple(Vec<TypeId>),
    List(TypeId),
    Array { elem: TypeId, len: usize },
    Map { key: TypeId, value: TypeId },
    Function(SignatureType),
    Aggregate(AggregateId),
    Enum(EnumId),
    DataRef(AggregateId),
    Extern(ExternTypeId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureType {
    pub params: Vec<TypeId>,
    pub ret: TypeId,
}

impl SignatureType {
    pub fn new(params: Vec<TypeId>, ret: TypeId) -> Self {
        Self { params, ret }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ConstArena {
    data: Vec<ConstData>,
}

impl ConstArena {
    pub fn alloc(&mut self, data: ConstData) -> ConstId {
        let id = ConstId::from_index(self.data.len());
        self.data.push(data);
        id
    }

    pub fn get(&self, id: ConstId) -> &ConstData {
        &self.data[id.index()]
    }

    pub fn get_mut(&mut self, id: ConstId) -> &mut ConstData {
        &mut self.data[id.index()]
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstData {
    pub ty: TypeId,
    pub value: ConstValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(Box<str>),
    Nil,
}
