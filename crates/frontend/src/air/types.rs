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

    pub fn get(&self, id: TypeId) -> Option<&TypeData> {
        self.data.get(id.index())
    }

    pub fn data_mut(&mut self, id: TypeId) -> &mut TypeData {
        &mut self.data[id.index()]
    }

    pub fn iter(&self) -> impl Iterator<Item = &TypeData> {
        self.data.iter()
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
    Array {
        elem: TypeId,
        len: usize,
    },
    Map {
        key: TypeId,
        value: TypeId,
        order: MapOrder,
    },
    Slice(TypeId),
    Function(SignatureType),
    Dyn(DynContractData),
    Aggregate(AggregateId),
    Enum(EnumId),
    DataRef(AggregateId),
    Extern(ExternTypeId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MapOrder {
    Insertion,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DynContractData {
    pub display_name: String,
    pub method_table_key: String,
    pub concrete_printer: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureType {
    pub params: Vec<ParamType>,
    pub ret: ReturnMode,
}

impl SignatureType {
    pub fn new(params: Vec<ParamType>, ret: ReturnMode) -> Self {
        Self { params, ret }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParamMode {
    Value,
    SharedBorrow,
    MutBorrow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParamType {
    pub ty: TypeId,
    pub mode: ParamMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReturnMode {
    Value(TypeId),
    Place(TypeId),
}

impl ReturnMode {
    pub fn ty(self) -> TypeId {
        match self {
            Self::Value(ty) | Self::Place(ty) => ty,
        }
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

    pub fn get_checked(&self, id: ConstId) -> Option<&ConstData> {
        self.data.get(id.index())
    }

    pub fn iter(&self) -> impl Iterator<Item = &ConstData> {
        self.data.iter()
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
