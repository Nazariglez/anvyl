use std::{
    collections::HashMap,
    fmt,
    hash::{Hash, Hasher},
};

use indexmap::IndexMap;

use super::managed_rc::ManagedRc;

#[derive(Debug, Clone)]
pub enum MapStorage {
    Unordered(HashMap<Value, Value>),
    Ordered(IndexMap<Value, Value>),
}

impl MapStorage {
    pub fn new_unordered() -> Self {
        MapStorage::Unordered(HashMap::new())
    }

    pub fn new_ordered() -> Self {
        MapStorage::Ordered(IndexMap::new())
    }

    pub fn with_capacity_unordered(cap: usize) -> Self {
        MapStorage::Unordered(HashMap::with_capacity(cap))
    }

    pub fn get(&self, key: &Value) -> Option<&Value> {
        match self {
            MapStorage::Unordered(m) => m.get(key),
            MapStorage::Ordered(m) => m.get(key),
        }
    }

    pub fn insert(&mut self, key: Value, value: Value) -> Option<Value> {
        match self {
            MapStorage::Unordered(m) => m.insert(key, value),
            MapStorage::Ordered(m) => m.insert(key, value),
        }
    }

    pub fn remove(&mut self, key: &Value) -> Option<Value> {
        match self {
            MapStorage::Unordered(m) => m.remove(key),
            MapStorage::Ordered(m) => m.shift_remove(key),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            MapStorage::Unordered(m) => m.len(),
            MapStorage::Ordered(m) => m.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_ordered(&self) -> bool {
        matches!(self, MapStorage::Ordered(_))
    }

    pub fn iter(&self) -> Box<dyn Iterator<Item = (&Value, &Value)> + '_> {
        match self {
            MapStorage::Unordered(m) => Box::new(m.iter()),
            MapStorage::Ordered(m) => Box::new(m.iter()),
        }
    }

    pub fn get_index(&self, idx: usize) -> Option<(&Value, &Value)> {
        match self {
            MapStorage::Ordered(m) => m.get_index(idx),
            MapStorage::Unordered(m) => m.iter().nth(idx),
        }
    }

    pub fn drain(&mut self) -> Box<dyn Iterator<Item = (Value, Value)> + '_> {
        match self {
            MapStorage::Unordered(m) => Box::new(m.drain()),
            MapStorage::Ordered(m) => Box::new(m.drain(..)),
        }
    }
}

impl<'a> IntoIterator for &'a MapStorage {
    type Item = (&'a Value, &'a Value);
    type IntoIter = Box<dyn Iterator<Item = Self::Item> + 'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl PartialEq for MapStorage {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for (k, v) in self {
            match other.get(k) {
                Some(ov) if ov == v => {}
                _ => return false,
            }
        }
        true
    }
}

impl Eq for MapStorage {}

impl Hash for MapStorage {
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructData {
    pub type_id: u32,
    pub fields: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumData {
    pub type_id: u32,
    pub variant: u16,
    pub fields: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClosureData {
    pub fn_id: u16,
    pub captures: Vec<Value>,
}

pub struct ExternHandleData {
    pub id: u64,
    pub drop_fn: fn(u64),
    pub type_name: &'static str,
    pub to_string_fn: fn(u64) -> String,
}

pub struct DisplayDetect<'a, T>(pub &'a T);

pub trait DisplayDetectFallback {
    fn anvyx_display(&self, name: &str) -> String;
}

impl<T> DisplayDetectFallback for DisplayDetect<'_, T> {
    fn anvyx_display(&self, name: &str) -> String {
        format!("<{name}>")
    }
}

impl<T: fmt::Display> DisplayDetect<'_, T> {
    pub fn anvyx_display(&self, _name: &str) -> String {
        self.0.to_string()
    }
}

impl Drop for ExternHandleData {
    fn drop(&mut self) {
        (self.drop_fn)(self.id);
    }
}

impl fmt::Debug for ExternHandleData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExternHandleData")
            .field("id", &self.id)
            .finish_non_exhaustive()
    }
}

impl PartialEq for ExternHandleData {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for ExternHandleData {}

impl Hash for ExternHandleData {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RefPath {
    pub root: u32,
    pub depth: u8,
    pub segments: [u16; 4],
}

impl RefPath {
    pub fn new(root: u32, depth: u8, segments: [u16; 4]) -> Self {
        assert!(depth <= 4, "RefPath depth exceeds maximum of 4");
        Self {
            root,
            depth,
            segments,
        }
    }

    pub fn path(&self) -> &[u16] {
        &self.segments[..self.depth as usize]
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f32),
    Double(f64),
    Bool(bool),
    Nil,
    String(ManagedRc<String>),
    List(ManagedRc<Vec<Value>>),
    Array(ManagedRc<Vec<Value>>),
    Map(ManagedRc<MapStorage>),
    Struct(ManagedRc<StructData>),
    Tuple(ManagedRc<Vec<Value>>),
    Enum(ManagedRc<EnumData>),
    ExternHandle(ManagedRc<ExternHandleData>),
    DataRef(ManagedRc<StructData>),
    Closure(ManagedRc<ClosureData>),
    Slice {
        data: ManagedRc<Vec<Value>>,
        offset: usize,
        len: usize,
    },
    StackRef(u32), // absolute stack index of the referenced value
    PathRef(RefPath),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Double(a), Value::Double(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::List(a), Value::List(b))
            | (Value::Array(a), Value::Array(b))
            | (Value::Tuple(a), Value::Tuple(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => a == b,
            (Value::Struct(a), Value::Struct(b)) => a == b,
            (Value::Enum(a), Value::Enum(b)) => a == b,
            (Value::ExternHandle(a), Value::ExternHandle(b)) => a == b,
            (Value::DataRef(a), Value::DataRef(b)) => ManagedRc::ptr_eq(a, b),
            (Value::Closure(a), Value::Closure(b)) => a == b,
            (
                Value::Slice {
                    data: a,
                    offset: ao,
                    len: al,
                },
                Value::Slice {
                    data: b,
                    offset: bo,
                    len: bl,
                },
            ) => al == bl && a[*ao..*ao + *al] == b[*bo..*bo + *bl],
            (Value::StackRef(a), Value::StackRef(b)) => a == b,
            (Value::PathRef(a), Value::PathRef(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Value::Int(v) => v.hash(state),
            Value::Float(v) => v.to_bits().hash(state),
            Value::Double(v) => v.to_bits().hash(state),
            Value::Bool(v) => v.hash(state),
            Value::Nil | Value::Map(_) => {}
            Value::String(s) => s.hash(state),
            Value::List(l) => l.hash(state),
            Value::Array(a) => a.hash(state),
            Value::Struct(s) => s.hash(state),
            Value::Tuple(t) => t.hash(state),
            Value::Enum(e) => e.hash(state),
            Value::ExternHandle(data) => data.hash(state),
            Value::DataRef(s) => (s.as_ptr() as usize).hash(state),
            Value::Closure(c) => c.hash(state),
            Value::Slice { data, offset, len } => {
                data[*offset..*offset + *len].hash(state);
            }
            Value::StackRef(idx) => idx.hash(state),
            Value::PathRef(p) => p.hash(state),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{v}"),
            Value::Float(v) => {
                if v.fract() == 0.0 && v.is_finite() {
                    write!(f, "{v:.1}")
                } else {
                    write!(f, "{v}")
                }
            }
            Value::Double(v) => {
                if v.fract() == 0.0 && v.is_finite() {
                    write!(f, "{v:.1}")
                } else {
                    write!(f, "{v}")
                }
            }
            Value::Bool(v) => write!(f, "{v}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Nil => write!(f, "nil"),
            Value::List(l) => {
                write!(f, "[")?;
                for (i, v) in l.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{v}")?;
                }
                write!(f, "]")
            }
            Value::Array(a) => {
                write!(f, "[")?;
                for (i, v) in a.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{v}")?;
                }
                write!(f, "]")
            }
            Value::Map(m) => {
                write!(f, "[")?;
                if m.is_ordered() {
                    for (i, (k, v)) in m.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{k}: {v}")?;
                    }
                } else {
                    let mut entries: Vec<_> = m.iter().collect();
                    entries.sort_by_key(|(a, _)| a.to_string());
                    for (i, (k, v)) in entries.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{k}: {v}")?;
                    }
                }
                write!(f, "]")
            }
            Value::Struct(s) => write!(f, "<struct:{}>", s.type_id),
            Value::Tuple(t) => {
                write!(f, "(")?;
                for (i, v) in t.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{v}")?;
                }
                write!(f, ")")
            }
            Value::Enum(e) => write!(f, "<enum:{}:{}>", e.type_id, e.variant),
            Value::ExternHandle(data) => write!(f, "<extern:{}>", data.id),
            Value::DataRef(s) => write!(f, "<dataref:{}>", s.type_id),
            Value::Closure(_) => write!(f, "<fn>"),
            Value::Slice { data, offset, len } => {
                write!(f, "[")?;
                for (i, v) in data[*offset..*offset + *len].iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{v}")?;
                }
                write!(f, "]")
            }
            Value::StackRef(idx) => write!(f, "<ref@{idx}>"),
            Value::PathRef(p) => {
                write!(f, "<pathref@{}[", p.root)?;
                for (i, seg) in p.path().iter().enumerate() {
                    if i > 0 {
                        write!(f, ".")?;
                    }
                    write!(f, "{seg}")?;
                }
                write!(f, "]>")
            }
        }
    }
}

/// Shared error type for all VM-layer failures
#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeError {
    pub message: String,
}

impl RuntimeError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub fn type_name(v: &Value) -> &'static str {
    match v {
        Value::Int(_) => "int",
        Value::Float(_) => "float",
        Value::Double(_) => "double",
        Value::Bool(_) => "bool",
        Value::String(_) => "string",
        Value::Nil => "nil",
        Value::List(_) => "list",
        Value::Array(_) => "array",
        Value::Map(_) => "map",
        Value::Struct(_) => "struct",
        Value::Tuple(_) => "tuple",
        Value::Enum(_) => "enum",
        Value::ExternHandle(_) => "extern handle",
        Value::DataRef(_) => "dataref",
        Value::Closure(_) => "fn",
        Value::Slice { .. } => "slice",
        Value::StackRef(_) => "stackref",
        Value::PathRef(_) => "pathref",
    }
}

pub fn value_add(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
        (Value::Double(a), Value::Double(b)) => Ok(Value::Double(a + b)),
        // string concatenation, either side may be a non-string
        (Value::String(a), b) => Ok(Value::String(ManagedRc::new(format!("{a}{b}")))),
        (a, Value::String(b)) => Ok(Value::String(ManagedRc::new(format!("{a}{b}")))),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: cannot add {} and {}",
            type_name(&a),
            type_name(&b)
        ))),
    }
}

pub fn value_sub(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
        (Value::Double(a), Value::Double(b)) => Ok(Value::Double(a - b)),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: cannot subtract {} from {}",
            type_name(&b),
            type_name(&a)
        ))),
    }
}

pub fn value_mul(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
        (Value::Double(a), Value::Double(b)) => Ok(Value::Double(a * b)),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: cannot multiply {} and {}",
            type_name(&a),
            type_name(&b)
        ))),
    }
}

pub fn value_div(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => {
            if b == 0 {
                Err(RuntimeError::new("division by zero"))
            } else {
                Ok(Value::Int(a / b))
            }
        }
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
        (Value::Double(a), Value::Double(b)) => Ok(Value::Double(a / b)),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: cannot divide {} by {}",
            type_name(&a),
            type_name(&b)
        ))),
    }
}

pub fn value_rem(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => {
            if b == 0 {
                Err(RuntimeError::new("remainder by zero"))
            } else {
                Ok(Value::Int(a % b))
            }
        }
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a % b)),
        (Value::Double(a), Value::Double(b)) => Ok(Value::Double(a % b)),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: cannot compute remainder of {} and {}",
            type_name(&a),
            type_name(&b)
        ))),
    }
}

pub fn value_negate(v: Value) -> Result<Value, RuntimeError> {
    match v {
        Value::Int(n) => Ok(Value::Int(-n)),
        Value::Float(f) => Ok(Value::Float(-f)),
        Value::Double(f) => Ok(Value::Double(-f)),
        other => Err(RuntimeError::new(format!(
            "type error: cannot negate {}",
            type_name(&other)
        ))),
    }
}

pub fn value_not(v: Value) -> Result<Value, RuntimeError> {
    match v {
        Value::Bool(b) => Ok(Value::Bool(!b)),
        other => Err(RuntimeError::new(format!(
            "type error: logical not requires bool, got {}",
            type_name(&other)
        ))),
    }
}

pub fn value_eq(lhs: &Value, rhs: &Value) -> Value {
    Value::Bool(lhs == rhs)
}

pub fn value_neq(lhs: &Value, rhs: &Value) -> Value {
    Value::Bool(lhs != rhs)
}

pub fn value_lt(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
        (Value::Double(a), Value::Double(b)) => Ok(Value::Bool(a < b)),
        (Value::String(a), Value::String(b)) => Ok(Value::Bool(a < b)),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: cannot compare {} < {}",
            type_name(&a),
            type_name(&b)
        ))),
    }
}

pub fn value_gt(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
        (Value::Double(a), Value::Double(b)) => Ok(Value::Bool(a > b)),
        (Value::String(a), Value::String(b)) => Ok(Value::Bool(a > b)),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: cannot compare {} > {}",
            type_name(&a),
            type_name(&b)
        ))),
    }
}

pub fn value_lte(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
        (Value::Double(a), Value::Double(b)) => Ok(Value::Bool(a <= b)),
        (Value::String(a), Value::String(b)) => Ok(Value::Bool(a <= b)),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: cannot compare {} <= {}",
            type_name(&a),
            type_name(&b)
        ))),
    }
}

pub fn value_gte(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
        (Value::Double(a), Value::Double(b)) => Ok(Value::Bool(a >= b)),
        (Value::String(a), Value::String(b)) => Ok(Value::Bool(a >= b)),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: cannot compare {} >= {}",
            type_name(&a),
            type_name(&b)
        ))),
    }
}

pub fn value_and(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: logical and requires bool, got {} and {}",
            type_name(&a),
            type_name(&b)
        ))),
    }
}

pub fn value_or(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: logical or requires bool, got {} and {}",
            type_name(&a),
            type_name(&b)
        ))),
    }
}

pub fn value_xor(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a ^ b)),
        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a ^ b)),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: xor requires int or bool, got {} and {}",
            type_name(&a),
            type_name(&b)
        ))),
    }
}

pub fn value_bit_and(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a & b)),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: bitwise AND requires int, got {} and {}",
            type_name(&a),
            type_name(&b)
        ))),
    }
}

pub fn value_bit_or(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a | b)),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: bitwise OR requires int, got {} and {}",
            type_name(&a),
            type_name(&b)
        ))),
    }
}

pub fn value_bit_not(val: Value) -> Result<Value, RuntimeError> {
    match val {
        Value::Int(v) => Ok(Value::Int(!v)),
        other => Err(RuntimeError::new(format!(
            "type error: bitwise NOT requires int, got {}",
            type_name(&other)
        ))),
    }
}

pub fn value_shl(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_shl(b as u32))),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: left shift requires int, got {} and {}",
            type_name(&a),
            type_name(&b)
        ))),
    }
}

pub fn value_shr(lhs: Value, rhs: Value) -> Result<Value, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_shr(b as u32))),
        (a, b) => Err(RuntimeError::new(format!(
            "type error: right shift requires int, got {} and {}",
            type_name(&a),
            type_name(&b)
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::managed_rc::ManagedRc;

    fn s(text: &str) -> Value {
        Value::String(ManagedRc::new(text.to_string()))
    }

    #[test]
    fn display_int() {
        assert_eq!(Value::Int(42).to_string(), "42");
    }

    #[test]
    fn display_float() {
        assert_eq!(Value::Float(std::f32::consts::PI).to_string(), "3.1415927");
        assert_eq!(Value::Float(42.0_f32).to_string(), "42.0");
    }

    #[test]
    fn display_double() {
        assert_eq!(
            Value::Double(std::f64::consts::PI).to_string(),
            "3.141592653589793"
        );
        assert_eq!(Value::Double(42.0).to_string(), "42.0");
    }

    #[test]
    fn display_bool() {
        assert_eq!(Value::Bool(true).to_string(), "true");
        assert_eq!(Value::Bool(false).to_string(), "false");
    }

    #[test]
    fn display_string() {
        assert_eq!(s("hello").to_string(), "hello");
    }

    #[test]
    fn display_nil() {
        assert_eq!(Value::Nil.to_string(), "nil");
    }

    #[test]
    fn display_empty_list() {
        let v = Value::List(ManagedRc::new(vec![]));
        assert_eq!(v.to_string(), "[]");
    }

    #[test]
    fn display_list() {
        let v = Value::List(ManagedRc::new(vec![Value::Int(1), Value::Int(2)]));
        assert_eq!(v.to_string(), "[1, 2]");
    }

    #[test]
    fn display_empty_map() {
        let v = Value::Map(ManagedRc::new(MapStorage::new_unordered()));
        assert_eq!(v.to_string(), "[]");
    }

    #[test]
    fn display_map() {
        let mut storage = MapStorage::new_unordered();
        storage.insert(s("key"), Value::Int(42));
        let v = Value::Map(ManagedRc::new(storage));
        assert_eq!(v.to_string(), "[key: 42]");
    }

    #[test]
    fn display_struct() {
        let v = Value::Struct(ManagedRc::new(StructData {
            type_id: 7,
            fields: vec![],
        }));
        assert_eq!(v.to_string(), "<struct:7>");
    }

    #[test]
    fn display_empty_tuple() {
        let v = Value::Tuple(ManagedRc::new(vec![]));
        assert_eq!(v.to_string(), "()");
    }

    #[test]
    fn display_tuple() {
        let v = Value::Tuple(ManagedRc::new(vec![Value::Int(1), Value::Bool(true)]));
        assert_eq!(v.to_string(), "(1, true)");
    }

    #[test]
    fn display_enum() {
        let v = Value::Enum(ManagedRc::new(EnumData {
            type_id: 3,
            variant: 1,
            fields: vec![],
        }));
        assert_eq!(v.to_string(), "<enum:3:1>");
    }

    #[test]
    fn type_name_list() {
        assert_eq!(type_name(&Value::List(ManagedRc::new(vec![]))), "list");
    }

    #[test]
    fn type_name_map() {
        assert_eq!(
            type_name(&Value::Map(ManagedRc::new(MapStorage::new_unordered()))),
            "map"
        );
    }

    #[test]
    fn type_name_struct() {
        let v = Value::Struct(ManagedRc::new(StructData {
            type_id: 0,
            fields: vec![],
        }));
        assert_eq!(type_name(&v), "struct");
    }

    #[test]
    fn type_name_tuple() {
        assert_eq!(type_name(&Value::Tuple(ManagedRc::new(vec![]))), "tuple");
    }

    #[test]
    fn type_name_enum() {
        let v = Value::Enum(ManagedRc::new(EnumData {
            type_id: 0,
            variant: 0,
            fields: vec![],
        }));
        assert_eq!(type_name(&v), "enum");
    }

    #[test]
    fn list_clone_shares_rc() {
        let v = Value::List(ManagedRc::new(vec![Value::Int(1)]));
        let v2 = v.clone();
        let Value::List(rc) = v else { panic!() };
        let Value::List(rc2) = v2 else { panic!() };
        assert_eq!(rc.strong_count(), 2);
        assert!(ManagedRc::ptr_eq(&rc, &rc2));
    }

    #[test]
    fn struct_clone_shares_rc() {
        let v = Value::Struct(ManagedRc::new(StructData {
            type_id: 1,
            fields: vec![],
        }));
        let v2 = v.clone();
        let Value::Struct(rc) = v else { panic!() };
        let Value::Struct(rc2) = v2 else { panic!() };
        assert_eq!(rc.strong_count(), 2);
        assert!(ManagedRc::ptr_eq(&rc, &rc2));
    }

    #[test]
    fn map_clone_shares_rc() {
        let v = Value::Map(ManagedRc::new(MapStorage::new_unordered()));
        let v2 = v.clone();
        let Value::Map(rc) = v else { panic!() };
        let Value::Map(rc2) = v2 else { panic!() };
        assert_eq!(rc.strong_count(), 2);
        assert!(ManagedRc::ptr_eq(&rc, &rc2));
    }

    #[test]
    fn map_eq_across_storage_variants() {
        let mut unordered = MapStorage::new_unordered();
        unordered.insert(s("a"), Value::Int(1));
        let mut ordered = MapStorage::new_ordered();
        ordered.insert(s("a"), Value::Int(1));
        let a = Value::Map(ManagedRc::new(unordered));
        let b = Value::Map(ManagedRc::new(ordered));
        assert_eq!(a, b);
    }

    #[test]
    fn list_eq() {
        let a = Value::List(ManagedRc::new(vec![Value::Int(1), Value::Int(2)]));
        let b = Value::List(ManagedRc::new(vec![Value::Int(1), Value::Int(2)]));
        assert_eq!(a, b);
    }

    #[test]
    fn list_neq() {
        let a = Value::List(ManagedRc::new(vec![Value::Int(1)]));
        let b = Value::List(ManagedRc::new(vec![Value::Int(2)]));
        assert_ne!(a, b);
    }

    #[test]
    fn display_array() {
        let v = Value::Array(ManagedRc::new(vec![Value::Int(1), Value::Int(2)]));
        assert_eq!(v.to_string(), "[1, 2]");
    }

    #[test]
    fn display_empty_array() {
        let v = Value::Array(ManagedRc::new(vec![]));
        assert_eq!(v.to_string(), "[]");
    }

    #[test]
    fn type_name_array() {
        assert_eq!(type_name(&Value::Array(ManagedRc::new(vec![]))), "array");
    }

    #[test]
    fn array_eq() {
        let a = Value::Array(ManagedRc::new(vec![Value::Int(1), Value::Int(2)]));
        let b = Value::Array(ManagedRc::new(vec![Value::Int(1), Value::Int(2)]));
        assert_eq!(a, b);
    }

    #[test]
    fn array_neq() {
        let a = Value::Array(ManagedRc::new(vec![Value::Int(1)]));
        let b = Value::Array(ManagedRc::new(vec![Value::Int(2)]));
        assert_ne!(a, b);
    }

    #[test]
    fn array_clone_shares_rc() {
        let v = Value::Array(ManagedRc::new(vec![Value::Int(1)]));
        let v2 = v.clone();
        let Value::Array(rc) = v else { panic!() };
        let Value::Array(rc2) = v2 else { panic!() };
        assert_eq!(rc.strong_count(), 2);
        assert!(ManagedRc::ptr_eq(&rc, &rc2));
    }

    #[test]
    fn tuple_eq() {
        let a = Value::Tuple(ManagedRc::new(vec![Value::Int(1), Value::Bool(true)]));
        let b = Value::Tuple(ManagedRc::new(vec![Value::Int(1), Value::Bool(true)]));
        assert_eq!(a, b);
    }

    #[test]
    fn struct_eq() {
        let a = Value::Struct(ManagedRc::new(StructData {
            type_id: 5,
            fields: vec![Value::Int(10)],
        }));
        let b = Value::Struct(ManagedRc::new(StructData {
            type_id: 5,
            fields: vec![Value::Int(10)],
        }));
        assert_eq!(a, b);
    }

    #[test]
    fn struct_neq_different_type_id() {
        let a = Value::Struct(ManagedRc::new(StructData {
            type_id: 1,
            fields: vec![],
        }));
        let b = Value::Struct(ManagedRc::new(StructData {
            type_id: 2,
            fields: vec![],
        }));
        assert_ne!(a, b);
    }

    #[test]
    fn enum_eq() {
        let a = Value::Enum(ManagedRc::new(EnumData {
            type_id: 3,
            variant: 1,
            fields: vec![Value::Int(99)],
        }));
        let b = Value::Enum(ManagedRc::new(EnumData {
            type_id: 3,
            variant: 1,
            fields: vec![Value::Int(99)],
        }));
        assert_eq!(a, b);
    }

    #[test]
    fn enum_neq_different_variant() {
        let a = Value::Enum(ManagedRc::new(EnumData {
            type_id: 1,
            variant: 0,
            fields: vec![],
        }));
        let b = Value::Enum(ManagedRc::new(EnumData {
            type_id: 1,
            variant: 1,
            fields: vec![],
        }));
        assert_ne!(a, b);
    }

    #[test]
    fn add_int() {
        assert_eq!(value_add(Value::Int(1), Value::Int(2)), Ok(Value::Int(3)));
    }

    #[test]
    fn add_float() {
        assert_eq!(
            value_add(Value::Float(1.0_f32), Value::Float(2.0_f32)),
            Ok(Value::Float(3.0_f32))
        );
    }

    #[test]
    fn add_double() {
        assert_eq!(
            value_add(Value::Double(1.0), Value::Double(2.0)),
            Ok(Value::Double(3.0))
        );
    }

    #[test]
    fn add_string_concat() {
        assert_eq!(value_add(s("a"), s("b")), Ok(s("ab")));
    }

    #[test]
    fn add_string_int_concat() {
        assert_eq!(value_add(s("n="), Value::Int(3)), Ok(s("n=3")));
    }

    #[test]
    fn add_int_string_concat() {
        assert_eq!(value_add(Value::Int(3), s("!")), Ok(s("3!")));
    }

    #[test]
    fn add_float_string_concat() {
        assert_eq!(value_add(Value::Float(1.5_f32), s("x")), Ok(s("1.5x")));
    }

    #[test]
    fn add_type_mismatch_error() {
        assert!(value_add(Value::Int(1), Value::Bool(true)).is_err());
    }

    #[test]
    fn sub_int() {
        assert_eq!(value_sub(Value::Int(5), Value::Int(3)), Ok(Value::Int(2)));
    }

    #[test]
    fn mul_int() {
        assert_eq!(value_mul(Value::Int(3), Value::Int(4)), Ok(Value::Int(12)));
    }

    #[test]
    fn div_int() {
        assert_eq!(value_div(Value::Int(10), Value::Int(3)), Ok(Value::Int(3)));
    }

    #[test]
    fn div_by_zero_error() {
        assert!(value_div(Value::Int(1), Value::Int(0)).is_err());
    }

    #[test]
    fn rem_int() {
        assert_eq!(value_rem(Value::Int(10), Value::Int(3)), Ok(Value::Int(1)));
    }

    #[test]
    fn negate_int() {
        assert_eq!(value_negate(Value::Int(5)), Ok(Value::Int(-5)));
    }

    #[test]
    fn negate_float() {
        assert_eq!(
            value_negate(Value::Float(1.5_f32)),
            Ok(Value::Float(-1.5_f32))
        );
    }

    #[test]
    fn negate_double() {
        assert_eq!(value_negate(Value::Double(1.5)), Ok(Value::Double(-1.5)));
    }

    #[test]
    fn negate_type_error() {
        assert!(value_negate(Value::Bool(true)).is_err());
    }

    #[test]
    fn not_bool() {
        assert_eq!(value_not(Value::Bool(true)), Ok(Value::Bool(false)));
        assert_eq!(value_not(Value::Bool(false)), Ok(Value::Bool(true)));
    }

    #[test]
    fn not_type_error() {
        assert!(value_not(Value::Int(1)).is_err());
    }

    #[test]
    fn eq_same_type() {
        assert_eq!(value_eq(&Value::Int(1), &Value::Int(1)), Value::Bool(true));
        assert_eq!(value_eq(&Value::Int(1), &Value::Int(2)), Value::Bool(false));
    }

    #[test]
    fn eq_different_types_is_false() {
        assert_eq!(
            value_eq(&Value::Int(1), &Value::Bool(true)),
            Value::Bool(false)
        );
    }

    #[test]
    fn neq_same_type() {
        assert_eq!(value_neq(&Value::Int(1), &Value::Int(2)), Value::Bool(true));
    }

    #[test]
    fn lt_int() {
        assert_eq!(
            value_lt(Value::Int(1), Value::Int(2)),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            value_lt(Value::Int(2), Value::Int(1)),
            Ok(Value::Bool(false))
        );
    }

    #[test]
    fn gt_float() {
        assert_eq!(
            value_gt(Value::Float(2.0_f32), Value::Float(1.0_f32)),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn gt_double() {
        assert_eq!(
            value_gt(Value::Double(2.0), Value::Double(1.0)),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn lte_string() {
        assert_eq!(value_lte(s("abc"), s("abc")), Ok(Value::Bool(true)));
        assert_eq!(value_lte(s("abc"), s("abd")), Ok(Value::Bool(true)));
    }

    #[test]
    fn gte_int() {
        assert_eq!(
            value_gte(Value::Int(3), Value::Int(3)),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn comparison_type_error() {
        assert!(value_lt(Value::Int(1), Value::Float(1.0_f32)).is_err());
    }

    #[test]
    fn type_name_double() {
        assert_eq!(type_name(&Value::Double(1.0)), "double");
    }

    #[test]
    fn and_bools() {
        assert_eq!(
            value_and(Value::Bool(true), Value::Bool(false)),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            value_and(Value::Bool(true), Value::Bool(true)),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn or_bools() {
        assert_eq!(
            value_or(Value::Bool(false), Value::Bool(true)),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            value_or(Value::Bool(false), Value::Bool(false)),
            Ok(Value::Bool(false))
        );
    }

    #[test]
    fn xor_bools() {
        assert_eq!(
            value_xor(Value::Bool(true), Value::Bool(true)),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            value_xor(Value::Bool(true), Value::Bool(false)),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn logical_type_error() {
        assert!(value_and(Value::Int(1), Value::Bool(true)).is_err());
    }

    fn noop_drop(_id: u64) {}

    fn extern_handle(id: u64) -> Value {
        Value::ExternHandle(ManagedRc::new(ExternHandleData {
            id,
            drop_fn: noop_drop,
            type_name: "Extern",
            to_string_fn: |_| "<Extern>".to_string(),
        }))
    }

    #[test]
    fn extern_handle_display() {
        assert_eq!(extern_handle(99).to_string(), "<extern:99>");
    }

    #[test]
    fn extern_handle_eq_by_id() {
        assert_eq!(extern_handle(42), extern_handle(42));
        assert_ne!(extern_handle(1), extern_handle(2));
    }

    #[test]
    fn extern_handle_clone_shares_rc() {
        let v = extern_handle(10);
        let v2 = v.clone();
        let Value::ExternHandle(rc) = v else { panic!() };
        let Value::ExternHandle(rc2) = v2 else {
            panic!()
        };
        assert_eq!(rc.strong_count(), 2);
        assert!(ManagedRc::ptr_eq(&rc, &rc2));
    }

    #[test]
    fn extern_handle_data_drop_calls_cleanup() {
        use std::cell::Cell;
        thread_local! { static CLEANED: Cell<u64> = const { Cell::new(0) }; }
        fn cleanup(id: u64) {
            CLEANED.with(|c| c.set(id));
        }
        {
            let _v = Value::ExternHandle(ManagedRc::new(ExternHandleData {
                id: 77,
                drop_fn: cleanup,
                type_name: "Test",
                to_string_fn: |_| "<Test>".to_string(),
            }));
        }
        CLEANED.with(|c| assert_eq!(c.get(), 77));
    }

    #[test]
    fn extern_handle_data_shared_drop_once() {
        use std::cell::Cell;
        thread_local! { static COUNT: Cell<u32> = const { Cell::new(0) }; }
        fn cleanup(_id: u64) {
            COUNT.with(|c| c.set(c.get() + 1));
        }
        COUNT.with(|c| c.set(0));
        {
            let v = Value::ExternHandle(ManagedRc::new(ExternHandleData {
                id: 1,
                drop_fn: cleanup,
                type_name: "Test",
                to_string_fn: |_| "<Test>".to_string(),
            }));
            let _v2 = v.clone();
            let _v3 = v.clone();
        }
        COUNT.with(|c| assert_eq!(c.get(), 1));
    }

    #[test]
    fn extern_handle_in_list_cleanup_on_drop() {
        use std::cell::Cell;
        thread_local! { static CLEANED: Cell<u32> = const { Cell::new(0) }; }
        fn cleanup(_id: u64) {
            CLEANED.with(|c| c.set(c.get() + 1));
        }
        CLEANED.with(|c| c.set(0));
        {
            let handle = Value::ExternHandle(ManagedRc::new(ExternHandleData {
                id: 42,
                drop_fn: cleanup,
                type_name: "Test",
                to_string_fn: |_| "<Test>".to_string(),
            }));
            let list = Value::List(ManagedRc::new(vec![handle]));
            CLEANED.with(|c| assert_eq!(c.get(), 0));
            drop(list);
        }
        CLEANED.with(|c| assert_eq!(c.get(), 1));
    }
}
