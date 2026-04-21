use std::{cell::RefCell, fmt, marker::PhantomData};

use super::{
    handle_store::HandleStore,
    managed_rc::ManagedRc,
    value::{ExternHandleData, RuntimeError, Value},
};

pub trait AnvyxExternType: Sized + 'static {
    const TYPE_NAME: &'static str;
    fn with_store<R>(f: impl FnOnce(&RefCell<HandleStore<Self>>) -> R) -> R;
    fn cleanup(id: u64);
    fn to_display(id: u64) -> String;
}

pub fn extern_handle<T: AnvyxExternType>(value: T) -> Value {
    let id = T::with_store(|s| s.borrow_mut().insert(value));
    Value::ExternHandle(ManagedRc::new(ExternHandleData {
        id,
        drop_fn: T::cleanup,
        type_name: T::TYPE_NAME,
        to_string_fn: T::to_display,
    }))
}

/// A typed, refcounted handle to an extern value in the `HandleStore`.
///
/// Cloning only bumps the refcount. `into_anvyx` turns it into
/// `Value::ExternHandle` without touching the store.
pub struct ExternHandle<T: AnvyxExternType> {
    rc: ManagedRc<ExternHandleData>,
    _marker: PhantomData<T>,
}

impl<T: AnvyxExternType> ExternHandle<T> {
    /// Insert `value` into the store and return a handle to it
    pub fn new(value: T) -> Self {
        let id = T::with_store(|s| s.borrow_mut().insert(value));
        let rc = ManagedRc::new(ExternHandleData {
            id,
            drop_fn: T::cleanup,
            type_name: T::TYPE_NAME,
            to_string_fn: T::to_display,
        });
        Self {
            rc,
            _marker: PhantomData,
        }
    }

    /// Returns the store ID for this handle
    pub fn id(&self) -> u64 {
        self.rc.id
    }

    /// Borrow the stored value immutably and run `f` on it
    pub fn with_borrow<R>(&self, f: impl FnOnce(&T) -> R) -> Result<R, RuntimeError> {
        T::with_store(|s| {
            let store = s.borrow();
            let guard = store.borrow(self.rc.id)?;
            Ok(f(&guard))
        })
    }

    /// Borrow the stored value mutably and run `f` on it
    pub fn with_borrow_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> Result<R, RuntimeError> {
        T::with_store(|s| {
            let store = s.borrow();
            let mut guard = store.borrow_mut(self.rc.id)?;
            Ok(f(&mut guard))
        })
    }
}

impl<T: AnvyxExternType> Clone for ExternHandle<T> {
    fn clone(&self) -> Self {
        Self {
            rc: self.rc.clone(),
            _marker: PhantomData,
        }
    }
}

impl<T: AnvyxExternType> fmt::Debug for ExternHandle<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExternHandle")
            .field("type", &T::TYPE_NAME)
            .field("id", &self.rc.id)
            .finish()
    }
}

pub trait AnvyxConvert: Sized {
    fn anvyx_type() -> &'static str;
    fn anvyx_option_type() -> &'static str;
    fn into_anvyx(self) -> Value;
    fn from_anvyx(v: &Value) -> Result<Self, RuntimeError>;
}

macro_rules! impl_anvyx_convert {
    ($rust_ty:ty, $anvyx_name:literal, $variant:ident) => {
        impl AnvyxConvert for $rust_ty {
            fn anvyx_type() -> &'static str {
                $anvyx_name
            }
            fn anvyx_option_type() -> &'static str {
                concat!("Option<", $anvyx_name, ">")
            }
            fn into_anvyx(self) -> Value {
                Value::$variant(self)
            }
            fn from_anvyx(v: &Value) -> Result<Self, RuntimeError> {
                match v {
                    Value::$variant(n) => Ok(*n),
                    _ => Err(RuntimeError::new(concat!("expected ", $anvyx_name))),
                }
            }
        }
    };
}

impl_anvyx_convert!(i64, "int", Int);
impl_anvyx_convert!(f32, "float", Float);
impl_anvyx_convert!(f64, "double", Double);
impl_anvyx_convert!(bool, "bool", Bool);

impl AnvyxConvert for String {
    fn anvyx_type() -> &'static str {
        "string"
    }
    fn anvyx_option_type() -> &'static str {
        "Option<string>"
    }
    fn into_anvyx(self) -> Value {
        Value::String(ManagedRc::new(self))
    }
    fn from_anvyx(v: &Value) -> Result<Self, RuntimeError> {
        match v {
            Value::String(s) => Ok((**s).clone()),
            _ => Err(RuntimeError::new("expected string")),
        }
    }
}

impl AnvyxConvert for Value {
    fn anvyx_type() -> &'static str {
        "any"
    }
    fn anvyx_option_type() -> &'static str {
        "Option<any>"
    }
    fn into_anvyx(self) -> Value {
        self
    }
    fn from_anvyx(v: &Value) -> Result<Self, RuntimeError> {
        Ok(v.clone())
    }
}

impl AnvyxConvert for () {
    fn anvyx_type() -> &'static str {
        "void"
    }
    fn anvyx_option_type() -> &'static str {
        "void?"
    }
    fn into_anvyx(self) -> Value {
        Value::Nil
    }
    fn from_anvyx(_v: &Value) -> Result<Self, RuntimeError> {
        Ok(())
    }
}

impl AnvyxConvert for Vec<Value> {
    fn anvyx_type() -> &'static str {
        "list"
    }
    fn anvyx_option_type() -> &'static str {
        "Option<list>"
    }
    fn into_anvyx(self) -> Value {
        Value::List(ManagedRc::new(self))
    }
    fn from_anvyx(v: &Value) -> Result<Self, RuntimeError> {
        match v {
            Value::List(l) => Ok((**l).clone()),
            _ => Err(RuntimeError::new("expected list")),
        }
    }
}

impl<T: AnvyxExternType> AnvyxConvert for ExternHandle<T> {
    fn anvyx_type() -> &'static str {
        T::TYPE_NAME
    }
    fn anvyx_option_type() -> &'static str {
        ""
    }

    fn into_anvyx(self) -> Value {
        Value::ExternHandle(self.rc)
    }

    fn from_anvyx(v: &Value) -> Result<Self, RuntimeError> {
        let Value::ExternHandle(rc) = v else {
            return Err(RuntimeError::new(format!("expected {}", T::TYPE_NAME)));
        };
        if rc.type_name != T::TYPE_NAME {
            return Err(RuntimeError::new(format!(
                "expected {}, got {}",
                T::TYPE_NAME,
                rc.type_name
            )));
        }
        Ok(ExternHandle {
            rc: rc.clone(),
            _marker: PhantomData,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone)]
    struct TestPoint {
        x: f64,
        y: f64,
    }

    thread_local! {
        static TEST_POINT_STORE: RefCell<HandleStore<TestPoint>> = RefCell::new(HandleStore::new());
    }

    impl AnvyxExternType for TestPoint {
        const TYPE_NAME: &'static str = "TestPoint";

        fn with_store<R>(f: impl FnOnce(&RefCell<HandleStore<Self>>) -> R) -> R {
            TEST_POINT_STORE.with(f)
        }

        fn cleanup(id: u64) {
            Self::with_store(|s| {
                let _ = s.borrow_mut().remove(id);
            });
        }

        fn to_display(id: u64) -> String {
            Self::with_store(|s| {
                s.borrow().borrow(id).map_or_else(
                    |_| "<invalid>".to_string(),
                    |p| format!("TestPoint({}, {})", p.x, p.y),
                )
            })
        }
    }

    struct OtherType;

    thread_local! {
        static OTHER_STORE: RefCell<HandleStore<OtherType>> = RefCell::new(HandleStore::new());
    }

    impl AnvyxExternType for OtherType {
        const TYPE_NAME: &'static str = "OtherType";
        fn with_store<R>(f: impl FnOnce(&RefCell<HandleStore<Self>>) -> R) -> R {
            OTHER_STORE.with(f)
        }
        fn cleanup(id: u64) {
            Self::with_store(|s| {
                let _ = s.borrow_mut().remove(id);
            });
        }
        fn to_display(_id: u64) -> String {
            "<OtherType>".to_string()
        }
    }

    #[test]
    fn new_inserts_into_store() {
        let handle = ExternHandle::new(TestPoint { x: 1.0, y: 2.0 });
        let result = handle.with_borrow(|p| (p.x, p.y));
        assert_eq!(result.unwrap(), (1.0, 2.0));
    }

    #[test]
    fn clone_shares_same_store_entry() {
        let a = ExternHandle::new(TestPoint { x: 3.0, y: 4.0 });
        let b = a.clone();
        assert_eq!(a.id(), b.id());
        b.with_borrow_mut(|p| p.x = 99.0).unwrap();
        let x = a.with_borrow(|p| p.x).unwrap();
        assert_eq!(x, 99.0);
    }

    #[test]
    fn with_borrow_reads_value() {
        let handle = ExternHandle::new(TestPoint { x: 5.0, y: 6.0 });
        let sum = handle.with_borrow(|p| p.x + p.y).unwrap();
        assert_eq!(sum, 11.0);
    }

    #[test]
    fn with_borrow_mut_modifies_value() {
        let handle = ExternHandle::new(TestPoint { x: 1.0, y: 1.0 });
        handle
            .with_borrow_mut(|p| {
                p.x = 10.0;
                p.y = 20.0;
            })
            .unwrap();
        let result = handle.with_borrow(|p| (p.x, p.y)).unwrap();
        assert_eq!(result, (10.0, 20.0));
    }

    #[test]
    fn into_anvyx_produces_extern_handle_value() {
        let handle = ExternHandle::new(TestPoint { x: 7.0, y: 8.0 });
        let value = handle.into_anvyx();
        assert!(matches!(value, Value::ExternHandle(_)));
    }

    #[test]
    fn from_anvyx_roundtrip() {
        let handle = ExternHandle::new(TestPoint { x: 9.0, y: 10.0 });
        let value = handle.into_anvyx();
        let recovered = ExternHandle::<TestPoint>::from_anvyx(&value).unwrap();
        let result = recovered.with_borrow(|p| (p.x, p.y)).unwrap();
        assert_eq!(result, (9.0, 10.0));
    }

    #[test]
    fn from_anvyx_twice_succeeds() {
        let handle = ExternHandle::new(TestPoint { x: 1.0, y: 2.0 });
        let value = handle.into_anvyx();
        let a = ExternHandle::<TestPoint>::from_anvyx(&value).unwrap();
        let b = ExternHandle::<TestPoint>::from_anvyx(&value).unwrap();
        assert_eq!(a.id(), b.id());
    }

    #[test]
    fn from_anvyx_type_mismatch_errors() {
        let other_handle = ExternHandle::new(OtherType);
        let value = other_handle.into_anvyx();
        let result = ExternHandle::<TestPoint>::from_anvyx(&value);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message.contains("expected TestPoint"));
        assert!(err.message.contains("got OtherType"));
    }

    #[test]
    fn from_anvyx_non_handle_errors() {
        let value = Value::Int(42);
        let result = ExternHandle::<TestPoint>::from_anvyx(&value);
        assert!(result.is_err());
    }

    #[test]
    fn drop_cleans_up_store_entry() {
        let handle = ExternHandle::new(TestPoint { x: 0.0, y: 0.0 });
        let id = handle.id();
        drop(handle);
        let entry_exists = TestPoint::with_store(|s| s.borrow().borrow(id).is_ok());
        assert!(!entry_exists);
    }

    #[test]
    fn clone_keeps_entry_alive_after_original_drop() {
        let handle = ExternHandle::new(TestPoint { x: 1.0, y: 1.0 });
        let clone = handle.clone();
        let id = handle.id();
        drop(handle);
        let result = clone.with_borrow(|p| p.x).unwrap();
        assert_eq!(result, 1.0);
        drop(clone);
        let entry_exists = TestPoint::with_store(|s| s.borrow().borrow(id).is_ok());
        assert!(!entry_exists);
    }

    #[test]
    fn vec_value_into_anvyx_produces_list() {
        let v: Vec<Value> = vec![Value::Int(1), Value::Int(2), Value::Int(3)];
        let anvyx_val = v.into_anvyx();
        assert!(matches!(anvyx_val, Value::List(_)));
    }

    #[test]
    fn vec_value_roundtrip() {
        let original = vec![
            Value::Int(10),
            Value::String(ManagedRc::new("hello".into())),
            Value::Bool(true),
        ];
        let anvyx_val = original.clone().into_anvyx();
        let recovered = Vec::<Value>::from_anvyx(&anvyx_val).unwrap();
        assert_eq!(recovered.len(), 3);
        assert!(matches!(recovered[0], Value::Int(10)));
        assert!(matches!(recovered[2], Value::Bool(true)));
    }

    #[test]
    fn vec_value_empty_roundtrip() {
        let empty: Vec<Value> = vec![];
        let anvyx_val = empty.into_anvyx();
        let recovered = Vec::<Value>::from_anvyx(&anvyx_val).unwrap();
        assert!(recovered.is_empty());
    }

    #[test]
    fn vec_value_from_non_list_errors() {
        let int_val = Value::Int(42);
        let result = Vec::<Value>::from_anvyx(&int_val);
        assert!(result.is_err());
        assert!(result.unwrap_err().message.contains("expected list"));
    }

    #[test]
    fn vec_value_anvyx_type() {
        assert_eq!(Vec::<Value>::anvyx_type(), "list");
        assert_eq!(Vec::<Value>::anvyx_option_type(), "Option<list>");
    }
}
