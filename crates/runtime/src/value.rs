use std::{borrow::Borrow, fmt, hash::Hash, marker::PhantomData, ops::Range, ptr::NonNull};

use ecow::EcoString;

use crate::{
    CollectionLoanState, Ctx, HeapType, ListStorage, MapStorage, RuntimeError, ShapeLoanGuard,
    Trace, TraceDriver, ValueLoanGuard, Visitor,
    cow_storage::{CowStorageOwner, CowStorageView},
    heap_access_error,
};

#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AnvString {
    text: EcoString,
}

impl AnvString {
    pub fn as_str(&self) -> &str {
        self.text.as_str()
    }

    #[must_use]
    pub fn share(&self) -> Self {
        self.clone()
    }

    pub fn len(&self) -> usize {
        self.text.len()
    }

    pub fn is_empty(&self) -> bool {
        self.text.is_empty()
    }

    pub fn concat(parts: impl IntoIterator<Item = impl AsRef<str>>) -> Self {
        let mut text = EcoString::new();
        for part in parts {
            text.push_str(part.as_ref());
        }
        Self { text }
    }
}

impl From<&str> for AnvString {
    fn from(text: &str) -> Self {
        Self { text: text.into() }
    }
}

impl From<String> for AnvString {
    fn from(text: String) -> Self {
        Self { text: text.into() }
    }
}

impl AsRef<str> for AnvString {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Borrow<str> for AnvString {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Debug for AnvString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl fmt::Display for AnvString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

pub fn display_float(value: f64) -> String {
    let mut text = value.to_string();
    if value.is_finite() && !text.contains(['.', 'e', 'E']) {
        text.push_str(".0");
    }
    text
}

// SAFETY: `AnvString` owns only `EcoString` text and cannot contain heap handles.
unsafe impl<'cx> Trace<'cx> for AnvString {
    #[inline]
    fn trace<D: TraceDriver<'cx>>(&self, _visitor: &mut Visitor<'cx, '_, D>) {}
}

pub struct AnvList<'cx, T> {
    storage: CowStorageOwner<'cx, ListStorage<'cx, T>>,
    storage_ty: HeapType<'cx, ListStorage<'cx, T>>,
    loan: CollectionLoanState,
    len: usize,
}

impl<'cx, T: 'cx> AnvList<'cx, T> {
    pub fn from_elems<'rt>(
        ctx: &mut Ctx<'cx, 'rt>,
        storage_ty: HeapType<'cx, ListStorage<'cx, T>>,
        elems: impl IntoIterator<Item = T>,
    ) -> Self {
        let storage = ListStorage::from_elems(elems);
        let len = storage.len();
        Self {
            storage: CowStorageOwner::alloc(ctx.heap(), storage_ty, storage),
            storage_ty,
            loan: CollectionLoanState::default(),
            len,
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn len_i64(&self) -> i64 {
        i64::try_from(self.len()).expect("list length exceeds i64::MAX")
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn storage_view(&self) -> CowStorageView<'cx, ListStorage<'cx, T>> {
        self.storage.view()
    }

    pub fn begin_shape_loan(&self) -> Result<ShapeLoanGuard, RuntimeError> {
        self.loan.begin_shape_loan()
    }

    pub fn structural_version(&self) -> u64 {
        self.loan.current_version()
    }

    fn with_storage<'rt, R>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        f: impl FnOnce(&ListStorage<'cx, T>) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        ctx.heap_ref()
            .try_with(self.storage.handle(), f)
            .map_err(heap_access_error)?
    }

    fn make_unique<'rt>(&mut self, ctx: &mut Ctx<'cx, 'rt>) -> Result<(), RuntimeError>
    where
        T: Clone,
    {
        self.storage
            .make_unique(ctx.heap(), self.storage_ty)
            .map_err(heap_access_error)
    }

    fn with_storage_mut<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut ListStorage<'cx, T>) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        ctx.heap()
            .try_with_mut(self.storage.handle(), f)
            .map_err(heap_access_error)?
    }

    fn structurally_mutate_storage<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut ListStorage<'cx, T>) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError>
    where
        T: Clone,
    {
        let loan = self.loan.clone();
        loan.structural_mutation_result(|| {
            self.make_unique(ctx)?;
            let (result, len) = ctx
                .heap()
                .try_with_mut(self.storage.handle(), |storage| {
                    let result = f(storage)?;
                    Ok((result, storage.len()))
                })
                .map_err(heap_access_error)??;
            self.len = len;
            Ok(result)
        })
    }

    pub fn to_vec<'rt>(&self, ctx: &Ctx<'cx, 'rt>) -> Result<Vec<T>, RuntimeError>
    where
        T: Clone,
    {
        self.with_storage(ctx, |storage| Ok(storage.as_slice().to_vec()))
    }

    pub fn checked_index<'rt>(&self, ctx: &Ctx<'cx, 'rt>, index: i64) -> Result<T, RuntimeError>
    where
        T: Clone,
    {
        let index = crate::check::checked_index(index, self.len());
        self.with_storage(ctx, |storage| {
            storage.get(index).cloned().ok_or_else(|| {
                RuntimeError::new(format!(
                    "list index {index} out of bounds for len {}",
                    self.len()
                ))
            })
        })
    }

    pub fn with_elem_shared_short<'rt, R>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        index: usize,
        expected_version: u64,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.loan.check_stable(expected_version)?;
        self.with_storage(ctx, |storage| {
            let Some(elem) = storage.get(index) else {
                return Err(RuntimeError::new(format!(
                    "list index {index} out of bounds for len {}",
                    self.len()
                )));
            };
            f(elem)
        })
    }

    /// # Safety
    ///
    /// The callback must not access this list's storage while it receives the element reference.
    pub unsafe fn with_elem_mut_short<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        index: usize,
        expected_version: u64,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError>
    where
        T: Clone,
    {
        self.loan.check_stable(expected_version)?;
        self.check_elem_index(index)?;
        self.make_unique(ctx)?;
        self.with_storage_mut(ctx, |storage| {
            let elem = storage
                .get_mut(index)
                .expect("list storage length drifted from list length");
            f(elem)
        })
    }

    pub fn with_elem_owned_mut_ctx_short<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        index: usize,
        expected_version: u64,
        f: impl FnOnce(&mut Ctx<'cx, 'rt>, &mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError>
    where
        T: Clone,
    {
        self.loan.check_stable(expected_version)?;
        self.check_elem_index(index)?;
        self.make_unique(ctx)?;
        let mut elem = self.with_storage(ctx, |storage| {
            Ok(storage
                .get(index)
                .expect("list storage length drifted from list length")
                .clone())
        })?;
        let result = f(ctx, &mut elem)?;
        self.loan.check_stable(expected_version)?;
        self.check_elem_index(index)?;
        self.with_storage_mut(ctx, |storage| {
            *storage
                .get_mut(index)
                .expect("list storage length drifted from list length") = elem;
            Ok(())
        })?;
        Ok(result)
    }

    fn check_elem_index(&self, index: usize) -> Result<(), RuntimeError> {
        let len = self.len();
        if index < len {
            return Ok(());
        }
        Err(RuntimeError::new(format!(
            "list index {index} out of bounds for len {len}"
        )))
    }

    #[must_use]
    pub fn share(&self) -> Self {
        Self {
            storage: self.storage.share(),
            storage_ty: self.storage_ty,
            loan: CollectionLoanState::default(),
            len: self.len,
        }
    }

    pub fn elem_at_shared<'rt>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        index: usize,
        expected_version: u64,
    ) -> Result<T, RuntimeError>
    where
        T: Clone,
    {
        self.with_elem_shared_short(ctx, index, expected_version, |elem| Ok(elem.clone()))
    }

    pub fn push<'rt>(&mut self, ctx: &mut Ctx<'cx, 'rt>, elem: T) -> Result<(), RuntimeError>
    where
        T: Clone,
    {
        self.structurally_mutate_storage(ctx, |storage| {
            storage.push(elem);
            Ok(())
        })
    }

    pub fn replace_with(&mut self, other: Self) -> Result<(), RuntimeError> {
        let loan = self.loan.clone();
        loan.structural_mutation_result(|| {
            let Self {
                storage,
                storage_ty,
                loan: _,
                len,
            } = other;
            self.storage = storage;
            self.storage_ty = storage_ty;
            self.len = len;
            Ok(())
        })
    }
}

impl<'cx, T: 'cx> Clone for AnvList<'cx, T> {
    fn clone(&self) -> Self {
        self.share()
    }
}

impl<T: fmt::Debug> fmt::Debug for AnvList<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AnvList").field("len", &self.len).finish()
    }
}

// SAFETY: logical list values trace only the heap-visible storage owner; storage traces elements.
unsafe impl<'cx, T> Trace<'cx> for AnvList<'cx, T> {
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        self.storage.trace(visitor);
    }
}

pub struct AnvMap<'cx, K, V> {
    storage: CowStorageOwner<'cx, MapStorage<'cx, K, V>>,
    storage_ty: HeapType<'cx, MapStorage<'cx, K, V>>,
    loan: CollectionLoanState,
    len: usize,
}

impl<'cx, K: Eq + Hash + 'cx, V: 'cx> AnvMap<'cx, K, V> {
    pub fn from_entries<'rt>(
        ctx: &mut Ctx<'cx, 'rt>,
        storage_ty: HeapType<'cx, MapStorage<'cx, K, V>>,
        entries: impl IntoIterator<Item = (K, V)>,
    ) -> Self {
        let storage = MapStorage::from_entries(entries);
        let len = storage.len();
        Self {
            storage: CowStorageOwner::alloc(ctx.heap(), storage_ty, storage),
            storage_ty,
            loan: CollectionLoanState::default(),
            len,
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn begin_shape_loan(&self) -> Result<ShapeLoanGuard, RuntimeError> {
        self.loan.before_unloaned_storage_access()?;
        self.loan.begin_shape_loan()
    }

    pub fn begin_value_loan_by_key<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        key: &K,
    ) -> Result<ValueLoanGuard, RuntimeError>
    where
        K: Clone,
        V: Clone,
    {
        self.with_unloaned_storage(ctx, |storage| {
            storage.get_index_of(key).ok_or_else(Self::missing_key)?;
            Ok(())
        })?;
        self.make_unique(ctx)?;
        self.loan.begin_value_loan()
    }

    pub fn structural_version(&self) -> u64 {
        self.loan.current_version()
    }

    fn with_storage_unchecked<'rt, R>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        f: impl FnOnce(&MapStorage<'cx, K, V>) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        ctx.heap_ref()
            .try_with(self.storage.handle(), f)
            .map_err(heap_access_error)?
    }

    fn with_storage_mut_unchecked<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut MapStorage<'cx, K, V>) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        ctx.heap()
            .try_with_mut(self.storage.handle(), f)
            .map_err(heap_access_error)?
    }

    fn with_unloaned_storage<'rt, R>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        f: impl FnOnce(&MapStorage<'cx, K, V>) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.loan.before_unloaned_storage_access()?;
        self.with_storage_unchecked(ctx, f)
    }

    fn check_projected_value_storage(&self) -> Result<(), RuntimeError> {
        if self.storage.is_unique() {
            Ok(())
        } else {
            Err(RuntimeError::new("map entry value loan storage is shared"))
        }
    }

    fn missing_key() -> RuntimeError {
        RuntimeError::new("map entry key is missing")
    }

    pub fn get<'rt>(&self, ctx: &Ctx<'cx, 'rt>, key: &K) -> Result<Option<V>, RuntimeError>
    where
        V: Clone,
    {
        self.with_unloaned_storage(ctx, |storage| Ok(storage.get(key).cloned()))
    }

    pub fn contains_key<'rt>(&self, ctx: &Ctx<'cx, 'rt>, key: &K) -> Result<bool, RuntimeError> {
        self.with_unloaned_storage(ctx, |storage| Ok(storage.contains_key(key)))
    }

    fn make_unique<'rt>(&mut self, ctx: &mut Ctx<'cx, 'rt>) -> Result<(), RuntimeError>
    where
        K: Clone,
        V: Clone,
    {
        self.storage
            .make_unique(ctx.heap(), self.storage_ty)
            .map_err(heap_access_error)
    }

    fn structurally_mutate_storage<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        f: impl FnOnce(&mut MapStorage<'cx, K, V>) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError>
    where
        K: Clone,
        V: Clone,
    {
        let loan = self.loan.clone();
        loan.structural_mutation_result(|| {
            self.make_unique(ctx)?;
            let (result, len) = self.with_storage_mut_unchecked(ctx, |storage| {
                let result = f(storage)?;
                Ok((result, storage.len()))
            })?;
            self.len = len;
            Ok(result)
        })
    }

    fn entry_at_checked<'rt>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        index: usize,
        expected_version: u64,
    ) -> Result<(K, V), RuntimeError>
    where
        K: Clone,
        V: Clone,
    {
        self.loan.check_stable(expected_version)?;
        self.with_unloaned_storage(ctx, |storage| {
            storage
                .get_index(index)
                .map(|(key, value)| (key.clone(), value.clone()))
                .ok_or_else(|| {
                    RuntimeError::new(format!(
                        "map entry index {index} out of bounds for len {}",
                        self.len()
                    ))
                })
        })
    }

    /// # Safety
    ///
    /// The callback must not access this map's storage while it receives the value reference.
    pub unsafe fn with_value_mut_short<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        index: usize,
        expected_version: u64,
        f: impl FnOnce(&mut V) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError>
    where
        K: Clone,
        V: Clone,
    {
        self.loan.before_unloaned_storage_access()?;
        self.loan.check_stable(expected_version)?;
        let len = self.len();
        if index >= len {
            return Err(RuntimeError::new(format!(
                "map entry index {index} out of bounds for len {len}"
            )));
        }
        self.make_unique(ctx)?;
        self.with_storage_mut_unchecked(ctx, |storage| {
            let Some((_, value)) = storage.get_index_mut(index) else {
                return Err(RuntimeError::new(format!(
                    "map entry index {index} out of bounds for len {len}"
                )));
            };
            f(value)
        })
    }

    #[must_use]
    pub fn share(&self) -> Self {
        Self {
            storage: self.storage.share(),
            storage_ty: self.storage_ty,
            loan: CollectionLoanState::default(),
            len: self.len,
        }
    }

    pub fn key_at_shared<'rt>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        index: usize,
        expected_version: u64,
    ) -> Result<K, RuntimeError>
    where
        K: Clone,
        V: Clone,
    {
        self.entry_at_checked(ctx, index, expected_version)
            .map(|(key, _)| key)
    }

    pub fn value_at_shared<'rt>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        index: usize,
        expected_version: u64,
    ) -> Result<V, RuntimeError>
    where
        K: Clone,
        V: Clone,
    {
        self.entry_at_checked(ctx, index, expected_version)
            .map(|(_, value)| value)
    }

    pub fn with_value_shared_by_key<'rt, R>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        key: &K,
        expected_version: u64,
        value_loan: u64,
        f: impl FnOnce(&V) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.loan.check_value_loan(value_loan)?;
        self.loan.check_stable(expected_version)?;
        self.check_projected_value_storage()?;
        self.with_storage_unchecked(ctx, |storage| {
            let value = storage.get(key).ok_or_else(Self::missing_key)?;
            f(value)
        })
    }

    pub fn with_value_mut_by_key<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        key: &K,
        expected_version: u64,
        value_loan: u64,
        f: impl FnOnce(&mut V) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.loan.check_value_loan(value_loan)?;
        self.loan.check_stable(expected_version)?;
        self.check_projected_value_storage()?;
        self.with_storage_mut_unchecked(ctx, |storage| {
            let value = storage.get_mut(key).ok_or_else(Self::missing_key)?;
            f(value)
        })
    }

    pub fn insert<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        key: K,
        value: V,
    ) -> Result<Option<V>, RuntimeError>
    where
        K: Clone,
        V: Clone,
    {
        if self.contains_key(ctx, &key)? {
            self.make_unique(ctx)?;
            return self.with_storage_mut_unchecked(ctx, |storage| Ok(storage.insert(key, value)));
        }
        let old =
            self.structurally_mutate_storage(ctx, |storage| Ok(storage.insert(key, value)))?;
        debug_assert!(old.is_none());
        Ok(None)
    }

    pub fn remove<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        key: &K,
    ) -> Result<Option<V>, RuntimeError>
    where
        K: Clone,
        V: Clone,
    {
        if !self.contains_key(ctx, key)? {
            return Ok(None);
        }
        self.structurally_mutate_storage(ctx, |storage| Ok(storage.shift_remove(key)))
    }

    pub fn replace_with(&mut self, other: Self) -> Result<(), RuntimeError> {
        let loan = self.loan.clone();
        loan.structural_mutation_result(|| {
            let Self {
                storage,
                storage_ty,
                loan: _,
                len,
            } = other;
            self.storage = storage;
            self.storage_ty = storage_ty;
            self.len = len;
            Ok(())
        })
    }
}

impl<'cx, K: Eq + Hash + 'cx, V: 'cx> Clone for AnvMap<'cx, K, V> {
    fn clone(&self) -> Self {
        self.share()
    }
}

impl<K: fmt::Debug, V: fmt::Debug> fmt::Debug for AnvMap<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AnvMap").field("len", &self.len).finish()
    }
}

// SAFETY: logical map values trace only the heap-visible storage owner; storage traces entries.
unsafe impl<'cx, K, V> Trace<'cx> for AnvMap<'cx, K, V> {
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        self.storage.trace(visitor);
    }
}

enum SliceRoot<'cx, T> {
    Raw {
        ptr: NonNull<T>,
        len: usize,
    },
    List {
        storage: CowStorageView<'cx, ListStorage<'cx, T>>,
        storage_len: usize,
    },
}

impl<T> Clone for SliceRoot<'_, T> {
    fn clone(&self) -> Self {
        match self {
            Self::Raw { ptr, len } => Self::Raw {
                ptr: *ptr,
                len: *len,
            },
            Self::List {
                storage,
                storage_len,
            } => Self::List {
                storage: storage.clone(),
                storage_len: *storage_len,
            },
        }
    }
}

pub struct AnvSlice<'cx, T> {
    root: SliceRoot<'cx, T>,
    start: usize,
    len: usize,
    guard: Option<ShapeLoanGuard>,
    mutable: bool,
    _marker: PhantomData<T>,
}

impl<'cx, T: 'cx> AnvSlice<'cx, T> {
    /// # Safety
    ///
    /// `ptr` must point to initialized contiguous elements that outlive the returned descriptor.
    pub unsafe fn from_raw_parts(ptr: *const T, len: usize, start: usize, len_view: usize) -> Self {
        let ptr = NonNull::new(ptr.cast_mut()).expect("slice root pointer cannot be null");
        Self::new(SliceRoot::Raw { ptr, len }, start, len_view, None, false)
    }

    /// # Safety
    ///
    /// `ptr` must point to initialized contiguous elements, be uniquely mutable during short mutable
    /// accesses, and outlive the returned descriptor.
    pub unsafe fn from_raw_parts_mut(
        ptr: *mut T,
        len: usize,
        start: usize,
        len_view: usize,
    ) -> Self {
        let ptr = NonNull::new(ptr).expect("slice root pointer cannot be null");
        Self::new(SliceRoot::Raw { ptr, len }, start, len_view, None, true)
    }

    pub fn from_list(
        list: &AnvList<'cx, T>,
        start: usize,
        len: usize,
    ) -> Result<Self, RuntimeError> {
        let guard = list.begin_shape_loan()?;
        Ok(Self::new(
            SliceRoot::List {
                storage: list.storage_view(),
                storage_len: list.len(),
            },
            start,
            len,
            Some(guard),
            false,
        ))
    }

    pub fn from_list_mut<'rt>(
        ctx: &mut Ctx<'cx, 'rt>,
        list: &mut AnvList<'cx, T>,
        start: usize,
        len: usize,
    ) -> Result<Self, RuntimeError>
    where
        T: Clone,
    {
        list.make_unique(ctx)?;
        let guard = list.begin_shape_loan()?;
        Ok(Self::new(
            SliceRoot::List {
                storage: list.storage_view(),
                storage_len: list.len(),
            },
            start,
            len,
            Some(guard),
            true,
        ))
    }

    fn new(
        root: SliceRoot<'cx, T>,
        start: usize,
        len: usize,
        guard: Option<ShapeLoanGuard>,
        mutable: bool,
    ) -> Self {
        Self {
            root,
            start,
            len,
            guard,
            mutable,
            _marker: PhantomData,
        }
    }

    #[must_use]
    pub fn share(&self) -> Self {
        self.readonly()
    }

    #[must_use]
    pub fn readonly(&self) -> Self {
        Self::new(
            self.root.clone(),
            self.start,
            self.len,
            self.guard.clone(),
            false,
        )
    }

    #[must_use]
    pub fn slice(&self, start: usize, len: usize) -> Self {
        Self::new(
            self.root.clone(),
            self.start + start,
            len,
            self.guard.clone(),
            self.mutable,
        )
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn elem_at_shared<'rt>(&self, ctx: &Ctx<'cx, 'rt>, index: i64) -> Result<T, RuntimeError>
    where
        T: Clone,
    {
        let index = self.check_view_index(index)?;
        let absolute = self.start + index;
        match self.root {
            SliceRoot::Raw { ptr, .. } => unsafe {
                Ok(ptr.as_ptr().add(absolute).as_ref().unwrap().clone())
            },
            SliceRoot::List { ref storage, .. } => {
                self.check_stable()?;
                ctx.heap_ref()
                    .try_with(storage.handle(), |storage| {
                        storage.get(absolute).cloned().ok_or_else(|| {
                            RuntimeError::new(format!(
                                "slice index {index} out of bounds for len {}",
                                self.len
                            ))
                        })
                    })
                    .map_err(heap_access_error)?
            }
        }
    }

    /// # Safety
    ///
    ///  must not access, mutate, move, or invalidate the slice root while it receives element references.
    pub unsafe fn copy_range_with<'rt, U: 'cx + Clone>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        storage_ty: HeapType<'cx, ListStorage<'cx, U>>,
        range: Range<usize>,
        mut convert: impl FnMut(&T) -> U,
    ) -> Result<AnvList<'cx, U>, RuntimeError> {
        let absolute = self.check_view_range(range)?;
        let mut elems = vec![];
        match self.root {
            SliceRoot::Raw { ptr, .. } => unsafe {
                for index in absolute {
                    elems.push(convert(ptr.as_ptr().add(index).as_ref().unwrap()));
                }
            },
            SliceRoot::List { ref storage, .. } => {
                self.check_stable()?;
                ctx.heap_ref()
                    .try_with(storage.handle(), |storage| {
                        for index in absolute {
                            let Some(elem) = storage.get(index) else {
                                return Err(RuntimeError::new("slice range out of bounds"));
                            };
                            elems.push(convert(elem));
                        }
                        Ok(())
                    })
                    .map_err(heap_access_error)??;
            }
        }
        Ok(AnvList::from_elems(ctx, storage_ty, elems))
    }

    /// # Safety
    ///
    /// The callback must not access, mutate, move, or invalidate the slice root while it receives
    /// the element reference.
    pub unsafe fn with_elem_mut_short<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        index: i64,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError>
    where
        T: Clone,
    {
        if !self.mutable {
            return Err(RuntimeError::new("cannot mutate readonly slice"));
        }
        let index = self.check_view_index(index)?;
        let absolute = self.start + index;
        match self.root {
            SliceRoot::Raw { ptr, .. } => unsafe {
                f(ptr.as_ptr().add(absolute).as_mut().unwrap())
            },
            SliceRoot::List { ref storage, .. } => {
                self.check_stable()?;
                ctx.heap()
                    .try_with_mut(storage.handle(), |storage| {
                        let Some(elem) = storage.get_mut(absolute) else {
                            return Err(RuntimeError::new("slice index out of bounds"));
                        };
                        f(elem)
                    })
                    .map_err(heap_access_error)?
            }
        }
    }

    pub fn with_elem_owned_mut_ctx_short<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        index: i64,
        f: impl FnOnce(&mut Ctx<'cx, 'rt>, &mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError>
    where
        T: Clone,
    {
        if !self.mutable {
            return Err(RuntimeError::new("cannot mutate readonly slice"));
        }
        let index = self.check_view_index(index)?;
        let absolute = self.start + index;
        let mut elem = match self.root {
            SliceRoot::Raw { ptr, .. } => unsafe {
                ptr.as_ptr().add(absolute).as_ref().unwrap().clone()
            },
            SliceRoot::List { ref storage, .. } => {
                self.check_stable()?;
                ctx.heap_ref()
                    .try_with(storage.handle(), |storage| {
                        storage
                            .get(absolute)
                            .cloned()
                            .ok_or_else(|| RuntimeError::new("slice index out of bounds"))
                    })
                    .map_err(heap_access_error)??
            }
        };
        let result = f(ctx, &mut elem)?;
        match self.root {
            SliceRoot::Raw { ptr, .. } => unsafe {
                *ptr.as_ptr().add(absolute) = elem;
            },
            SliceRoot::List { ref storage, .. } => {
                self.check_stable()?;
                ctx.heap()
                    .try_with_mut(storage.handle(), |storage| {
                        let Some(slot) = storage.get_mut(absolute) else {
                            return Err(RuntimeError::new("slice index out of bounds"));
                        };
                        *slot = elem;
                        Ok(())
                    })
                    .map_err(heap_access_error)??;
            }
        }
        Ok(result)
    }

    fn check_stable(&self) -> Result<(), RuntimeError> {
        match &self.guard {
            Some(guard) => guard.check_stable(),
            None => Ok(()),
        }
    }

    fn check_view_range(&self, range: Range<usize>) -> Result<Range<usize>, RuntimeError> {
        let Some(view_end) = self.start.checked_add(self.len) else {
            return Err(RuntimeError::new("slice range out of bounds"));
        };
        let root_len = match self.root {
            SliceRoot::Raw { len, .. } => len,
            SliceRoot::List { storage_len, .. } => storage_len,
        };
        if view_end > root_len || range.start > range.end || range.end > self.len {
            return Err(RuntimeError::new("slice range out of bounds"));
        }
        let start = self.start + range.start;
        let end = self.start + range.end;
        Ok(start..end)
    }

    fn check_view_index(&self, index: i64) -> Result<usize, RuntimeError> {
        self.check_view_range(0..self.len)?;
        let index =
            usize::try_from(index).map_err(|_| RuntimeError::new("negative slice index"))?;
        if index < self.len {
            Ok(index)
        } else {
            Err(RuntimeError::new(format!(
                "slice index {index} out of bounds for len {}",
                self.len
            )))
        }
    }
}

// SAFETY: list-backed slices trace their storage view; raw slices only borrow non-heap array memory.
unsafe impl<'cx, T> Trace<'cx> for AnvSlice<'cx, T> {
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        if let SliceRoot::List { storage, .. } = &self.root {
            storage.trace(visitor);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::Cell, rc::Rc};

    use super::{AnvList, AnvMap, AnvSlice, AnvString, display_float};
    use crate::{
        Ctx, Handle, Heap, HeapType, ListStorage, MapStorage, Trace, TraceDriver, Visitor,
    };

    fn list_ty<'cx, T: 'cx>(heap: &mut Heap<'cx>) -> HeapType<'cx, ListStorage<'cx, T>> {
        heap.register_untracked::<ListStorage<'_, T>>()
    }

    fn map_ty<'cx, K: 'cx, V: 'cx>(heap: &mut Heap<'cx>) -> HeapType<'cx, MapStorage<'cx, K, V>> {
        heap.register_untracked::<MapStorage<'_, K, V>>()
    }

    #[test]
    fn display_float_marks_finite_whole_values() {
        assert_eq!(display_float(1.0), "1.0");
        assert_eq!(display_float(-2.0), "-2.0");
        assert_eq!(display_float(0.0), "0.0");
        assert_eq!(display_float(-0.0), "-0.0");
    }

    #[test]
    fn display_float_preserves_non_whole_and_non_finite_values() {
        assert_eq!(display_float(1.25), "1.25");
        assert_eq!(display_float(f64::NAN), "NaN");
        assert_eq!(display_float(f64::INFINITY), "inf");
        assert_eq!(display_float(f64::NEG_INFINITY), "-inf");
    }

    #[test]
    fn anv_string_static_and_owned_construction() {
        assert_eq!(AnvString::default().as_str(), "");
        let static_text = AnvString::from("static");
        assert_eq!(static_text.as_str(), "static");
        assert_eq!(static_text.len(), 6);
        assert!(!static_text.is_empty());
        assert_eq!(AnvString::from(String::from("owned")).as_str(), "owned");
        assert_eq!(AnvString::from("borrowed").as_str(), "borrowed");
    }

    #[test]
    fn tracked_heap_payload_can_contain_anv_string() {
        struct Payload {
            text: AnvString,
        }

        unsafe impl<'cx> Trace<'cx> for Payload {
            fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
                self.text.trace(visitor);
            }
        }

        Heap::scope(|heap| {
            let payload = heap.register_tracked::<Payload>();
            let handle = heap.alloc(
                payload,
                Payload {
                    text: AnvString::from("tracked"),
                },
            );

            assert_eq!(heap.with(&handle, |payload| payload.text.as_str().len()), 7);
            assert_eq!(heap.collect_all().collected, 0);
            drop(handle);
            assert_eq!(heap.collect_all().collected, 1);
            assert_eq!(heap.stats().live, 0);
        });
    }

    #[test]
    fn list_constructs_in_heap_storage() {
        Heap::scope(|heap| {
            let ty = list_ty::<i64>(heap);
            let mut ctx = Ctx::new(heap);
            let list = AnvList::from_elems(&mut ctx, ty, [1_i64, 2, 3]);

            assert_eq!(list.len(), 3);
            assert_eq!(list.len_i64(), 3);
            assert_eq!(list.checked_index(&ctx, 2).unwrap(), 3);
            assert_eq!(list.to_vec(&ctx).unwrap(), vec![1, 2, 3]);
        });
    }

    #[test]
    fn list_push_updates_storage_and_version() {
        Heap::scope(|heap| {
            let ty = list_ty::<i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut list = AnvList::from_elems(&mut ctx, ty, [1_i64, 2]);

            list.push(&mut ctx, 3).unwrap();

            assert_eq!(list.structural_version(), 1);
            assert_eq!(list.to_vec(&ctx).unwrap(), vec![1, 2, 3]);
        });
    }

    #[test]
    fn list_share_detaches_on_structural_mutation() {
        Heap::scope(|heap| {
            let ty = list_ty::<i64>(heap);
            let mut ctx = Ctx::new(heap);
            let list = AnvList::from_elems(&mut ctx, ty, [1_i64]);
            let mut shared = list.share();

            shared.push(&mut ctx, 2).unwrap();

            assert_eq!(list.to_vec(&ctx).unwrap(), vec![1]);
            assert_eq!(shared.to_vec(&ctx).unwrap(), vec![1, 2]);
            assert!(list.storage.is_unique());
            assert!(shared.storage.is_unique());
        });
    }

    #[test]
    fn list_storage_view_does_not_force_detach() {
        Heap::scope(|heap| {
            let ty = list_ty::<i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut list = AnvList::from_elems(&mut ctx, ty, [1_i64]);
            let view = list.storage.view();

            list.push(&mut ctx, 2).unwrap();

            assert_eq!(list.to_vec(&ctx).unwrap(), vec![1, 2]);
            assert_eq!(view.handle().strong_count(), 2);
            assert!(list.storage.is_unique());
        });
    }

    #[test]
    fn list_active_loan_blocks_before_detach() {
        Heap::scope(|heap| {
            let ty = list_ty::<i64>(heap);
            let mut ctx = Ctx::new(heap);
            let list = AnvList::from_elems(&mut ctx, ty, [1_i64]);
            let mut shared = list.share();
            let _guard = shared.begin_shape_loan().unwrap();

            assert!(shared.push(&mut ctx, 2).is_err());

            assert_eq!(list.storage.logical_owners(), 2);
            assert_eq!(shared.storage.logical_owners(), 2);
            assert_eq!(list.to_vec(&ctx).unwrap(), vec![1]);
            assert_eq!(shared.to_vec(&ctx).unwrap(), vec![1]);
        });
    }

    #[test]
    fn list_element_mutation_uses_heap_storage() {
        Heap::scope(|heap| {
            let ty = list_ty::<i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut list = AnvList::from_elems(&mut ctx, ty, [1_i64, 2]);
            let guard = list.begin_shape_loan().unwrap();
            let version = guard.version();

            unsafe {
                list.with_elem_mut_short(&mut ctx, 1, version, |elem| {
                    *elem = 5;
                    Ok(())
                })
            }
            .unwrap();

            assert_eq!(list.structural_version(), 0);
            assert_eq!(list.elem_at_shared(&ctx, 1, version).unwrap(), 5);
            assert_eq!(list.to_vec(&ctx).unwrap(), vec![1, 5]);
        });
    }

    struct TraceChild {
        traces: Rc<Cell<u32>>,
    }

    struct TraceRoot<'cx, T> {
        payload: T,
        self_ref: Option<Handle<'cx, TraceRoot<'cx, T>>>,
        root_traces: Rc<Cell<u32>>,
    }

    unsafe impl<'cx> Trace<'cx> for TraceChild {
        fn trace<D: TraceDriver<'cx>>(&self, _visitor: &mut Visitor<'cx, '_, D>) {
            self.traces.set(self.traces.get() + 1);
        }
    }

    unsafe impl<'cx, T: Trace<'cx>> Trace<'cx> for TraceRoot<'cx, T> {
        fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
            self.root_traces.set(self.root_traces.get() + 1);
            self.payload.trace(visitor);
            self.self_ref.trace(visitor);
        }
    }

    fn cyclic_trace_root<'cx, 'rt, T: 'cx>(
        ctx: &mut Ctx<'cx, 'rt>,
        ty: HeapType<'cx, TraceRoot<'cx, T>>,
        payload: T,
        root_traces: Rc<Cell<u32>>,
    ) -> Handle<'cx, TraceRoot<'cx, T>> {
        let root = ctx.heap().alloc(
            ty,
            TraceRoot {
                payload,
                self_ref: None,
                root_traces,
            },
        );
        ctx.heap()
            .with_mut(&root, |root_data| root_data.self_ref = Some(root.clone()));
        root
    }

    #[test]
    fn list_traces_storage_handle_and_storage_traces_elements_once() {
        Heap::scope(|heap| {
            let child_traces = Rc::new(Cell::new(0));
            let root_traces = Rc::new(Cell::new(0));
            let child_ty = heap.register_tracked::<TraceChild>();
            let list_ty = heap.register_tracked::<ListStorage<'_, Handle<'_, TraceChild>>>();
            let root_ty =
                heap.register_tracked::<TraceRoot<'_, AnvList<'_, Handle<'_, TraceChild>>>>();
            let child = heap.alloc(
                child_ty,
                TraceChild {
                    traces: Rc::clone(&child_traces),
                },
            );
            let mut ctx = Ctx::new(heap);
            let list = AnvList::from_elems(&mut ctx, list_ty, [child]);
            let root = cyclic_trace_root(&mut ctx, root_ty, list, Rc::clone(&root_traces));

            drop(root);
            ctx.heap().reset_stats();
            let outcome = ctx.heap().collect_all();

            assert_eq!(outcome.collected, 3);
            assert_eq!(root_traces.get(), 1);
            assert_eq!(child_traces.get(), 1);
            assert_eq!(ctx.heap().stats().edge_visits, 3);
            assert_eq!(ctx.heap().stats().internal_edges, 3);
        });
    }

    #[test]
    fn map_traces_storage_handle_and_storage_traces_entries_once() {
        Heap::scope(|heap| {
            let child_traces = Rc::new(Cell::new(0));
            let root_traces = Rc::new(Cell::new(0));
            let child_ty = heap.register_tracked::<TraceChild>();
            let map_ty = heap.register_tracked::<MapStorage<'_, i64, Handle<'_, TraceChild>>>();
            let root_ty =
                heap.register_tracked::<TraceRoot<'_, AnvMap<'_, i64, Handle<'_, TraceChild>>>>();
            let child = heap.alloc(
                child_ty,
                TraceChild {
                    traces: Rc::clone(&child_traces),
                },
            );
            let mut ctx = Ctx::new(heap);
            let map = AnvMap::from_entries(&mut ctx, map_ty, [(1_i64, child)]);
            let root = cyclic_trace_root(&mut ctx, root_ty, map, Rc::clone(&root_traces));

            drop(root);
            ctx.heap().reset_stats();
            let outcome = ctx.heap().collect_all();

            assert_eq!(outcome.collected, 3);
            assert_eq!(root_traces.get(), 1);
            assert_eq!(child_traces.get(), 1);
            assert_eq!(ctx.heap().stats().edge_visits, 3);
            assert_eq!(ctx.heap().stats().internal_edges, 3);
        });
    }

    #[test]
    fn nested_list_traces_outer_and_inner_storage_once() {
        Heap::scope(|heap| {
            let child_traces = Rc::new(Cell::new(0));
            let root_traces = Rc::new(Cell::new(0));
            let child_ty = heap.register_tracked::<TraceChild>();
            let inner_ty = heap.register_tracked::<ListStorage<'_, Handle<'_, TraceChild>>>();
            let outer_ty =
                heap.register_tracked::<ListStorage<'_, AnvList<'_, Handle<'_, TraceChild>>>>();
            let root_ty = heap
                .register_tracked::<TraceRoot<'_, AnvList<'_, AnvList<'_, Handle<'_, TraceChild>>>>>();
            let child = heap.alloc(
                child_ty,
                TraceChild {
                    traces: Rc::clone(&child_traces),
                },
            );
            let mut ctx = Ctx::new(heap);
            let inner = AnvList::from_elems(&mut ctx, inner_ty, [child]);
            let list = AnvList::from_elems(&mut ctx, outer_ty, [inner]);
            let root = cyclic_trace_root(&mut ctx, root_ty, list, Rc::clone(&root_traces));

            drop(root);
            ctx.heap().reset_stats();
            let outcome = ctx.heap().collect_all();

            assert_eq!(outcome.collected, 4);
            assert_eq!(root_traces.get(), 1);
            assert_eq!(child_traces.get(), 1);
            assert_eq!(ctx.heap().stats().edge_visits, 4);
            assert_eq!(ctx.heap().stats().internal_edges, 4);
        });
    }

    #[test]
    fn map_list_values_trace_outer_and_inner_storage_once() {
        Heap::scope(|heap| {
            let child_traces = Rc::new(Cell::new(0));
            let root_traces = Rc::new(Cell::new(0));
            let child_ty = heap.register_tracked::<TraceChild>();
            let list_ty = heap.register_tracked::<ListStorage<'_, Handle<'_, TraceChild>>>();
            let map_ty =
                heap.register_tracked::<MapStorage<'_, i64, AnvList<'_, Handle<'_, TraceChild>>>>();
            let root_ty = heap.register_tracked::<TraceRoot<
                '_,
                AnvMap<'_, i64, AnvList<'_, Handle<'_, TraceChild>>>,
            >>();
            let child = heap.alloc(
                child_ty,
                TraceChild {
                    traces: Rc::clone(&child_traces),
                },
            );
            let mut ctx = Ctx::new(heap);
            let inner = AnvList::from_elems(&mut ctx, list_ty, [child]);
            let map = AnvMap::from_entries(&mut ctx, map_ty, [(1_i64, inner)]);
            let root = cyclic_trace_root(&mut ctx, root_ty, map, Rc::clone(&root_traces));

            drop(root);
            ctx.heap().reset_stats();
            let outcome = ctx.heap().collect_all();

            assert_eq!(outcome.collected, 4);
            assert_eq!(root_traces.get(), 1);
            assert_eq!(child_traces.get(), 1);
            assert_eq!(ctx.heap().stats().edge_visits, 4);
            assert_eq!(ctx.heap().stats().internal_edges, 4);
        });
    }

    #[test]
    fn detaching_outer_list_shares_inner_storage() {
        Heap::scope(|heap| {
            let int_list_ty = list_ty::<i64>(heap);
            let nested_ty = heap.register_tracked::<ListStorage<'_, AnvList<'_, i64>>>();
            let mut ctx = Ctx::new(heap);
            let inner = AnvList::from_elems(&mut ctx, int_list_ty, [1_i64]);
            let list = AnvList::from_elems(&mut ctx, nested_ty, [inner]);
            let mut shared = list.share();
            let replacement = AnvList::from_elems(&mut ctx, int_list_ty, [2_i64]);

            shared.push(&mut ctx, replacement).unwrap();

            let original_inner_owners = list
                .with_storage(&ctx, |storage| {
                    Ok(storage.get(0).unwrap().storage.logical_owners())
                })
                .unwrap();
            let shared_inner_owners = shared
                .with_storage(&ctx, |storage| {
                    Ok(storage.get(0).unwrap().storage.logical_owners())
                })
                .unwrap();
            let replacement_owners = shared
                .with_storage(&ctx, |storage| {
                    Ok(storage.get(1).unwrap().storage.logical_owners())
                })
                .unwrap();

            assert_eq!(original_inner_owners, 2);
            assert_eq!(shared_inner_owners, 2);
            assert_eq!(replacement_owners, 1);
        });
    }

    #[test]
    fn detaching_map_with_list_values_shares_inner_storage() {
        Heap::scope(|heap| {
            let list_ty = list_ty::<i64>(heap);
            let map_ty = heap.register_tracked::<MapStorage<'_, i64, AnvList<'_, i64>>>();
            let mut ctx = Ctx::new(heap);
            let inner = AnvList::from_elems(&mut ctx, list_ty, [1_i64]);
            let map = AnvMap::from_entries(&mut ctx, map_ty, [(1_i64, inner)]);
            let mut shared = map.share();
            let replacement = AnvList::from_elems(&mut ctx, list_ty, [2_i64]);

            shared.insert(&mut ctx, 2, replacement).unwrap();

            let original_inner_owners = map
                .with_storage_unchecked(&ctx, |storage| {
                    Ok(storage.get(&1).unwrap().storage.logical_owners())
                })
                .unwrap();
            let shared_inner_owners = shared
                .with_storage_unchecked(&ctx, |storage| {
                    Ok(storage.get(&1).unwrap().storage.logical_owners())
                })
                .unwrap();
            let replacement_owners = shared
                .with_storage_unchecked(&ctx, |storage| {
                    Ok(storage.get(&2).unwrap().storage.logical_owners())
                })
                .unwrap();

            assert_eq!(original_inner_owners, 2);
            assert_eq!(shared_inner_owners, 2);
            assert_eq!(replacement_owners, 1);
        });
    }

    #[test]
    fn map_shape_loan_allows_value_update_but_not_key_set_mutation() {
        Heap::scope(|heap| {
            let ty = map_ty::<&str, i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut map = AnvMap::from_entries(&mut ctx, ty, [("a", 1_i64)]);
            let guard = map.begin_shape_loan().unwrap();
            let version = guard.version();

            assert_eq!(map.insert(&mut ctx, "a", 2).unwrap(), Some(1));
            unsafe {
                map.with_value_mut_short(&mut ctx, 0, version, |value| {
                    *value = 3;
                    Ok(())
                })
            }
            .unwrap();
            assert_eq!(map.get(&ctx, &"a").unwrap(), Some(3));
            assert_eq!(map.structural_version(), 0);
            assert!(map.insert(&mut ctx, "b", 4).is_err());
            assert!(map.remove(&mut ctx, &"a").is_err());
            assert_eq!(map.remove(&mut ctx, &"missing").unwrap(), None);
            drop(guard);

            assert_eq!(map.insert(&mut ctx, "b", 4).unwrap(), None);
            assert_eq!(map.structural_version(), 1);
        });
    }

    #[test]
    fn map_value_loan_blocks_external_value_update() {
        Heap::scope(|heap| {
            let ty = map_ty::<&str, i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut map = AnvMap::from_entries(&mut ctx, ty, [("a", 1_i64)]);
            let shared = map.share();
            let guard = map.begin_value_loan_by_key(&mut ctx, &"a").unwrap();
            let version = guard.version();

            assert!(map.insert(&mut ctx, "a", 2).is_err());
            unsafe {
                assert!(
                    map.with_value_mut_short(&mut ctx, 0, version, |value| {
                        *value = 3;
                        Ok(())
                    })
                    .is_err()
                );
            }
            drop(guard);

            assert_eq!(map.get(&ctx, &"a").unwrap(), Some(1));
            assert_eq!(shared.get(&ctx, &"a").unwrap(), Some(1));
            assert_eq!(map.insert(&mut ctx, "a", 2).unwrap(), Some(1));
        });
    }

    #[test]
    fn map_value_loan_blocks_ordinary_storage_access() {
        Heap::scope(|heap| {
            let ty = map_ty::<&str, i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut map = AnvMap::from_entries(&mut ctx, ty, [("a", 1_i64)]);
            let guard = map.begin_value_loan_by_key(&mut ctx, &"a").unwrap();
            let version = guard.version();

            assert!(map.get(&ctx, &"a").is_err());
            assert!(map.contains_key(&ctx, &"a").is_err());
            assert!(map.begin_shape_loan().is_err());
            assert!(map.key_at_shared(&ctx, 0, version).is_err());
            assert!(map.value_at_shared(&ctx, 0, version).is_err());
            assert!(map.insert(&mut ctx, "b", 2).is_err());
            assert!(map.remove(&mut ctx, &"missing").is_err());

            map.with_value_mut_by_key(&mut ctx, &"a", version, guard.id(), |value| {
                *value = 3;
                Ok(())
            })
            .unwrap();
            drop(guard);

            assert_eq!(map.get(&ctx, &"a").unwrap(), Some(3));
        });
    }

    #[test]
    fn map_value_loan_rejects_projection_after_share() {
        Heap::scope(|heap| {
            let ty = map_ty::<&str, i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut map = AnvMap::from_entries(&mut ctx, ty, [("a", 1_i64)]);
            let guard = map.begin_value_loan_by_key(&mut ctx, &"a").unwrap();
            let shared = map.share();

            assert!(
                map.with_value_mut_by_key(&mut ctx, &"a", guard.version(), guard.id(), |value| {
                    *value = 3;
                    Ok(())
                })
                .is_err()
            );
            drop(guard);

            assert_eq!(map.get(&ctx, &"a").unwrap(), Some(1));
            assert_eq!(shared.get(&ctx, &"a").unwrap(), Some(1));
        });
    }

    #[test]
    fn map_share_detaches_on_value_update() {
        Heap::scope(|heap| {
            let ty = map_ty::<&str, i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut map = AnvMap::from_entries(&mut ctx, ty, [("a", 1_i64)]);
            let shared = map.share();

            assert_eq!(map.insert(&mut ctx, "a", 2).unwrap(), Some(1));
            assert_eq!(map.get(&ctx, &"a").unwrap(), Some(2));
            assert_eq!(shared.get(&ctx, &"a").unwrap(), Some(1));
            assert_eq!(map.structural_version(), 0);
        });
    }

    #[test]
    fn map_loan_blocks_structural_insert_before_detach() {
        Heap::scope(|heap| {
            let ty = map_ty::<&str, i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut map = AnvMap::from_entries(&mut ctx, ty, [("a", 1_i64)]);
            let shared = map.share();
            let guard = map.begin_shape_loan().unwrap();

            assert!(map.insert(&mut ctx, "b", 2).is_err());
            drop(guard);

            assert_eq!(map.get(&ctx, &"a").unwrap(), Some(1));
            assert_eq!(shared.get(&ctx, &"a").unwrap(), Some(1));
            assert_eq!(map.insert(&mut ctx, "b", 2).unwrap(), None);
            assert_eq!(map.structural_version(), 1);
        });
    }

    #[test]
    fn map_value_mutation_detaches_without_structural_version_change() {
        Heap::scope(|heap| {
            let ty = map_ty::<&str, i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut map = AnvMap::from_entries(&mut ctx, ty, [("a", 1_i64)]);
            let shared = map.share();
            let version = map.structural_version();

            unsafe {
                map.with_value_mut_short(&mut ctx, 0, version, |value| {
                    *value = 3;
                    Ok(())
                })
            }
            .unwrap();

            assert_eq!(map.get(&ctx, &"a").unwrap(), Some(3));
            assert_eq!(shared.get(&ctx, &"a").unwrap(), Some(1));
            assert_eq!(map.structural_version(), 0);
        });
    }

    #[test]
    fn raw_slice_descriptor_uses_short_access() {
        Heap::scope(|heap| {
            let mut ctx = Ctx::new(heap);
            let mut values = [1_i64, 2, 3];
            let slice = unsafe { AnvSlice::from_raw_parts(values.as_ptr(), values.len(), 1, 2) };
            assert_eq!(slice.elem_at_shared(&ctx, 0).unwrap(), 2);

            let mut slice =
                unsafe { AnvSlice::from_raw_parts_mut(values.as_mut_ptr(), values.len(), 1, 2) };
            unsafe {
                slice.with_elem_mut_short(&mut ctx, 0, |value| {
                    *value = 5;
                    Ok(())
                })
            }
            .unwrap();

            assert_eq!(values, [1, 5, 3]);
        });
    }

    #[test]
    fn list_slice_keeps_storage_alive_after_owner_drops() {
        Heap::scope(|heap| {
            let ty = list_ty::<i64>(heap);
            let mut ctx = Ctx::new(heap);
            let list = AnvList::from_elems(&mut ctx, ty, [1_i64, 2, 3]);
            let slice = AnvSlice::from_list(&list, 1, 2).unwrap();
            drop(list);

            assert_eq!(slice.elem_at_shared(&ctx, 0).unwrap(), 2);
            assert_eq!(slice.elem_at_shared(&ctx, 1).unwrap(), 3);
        });
    }

    #[test]
    fn list_slice_keeps_old_storage_after_owner_detaches() {
        Heap::scope(|heap| {
            let ty = list_ty::<i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut list = AnvList::from_elems(&mut ctx, ty, [1_i64, 2]);
            let shared = list.share();
            let version = list.structural_version();
            let slice = AnvSlice::from_list(&list, 0, 2).unwrap();

            unsafe {
                list.with_elem_mut_short(&mut ctx, 0, version, |value| {
                    *value = 10;
                    Ok(())
                })
            }
            .unwrap();

            assert_eq!(list.to_vec(&ctx).unwrap(), vec![10, 2]);
            assert_eq!(shared.to_vec(&ctx).unwrap(), vec![1, 2]);
            assert_eq!(slice.elem_at_shared(&ctx, 0).unwrap(), 1);
        });
    }

    #[test]
    fn mutable_list_slice_writes_through_unique_storage() {
        Heap::scope(|heap| {
            let ty = list_ty::<i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut list = AnvList::from_elems(&mut ctx, ty, [1_i64, 2, 3]);
            let mut slice = AnvSlice::from_list_mut(&mut ctx, &mut list, 1, 2).unwrap();

            slice
                .with_elem_owned_mut_ctx_short(&mut ctx, 0, |_, value| {
                    *value = 20;
                    Ok(())
                })
                .unwrap();

            assert_eq!(list.to_vec(&ctx).unwrap(), vec![1, 20, 3]);
        });
    }

    #[test]
    fn mutable_list_slice_detaches_shared_storage() {
        Heap::scope(|heap| {
            let ty = list_ty::<i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut list = AnvList::from_elems(&mut ctx, ty, [1_i64, 2, 3]);
            let shared = list.share();
            let mut slice = AnvSlice::from_list_mut(&mut ctx, &mut list, 1, 2).unwrap();

            slice
                .with_elem_owned_mut_ctx_short(&mut ctx, 0, |_, value| {
                    *value = 20;
                    Ok(())
                })
                .unwrap();

            assert_eq!(list.to_vec(&ctx).unwrap(), vec![1, 20, 3]);
            assert_eq!(shared.to_vec(&ctx).unwrap(), vec![1, 2, 3]);
        });
    }

    #[test]
    fn raw_slice_copy_range_creates_heap_visible_list() {
        Heap::scope(|heap| {
            let ty = list_ty::<i64>(heap);
            let mut ctx = Ctx::new(heap);
            let values = [1_i64, 2, 3, 4];
            let slice = unsafe { AnvSlice::from_raw_parts(values.as_ptr(), values.len(), 1, 3) };
            let copy = unsafe {
                slice
                    .copy_range_with(&mut ctx, ty, 1..3, |value| *value * 10)
                    .unwrap()
            };

            assert_eq!(copy.to_vec(&ctx).unwrap(), vec![30, 40]);
        });
    }
}
