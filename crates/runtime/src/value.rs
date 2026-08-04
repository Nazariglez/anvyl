use std::{borrow::Borrow, fmt, hash::Hash, marker::PhantomData, ops::Range, ptr::NonNull};

use ecow::EcoString;

use crate::{
    CollectionLoanState, Ctx, HeapType, ListStorage, MapStorage, RuntimeError, SafepointGuardKind,
    ShapeLoanGuard, Trace, TraceDriver, ValueLoanGuard, Visitor,
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

    pub fn push_str(&mut self, text: &str) {
        self.text.push_str(text);
    }

    pub fn push(&mut self, ch: char) {
        self.text.push(ch);
    }

    pub fn push_float(&mut self, value: f64) {
        use fmt::Write;

        let start = self.len();
        write!(self, "{value}").unwrap();
        if anvyx_semantics::display_float_needs_decimal(value, &self.as_str()[start..]) {
            self.push_str(".0");
        }
    }

    pub fn from_float(value: f64) -> Self {
        let mut text = Self::default();
        text.push_float(value);
        text
    }

    pub fn concat(parts: impl IntoIterator<Item = impl AsRef<str>>) -> Self {
        let mut text = Self::default();
        for part in parts {
            text.push_str(part.as_ref());
        }
        text
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

impl fmt::Write for AnvString {
    fn write_str(&mut self, text: &str) -> fmt::Result {
        self.push_str(text);
        Ok(())
    }

    fn write_char(&mut self, ch: char) -> fmt::Result {
        self.push(ch);
        Ok(())
    }
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
            storage: CowStorageOwner::alloc_in(ctx, storage_ty, storage),
            storage_ty,
            loan: CollectionLoanState::with_safepoint(ctx.__anvyx_safepoint_state()),
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

    fn make_unique<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        materialize: &mut impl FnMut(&T) -> T,
    ) -> Result<(), RuntimeError> {
        self.storage
            .reconstruct_if_shared_in(ctx, self.storage_ty, |storage| {
                ListStorage::from_elems(storage.as_slice().iter().map(materialize))
            })
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
        materialize: &mut impl FnMut(&T) -> T,
        f: impl FnOnce(&mut ListStorage<'cx, T>) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let loan = self.loan.clone();
        loan.structural_mutation_result(|| {
            self.make_unique(ctx, materialize)?;
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

    /// # Safety
    ///
    /// The materializer must not reenter Anvyx or access this list while an element reference is live.
    pub unsafe fn to_vec_with<'rt>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        mut materialize: impl FnMut(&T) -> T,
    ) -> Result<Vec<T>, RuntimeError> {
        self.with_storage(ctx, |storage| {
            Ok(storage.as_slice().iter().map(&mut materialize).collect())
        })
    }

    /// # Safety
    ///
    /// The materializer must not reenter Anvyx or access this list while the element reference is live.
    pub unsafe fn checked_index_with<'rt>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        index: i64,
        materialize: impl FnOnce(&T) -> T,
    ) -> Result<T, RuntimeError> {
        let index = crate::check::checked_index(index, self.len());
        self.with_storage(ctx, |storage| {
            storage.get(index).map(materialize).ok_or_else(|| {
                RuntimeError::new(format!(
                    "list index {index} out of bounds for len {}",
                    self.len()
                ))
            })
        })
    }

    /// # Safety
    ///
    /// The converter must not reenter Anvyx or access this list while an element reference is live.
    pub unsafe fn copy_range_with<'rt, U: 'cx>(
        &self,
        ctx: &mut Ctx<'cx, 'rt>,
        storage_ty: HeapType<'cx, ListStorage<'cx, U>>,
        range: Range<usize>,
        mut convert: impl FnMut(&T) -> U,
    ) -> Result<AnvList<'cx, U>, RuntimeError> {
        if range.start > range.end || range.end > self.len {
            return Err(RuntimeError::new("list range out of bounds"));
        }
        let elems = self.with_storage(ctx, |storage| {
            Ok(storage.as_slice()[range]
                .iter()
                .map(&mut convert)
                .collect::<Vec<_>>())
        })?;
        Ok(AnvList::from_elems(ctx, storage_ty, elems))
    }

    /// # Safety
    ///
    /// The callback must not reenter Anvyx or access this list's storage while the element
    /// reference is live.
    pub unsafe fn with_elem_shared_short<'rt, R>(
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
    pub unsafe fn with_elem_mut_leaf<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        index: usize,
        expected_version: u64,
        mut materialize: impl FnMut(&T) -> T,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.loan.check_stable(expected_version)?;
        self.check_elem_index(index)?;
        self.make_unique(ctx, &mut materialize)?;
        self.with_storage_mut(ctx, |storage| {
            let elem = storage
                .get_mut(index)
                .expect("list storage length drifted from list length");
            f(elem)
        })
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
            loan: self.loan.fresh_with_same_safepoint(),
            len: self.len,
        }
    }

    #[doc(hidden)]
    #[must_use]
    pub fn __anvyx_projection_owner(&self) -> Self {
        Self {
            storage: self.storage.share(),
            storage_ty: self.storage_ty,
            loan: self.loan.fresh_for_projection(),
            len: self.len,
        }
    }

    #[doc(hidden)]
    #[must_use]
    pub fn __anvyx_staged_owner(&self) -> Self {
        Self {
            storage: self.storage.stage(),
            storage_ty: self.storage_ty,
            loan: self.loan.clone(),
            len: self.len,
        }
    }

    #[doc(hidden)]
    #[must_use]
    pub fn __anvyx_commit_staged_owner(mut self) -> Self {
        self.storage = self.storage.commit_stage();
        self
    }

    /// # Safety
    ///
    /// The materializer must not reenter Anvyx or access this list while the element reference is live.
    pub unsafe fn elem_at_shared_with<'rt>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        index: usize,
        expected_version: u64,
        materialize: impl FnOnce(&T) -> T,
    ) -> Result<T, RuntimeError> {
        unsafe {
            self.with_elem_shared_short(ctx, index, expected_version, |elem| Ok(materialize(elem)))
        }
    }

    /// # Safety
    ///
    /// The materializer must not reenter Anvyx or access this list while storage is reconstructed.
    pub unsafe fn push_with<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        elem: T,
        mut materialize: impl FnMut(&T) -> T,
    ) -> Result<(), RuntimeError> {
        self.structurally_mutate_storage(ctx, &mut materialize, |storage| {
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

// Cloning shares the COW handle; it never materializes list payloads.
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
            storage: CowStorageOwner::alloc_in(ctx, storage_ty, storage),
            storage_ty,
            loan: CollectionLoanState::with_safepoint(ctx.__anvyx_safepoint_state()),
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

    /// # Safety
    ///
    /// The materializers must not reenter Anvyx or access this map while storage is reconstructed.
    /// Key materialization must preserve equality and hash identity.
    pub unsafe fn begin_value_loan_by_key_with<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        key: &K,
        mut materialize_key: impl FnMut(&K) -> K,
        mut materialize_value: impl FnMut(&V) -> V,
    ) -> Result<ValueLoanGuard, RuntimeError> {
        self.with_unloaned_storage(ctx, |storage| {
            storage.get_index_of(key).ok_or_else(Self::missing_key)?;
            Ok(())
        })?;
        self.make_unique_with(ctx, &mut materialize_key, &mut materialize_value)?;
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

    /// # Safety
    ///
    /// The materializer must not reenter Anvyx or access this map while the value reference is live.
    pub unsafe fn get_with<'rt>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        key: &K,
        materialize: impl FnOnce(&V) -> V,
    ) -> Result<Option<V>, RuntimeError> {
        self.with_unloaned_storage(ctx, |storage| Ok(storage.get(key).map(materialize)))
    }

    pub fn with_value_shared_short<'rt, R>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        key: &K,
        f: impl FnOnce(Option<&V>) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.with_unloaned_storage(ctx, |storage| f(storage.get(key)))
    }

    pub fn contains_key<'rt>(&self, ctx: &Ctx<'cx, 'rt>, key: &K) -> Result<bool, RuntimeError> {
        self.with_unloaned_storage(ctx, |storage| Ok(storage.contains_key(key)))
    }

    fn make_unique_with<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        materialize_key: &mut impl FnMut(&K) -> K,
        materialize_value: &mut impl FnMut(&V) -> V,
    ) -> Result<(), RuntimeError> {
        self.storage
            .reconstruct_if_shared_in(ctx, self.storage_ty, |storage| {
                Self::materialize_storage(storage, materialize_key, materialize_value)
            })
            .map_err(heap_access_error)
    }

    fn materialize_storage(
        storage: &MapStorage<'cx, K, V>,
        materialize_key: &mut impl FnMut(&K) -> K,
        materialize_value: &mut impl FnMut(&V) -> V,
    ) -> MapStorage<'cx, K, V> {
        let materialized = MapStorage::from_entries(
            storage
                .iter()
                .map(|(key, value)| (materialize_key(key), materialize_value(value))),
        );
        debug_assert_eq!(materialized.len(), storage.len());
        materialized
    }

    fn structurally_mutate_storage_with<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        materialize_key: &mut impl FnMut(&K) -> K,
        materialize_value: &mut impl FnMut(&V) -> V,
        f: impl FnOnce(&mut MapStorage<'cx, K, V>) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let loan = self.loan.clone();
        loan.structural_mutation_result(|| {
            self.make_unique_with(ctx, materialize_key, materialize_value)?;
            let (result, len) = self.with_storage_mut_unchecked(ctx, |storage| {
                let result = f(storage)?;
                Ok((result, storage.len()))
            })?;
            self.len = len;
            Ok(result)
        })
    }

    fn entry_index_error(&self, index: usize) -> RuntimeError {
        RuntimeError::new(format!(
            "map entry index {index} out of bounds for len {}",
            self.len()
        ))
    }

    /// # Safety
    ///
    /// The materializers must not reenter Anvyx or access this map while entry references are live.
    pub unsafe fn entry_at_shared_with<'rt>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        index: usize,
        expected_version: u64,
        materialize_key: impl FnOnce(&K) -> K,
        materialize_value: impl FnOnce(&V) -> V,
    ) -> Result<(K, V), RuntimeError> {
        self.loan.check_stable(expected_version)?;
        self.with_unloaned_storage(ctx, |storage| {
            storage
                .get_index(index)
                .map(|(key, value)| (materialize_key(key), materialize_value(value)))
                .ok_or_else(|| self.entry_index_error(index))
        })
    }

    /// # Safety
    ///
    /// The materializers and callback must not reenter Anvyx or access this map while storage or the
    /// value reference is live. Key materialization must preserve equality and hash identity.
    pub unsafe fn with_value_mut_short_with<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        index: usize,
        expected_version: u64,
        mut materialize_key: impl FnMut(&K) -> K,
        mut materialize_value: impl FnMut(&V) -> V,
        f: impl FnOnce(&mut V) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.loan.before_unloaned_storage_access()?;
        self.loan.check_stable(expected_version)?;
        self.check_entry_index(index)?;
        self.make_unique_with(ctx, &mut materialize_key, &mut materialize_value)?;
        self.with_storage_mut_unchecked(ctx, |storage| {
            let (_, value) = storage
                .get_index_mut(index)
                .expect("map storage length drifted from map length");
            f(value)
        })
    }

    fn check_entry_index(&self, index: usize) -> Result<(), RuntimeError> {
        if index < self.len() {
            Ok(())
        } else {
            Err(self.entry_index_error(index))
        }
    }

    #[must_use]
    pub fn share(&self) -> Self {
        Self {
            storage: self.storage.share(),
            storage_ty: self.storage_ty,
            loan: self.loan.fresh_with_same_safepoint(),
            len: self.len,
        }
    }

    #[doc(hidden)]
    #[must_use]
    pub fn __anvyx_projection_owner(&self) -> Self {
        Self {
            storage: self.storage.share(),
            storage_ty: self.storage_ty,
            loan: self.loan.fresh_for_projection(),
            len: self.len,
        }
    }

    #[doc(hidden)]
    #[must_use]
    pub fn __anvyx_staged_owner(&self) -> Self {
        Self {
            storage: self.storage.stage(),
            storage_ty: self.storage_ty,
            loan: self.loan.clone(),
            len: self.len,
        }
    }

    #[doc(hidden)]
    #[must_use]
    pub fn __anvyx_commit_staged_owner(mut self) -> Self {
        self.storage = self.storage.commit_stage();
        self
    }

    /// # Safety
    ///
    /// The materializer must not reenter Anvyx or access this map while the key reference is live.
    pub unsafe fn key_at_shared_with<'rt>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        index: usize,
        expected_version: u64,
        materialize: impl FnOnce(&K) -> K,
    ) -> Result<K, RuntimeError> {
        self.loan.check_stable(expected_version)?;
        self.with_unloaned_storage(ctx, |storage| {
            storage
                .get_index(index)
                .map(|(key, _)| materialize(key))
                .ok_or_else(|| self.entry_index_error(index))
        })
    }

    /// # Safety
    ///
    /// The materializer must not reenter Anvyx or access this map while the value reference is live.
    pub unsafe fn value_at_shared_with<'rt>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        index: usize,
        expected_version: u64,
        materialize: impl FnOnce(&V) -> V,
    ) -> Result<V, RuntimeError> {
        self.loan.check_stable(expected_version)?;
        self.with_unloaned_storage(ctx, |storage| {
            storage
                .get_index(index)
                .map(|(_, value)| materialize(value))
                .ok_or_else(|| self.entry_index_error(index))
        })
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
        let value = self.with_storage_unchecked(ctx, |storage| {
            storage
                .get(key)
                .map(std::ptr::from_ref)
                .ok_or_else(Self::missing_key)
        })?;
        let _safepoint = ctx
            .__anvyx_safepoint_state()
            .enter(SafepointGuardKind::MutPlace)?;
        f(unsafe { &*value })
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
        let value = self.with_storage_mut_unchecked(ctx, |storage| {
            storage
                .get_mut(key)
                .map(std::ptr::from_mut)
                .ok_or_else(Self::missing_key)
        })?;
        let _safepoint = ctx
            .__anvyx_safepoint_state()
            .enter(SafepointGuardKind::MutPlace)?;
        f(unsafe { &mut *value })
    }

    /// # Safety
    ///
    /// The materializers must not reenter Anvyx or access this map while storage is reconstructed.
    /// Key materialization must preserve equality and hash identity.
    pub unsafe fn insert_with<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        key: K,
        value: V,
        mut materialize_key: impl FnMut(&K) -> K,
        mut materialize_value: impl FnMut(&V) -> V,
    ) -> Result<Option<V>, RuntimeError> {
        if self.contains_key(ctx, &key)? {
            self.make_unique_with(ctx, &mut materialize_key, &mut materialize_value)?;
            return self.with_storage_mut_unchecked(ctx, |storage| Ok(storage.insert(key, value)));
        }
        let old = self.structurally_mutate_storage_with(
            ctx,
            &mut materialize_key,
            &mut materialize_value,
            |storage| Ok(storage.insert(key, value)),
        )?;
        debug_assert!(old.is_none());
        Ok(None)
    }

    /// # Safety
    ///
    /// The materializers must not reenter Anvyx or access this map while storage is reconstructed.
    /// Key materialization must preserve equality and hash identity.
    pub unsafe fn remove_with<'rt>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        key: &K,
        mut materialize_key: impl FnMut(&K) -> K,
        mut materialize_value: impl FnMut(&V) -> V,
    ) -> Result<Option<V>, RuntimeError> {
        if !self.contains_key(ctx, key)? {
            return Ok(None);
        }
        self.structurally_mutate_storage_with(
            ctx,
            &mut materialize_key,
            &mut materialize_value,
            |storage| Ok(storage.shift_remove(key)),
        )
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

// Cloning shares the COW handle; it never materializes map payloads.
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

    /// # Safety
    ///
    /// The materializer must not reenter Anvyx or access the list while storage is reconstructed.
    pub unsafe fn from_list_mut_with<'rt>(
        ctx: &mut Ctx<'cx, 'rt>,
        list: &mut AnvList<'cx, T>,
        start: usize,
        len: usize,
        mut materialize: impl FnMut(&T) -> T,
    ) -> Result<Self, RuntimeError> {
        list.make_unique(ctx, &mut materialize)?;
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

    #[doc(hidden)]
    #[must_use]
    pub fn __anvyx_projection_owner(&self) -> Self {
        Self::new(
            self.root.clone(),
            self.start,
            self.len,
            self.guard.clone(),
            self.mutable,
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

    /// # Safety
    ///
    /// The materializer must not reenter Anvyx or access the slice root while the element reference is live.
    pub unsafe fn elem_at_shared_with<'rt>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        index: i64,
        materialize: impl FnOnce(&T) -> T,
    ) -> Result<T, RuntimeError> {
        let index = self.check_view_index(index)?;
        let absolute = self.start + index;
        match self.root {
            SliceRoot::Raw { ptr, .. } => unsafe {
                Ok(materialize(ptr.as_ptr().add(absolute).as_ref().unwrap()))
            },
            SliceRoot::List { ref storage, .. } => {
                self.check_stable()?;
                ctx.heap_ref()
                    .try_with(storage.handle(), |storage| {
                        storage.get(absolute).map(materialize).ok_or_else(|| {
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
    pub unsafe fn copy_range_with<'rt, U: 'cx>(
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
    pub unsafe fn with_elem_shared_leaf<'rt, R>(
        &self,
        ctx: &Ctx<'cx, 'rt>,
        index: i64,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let index = self.check_view_index(index)?;
        let absolute = self.start + index;
        match self.root {
            SliceRoot::Raw { ptr, .. } => unsafe {
                f(ptr.as_ptr().add(absolute).as_ref().unwrap())
            },
            SliceRoot::List { ref storage, .. } => {
                self.check_stable()?;
                ctx.heap_ref()
                    .try_with(storage.handle(), |storage| {
                        let Some(elem) = storage.get(absolute) else {
                            return Err(RuntimeError::new("slice index out of bounds"));
                        };
                        f(elem)
                    })
                    .map_err(heap_access_error)?
            }
        }
    }

    /// # Safety
    ///
    /// The callback must not access, mutate, move, or invalidate the slice root while it receives
    /// the element reference.
    pub unsafe fn with_elem_mut_leaf<'rt, R>(
        &mut self,
        ctx: &mut Ctx<'cx, 'rt>,
        index: i64,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
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
    use super::{AnvList, AnvSlice};
    use crate::{Ctx, Heap, HeapType, ListStorage};

    fn list_ty<'cx, T: 'cx>(heap: &mut Heap<'cx>) -> HeapType<'cx, ListStorage<'cx, T>> {
        heap.register_untracked::<ListStorage<'_, T>>()
    }

    #[test]
    fn list_slice_keeps_storage_alive_after_owner_drops() {
        Heap::scope(|heap| {
            let ty = list_ty::<i64>(heap);
            let mut ctx = Ctx::new(heap);
            let list = AnvList::from_elems(&mut ctx, ty, [1_i64, 2, 3]);
            let slice = AnvSlice::from_list(&list, 1, 2).unwrap();
            drop(list);

            assert_eq!(
                unsafe { slice.elem_at_shared_with(&ctx, 0, |value| *value) }.unwrap(),
                2
            );
            assert_eq!(
                unsafe { slice.elem_at_shared_with(&ctx, 1, |value| *value) }.unwrap(),
                3
            );
        });
    }
}
