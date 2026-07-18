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
    use std::{
        cell::Cell,
        hash::{Hash, Hasher},
        rc::Rc,
    };

    use super::{AnvList, AnvMap, AnvSlice, AnvString};
    use crate::{
        Ctx, Handle, Heap, HeapType, ListStorage, MapStorage, MutPlace, RuntimeError, Trace,
        TraceDriver, Visitor,
    };

    fn list_ty<'cx, T: 'cx>(heap: &mut Heap<'cx>) -> HeapType<'cx, ListStorage<'cx, T>> {
        heap.register_untracked::<ListStorage<'_, T>>()
    }

    fn map_ty<'cx, K: 'cx, V: 'cx>(heap: &mut Heap<'cx>) -> HeapType<'cx, MapStorage<'cx, K, V>> {
        heap.register_untracked::<MapStorage<'_, K, V>>()
    }

    #[test]
    fn string_builder_writes_directly() {
        use std::fmt::Write;

        let mut text = AnvString::default();
        text.push_str("score: ");
        write!(text, "{}", 42).unwrap();
        text.push('!');

        assert_eq!(text.as_str(), "score: 42!");
        assert_eq!(AnvString::from_float(4.0).as_str(), "4.0");
        assert_eq!(AnvString::from_float(4.5).as_str(), "4.5");
        assert_eq!(AnvString::concat(["a", "b", "c"]).as_str(), "abc");
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
    fn list_storage_view_does_not_force_detach() {
        Heap::scope(|heap| {
            let ty = list_ty::<i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut list = AnvList::from_elems(&mut ctx, ty, [1_i64]);
            let view = list.storage.view();

            unsafe { list.push_with(&mut ctx, 2, |value| *value) }.unwrap();

            assert_eq!(
                unsafe { list.to_vec_with(&ctx, |value| *value) }.unwrap(),
                vec![1, 2]
            );
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

            assert!(unsafe { shared.push_with(&mut ctx, 2, |value| *value) }.is_err());

            assert_eq!(list.storage.logical_owners(), 2);
            assert_eq!(shared.storage.logical_owners(), 2);
            assert_eq!(
                unsafe { list.to_vec_with(&ctx, |value| *value) }.unwrap(),
                vec![1]
            );
            assert_eq!(
                unsafe { shared.to_vec_with(&ctx, |value| *value) }.unwrap(),
                vec![1]
            );
        });
    }

    #[test]
    fn list_materializers_support_non_clone_payloads() {
        struct NonClone(i64);

        Heap::scope(|heap| {
            let calls = Cell::new(0);
            let ty = list_ty::<NonClone>(heap);
            let mut ctx = Ctx::new(heap);
            let list = AnvList::from_elems(&mut ctx, ty, [NonClone(1), NonClone(2)]);
            let mut shared = list.share();
            let materialize = |value: &NonClone| {
                calls.set(calls.get() + 1);
                NonClone(value.0)
            };

            unsafe { shared.push_with(&mut ctx, NonClone(3), materialize) }.unwrap();
            assert_eq!(calls.get(), 2);
            assert_eq!(
                list.with_storage(&ctx, |storage| {
                    Ok(storage
                        .as_slice()
                        .iter()
                        .map(|value| value.0)
                        .collect::<Vec<_>>())
                })
                .unwrap(),
                [1, 2]
            );
            assert_eq!(
                shared
                    .with_storage(&ctx, |storage| {
                        Ok(storage
                            .as_slice()
                            .iter()
                            .map(|value| value.0)
                            .collect::<Vec<_>>())
                    })
                    .unwrap(),
                [1, 2, 3]
            );

            let value = unsafe {
                shared.checked_index_with(&ctx, 1, |value| {
                    calls.set(calls.get() + 1);
                    NonClone(value.0)
                })
            }
            .unwrap();
            assert_eq!(value.0, 2);
            assert_eq!(calls.get(), 3);

            let values = unsafe {
                list.to_vec_with(&ctx, |value| {
                    calls.set(calls.get() + 1);
                    NonClone(value.0)
                })
            }
            .unwrap();
            assert_eq!(
                values.iter().map(|value| value.0).collect::<Vec<_>>(),
                [1, 2]
            );
            assert_eq!(calls.get(), 5);

            let copied = unsafe {
                list.copy_range_with(&mut ctx, ty, 0..2, |value| {
                    calls.set(calls.get() + 1);
                    NonClone(value.0)
                })
            }
            .unwrap();
            assert_eq!(
                copied
                    .with_storage(&ctx, |storage| {
                        Ok(storage
                            .as_slice()
                            .iter()
                            .map(|value| value.0)
                            .collect::<Vec<_>>())
                    })
                    .unwrap(),
                [1, 2]
            );
            assert_eq!(calls.get(), 7);

            let retained = shared.share();
            let mut slice = unsafe {
                AnvSlice::from_list_mut_with(&mut ctx, &mut shared, 0, 3, |value| {
                    calls.set(calls.get() + 1);
                    NonClone(value.0)
                })
            }
            .unwrap();
            assert_eq!(calls.get(), 10);
            unsafe {
                slice
                    .with_elem_mut_leaf(&mut ctx, 0, |value| {
                        value.0 = 9;
                        Ok(())
                    })
                    .unwrap();
            }
            let value = unsafe {
                slice.elem_at_shared_with(&ctx, 0, |value| {
                    calls.set(calls.get() + 1);
                    NonClone(value.0)
                })
            }
            .unwrap();
            assert_eq!(value.0, 9);
            assert_eq!(calls.get(), 11);
            drop(slice);
            assert_eq!(
                retained
                    .with_storage(&ctx, |storage| {
                        Ok(storage
                            .as_slice()
                            .iter()
                            .map(|value| value.0)
                            .collect::<Vec<_>>())
                    })
                    .unwrap(),
                [1, 2, 3]
            );
        });
    }

    #[derive(Default)]
    struct PayloadCounts {
        created: Cell<u32>,
        dropped: Cell<u32>,
    }

    struct MapKey {
        id: i64,
        counts: Rc<PayloadCounts>,
    }

    struct MapValue {
        id: i64,
        counts: Rc<PayloadCounts>,
    }

    impl MapKey {
        fn tracked(id: i64, counts: &Rc<PayloadCounts>) -> Self {
            counts.created.set(counts.created.get() + 1);
            Self {
                id,
                counts: Rc::clone(counts),
            }
        }
    }

    impl MapValue {
        fn tracked(id: i64, counts: &Rc<PayloadCounts>) -> Self {
            counts.created.set(counts.created.get() + 1);
            Self {
                id,
                counts: Rc::clone(counts),
            }
        }
    }

    impl PartialEq for MapKey {
        fn eq(&self, other: &Self) -> bool {
            self.id == other.id
        }
    }

    impl Eq for MapKey {}

    impl Hash for MapKey {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.id.hash(state);
        }
    }

    impl Drop for MapKey {
        fn drop(&mut self) {
            self.counts.dropped.set(self.counts.dropped.get() + 1);
        }
    }

    impl Drop for MapValue {
        fn drop(&mut self) {
            self.counts.dropped.set(self.counts.dropped.get() + 1);
        }
    }

    #[test]
    fn map_materializers_support_non_clone_payloads() {
        let key_counts = Rc::new(PayloadCounts::default());
        let value_counts = Rc::new(PayloadCounts::default());
        let key_materializations = Cell::new(0);
        let value_materializations = Cell::new(0);

        Heap::scope(|heap| {
            let ty = map_ty::<MapKey, MapValue>(heap);
            let mut ctx = Ctx::new(heap);
            let key = |id| MapKey::tracked(id, &key_counts);
            let value = |id| MapValue::tracked(id, &value_counts);
            let materialize_key = |source: &MapKey| {
                key_materializations.set(key_materializations.get() + 1);
                key(source.id)
            };
            let materialize_value = |source: &MapValue| {
                value_materializations.set(value_materializations.get() + 1);
                value(source.id)
            };
            let mut map =
                AnvMap::from_entries(&mut ctx, ty, [(key(1), value(10)), (key(2), value(20))]);
            let original = map.share();
            let version = map.structural_version();

            let lookup = key(1);
            let read = unsafe { map.get_with(&ctx, &lookup, materialize_value) }
                .unwrap()
                .unwrap();
            assert_eq!(read.id, 10);
            let entry = unsafe {
                map.entry_at_shared_with(&ctx, 1, version, materialize_key, materialize_value)
            }
            .unwrap();
            assert_eq!((entry.0.id, entry.1.id), (2, 20));
            assert_eq!(
                unsafe { map.key_at_shared_with(&ctx, 0, version, materialize_key) }
                    .unwrap()
                    .id,
                1
            );
            assert_eq!(
                unsafe { map.value_at_shared_with(&ctx, 0, version, materialize_value) }
                    .unwrap()
                    .id,
                10
            );

            let old = unsafe {
                map.insert_with(
                    &mut ctx,
                    key(1),
                    value(11),
                    materialize_key,
                    materialize_value,
                )
            }
            .unwrap()
            .unwrap();
            assert_eq!(old.id, 10);
            assert_eq!(map.structural_version(), version);
            assert_eq!(
                original
                    .with_value_shared_short(&ctx, &lookup, |value| Ok(value.unwrap().id))
                    .unwrap(),
                10
            );

            let before_insert = map.share();
            unsafe {
                map.insert_with(
                    &mut ctx,
                    key(3),
                    value(30),
                    materialize_key,
                    materialize_value,
                )
            }
            .unwrap();
            let inserted_version = map.structural_version();
            assert!(!before_insert.contains_key(&ctx, &key(3)).unwrap());
            assert_eq!(inserted_version, version + 1);
            assert_eq!(
                unsafe { map.key_at_shared_with(&ctx, 1, inserted_version, materialize_key) }
                    .unwrap()
                    .id,
                2
            );

            let before_remove = map.share();
            let removed =
                unsafe { map.remove_with(&mut ctx, &key(2), materialize_key, materialize_value) }
                    .unwrap()
                    .unwrap();
            assert_eq!(removed.id, 20);
            assert_eq!(
                before_remove
                    .with_value_shared_short(&ctx, &key(2), |value| Ok(value.unwrap().id))
                    .unwrap(),
                20
            );
            let removed_version = map.structural_version();
            assert_eq!(removed_version, inserted_version + 1);
            assert_eq!(
                unsafe { map.key_at_shared_with(&ctx, 1, removed_version, materialize_key) }
                    .unwrap()
                    .id,
                3
            );

            let retained = map.share();
            unsafe {
                map.with_value_mut_short_with(
                    &mut ctx,
                    0,
                    removed_version,
                    materialize_key,
                    materialize_value,
                    |value| {
                        value.id = 99;
                        Ok(())
                    },
                )
            }
            .unwrap();
            assert_eq!(map.structural_version(), removed_version);
            assert_eq!(
                retained
                    .with_value_shared_short(&ctx, &lookup, |value| Ok(value.unwrap().id))
                    .unwrap(),
                11
            );
            assert_eq!(
                map.with_value_shared_short(&ctx, &lookup, |value| Ok(value.unwrap().id))
                    .unwrap(),
                99
            );

            let loan_alias = map.share();
            let loan = unsafe {
                map.begin_value_loan_by_key_with(
                    &mut ctx,
                    &lookup,
                    materialize_key,
                    materialize_value,
                )
            }
            .unwrap();
            map.with_value_mut_by_key(&mut ctx, &lookup, loan.version(), loan.id(), |value| {
                value.id = 100;
                Ok(())
            })
            .unwrap();
            assert_eq!(
                loan_alias
                    .with_value_shared_short(&ctx, &lookup, |value| Ok(value.unwrap().id))
                    .unwrap(),
                99
            );
            drop(loan);
            assert_eq!(
                map.with_value_shared_short(&ctx, &lookup, |value| Ok(value.unwrap().id))
                    .unwrap(),
                100
            );

            assert_eq!(key_materializations.get(), 15);
            assert_eq!(value_materializations.get(), 14);

            let shape = map.begin_shape_loan().unwrap();
            let key_calls = key_materializations.get();
            let value_calls = value_materializations.get();
            assert!(
                unsafe {
                    map.insert_with(
                        &mut ctx,
                        key(4),
                        value(40),
                        materialize_key,
                        materialize_value,
                    )
                }
                .is_err()
            );
            assert_eq!(key_materializations.get(), key_calls);
            assert_eq!(value_materializations.get(), value_calls);
            assert_eq!(map.structural_version(), removed_version);
            drop(shape);
        });

        assert_eq!(key_counts.dropped.get(), key_counts.created.get());
        assert_eq!(value_counts.dropped.get(), value_counts.created.get());
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
            let outcome = ctx.collect_all().unwrap();

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
            let outcome = ctx.collect_all().unwrap();

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
            let outcome = ctx.collect_all().unwrap();

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
            let outcome = ctx.collect_all().unwrap();

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

            unsafe { shared.push_with(&mut ctx, replacement, AnvList::share) }.unwrap();

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

            unsafe { shared.insert_with(&mut ctx, 2, replacement, |key| *key, AnvList::share) }
                .unwrap();

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
    fn map_value_loan_blocks_external_value_update() {
        Heap::scope(|heap| {
            let ty = map_ty::<&str, i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut map = AnvMap::from_entries(&mut ctx, ty, [("a", 1_i64)]);
            let shared = map.share();
            let guard = unsafe {
                map.begin_value_loan_by_key_with(&mut ctx, &"a", |key| *key, |value| *value)
            }
            .unwrap();
            let version = guard.version();

            assert!(
                unsafe { map.insert_with(&mut ctx, "a", 2, |key| *key, |value| *value) }.is_err()
            );
            unsafe {
                assert!(
                    map.with_value_mut_short_with(
                        &mut ctx,
                        0,
                        version,
                        |key| *key,
                        |value| *value,
                        |value| {
                            *value = 3;
                            Ok(())
                        }
                    )
                    .is_err()
                );
            }
            drop(guard);

            assert_eq!(
                unsafe { map.get_with(&ctx, &"a", |value| *value) }.unwrap(),
                Some(1)
            );
            assert_eq!(
                unsafe { shared.get_with(&ctx, &"a", |value| *value) }.unwrap(),
                Some(1)
            );
            assert_eq!(
                unsafe { map.insert_with(&mut ctx, "a", 2, |key| *key, |value| *value) }.unwrap(),
                Some(1)
            );
        });
    }

    #[test]
    fn map_value_loan_blocks_ordinary_storage_access() {
        Heap::scope(|heap| {
            let ty = map_ty::<&str, i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut map = AnvMap::from_entries(&mut ctx, ty, [("a", 1_i64)]);
            let guard = unsafe {
                map.begin_value_loan_by_key_with(&mut ctx, &"a", |key| *key, |value| *value)
            }
            .unwrap();
            let version = guard.version();

            assert!(unsafe { map.get_with(&ctx, &"a", |value| *value) }.is_err());
            assert!(map.contains_key(&ctx, &"a").is_err());
            assert!(map.begin_shape_loan().is_err());
            assert!(unsafe { map.key_at_shared_with(&ctx, 0, version, |key| *key) }.is_err());
            assert!(unsafe { map.value_at_shared_with(&ctx, 0, version, |value| *value) }.is_err());
            assert!(
                unsafe { map.insert_with(&mut ctx, "b", 2, |key| *key, |value| *value) }.is_err()
            );
            assert!(
                unsafe { map.remove_with(&mut ctx, &"missing", |key| *key, |value| *value) }
                    .is_err()
            );

            map.with_value_mut_by_key(&mut ctx, &"a", version, guard.id(), |value| {
                *value = 3;
                Ok(())
            })
            .unwrap();
            drop(guard);

            assert_eq!(
                unsafe { map.get_with(&ctx, &"a", |value| *value) }.unwrap(),
                Some(3)
            );
        });
    }

    #[test]
    fn map_value_loan_rejects_projection_after_share() {
        Heap::scope(|heap| {
            let ty = map_ty::<&str, i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut map = AnvMap::from_entries(&mut ctx, ty, [("a", 1_i64)]);
            let guard = unsafe {
                map.begin_value_loan_by_key_with(&mut ctx, &"a", |key| *key, |value| *value)
            }
            .unwrap();
            let shared = map.share();

            assert!(
                map.with_value_mut_by_key(&mut ctx, &"a", guard.version(), guard.id(), |value| {
                    *value = 3;
                    Ok(())
                })
                .is_err()
            );
            drop(guard);

            assert_eq!(
                unsafe { map.get_with(&ctx, &"a", |value| *value) }.unwrap(),
                Some(1)
            );
            assert_eq!(
                unsafe { shared.get_with(&ctx, &"a", |value| *value) }.unwrap(),
                Some(1)
            );
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
                map.with_value_mut_short_with(
                    &mut ctx,
                    0,
                    version,
                    |key| *key,
                    |value| *value,
                    |value| {
                        *value = 3;
                        Ok(())
                    },
                )
            }
            .unwrap();

            assert_eq!(
                unsafe { map.get_with(&ctx, &"a", |value| *value) }.unwrap(),
                Some(3)
            );
            assert_eq!(
                unsafe { shared.get_with(&ctx, &"a", |value| *value) }.unwrap(),
                Some(1)
            );
            assert_eq!(map.structural_version(), 0);
        });
    }

    #[test]
    fn mutable_slice_staging_preserves_list_loan_state() {
        Heap::scope(|heap| {
            let ty = list_ty::<i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut list = AnvList::from_elems(&mut ctx, ty, [1_i64]);
            unsafe { list.push_with(&mut ctx, 2, |value| *value) }.unwrap();
            let version = list.structural_version();
            let guard = list.begin_shape_loan().unwrap();
            let mut place = MutPlace::local(&mut list);
            let slice = unsafe { place.slice_view_mut_with(&mut ctx, 0, 1, false, |value| *value) }
                .unwrap();
            drop(slice);
            drop(place);

            assert_eq!(list.structural_version(), version);
            assert!(unsafe { list.push_with(&mut ctx, 3, |value| *value) }.is_err());
            drop(guard);
            unsafe { list.push_with(&mut ctx, 3, |value| *value) }.unwrap();
            assert_eq!(list.structural_version(), version + 1);

            let mut place = MutPlace::local(&mut list);
            let staged_loan = unsafe { place.begin_shape_loan_with_ctx(&mut ctx) }.unwrap();
            drop(place);
            assert!(unsafe { list.push_with(&mut ctx, 4, |value| *value) }.is_err());
            drop(staged_loan);
            unsafe { list.push_with(&mut ctx, 4, |value| *value) }.unwrap();
            assert_eq!(list.structural_version(), version + 2);
        });
    }

    #[test]
    fn projection_owner_preserves_list_version() {
        Heap::scope(|heap| {
            let ty = list_ty::<i64>(heap);
            let mut ctx = Ctx::new(heap);
            let mut list = AnvList::from_elems(&mut ctx, ty, [1_i64]);
            unsafe { list.push_with(&mut ctx, 2, |value| *value) }.unwrap();
            let version = list.structural_version();
            let mut owner = list.__anvyx_projection_owner();

            unsafe {
                owner.with_elem_mut_leaf(
                    &mut ctx,
                    0,
                    version,
                    |value| *value,
                    |value| {
                        *value = 3;
                        Ok(())
                    },
                )
            }
            .unwrap();

            assert_eq!(
                unsafe { owner.elem_at_shared_with(&ctx, 0, version, |value| *value) }.unwrap(),
                3
            );
            assert_eq!(
                unsafe { list.elem_at_shared_with(&ctx, 0, version, |value| *value) }.unwrap(),
                1
            );
        });
    }

    #[test]
    fn projection_owner_preserves_slice_mutability() {
        Heap::scope(|heap| {
            let mut ctx = Ctx::new(heap);
            let mut values = [1_i64];
            let slice = unsafe {
                AnvSlice::from_raw_parts_mut(values.as_mut_ptr(), values.len(), 0, values.len())
            };
            let mut owner = slice.__anvyx_projection_owner();

            unsafe {
                owner.with_elem_mut_leaf(&mut ctx, 0, |value| {
                    *value = 2;
                    Ok(())
                })
            }
            .unwrap();

            assert_eq!(values[0], 2);
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

    #[test]
    fn slice_leaf_error_preserves_mutation() {
        Heap::scope(|heap| {
            let mut ctx = Ctx::new(heap);
            let mut values = [1_i64, 2];
            let mut slice = unsafe {
                AnvSlice::from_raw_parts_mut(values.as_mut_ptr(), values.len(), 0, values.len())
            };

            let err = unsafe {
                slice.with_elem_mut_leaf(&mut ctx, 1, |value| {
                    *value = 4;
                    Err::<(), _>(RuntimeError::new("early"))
                })
            }
            .unwrap_err();
            assert_eq!(err.message(), "early");
            assert_eq!(
                unsafe { slice.elem_at_shared_with(&ctx, 1, |value| *value) }.unwrap(),
                4
            );
            unsafe {
                slice.with_elem_mut_leaf(&mut ctx, 1, |value| {
                    *value = 5;
                    Ok(())
                })
            }
            .unwrap();
            assert_eq!(values[1], 5);
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
                list.with_elem_mut_leaf(
                    &mut ctx,
                    0,
                    version,
                    |value| *value,
                    |value| {
                        *value = 10;
                        Ok(())
                    },
                )
            }
            .unwrap();

            assert_eq!(
                unsafe { list.to_vec_with(&ctx, |value| *value) }.unwrap(),
                vec![10, 2]
            );
            assert_eq!(
                unsafe { shared.to_vec_with(&ctx, |value| *value) }.unwrap(),
                vec![1, 2]
            );
            assert_eq!(
                unsafe { slice.elem_at_shared_with(&ctx, 0, |value| *value) }.unwrap(),
                1
            );
        });
    }
}
