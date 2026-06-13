use std::{
    borrow::Borrow,
    fmt,
    marker::PhantomData,
    ops::{Index, Range},
    ptr::NonNull,
};

use ecow::{EcoString, EcoVec};

use crate::{CollectionLoanState, RuntimeError, ShapeLoanGuard, Trace, TraceDriver, Visitor};

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

// SAFETY: `AnvString` owns only `EcoString` text and cannot contain heap handles.
unsafe impl<'cx> Trace<'cx> for AnvString {
    #[inline]
    fn trace<D: TraceDriver<'cx>>(&self, _visitor: &mut Visitor<'cx, '_, D>) {}
}

pub struct AnvList<T> {
    elems: EcoVec<T>,
    loan: CollectionLoanState,
}

impl<T> Default for AnvList<T> {
    fn default() -> Self {
        Self {
            elems: EcoVec::new(),
            loan: CollectionLoanState::default(),
        }
    }
}

impl<T> AnvList<T> {
    pub fn len(&self) -> usize {
        self.elems.len()
    }

    pub fn len_i64(&self) -> i64 {
        i64::try_from(self.len()).expect("list length exceeds i64::MAX")
    }

    pub fn is_empty(&self) -> bool {
        self.elems.is_empty()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.elems.get(index)
    }

    pub fn checked_index(&self, index: i64) -> &T {
        &self.elems[crate::check::checked_index(index, self.len())]
    }

    pub fn as_slice(&self) -> &[T] {
        self.elems.as_slice()
    }

    pub fn begin_shape_loan(&self) -> Result<ShapeLoanGuard, RuntimeError> {
        self.loan.begin_shape_loan()
    }

    pub fn structural_version(&self) -> u64 {
        self.loan.current_version()
    }

    pub fn with_elem_shared_short<R>(
        &self,
        index: usize,
        expected_version: u64,
        f: impl FnOnce(&T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        self.loan.check_stable(expected_version)?;
        let Some(elem) = self.elems.get(index) else {
            return Err(RuntimeError::new(format!(
                "list index {index} out of bounds for len {}",
                self.len()
            )));
        };
        f(elem)
    }

    pub fn with_elem_mut_short<R>(
        &mut self,
        index: usize,
        expected_version: u64,
        f: impl FnOnce(&mut T) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError>
    where
        T: Clone,
    {
        self.loan.check_stable(expected_version)?;
        let len = self.len();
        let Some(elem) = self.elems.make_mut().get_mut(index) else {
            return Err(RuntimeError::new(format!(
                "list index {index} out of bounds for len {len}"
            )));
        };
        f(elem)
    }
}

impl<T: Clone> Clone for AnvList<T> {
    fn clone(&self) -> Self {
        Self {
            elems: self.elems.clone(),
            loan: CollectionLoanState::default(),
        }
    }
}

impl<T: PartialEq> PartialEq for AnvList<T> {
    fn eq(&self, other: &Self) -> bool {
        self.elems == other.elems
    }
}

impl<T: Eq> Eq for AnvList<T> {}

impl<T: Clone> AnvList<T> {
    #[must_use]
    pub fn share(&self) -> Self {
        self.clone()
    }

    pub fn from_elems(elems: impl IntoIterator<Item = T>) -> Self {
        Self {
            elems: elems.into_iter().collect(),
            loan: CollectionLoanState::default(),
        }
    }

    pub fn elem_at_shared(&self, index: usize, expected_version: u64) -> Result<T, RuntimeError> {
        self.with_elem_shared_short(index, expected_version, |elem| Ok(elem.clone()))
    }

    pub fn push(&mut self, elem: T) -> Result<(), RuntimeError> {
        let loan = self.loan.clone();
        loan.structural_mutation(|| self.elems.push(elem))
    }

    pub fn replace_with(&mut self, other: Self) -> Result<(), RuntimeError> {
        let loan = self.loan.clone();
        loan.structural_mutation(|| *self = other)
    }
}

impl<T: Clone> FromIterator<T> for AnvList<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self::from_elems(iter)
    }
}

impl<T> Index<usize> for AnvList<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.elems
            .get(index)
            .unwrap_or_else(|| panic!("list index {index} out of bounds for len {}", self.len()))
    }
}

impl<T> Index<Range<usize>> for AnvList<T> {
    type Output = [T];

    fn index(&self, range: Range<usize>) -> &Self::Output {
        &self.as_slice()[range]
    }
}

pub struct AnvMap<K, V> {
    entries: EcoVec<(K, V)>,
    loan: CollectionLoanState,
}

impl<K, V> Default for AnvMap<K, V> {
    fn default() -> Self {
        Self {
            entries: EcoVec::new(),
            loan: CollectionLoanState::default(),
        }
    }
}

impl<K, V> AnvMap<K, V> {
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.entries.iter().map(|(key, value)| (key, value))
    }

    pub fn begin_shape_loan(&self) -> Result<ShapeLoanGuard, RuntimeError> {
        self.loan.begin_shape_loan()
    }

    pub fn structural_version(&self) -> u64 {
        self.loan.current_version()
    }
}

impl<K: PartialEq, V> AnvMap<K, V> {
    fn find_key(&self, key: &K) -> Option<usize> {
        self.entries
            .iter()
            .position(|(entry_key, _)| entry_key == key)
    }

    fn entry_at_checked(
        &self,
        index: usize,
        expected_version: u64,
    ) -> Result<(&K, &V), RuntimeError> {
        self.loan.check_stable(expected_version)?;
        self.entry_at(index).ok_or_else(|| {
            RuntimeError::new(format!(
                "map entry index {index} out of bounds for len {}",
                self.len()
            ))
        })
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.entries
            .iter()
            .find(|(entry_key, _)| entry_key == key)
            .map(|(_, value)| value)
    }

    pub fn entry_at(&self, index: usize) -> Option<(&K, &V)> {
        self.entries.get(index).map(|(key, value)| (key, value))
    }

    pub fn with_value_mut_short<R>(
        &mut self,
        index: usize,
        expected_version: u64,
        f: impl FnOnce(&mut V) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError>
    where
        K: Clone,
        V: Clone,
    {
        self.loan.check_stable(expected_version)?;
        let len = self.len();
        let Some((_, value)) = self.entries.make_mut().get_mut(index) else {
            return Err(RuntimeError::new(format!(
                "map entry index {index} out of bounds for len {len}"
            )));
        };
        f(value)
    }
}

impl<K: Clone, V: Clone> Clone for AnvMap<K, V> {
    fn clone(&self) -> Self {
        Self {
            entries: self.entries.clone(),
            loan: CollectionLoanState::default(),
        }
    }
}

impl<K: PartialEq, V: PartialEq> PartialEq for AnvMap<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.entries == other.entries
    }
}

impl<K: Eq, V: Eq> Eq for AnvMap<K, V> {}

impl<K: Clone + PartialEq, V: Clone> AnvMap<K, V> {
    #[must_use]
    pub fn share(&self) -> Self {
        self.clone()
    }

    pub fn from_entries(entries: impl IntoIterator<Item = (K, V)>) -> Self {
        let mut map = Self::default();
        for (key, value) in entries {
            map.insert_unchecked(key, value);
        }
        map
    }

    fn insert_unchecked(&mut self, key: K, value: V) -> Option<V> {
        if let Some(index) = self.find_key(&key) {
            return Some(std::mem::replace(
                &mut self.entries.make_mut()[index].1,
                value,
            ));
        }
        self.entries.push((key, value));
        None
    }

    pub fn key_at_shared(&self, index: usize, expected_version: u64) -> Result<K, RuntimeError> {
        self.entry_at_checked(index, expected_version)
            .map(|(key, _)| key.clone())
    }

    pub fn value_at_shared(&self, index: usize, expected_version: u64) -> Result<V, RuntimeError> {
        self.entry_at_checked(index, expected_version)
            .map(|(_, value)| value.clone())
    }

    pub fn insert(&mut self, key: K, value: V) -> Result<Option<V>, RuntimeError> {
        if let Some(index) = self.find_key(&key) {
            return Ok(Some(std::mem::replace(
                &mut self.entries.make_mut()[index].1,
                value,
            )));
        }
        let loan = self.loan.clone();
        loan.structural_mutation(|| self.entries.push((key, value)))?;
        Ok(None)
    }

    pub fn remove(&mut self, key: &K) -> Result<Option<V>, RuntimeError> {
        let Some(index) = self.find_key(key) else {
            return Ok(None);
        };
        let loan = self.loan.clone();
        loan.structural_mutation(|| self.entries.remove(index).1)
            .map(Some)
    }

    pub fn replace_with(&mut self, other: Self) -> Result<(), RuntimeError> {
        let loan = self.loan.clone();
        loan.structural_mutation(|| *self = other)
    }
}

impl<K: fmt::Debug, V: fmt::Debug> fmt::Debug for AnvMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<T: fmt::Debug> fmt::Debug for AnvList<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.elems.fmt(f)
    }
}

enum SliceRoot<T> {
    Raw { ptr: NonNull<T>, len: usize },
    List { list: NonNull<AnvList<T>> },
}

impl<T> Clone for SliceRoot<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for SliceRoot<T> {}

pub struct AnvSlice<T> {
    root: SliceRoot<T>,
    start: usize,
    len: usize,
    guard: Option<ShapeLoanGuard>,
    mutable: bool,
    _marker: PhantomData<T>,
}

impl<T> AnvSlice<T> {
    /// # Safety
    ///
    /// `ptr` must point to `len` initialized contiguous elements that outlive the returned descriptor.
    pub unsafe fn from_raw_parts(ptr: *const T, len: usize, start: usize, len_view: usize) -> Self {
        let ptr = NonNull::new(ptr.cast_mut()).expect("slice root pointer cannot be null");
        Self::new(SliceRoot::Raw { ptr, len }, start, len_view, None, false)
    }

    /// # Safety
    ///
    /// `ptr` must point to `len` initialized contiguous elements, be uniquely mutable during short mutable accesses, and outlive the returned descriptor.
    pub unsafe fn from_raw_parts_mut(
        ptr: *mut T,
        len: usize,
        start: usize,
        len_view: usize,
    ) -> Self {
        let ptr = NonNull::new(ptr).expect("slice root pointer cannot be null");
        Self::new(SliceRoot::Raw { ptr, len }, start, len_view, None, true)
    }

    /// # Safety
    ///
    /// `list` must remain valid and must not be moved while the returned descriptor can be used.
    pub unsafe fn from_list(
        list: *const AnvList<T>,
        start: usize,
        len: usize,
        guard: ShapeLoanGuard,
    ) -> Self {
        let list = NonNull::new(list.cast_mut()).expect("slice root pointer cannot be null");
        Self::new(SliceRoot::List { list }, start, len, Some(guard), false)
    }

    /// # Safety
    ///
    /// `list` must remain valid, must not be moved, and must be uniquely mutable during short mutable accesses while the descriptor can be used.
    pub unsafe fn from_list_mut(
        list: *mut AnvList<T>,
        start: usize,
        len: usize,
        guard: ShapeLoanGuard,
    ) -> Self {
        let list = NonNull::new(list).expect("slice root pointer cannot be null");
        Self::new(SliceRoot::List { list }, start, len, Some(guard), true)
    }

    fn new(
        root: SliceRoot<T>,
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
        Self::new(self.root, self.start, self.len, self.guard.clone(), false)
    }

    #[must_use]
    pub fn slice(&self, start: usize, len: usize) -> Self {
        Self::new(
            self.root,
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

    pub fn elem_at_shared(&self, index: i64) -> Result<T, RuntimeError>
    where
        T: Clone,
    {
        let index = self.check_view_index(index)?;
        let absolute = self.start + index;
        match self.root {
            SliceRoot::Raw { ptr, .. } => unsafe {
                Ok(ptr.as_ptr().add(absolute).as_ref().unwrap().clone())
            },
            SliceRoot::List { list } => unsafe {
                let list = list.as_ref();
                self.check_stable()?;
                list.elem_at_shared(absolute, self.version())
            },
        }
    }

    /// # Safety
    ///
    /// `convert` must not access, mutate, move, or invalidate the slice root while it receives element references.
    pub unsafe fn copy_range_with<U: Clone>(
        &self,
        range: Range<usize>,
        mut convert: impl FnMut(&T) -> U,
    ) -> Result<AnvList<U>, RuntimeError> {
        let absolute = self.check_view_range(range)?;
        match self.root {
            SliceRoot::Raw { ptr, .. } => unsafe {
                Ok(AnvList::from_elems(absolute.map(|index| {
                    convert(ptr.as_ptr().add(index).as_ref().unwrap())
                })))
            },
            SliceRoot::List { list } => unsafe {
                let list = list.as_ref();
                self.check_stable()?;
                Ok(AnvList::from_elems(
                    list.as_slice()[absolute].iter().map(convert),
                ))
            },
        }
    }

    pub fn with_elem_mut_short<R>(
        &mut self,
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
            SliceRoot::List { mut list } => unsafe {
                let list = list.as_mut();
                self.check_stable()?;
                list.with_elem_mut_short(absolute, self.version(), f)
            },
        }
    }

    fn check_stable(&self) -> Result<(), RuntimeError> {
        match &self.guard {
            Some(guard) => guard.check_stable(),
            None => Ok(()),
        }
    }

    fn version(&self) -> u64 {
        self.guard
            .as_ref()
            .expect("list slice needs guard")
            .version()
    }

    fn check_view_range(&self, range: Range<usize>) -> Result<Range<usize>, RuntimeError> {
        let Some(view_end) = self.start.checked_add(self.len) else {
            return Err(RuntimeError::new("slice range out of bounds"));
        };
        let root_len = match self.root {
            SliceRoot::Raw { len, .. } => len,
            SliceRoot::List { list } => unsafe { list.as_ref().len() },
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

#[cfg(test)]
mod tests {
    use super::{AnvList, AnvMap, AnvSlice, AnvString};
    use crate::{Heap, Trace, TraceDriver, Visitor};

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
    fn anv_string_concat_keeps_order() {
        let text = AnvString::concat(["red", " ", "fox"]);
        assert_eq!(text.as_str(), "red fox");
    }

    #[test]
    fn anv_string_heap_clone_is_independent_after_mutation() {
        let original = AnvString::from("this string is long enough to spill out of inline storage");
        let mut changed = original.clone();

        changed.text.push_str("+");

        assert_eq!(
            original.as_str(),
            "this string is long enough to spill out of inline storage"
        );
        assert_eq!(
            changed.as_str(),
            "this string is long enough to spill out of inline storage+"
        );
    }

    #[test]
    fn anv_string_as_str_needs_no_runtime_context() {
        fn accepts_str(text: &str) -> usize {
            text.len()
        }

        let text = AnvString::from("ctx-free");

        assert_eq!(accepts_str(text.as_str()), 8);
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
    fn anv_list_constructs_indexes_and_reports_len() {
        let list = AnvList::from_elems([1_i64, 2, 3]);

        assert_eq!(list.len(), 3);
        assert_eq!(list.len_i64(), 3);
        assert_eq!(list.get(1), Some(&2));
        assert_eq!(*list.checked_index(2), 3);
        assert_eq!(list.as_slice(), &[1, 2, 3]);
    }

    #[test]
    #[should_panic(expected = "negative index")]
    fn anv_list_rejects_negative_index() {
        AnvList::from_elems([1_i64]).checked_index(-1);
    }

    #[test]
    #[should_panic(expected = "index out of bounds")]
    fn anv_list_rejects_out_of_bounds_index() {
        AnvList::from_elems([1_i64]).checked_index(1);
    }

    #[test]
    fn anv_list_clone_mutation_detaches() {
        let original = AnvList::from_elems([1_i64, 2]);
        let mut changed = original.clone();

        changed.push(3).unwrap();

        assert_eq!(original.as_slice(), &[1, 2]);
        assert_eq!(changed.as_slice(), &[1, 2, 3]);
    }

    #[test]
    fn anv_list_preserves_value_semantics_after_aliasing() {
        let a = AnvList::from_elems([AnvString::from("a")]);
        let mut b = a.clone();

        b.push(AnvString::from("b")).unwrap();

        assert_eq!(a.checked_index(0).as_str(), "a");
        assert_eq!(a.len(), 1);
        assert_eq!(b.checked_index(1).as_str(), "b");
    }

    #[test]
    fn anv_map_preserves_insertion_order_and_replaces() {
        let mut map = AnvMap::from_entries([("a", 1_i64), ("b", 2)]);

        assert_eq!(map.insert("a", 3).unwrap(), Some(1));
        assert_eq!(map.get(&"a"), Some(&3));
        assert_eq!(map.iter().collect::<Vec<_>>(), vec![(&"a", &3), (&"b", &2)]);
    }

    #[test]
    fn anv_map_remove_and_alias_detach() {
        let original = AnvMap::from_entries([(1_i64, AnvString::from("one"))]);
        let mut changed = original.share();

        changed.insert(2, AnvString::from("two")).unwrap();
        assert!(original.get(&2).is_none());
        assert_eq!(
            changed.remove(&1).unwrap().as_ref().map(AnvString::as_str),
            Some("one")
        );
        assert!(changed.get(&1).is_none());
    }

    #[test]
    fn list_structural_mutation_is_guarded() {
        let mut list = AnvList::from_elems([1_i64, 2]);
        let guard = list.begin_shape_loan().unwrap();

        assert!(list.push(3).is_err());
        assert_eq!(list.as_slice(), &[1, 2]);
        drop(guard);

        list.push(3).unwrap();
        assert_eq!(list.structural_version(), 1);
        assert_eq!(list.as_slice(), &[1, 2, 3]);
    }

    #[test]
    fn list_element_mutation_is_not_structural() {
        let mut list = AnvList::from_elems([1_i64, 2]);
        let guard = list.begin_shape_loan().unwrap();
        let version = guard.version();

        list.with_elem_mut_short(1, version, |elem| {
            *elem = 5;
            Ok(())
        })
        .unwrap();

        assert_eq!(list.structural_version(), 0);
        assert_eq!(list.elem_at_shared(1, version).unwrap(), 5);
        assert_eq!(list.as_slice(), &[1, 5]);
    }

    #[test]
    fn list_replacement_is_guarded() {
        let mut list = AnvList::from_elems([1_i64, 2]);
        let guard = list.begin_shape_loan().unwrap();

        assert!(list.replace_with(AnvList::from_elems([3])).is_err());
        drop(guard);
        list.replace_with(AnvList::from_elems([3])).unwrap();
        assert_eq!(list.as_slice(), &[3]);
    }

    #[test]
    fn copied_list_has_independent_loan_state() {
        let original = AnvList::from_elems([1_i64]);
        let _guard = original.begin_shape_loan().unwrap();
        let mut copy = original.share();

        copy.push(2).unwrap();

        assert_eq!(original.as_slice(), &[1]);
        assert_eq!(copy.as_slice(), &[1, 2]);
    }

    #[test]
    fn stale_list_version_rejects_short_access() {
        let mut list = AnvList::from_elems([1_i64]);
        let version = list.structural_version();
        list.push(2).unwrap();

        assert!(list.elem_at_shared(0, version).is_err());
        assert!(list.with_elem_mut_short(0, version, |_| Ok(())).is_err());
    }

    #[test]
    fn map_loan_allows_value_update_but_not_key_set_mutation() {
        let mut map = AnvMap::from_entries([("a", 1_i64)]);
        let guard = map.begin_shape_loan().unwrap();

        assert_eq!(map.insert("a", 2).unwrap(), Some(1));
        assert_eq!(map.get(&"a"), Some(&2));
        assert_eq!(map.structural_version(), 0);
        assert!(map.insert("b", 3).is_err());
        assert!(map.remove(&"a").is_err());
        assert_eq!(map.remove(&"missing").unwrap(), None);
        assert!(map.replace_with(AnvMap::from_entries([("z", 9)])).is_err());
        drop(guard);

        assert_eq!(map.insert("b", 3).unwrap(), None);
        assert_eq!(map.structural_version(), 1);
    }

    #[test]
    fn map_short_access_uses_stable_entry_index() {
        let mut map = AnvMap::from_entries([("a", 1_i64), ("b", 2)]);
        let guard = map.begin_shape_loan().unwrap();
        let version = guard.version();

        assert_eq!(map.key_at_shared(1, version).unwrap(), "b");
        assert_eq!(map.value_at_shared(1, version).unwrap(), 2);
        map.with_value_mut_short(1, version, |value| {
            *value = 7;
            Ok(())
        })
        .unwrap();
        assert_eq!(map.value_at_shared(1, version).unwrap(), 7);
        assert_eq!(map.structural_version(), 0);
    }

    #[test]
    fn stale_map_version_rejects_short_access() {
        let mut map = AnvMap::from_entries([("a", 1_i64)]);
        let version = map.structural_version();
        map.insert("b", 2).unwrap();

        assert!(map.key_at_shared(0, version).is_err());
        assert!(map.value_at_shared(0, version).is_err());
        assert!(map.with_value_mut_short(0, version, |_| Ok(())).is_err());
    }

    #[test]
    fn copied_map_has_independent_loan_state() {
        let original = AnvMap::from_entries([("a", 1_i64)]);
        let _guard = original.begin_shape_loan().unwrap();
        let mut copy = original.share();

        copy.insert("b", 2).unwrap();
        copy.remove(&"a").unwrap();

        assert_eq!(original.iter().collect::<Vec<_>>(), vec![(&"a", &1)]);
        assert_eq!(copy.iter().collect::<Vec<_>>(), vec![(&"b", &2)]);
    }

    #[test]
    fn slice_descriptor_reads_array_without_borrowing_for_lifetime() {
        let values = [1_i64, 2, 3];
        let slice = unsafe { AnvSlice::from_raw_parts(values.as_ptr(), values.len(), 1, 2) };

        assert_eq!(slice.len(), 2);
        assert_eq!(slice.elem_at_shared(0).unwrap(), 2);
        assert_eq!(slice.elem_at_shared(1).unwrap(), 3);
        assert!(slice.elem_at_shared(2).is_err());
    }

    #[test]
    fn mutable_slice_descriptor_uses_short_array_access() {
        let mut values = [1_i64, 2, 3];
        let mut slice =
            unsafe { AnvSlice::from_raw_parts_mut(values.as_mut_ptr(), values.len(), 1, 2) };

        slice
            .with_elem_mut_short(0, |value| {
                *value = 5;
                Ok(())
            })
            .unwrap();

        assert_eq!(values, [1, 5, 3]);
    }

    #[test]
    fn shared_mutable_slice_descriptor_is_readonly() {
        let mut values = [1_i64];
        let slice =
            unsafe { AnvSlice::from_raw_parts_mut(values.as_mut_ptr(), values.len(), 0, 1) };
        let mut shared = slice.share();

        assert!(shared.with_elem_mut_short(0, |_| Ok(())).is_err());
        assert_eq!(values, [1]);
    }

    #[test]
    fn list_slice_descriptor_owns_shape_loan() {
        let mut list = AnvList::from_elems([1_i64, 2, 3]);
        let guard = list.begin_shape_loan().unwrap();
        let slice = unsafe { AnvSlice::from_list(std::ptr::from_ref(&list), 1, 2, guard) };
        assert_eq!(slice.elem_at_shared(0).unwrap(), 2);

        assert!(list.push(4).is_err());
        drop(slice);
        list.push(4).unwrap();
    }

    #[test]
    fn shared_list_slice_descriptor_extends_shape_loan() {
        let mut list = AnvList::from_elems([1_i64, 2, 3]);
        let guard = list.begin_shape_loan().unwrap();
        let slice = unsafe { AnvSlice::from_list(std::ptr::from_ref(&list), 0, 2, guard) };
        let shared = slice.share();

        drop(slice);
        assert!(list.push(4).is_err());
        drop(shared);
        list.push(4).unwrap();
    }

    #[test]
    fn mutable_list_slice_descriptor_uses_short_list_access() {
        let mut list = AnvList::from_elems([1_i64, 2, 3]);
        let guard = list.begin_shape_loan().unwrap();
        let mut slice =
            unsafe { AnvSlice::from_list_mut(std::ptr::from_mut(&mut list), 1, 2, guard) };

        slice
            .with_elem_mut_short(1, |value| {
                *value = 8;
                Ok(())
            })
            .unwrap();

        assert_eq!(list.as_slice(), &[1, 2, 8]);
    }

    #[test]
    fn slice_copy_range_copies_views() {
        let values = [1_i64, 2, 3, 4];
        let slice = unsafe { AnvSlice::from_raw_parts(values.as_ptr(), values.len(), 1, 3) };
        let copy = unsafe { slice.copy_range_with(1..3, |value| *value * 10) }.unwrap();
        assert_eq!(copy.as_slice(), &[30, 40]);

        let list = AnvList::from_elems(values);
        let guard = list.begin_shape_loan().unwrap();
        let slice = unsafe { AnvSlice::from_list(std::ptr::from_ref(&list), 1, 3, guard) };
        let copy = unsafe { slice.copy_range_with(0..2, |value| *value) }.unwrap();
        assert_eq!(copy.as_slice(), &[2, 3]);

        let slice = unsafe { AnvSlice::from_raw_parts(values.as_ptr(), values.len(), 1, 3) };
        let copy = unsafe { slice.slice(1, 2).copy_range_with(0..2, |value| *value) }.unwrap();
        assert_eq!(copy.as_slice(), &[3, 4]);
    }

    #[test]
    fn slice_copy_range_result_has_independent_loan_state() {
        let mut list = AnvList::from_elems([1_i64, 2, 3]);
        let guard = list.begin_shape_loan().unwrap();
        let slice = unsafe { AnvSlice::from_list(std::ptr::from_ref(&list), 0, 3, guard) };
        let mut copy = unsafe { slice.copy_range_with(0..2, |value| *value) }.unwrap();

        drop(slice);
        list.push(4).unwrap();
        copy.push(9).unwrap();

        assert_eq!(list.as_slice(), &[1, 2, 3, 4]);
        assert_eq!(copy.as_slice(), &[1, 2, 9]);
    }

    #[test]
    fn anv_list_default_is_empty() {
        let list = AnvList::<i64>::default();

        assert!(list.is_empty());
        assert_eq!(list.len_i64(), 0);
    }
}
