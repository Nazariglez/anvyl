use std::{
    borrow::Borrow,
    fmt,
    ops::{Index, Range},
};

use ecow::{EcoString, EcoVec};

use crate::{Trace, TraceDriver, Visitor};

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

#[derive(Clone, PartialEq, Eq)]
pub struct AnvList<T> {
    elems: EcoVec<T>,
}

impl<T> Default for AnvList<T> {
    fn default() -> Self {
        Self {
            elems: EcoVec::new(),
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
}

impl<T: Clone> AnvList<T> {
    #[must_use]
    pub fn share(&self) -> Self {
        self.clone()
    }

    pub fn from_elems(elems: impl IntoIterator<Item = T>) -> Self {
        Self {
            elems: elems.into_iter().collect(),
        }
    }

    pub fn push(&mut self, elem: T) {
        self.elems.push(elem);
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

#[derive(Clone, PartialEq, Eq)]
pub struct AnvMap<K, V> {
    entries: EcoVec<(K, V)>,
}

impl<K, V> Default for AnvMap<K, V> {
    fn default() -> Self {
        Self {
            entries: EcoVec::new(),
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
}

impl<K: PartialEq, V> AnvMap<K, V> {
    pub fn get(&self, key: &K) -> Option<&V> {
        self.entries
            .iter()
            .find(|(entry_key, _)| entry_key == key)
            .map(|(_, value)| value)
    }

    pub fn entry_at(&self, index: usize) -> Option<(&K, &V)> {
        self.entries.get(index).map(|(key, value)| (key, value))
    }
}

impl<K: Clone + PartialEq, V: Clone> AnvMap<K, V> {
    #[must_use]
    pub fn share(&self) -> Self {
        self.clone()
    }

    pub fn from_entries(entries: impl IntoIterator<Item = (K, V)>) -> Self {
        let mut map = Self::default();
        for (key, value) in entries {
            map.insert(key, value);
        }
        map
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        if let Some(index) = self
            .entries
            .iter()
            .position(|(entry_key, _)| entry_key == &key)
        {
            return Some(std::mem::replace(
                &mut self.entries.make_mut()[index].1,
                value,
            ));
        }
        self.entries.push((key, value));
        None
    }

    pub fn remove(&mut self, key: &K) -> Option<V> {
        let index = self
            .entries
            .iter()
            .position(|(entry_key, _)| entry_key == key)?;
        Some(self.entries.remove(index).1)
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

#[cfg(test)]
mod tests {
    use super::{AnvList, AnvMap, AnvString};
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

        changed.push(3);

        assert_eq!(original.as_slice(), &[1, 2]);
        assert_eq!(changed.as_slice(), &[1, 2, 3]);
    }

    #[test]
    fn anv_list_preserves_value_semantics_after_aliasing() {
        let a = AnvList::from_elems([AnvString::from("a")]);
        let mut b = a.clone();

        b.push(AnvString::from("b"));

        assert_eq!(a.checked_index(0).as_str(), "a");
        assert_eq!(a.len(), 1);
        assert_eq!(b.checked_index(1).as_str(), "b");
    }

    #[test]
    fn anv_map_preserves_insertion_order_and_replaces() {
        let mut map = AnvMap::from_entries([("a", 1_i64), ("b", 2)]);

        assert_eq!(map.insert("a", 3), Some(1));
        assert_eq!(map.get(&"a"), Some(&3));
        assert_eq!(map.iter().collect::<Vec<_>>(), vec![(&"a", &3), (&"b", &2)]);
    }

    #[test]
    fn anv_map_remove_and_alias_detach() {
        let original = AnvMap::from_entries([(1_i64, AnvString::from("one"))]);
        let mut changed = original.share();

        changed.insert(2, AnvString::from("two"));
        assert!(original.get(&2).is_none());
        assert_eq!(
            changed.remove(&1).as_ref().map(AnvString::as_str),
            Some("one")
        );
        assert!(changed.get(&1).is_none());
    }

    #[test]
    fn anv_list_default_is_empty() {
        let list = AnvList::<i64>::default();

        assert!(list.is_empty());
        assert_eq!(list.len_i64(), 0);
    }
}
