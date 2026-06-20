use std::{hash::Hash, marker::PhantomData};

use indexmap::IndexMap;

use crate::{Trace, TraceDriver, Visitor};

#[derive(Clone)]
pub struct ListStorage<'cx, T> {
    elems: Vec<T>,
    _brand: PhantomData<&'cx ()>,
}

impl<T> ListStorage<'_, T> {
    pub(crate) fn from_elems(elems: impl IntoIterator<Item = T>) -> Self {
        Self {
            elems: elems.into_iter().collect(),
            _brand: PhantomData,
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.elems.len()
    }

    pub(crate) fn get(&self, index: usize) -> Option<&T> {
        self.elems.get(index)
    }

    pub(crate) fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.elems.get_mut(index)
    }

    pub(crate) fn as_slice(&self) -> &[T] {
        &self.elems
    }

    pub(crate) fn push(&mut self, elem: T) {
        self.elems.push(elem);
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for ListStorage<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.elems.fmt(f)
    }
}

#[derive(Clone)]
pub struct MapStorage<'cx, K, V> {
    entries: IndexMap<K, V>,
    _brand: PhantomData<&'cx ()>,
}

impl<K: Eq + Hash, V> MapStorage<'_, K, V> {
    pub(crate) fn from_entries(entries: impl IntoIterator<Item = (K, V)>) -> Self {
        Self {
            entries: entries.into_iter().collect(),
            _brand: PhantomData,
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.entries.len()
    }

    pub(crate) fn get(&self, key: &K) -> Option<&V> {
        self.entries.get(key)
    }

    pub(crate) fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.entries.get_mut(key)
    }

    pub(crate) fn contains_key(&self, key: &K) -> bool {
        self.entries.get_index_of(key).is_some()
    }

    pub(crate) fn get_index(&self, index: usize) -> Option<(&K, &V)> {
        self.entries.get_index(index)
    }

    pub(crate) fn get_index_of(&self, key: &K) -> Option<usize> {
        self.entries.get_index_of(key)
    }

    pub(crate) fn get_index_mut(&mut self, index: usize) -> Option<(&K, &mut V)> {
        self.entries.get_index_mut(index)
    }

    pub(crate) fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.entries.insert(key, value)
    }

    pub(crate) fn shift_remove(&mut self, key: &K) -> Option<V> {
        self.entries.shift_remove(key)
    }
}

impl<K: std::fmt::Debug, V: std::fmt::Debug> std::fmt::Debug for MapStorage<'_, K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.entries.fmt(f)
    }
}

// SAFETY: list storage is the physical owner of each element and traces each element exactly once.
unsafe impl<'cx, T: Trace<'cx>> Trace<'cx> for ListStorage<'cx, T> {
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        self.elems.trace(visitor);
    }
}

// SAFETY: map storage is the physical owner of each key and value and traces each once.
unsafe impl<'cx, K: Trace<'cx>, V: Trace<'cx>> Trace<'cx> for MapStorage<'cx, K, V> {
    fn trace<D: TraceDriver<'cx>>(&self, visitor: &mut Visitor<'cx, '_, D>) {
        for (key, value) in &self.entries {
            key.trace(visitor);
            value.trace(visitor);
        }
    }
}
