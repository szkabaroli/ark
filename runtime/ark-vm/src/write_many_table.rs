//! A key/value table using strings as keys.

use crate::table::Table;
use std::hash::Hash;
use std::collections::HashMap;

/// A table which allows values to be overwritten.
#[derive(Debug, Default)]
pub struct WriteManyTable<K, T>(HashMap<K, T>);

impl<K, T> WriteManyTable<K, T> {
    /// Return a new, empty `WriteManyTable`.
    pub fn new() -> WriteManyTable<K, T> {
        WriteManyTable(HashMap::new())
    }
}

impl<K, T> Table for WriteManyTable<K, T>
where
    K: Eq + Hash,
{
    type Item = T;
    type Key = K;

    fn insert(&mut self, key: K, value: T) {
        self.0.insert(key, value);
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn contains_key(&self, key: K) -> bool {
        self.0.contains_key(&key)
    }

    fn get(&self, key: K) -> Option<&T> {
        self.0.get(&key)
    }
}
