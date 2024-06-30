use std::collections::HashMap;

use crate::table::Table;

/// A table which does not allow values to be overwritten.
#[derive(Debug, Default)]
pub struct WriteOnceTable<T>(HashMap<String, T>);

impl<T> WriteOnceTable<T> {
    /// Return a new, empty `WriteOnceTable`.
    pub fn new() -> WriteOnceTable<T> {
        WriteOnceTable(HashMap::new())
    }

    fn already_exists_guard(&self, name: &String) {
        if self.0.contains_key(name) {
            panic!("Error: redefining constant {} not allowed.", name);
        }
    }

    pub fn keys(&self) -> Vec<String> {
        let mut result = vec![];
        self.0.keys().for_each(|ref k| result.push(k.to_string()));
        result
    }
}

impl<T> Table for WriteOnceTable<T> {
    type Item = T;
    type Key = String;

    fn insert(&mut self, name: String, value: T) {
        self.already_exists_guard(&name);
        let name = String::from(name);
        self.0.insert(name, value);
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn contains_key(&self, name: String) -> bool {
        self.0.contains_key(&name)
    }

    fn get(&self, name: String) -> Option<&T> {
        self.0.get(&name)
    }
}