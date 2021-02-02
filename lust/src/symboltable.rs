use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// A symbol table. Can be used to resolve variables in the current
/// lexical scope.
pub struct SymbolTable<T> {
    /// A reference counted pointer to our parent. The Arc and Mutex
    /// are needed so that we guarentee that accessing our parent is
    /// thread safe and that our parent stays alive at least as long
    /// as us. If we are asked to perform a lookup and can't locate
    /// the value in our own tabe we fall back to the parent.
    parent: Option<Arc<Mutex<SymbolTable<T>>>>,
    /// A map between strings and whatever value the JIT is using to
    /// represent values internally.
    map: HashMap<String, T>,
}

impl<T> SymbolTable<T>
where
    T: Clone,
{
    /// Creates a new symbol table. In order to be used this will
    /// likely need to be wrapped in an Arc and Mutex, so something
    /// like: `Arc::new(Mutex::new(SymbolTable::new())) is fairly
    /// common.
    pub fn new() -> Self {
        Self {
            parent: None,
            map: HashMap::new(),
        }
    }

    /// Creates a new symbol table with parent PARENT. Failed lookups
    /// in the created symbol table will be routed to PARENT.
    pub fn new_with_parent(parent: &Arc<Mutex<SymbolTable<T>>>) -> Self {
        Self {
            parent: Some(parent.clone()),
            map: HashMap::new(),
        }
    }

    /// Locates LOOKUP in the current lexical scope. This involves
    /// asking the parent if LOOKUP is not in our table. Returns None
    /// of LOOKUP can not be found.
    pub fn resolve(&self, lookup: &String) -> Option<T> {
        match self.map.get(lookup) {
            None => match &self.parent {
                Some(ref p) => {
                    let parent = p.lock().unwrap();
                    parent.resolve(lookup)
                }
                None => None,
            },
            Some(t) => Some(t.clone()),
        }
    }

    /// Inserts a new value in to this symbol table.
    pub fn insert(&mut self, what: &String, val: T) -> Option<T> {
        self.map.insert(what.clone(), val)
    }
}
