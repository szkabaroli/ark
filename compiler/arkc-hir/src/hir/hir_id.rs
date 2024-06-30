use serde::{Serialize, Deserialize};
use std::fmt;

#[derive(Clone, Copy, Eq, Hash, PartialEq, PartialOrd, Ord, Deserialize, Serialize)]
pub struct HirId(pub u32);

impl fmt::Debug for HirId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "HirId({})", self.0)
    }
}

#[derive(Clone, Copy, Eq, Hash, PartialEq, PartialOrd, Ord, Deserialize, Serialize)]
pub struct FnBodyId(pub u32);

impl fmt::Debug for FnBodyId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "FnBodyId({})", self.0)
    }
}