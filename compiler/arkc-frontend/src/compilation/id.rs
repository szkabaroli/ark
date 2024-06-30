use std::fmt;

#[derive(PartialEq, Eq, Copy, Clone, Debug, Hash)]
pub struct HirId(pub usize);

impl fmt::Display for HirId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}