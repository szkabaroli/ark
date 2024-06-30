use crate::write_many_table::WriteManyTable;
use crate::table::Table;
use crate::Operand;

/// A call frame.
///
/// Contains:
/// * A `WriteManyTable` for storage of local variables.
/// * A return address - the instruction pointer for the machine to return to
///   when returning from this call.
#[derive(Debug)]
pub struct Frame {
    locals: WriteManyTable<u16, Operand>,
    pub return_address: usize,
}

impl Frame {
    /// Creates a new call frame with the specified return address.
    pub fn new(return_address: usize) -> Frame {
        Frame {
            locals: WriteManyTable::new(),
            return_address
        }
    }

    /// Return a reference to the specified local variable.
    pub fn get_local(&self, idx: u16) -> Option<&Operand> {
        self.locals.get(idx)
    }

    /// Set the value of a local variable.
    pub fn set_local(&mut self, idx: u16, value: Operand) {
        self.locals.insert(idx, value);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn new_has_locals() {
        let frame: Frame = Frame::new(0);
        assert!(frame.locals.is_empty())
    }
}