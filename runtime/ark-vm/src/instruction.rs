//! A virtual instruction.
//!
//! Instructions consist of an op code, a name, an arity and a function.
//!
//! Instruction::new(1, "jump", 1, jump);
//! ```

use bincode::{Decode, Encode};
use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd, Encode, Decode)]
#[repr(u8)]
pub enum OpCode {
    Return,
    Push,
    Load,
    Store,
    Add,
    Pop,
    Call,
    Jump,
    JumpIf,
    CallRuntime
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &OpCode::Return => write!(f, "ret"),
            &OpCode::Load => write!(f, "ldloc"),
            &OpCode::Store => write!(f, "stloc"),
            &OpCode::Push => write!(f, "push"),
            &OpCode::Add => write!(f, "add"),
            &OpCode::Pop => write!(f, "pop"),
            &OpCode::Call => write!(f, "call"),
            &OpCode::Jump => write!(f, "jmp"),
            &OpCode::JumpIf => write!(f, "jmpif"),
            &OpCode::CallRuntime => write!(f, "callrt"),
        }
    }
}

/// Describes a single instruction which can be used to execute programs.
///
/// Contains:
/// * An op code - a unique integer to identify this instruction.
/// * A name for serialisation and debugging reasons.
/// * An arity - the number of arguments this instruction expects to receive.
/// * A function which is used to execute the instruction.
pub struct Instruction {
    pub op_code: OpCode,
    pub name: String,
    pub arity: usize,
}

impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Instruction {{ op_code: {:?}, name: {}, arity: {} }}",
            self.op_code, self.name, self.arity
        )
    }
}

impl Instruction {
    /// Create a new instruction.
    pub fn new(op_code: OpCode, name: &str, arity: usize) -> Instruction {
        Instruction {
            op_code,
            name: String::from(name),
            arity,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, Clone, Copy, Decode, Encode)]
    struct Operand(i64);

    #[test]
    fn new() {
        let operand = Instruction::new(OpCode::Pop, "pop", 7);
        assert_eq!(operand.op_code, OpCode::Pop);
        assert_eq!(operand.name, "noop".to_string());
        assert_eq!(operand.arity, 7);
    }
}
