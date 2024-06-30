use std::fmt;

use bincode::{Decode, Encode};
use lazy_static::lazy_static;

use crate::{
    builder::EncodedInstruction, code::Code, frame::Frame, instruction::OpCode, operand::Operand,
    stack::Stack, table::Table,
};

/// `VM` contains all the information needed to run your program.
///
/// * A `Code`, used describe the source instructions and data to execute.
/// * An instruction pointer, which points to the currently-executing
///   instruciton.
/// * A `Table` of constants, which you can use in your instructions if needed.
/// * A `Stack` of `Frame` used to keep track of calls being executed.
/// * A `Stack` of `T` which is used as the main operand stack.
pub struct VM<'a, T: fmt::Debug + Copy + Encode + Decode + 'static> {
    pub code: Code,
    pub ip: usize,
    pub constants: &'a dyn Table<Key = String, Item = T>,
    pub call_stack: Stack<Frame>,
    pub operand_stack: Stack<Operand>,
}

impl<'a, T: 'a + std::fmt::Debug + Copy + Encode + Decode> VM<'a, T> {
    /// Returns a new `Machine` ready to execute instructions.
    ///
    /// The machine is initialised by passing in your `Code` which contains
    /// all the code and data of your program, and a `Table` of constants`.
    pub fn new(code: Code, constants: &'a dyn Table<Key = String, Item = T>) -> VM<'a, T> {
        let frame: Frame = Frame::new(code.code.len());
        let mut call_stack = Stack::new();
        call_stack.push(frame);

        VM {
            code,
            ip: 0,
            constants,
            call_stack,
            operand_stack: Stack::new(),
        }
    }

    /// Run the machine.
    ///
    /// Kick off the process of running the program.
    ///
    /// Steps through the instructions in your program executing them
    /// one-by-one.  Each instruction function is executed, much like a
    /// callback.
    ///
    /// Stops when either the last instruction is executed or when the
    /// last frame is removed from the call stack.
    pub fn run(&mut self) {
        'hlt: loop {
            if self.ip == self.code.code.len() {
                break 'hlt;
            }

            let inst = self.next_instruction();
            let arity = inst.1;
            let data = inst.2;

            let mut args: Vec<usize> = vec![];

            for idx in 0..arity {
                args.push(data[idx]);
            }

            match inst.0 {
                OpCode::Load => {
                    let idx = self.code.data[args[0]].to_uint16();
                    let value = self.operand_pop();
                    self.set_local(idx, value)
                }
                OpCode::Store => {
                    let idx = self.code.data[args[0]].to_uint16();
                    let data = self.get_local(idx).unwrap();
                    self.operand_push(*data)
                }
                OpCode::Return => self.ret(),
                OpCode::Push => {
                    let arg = &self.code.data[args[0]];
                    self.operand_stack.push(*arg);
                }
                OpCode::Pop => {
                    self.operand_pop();
                }
                OpCode::Call => {
                    let label = self.get_data(args[0]).to_int();
                    self.call(label)
                }
                OpCode::CallRuntime => {
                    let fn_idx = &self.code.data[args[0]].to_uint16();

                    match *fn_idx {
                        0u16 => ark_rt::arkrt_print(),
                        0u16 => {
                            let obj = ark_rt::ark::alloc_object(std::ptr::null(), 0, 0);
                        },
                        _ => panic!("unknown runtime function called"),
                    }
                }
                OpCode::Jump => {
                    let label = self.get_data(args[0]).to_int();
                    self.jump(label);
                }
                OpCode::JumpIf => {
                    let condition = self.operand_pop().to_int();
                    if condition != 0 {
                        let label = self.get_data(args[0]).clone();
                        self.jump(label.to_int());
                    }
                }
                OpCode::Add => {
                    let lhs = self.operand_pop().to_int();
                    let rhs = self.operand_pop().to_int();
                    self.operand_push(Operand::Int(lhs + rhs))
                }
            }
        }
    }

    /// Retrieve the next instruction of the program and increment
    /// the instruction pointer.
    #[inline]
    fn next_instruction(&mut self) -> EncodedInstruction {
        let code = self.code.code[self.ip];
        self.ip += 1;
        code
    }

    /// Set a local variable in the current call frame.
    ///
    /// Places a value in the frame's local variable table.
    #[inline(always)]
    pub fn set_local(&mut self, idx: u16, value: Operand) {
        self.call_stack.peek_mut().set_local(idx, value)
    }

    /// Look up a local variable in the current call frame.
    ///
    /// Note that the variable may not be set in the current frame but it's up
    /// to your instruction to figure out how to deal with this situation.
    #[inline(always)]
    pub fn get_local(&self, idx: u16) -> Option<&Operand> {
        self.call_stack.peek().get_local(idx)
    }

    /// Look for a local variable in all call frames.
    ///
    /// The machine will look in each frame in the call stack starting at the
    /// top and moving down until it locates the local variable in question
    /// or runs out of stack frames.
    #[inline(always)]
    pub fn get_local_deep(&self, idx: u16) -> Option<&Operand> {
        for frame in self.call_stack.as_slice().iter().rev() {
            let local = frame.get_local(idx);
            if local.is_some() {
                return local;
            }
        }
        None
    }

    /// Push an operand onto the operand stack.
    #[inline(always)]
    pub fn operand_push(&mut self, value: Operand) {
        self.operand_stack.push(value);
    }

    /// Pop an operand off the operand stack.
    #[inline(always)]
    pub fn operand_pop(&mut self) -> Operand {
        self.operand_stack.pop()
    }

    /// Retrieve a reference to a `T` stored in the Code's data section.
    #[inline(always)]
    pub fn get_data(&self, id: usize) -> &Operand {
        self.code
            .data
            .get(id)
            .unwrap_or_else(|| panic!("Constant data is not present at index {}.", id))
    }

    /// Perform a jump to a named label.
    ///
    /// This method performs the following actions:
    /// * Retrieve the instruction pointer for a given label from the Code.
    /// * Set the machine's instruction pointer to the new location.
    ///
    /// This method will panic the thread if the label does not exist.
    #[inline(always)]
    pub fn jump(&mut self, label: usize) {
        self.ip = self
            .code
            .get_label_ip(label)
            .unwrap_or_else(|| panic!("Attempted to jump to unknown label {}", label));
    }

    /// Performs a call to a named label.
    ///
    /// This method is very similar to `jump` except that it records it's
    /// current instruction pointer and saves it in the call stack.
    ///
    /// This method performs the following actions:
    /// * Create a new frame with it's return address set to the current
    ///   instruction pointer.
    /// * Jump to the named label using `jump`.
    ///
    /// This method specifically does not transfer operands to call arguments.
    #[inline(always)]
    pub fn call(&mut self, label: usize) {
        self.call_stack.push(Frame::new(self.ip));
        self.jump(label);
    }

    /// Performs a return.
    ///
    /// This method pops the top frame off the call stack and moves the
    /// instruction pointer back to the frame's return address.
    /// It's up to you to push your return value onto the operand stack (if
    /// your language has such return semantics).
    ///
    /// The last call frame contains a return address at the end of the source
    /// code, so the machine will stop executing at the beginning of the next
    /// iteration.
    ///
    /// If you call `ret` too many times then the machine will panic when it
    /// attempts to pop the last frame off the stack.
    #[inline(always)]
    pub fn ret(&mut self) {
        let frame = self.call_stack.pop();
        self.ip = frame.return_address;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::builder::Builder;
    use crate::write_many_table::WriteManyTable;

    #[test]
    fn new() {
        let builder: Builder = Builder::new();
        let constants: WriteManyTable<String, usize> = WriteManyTable::new();

        let machine = VM::new(Code::from(builder), &constants);
        assert_eq!(machine.ip, 0);
        assert!(!machine.call_stack.is_empty());
        assert!(machine.operand_stack.is_empty());
    }

    #[test]
    fn run() {
        let mut builder: Builder = Builder::new();
        builder.push(Operand::Int(2));
        builder.push(Operand::Int(3));
        builder.add();

        let constants: WriteManyTable<String, usize> = WriteManyTable::new();
        let mut machine = VM::new(Code::from(builder), &constants);

        machine.run();

        let result = machine.operand_stack.pop();
        assert_eq!(result.to_int(), 5);
    }

    #[test]
    fn get_local() {
        let builder: Builder = Builder::new();
        let constants: WriteManyTable<String, usize> = WriteManyTable::new();
        let mut machine = VM::new(Code::from(builder), &constants);

        assert!(machine.get_local(0).is_none());
        machine.set_local(0, Operand::Int(13));
        assert!(machine.get_local(0).is_some());
    }
}
