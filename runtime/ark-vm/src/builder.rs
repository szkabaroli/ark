use std::{collections::HashMap, fmt};

use bincode::{Decode, Encode};

use crate::{instruction::OpCode, operand::Operand};

#[derive(Clone, Copy, Debug, PartialEq, Encode, Decode)]
pub struct EncodedInstruction(pub OpCode, pub usize, pub [usize; 2]);

#[derive(Clone, Debug, PartialEq)]
pub struct RawInstruction(OpCode, usize, Vec<Operand>);

/// The builder struct.
///
/// Contains:
/// * an `InstructionTable`.
/// * a list of instructions that have been pushed into this builder.
/// * a `Table` of labels used for jumping.
/// * a list of `T` to be stored in the builder's data section.
#[derive(Debug)]
pub struct Builder {
    pub instructions: Vec<EncodedInstruction>,
    pub named_labels: Vec<String>,
    pub labels: HashMap<usize, usize>,
    pub labels_rev: HashMap<usize, usize>,
    pub data: Vec<Operand>,
}

impl Builder {
    /// Create a new `Builder` from an `InstructionTable`.
    pub fn new() -> Builder {
        let mut named_labels = Vec::new();
        let mut labels = HashMap::new();
        let mut labels_rev = HashMap::new();

        named_labels.push("main".to_owned());
        labels.insert(0, 0);
        labels_rev.insert(0, 0);

        Builder {
            instructions: vec![],
            labels,
            labels_rev,
            named_labels,
            data: vec![],
        }
    }

    /// Store a label in the builder that can be used in a later label call.
    pub fn create_named_label(&mut self, name: &str) -> usize {
        let label_idx = self.named_labels.len();
        self.named_labels.push(name.to_string());
        label_idx
    }

    /// Insert a label at this point in the code.
    ///
    /// Labels are used as targets for jumps.  When you call this method a
    /// label is stored which points to the position of the next instruction.
    pub fn label(&mut self, label_idx: usize) {
        let ip = self.instructions.len();
        self.labels.insert(ip, label_idx);
        self.labels_rev.insert(label_idx, ip);
    }

    /// Push an instruction into the code.
    fn push_op(&mut self, mut raw_inst: RawInstruction) {
        if raw_inst.2.len() != raw_inst.1 {
            panic!(
                "Instruction {:?} has arity of {}, but you provided {} arguments.",
                raw_inst.0,
                raw_inst.1,
                raw_inst.2.len()
            )
        }

        let mut inst = EncodedInstruction(raw_inst.0, raw_inst.1, [0, 0]);

        for idx in 0..raw_inst.2.len() {
            let const_pos = self.push_data(raw_inst.2.remove(idx));
            inst.2[idx] = const_pos;
        }

        self.instructions.push(inst);
    }

    /// Push a return instruction into the code.
    pub fn ret(&mut self) {
        self.push_op(RawInstruction(OpCode::Return, 0, vec![]))
    }

    /// Push a push instruction into the code.
    pub fn push(&mut self, operand: Operand) {
        self.push_op(RawInstruction(OpCode::Push, 1, vec![operand]))
    }

    /// Push a push instruction into the code.
    pub fn load(&mut self, idx: Operand) {
        self.push_op(RawInstruction(OpCode::Load, 1, vec![idx]))
    }

    /// Push a push instruction into the code.
    pub fn store(&mut self, idx: Operand) {
        self.push_op(RawInstruction(OpCode::Store, 1, vec![idx]))
    }

    /// Push an add instruction into the code.
    pub fn add(&mut self) {
        self.push_op(RawInstruction(OpCode::Add, 0, vec![]))
    }

    /// Push an pop instruction into the code.
    pub fn pop(&mut self) {
        self.push_op(RawInstruction(OpCode::Pop, 0, vec![]))
    }

    /// Push an jump instruction into the code.
    pub fn jump(&mut self, label: usize) {
        self.push_op(RawInstruction(
            OpCode::Jump,
            1,
            vec![Operand::LabelPlaceholder(label)],
        ))
    }

    /// Push an jump_if instruction into the code.
    pub fn jump_if(&mut self, label: usize) {
        self.push_op(RawInstruction(
            OpCode::JumpIf,
            1,
            vec![Operand::LabelPlaceholder(label)],
        ))
    }

    fn push_data(&mut self, data: Operand) -> usize {
        let pos = self.data.iter().position(|d| d == &data);
        match pos {
            Some(pos) => pos,
            None => {
                self.data.push(data);
                self.data.len() - 1
            }
        }
    }
}
