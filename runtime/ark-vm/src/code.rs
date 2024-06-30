use std::{collections::HashMap, fmt};

use bincode::{Decode, Encode};

use crate::{
    builder::{Builder, EncodedInstruction},
    operand::Operand
};

mod debug;
mod tests;

/// A structure containing runnable or dumpable code.
///
/// See the module-level docs for more details.
#[derive(Encode, Decode)]
pub struct Code {
    pub code: Vec<EncodedInstruction>,
    pub data: Vec<Operand>,
    pub labels: Vec<(usize, usize)>, // addr, ip
    pub debug_labels: HashMap<usize, String>,
}

impl Code {
    /// Create an empty code.
    ///
    /// Not useful for anything except tests and documentation.
    pub fn empty() -> Code {
        Code {
            code: vec![],
            data: vec![],
            labels: vec![],
            debug_labels: HashMap::new(),
        }
    }

    /// Retrieve a list of labels used in the program.
    ///
    /// Returns a list of tuples containing the IP of the label and the name of
    /// the label.
    pub fn labels(&self) -> &[(usize, usize)] {
        self.labels.as_slice()
    }

    /// Returns the IP for a given label.
    ///
    /// This function is used within the `Machine` to perform jumps.
    pub fn get_label_ip(&self, addr: usize) -> Option<usize> {
        for label in self.labels.as_slice() {
            if label.1 == addr {
                return Some(label.0);
            }
        }
        None
    }
}

impl<'a> From<Builder> for Code {
    /// Convert a `Builder` into `Code`.
    ///
    /// This function consumes the builder and returns a `Code`.
    fn from(builder: Builder) -> Code {
        let code = builder.instructions;
        let mut data = vec![];

        for d in builder.data.iter() {
            match d {
                Operand::LabelPlaceholder(label) => data.push(Operand::Int(builder.labels_rev[label])),
                op => data.push(*op),
            }
        }

        let mut labels = Vec::new();
        let mut debug_labels = HashMap::new();

        let mut idx: usize = 0;
        for (ip, label_idx) in builder.labels.iter() {
            let label_name = builder.named_labels.get(*label_idx).unwrap();

            labels.push((idx, *ip));
            debug_labels.insert(idx, label_name.clone());
            idx += 1;
        }

        labels.sort_by(|lhs, rhs| lhs.0.cmp(&rhs.0));

        Code {
            code,
            data,
            labels,
            debug_labels,
        }
    }
}
