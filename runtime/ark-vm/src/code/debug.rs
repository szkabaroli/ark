use crate::{instruction::OpCode, Operand};

use super::Code;
use std::fmt;

impl fmt::Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut result = String::new();

        // Write out constant data into the header.
        for i in 0..self.data.len() {
            match self.data[i] {
                Operand::Int(int) => {
                    result.push_str(&format!("@{} = int {}\n", i, int));
                }
                Operand::Int64(int) => {
                    result.push_str(&format!("@{} = int64 {}\n", i, int));
                }
                Operand::UInt16(val) => {
                    result.push_str(&format!("@{} = u16 {}\n", i, val));
                }
                Operand::Ptr(val) => {
                    result.push_str(&format!("@{} = ptr\n", i));
                }
                Operand::LabelPlaceholder(lbl) => {
                    result.push_str(&format!("@{} = label {}\n", i, lbl));
                }
            }
        }

        // Loop through the code and print out useful stuff.
        let mut ip = 0;
        let len = self.code.len();
        loop {
            // If this IP has a label, then print it out.
            for (label_idx, label_ip) in self.labels() {
                if ip == *label_ip {
                    result.push_str(&format!(
                        "\n.{}.{}:\n",
                        self.debug_labels.get(&label_idx).unwrap(),
                        label_ip
                    ));
                    break;
                }
            }

            if ip == len {
                break;
            }

            let inst = self.code[ip];
            ip += 1;

            let op_code = inst.0;
            let arity = inst.1;

            // Print this instruction's name
            result.push_str(&format!("\t{}", op_code));

            let args = inst.2;
            for idx in 0..arity {
                match op_code {
                    OpCode::Jump | OpCode::Call | OpCode::JumpIf => {
                        let const_idx = args[idx];
                        let data = self.data[const_idx].to_int();
                        let (idx, _) = &self
                            .labels
                            .iter()
                            .find(|(_, ip)| *ip == data)
                            .unwrap();
                        let debug_label = &self.debug_labels[idx];
                        result.push_str(&format!(" .{}", debug_label));
                    }
                    _ => {
                        let const_idx = args[idx];
                        let data = self.data[const_idx];
                        result.push_str(&format!(" {:?}", data));
                    }
                }
            }

            result.push_str("\n");
        }

        write!(f, "{}", result)
    }
}
