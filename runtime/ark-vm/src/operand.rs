use bincode::{Decode, Encode};

#[derive(Encode, Decode, Clone, Copy, Debug, PartialEq)]
pub enum Operand {
    Int(usize),
    Int64(i64),
    UInt16(u16),
    Ptr(usize),
    LabelPlaceholder(usize)
}

impl Operand {
    pub fn to_int(&self) -> usize {
        match self {
            Operand::Int(val) => *val,
            _ => panic!("operand is not Int but {:?}", self)
        }
    }

    pub fn to_int64(&self) -> i64 {
        match self {
            Operand::Int64(val) => *val,
            _ => panic!("operand is not Int64 but {:?}", self)
        }
    }

    pub fn to_uint16(&self) -> u16 {
        match self {
            Operand::UInt16(val) => *val,
            _ => panic!("operand is not UInt16 but {:?}", self)
        }
    }
}
