use std::fmt::{Display, Error, Formatter};
use std::result::Result;

use crate::source_file::SourceFileId;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Span {
    pub file_id: SourceFileId,
    start: u32,
    len: u32,
}

impl Span {
    pub fn new(file_id: SourceFileId, start: u32, len: u32) -> Span {
        Span {
            file_id,
            start,
            len,
        }
    }

    pub fn at(file_id: SourceFileId, start: u32) -> Span {
        Span {
            file_id,
            start: start,
            len: 0,
        }
    }

    pub fn start(&self) -> u32 {
        self.start
    }

    pub fn len(&self) -> u32 {
        self.len
    }

    pub fn end(&self) -> u32 {
        self.start + self.len
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}-{}", self.start, self.end())
    }
}
