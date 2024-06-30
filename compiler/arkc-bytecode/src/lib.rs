mod writer;

use writer::BytecodeWriter;

pub struct BytecodeBuilder {
    writer: BytecodeWriter,
}

impl BytecodeBuilder {
    pub fn new() -> BytecodeBuilder {
        BytecodeBuilder {
            writer: BytecodeWriter::new(),
        }
    }
}
