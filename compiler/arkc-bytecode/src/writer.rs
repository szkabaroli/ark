pub struct BytecodeWriter {
    code: Vec<u8>,
}

impl BytecodeWriter {
    pub fn new() -> BytecodeWriter {
        BytecodeWriter { code: Vec::new() }
    }
}
