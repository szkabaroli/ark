use crate::sema::FctDefinitionId;

#[derive(Debug)]
pub struct KnownElements {
    pub functions: KnownFunctions,
}

impl KnownElements {
    pub fn new() -> KnownElements {
        KnownElements {
            functions: KnownFunctions::new(),
        }
    }
}

#[derive(Debug)]
pub struct KnownFunctions {
    pub assert: Option<FctDefinitionId>,
    pub compile: Option<FctDefinitionId>,
}

impl KnownFunctions {
    pub fn new() -> KnownFunctions {
        KnownFunctions {
            assert: None,
            compile: None,
        }
    }

    pub fn assert(&self) -> FctDefinitionId {
        self.assert.expect("uninitialized")
    }

    pub fn compile(&self) -> FctDefinitionId {
        self.compile.expect("uninitialized")
    }
}
