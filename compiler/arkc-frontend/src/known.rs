use arkc_hir::hir::HirId;

#[derive(Debug)]
pub struct KnownElements {
    //pub functions: KnownFunctions,
    //pub enums: KnownEnums,
    pub structs: KnownStructs,
}

impl KnownElements {
    pub fn new() -> KnownElements {
        KnownElements {
            //functions: KnownFunctions::new(),
            //enums: KnownEnums::new(),
            structs: KnownStructs::new(),
        }
    }
}

#[derive(Debug)]
pub struct KnownStructs {
    pub bool: Option<HirId>,
    //pub uint8: Option<HirId>,
    pub char: Option<HirId>,
    pub int32: Option<HirId>,
    pub int64: Option<HirId>,
    //pub float32: Option<HirId>,
    pub float64: Option<HirId>,
}

impl KnownStructs {
    pub fn new() -> KnownStructs {
        KnownStructs {
            bool: None,
            //uint8: None,
            char: None,
            int32: None,
            int64: None,
            //float32: None,
            float64: None,
        }
    }

    pub fn bool(&self) -> HirId {
        self.bool.expect("uninitialized")
    }

    pub fn int32(&self) -> HirId {
        self.int32.expect("uninitialized")
    }

    pub fn int64(&self) -> HirId {
        self.int64.expect("uninitialized")
    }

    pub fn float64(&self) -> HirId {
        self.float64.expect("uninitialized")
    }
}
