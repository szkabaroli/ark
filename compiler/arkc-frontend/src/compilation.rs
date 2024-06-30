mod modules;

use std::{cell::{OnceCell, RefCell}, collections::HashMap, sync::Arc};

use arkc_hir::hir;
use id_arena::Arena;
pub use modules::{Module, ModuleId, ModuleName};
use parser::{ast, SourceFile, SourceFileId};

use crate::known::KnownElements;

#[derive(Debug)]
pub struct Compilation {
    pub ast: OnceCell<Vec<Arc<ast::File>>>,
    pub hir: RefCell<Vec<hir::File>>,
    pub module_names: HashMap<String, ModuleId>,
    pub program_module_id: Option<ModuleId>,
    pub source_files: Arena<SourceFile>,
    pub modules: Arena<Module>,
    pub known: KnownElements,
}

impl Compilation {
    pub fn new() -> Self {
        Compilation {
            source_files: Arena::new(),
            modules: Arena::new(),
            module_names: HashMap::new(),
            program_module_id: None,
            ast: OnceCell::new(),
            hir: RefCell::new(vec![]),
            known: KnownElements::new(),
        }
    }

    pub fn file(&self, id: SourceFileId) -> &SourceFile {
        &self.source_files[id]
    }

    pub fn module(&self, id: ModuleId) -> &Module {
        &self.modules[id]
    }

    pub fn module_mut(&mut self, id: ModuleId) -> &mut Module {
        &mut self.modules[id]
    }

    pub fn program_module_id(&self) -> ModuleId {
        self.program_module_id.expect("uninitialized module id")
    }

    pub fn set_program_module_id(&mut self, module_id: ModuleId) {
        assert!(self.program_module_id.is_none());
        self.program_module_id = Some(module_id);
    }
}
