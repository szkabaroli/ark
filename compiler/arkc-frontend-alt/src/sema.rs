use std::{cell::RefCell, collections::HashMap, path::PathBuf};

use id_arena::Arena;
use parser::Span;

use crate::{
    error::{diag::Diagnostic, msg::ErrorMessage},
    hir::{
        FnDefinition, FnDefinitionId, ModuleDefinition, ModuleDefinitionId, PackageDefinition,
        PackageDefinitionId, SourceFile, SourceFileId,
    },
    interner::Interner,
    sym::ModuleSymTable,
};

pub struct SemaArgs {
    pub packages: Vec<(String, Vec<PathBuf>)>,
    pub arg_file: Option<String>,
}

pub struct Sema {
    pub args: SemaArgs,
    pub interner: Interner,
    pub diag: RefCell<Diagnostic>,
    pub source_files: Arena<SourceFile>,
    pub modules: Arena<ModuleDefinition>, // stores all module definitions
    pub packages: Arena<PackageDefinition>, // stores all module definitions
    pub functions: Arena<FnDefinition>,
    pub package_names: HashMap<String, PackageDefinitionId>,
    pub program_module_id: Option<ModuleDefinitionId>,
    pub program_package_id: Option<PackageDefinitionId>,
}

pub fn fn_by_name(sa: &Sema, name: &str) -> FnDefinitionId {
    let name = sa.interner.intern(name);
    ModuleSymTable::new(sa, sa.program_module_id())
        .get(name)
        .expect("symbol not found")
        .to_fn()
        .expect("function expected")
}

impl Sema {
    pub fn new(args: SemaArgs) -> Sema {
        Sema {
            args,
            interner: Interner::new(),
            diag: RefCell::new(Diagnostic::new()),
            source_files: Arena::new(),
            modules: Arena::new(),
            packages: Arena::new(),
            functions: Arena::new(),
            package_names: HashMap::new(),
            program_module_id: None,
            program_package_id: None,
        }
    }

    pub fn file(&self, id: SourceFileId) -> &SourceFile {
        &self.source_files[id]
    }

    pub fn module(&self, id: ModuleDefinitionId) -> &ModuleDefinition {
        &self.modules[id]
    }

    pub fn program_module_id(&self) -> ModuleDefinitionId {
        self.program_module_id.expect("uninitialized module id")
    }

    pub fn set_program_module_id(&mut self, module_id: ModuleDefinitionId) {
        assert!(self.program_module_id.is_none());
        self.program_module_id = Some(module_id);
    }

    pub fn set_program_package_id(&mut self, package_id: PackageDefinitionId) {
        assert!(self.program_package_id.is_none());
        self.program_package_id = Some(package_id);
    }

    pub fn report(&self, file: SourceFileId, span: Span, msg: ErrorMessage) {
        self.diag.borrow_mut().report(file, span, msg);
    }

    pub fn report_without_location(&self, msg: ErrorMessage) {
        self.diag.borrow_mut().report_without_location(msg);
    }

    pub fn warn(&self, file: SourceFileId, span: Span, msg: ErrorMessage) {
        self.diag.borrow_mut().warn(file, span, msg);
    }
}
