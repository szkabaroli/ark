mod functions;
mod known;
mod modules;
mod packages;
mod source_files;
mod src;
mod visibility;

use std::collections::HashMap;
use std::{cell::RefCell, path::PathBuf};

use bytecode::Location;
use id_arena::Arena;
use parser::{compute_line_column, Span};

pub use self::functions::{emit_as_bytecode_operation, FctDefinition, FctDefinitionId, FctParent};
pub use self::known::KnownElements;
pub use self::modules::{module_path, ModuleDefinition, ModuleDefinitionId};
pub use self::packages::{PackageDefinition, PackageDefinitionId, PackageName};
pub use self::source_files::{SourceFile, SourceFileId};
pub use self::src::{
    AnalysisData, CallType, ContextFieldId, IdentType, InnerContextId, LazyContextData,
    NestedScopeId, NestedVarId, ScopeId, VarId, VarLocation, VarAccess, Var
};
pub use self::visibility::Visibility;

use crate::error::diag::Diagnostic;
use crate::error::msg::ErrorMessage;
use crate::interner::Interner;

pub struct SemaArgs {
    pub packages: Vec<(String, PathBuf)>,
    pub arg_file: Option<String>,
    pub test_file_as_string: Option<String>,
}

impl SemaArgs {
    pub fn for_test(input: &'static str) -> SemaArgs {
        SemaArgs {
            packages: Vec::new(),
            arg_file: None,
            test_file_as_string: Some(input.into()),
        }
    }
}

pub struct Sema {
    pub args: SemaArgs,
    pub interner: Interner,
    pub known: KnownElements,
    pub diag: RefCell<Diagnostic>,
    pub source_files: Arena<SourceFile>,
    pub modules: Arena<ModuleDefinition>, // stores all module definitions
    pub packages: Arena<PackageDefinition>, // stores all module definitions
    pub functions: Arena<FctDefinition>,  // stores all module definitions
    pub package_names: HashMap<String, PackageDefinitionId>,
    pub prelude_module_id: Option<ModuleDefinitionId>,
    pub program_module_id: Option<ModuleDefinitionId>,
    pub program_package_id: Option<PackageDefinitionId>,
    pub stdlib_module_id: Option<ModuleDefinitionId>,
    pub stdlib_package_id: Option<PackageDefinitionId>,
}

impl Sema {
    pub fn new(args: SemaArgs) -> Sema {
        Sema {
            args,
            interner: Interner::new(),
            known: KnownElements::new(),
            diag: RefCell::new(Diagnostic::new()),
            source_files: Arena::new(),
            functions: Arena::new(),
            modules: Arena::new(),
            packages: Arena::new(),
            package_names: HashMap::new(),
            prelude_module_id: None,
            program_module_id: None,
            program_package_id: None,
            stdlib_module_id: None,
            stdlib_package_id: None,
        }
    }

    pub fn compute_loc(&self, file_id: SourceFileId, span: Span) -> Location {
        let file = self.file(file_id);
        let (line, column) = compute_line_column(&file.line_starts, span.start());
        Location::new(line, column)
    }

    pub fn file(&self, id: SourceFileId) -> &SourceFile {
        &self.source_files[id]
    }

    pub fn module(&self, id: ModuleDefinitionId) -> &ModuleDefinition {
        &self.modules[id]
    }

    pub fn function(&self, id: FctDefinitionId) -> &FctDefinition {
        &self.functions[id]
    }

    pub fn prelude_module_id(&self) -> ModuleDefinitionId {
        self.prelude_module_id.expect("uninitialized module id")
    }

    pub fn program_module_id(&self) -> ModuleDefinitionId {
        self.program_module_id.expect("uninitialized module id")
    }

    pub fn stdlib_module_id(&self) -> ModuleDefinitionId {
        self.stdlib_module_id.expect("uninitialized module id")
    }

    pub fn set_prelude_module_id(&mut self, module_id: ModuleDefinitionId) {
        assert!(self.prelude_module_id.is_none());
        self.prelude_module_id = Some(module_id);
    }

    pub fn set_program_module_id(&mut self, module_id: ModuleDefinitionId) {
        assert!(self.program_module_id.is_none());
        self.program_module_id = Some(module_id);
    }

    pub fn set_program_package_id(&mut self, package_id: PackageDefinitionId) {
        assert!(self.program_package_id.is_none());
        self.program_package_id = Some(package_id);
    }

    pub fn set_stdlib_module_id(&mut self, module_id: ModuleDefinitionId) {
        assert!(self.stdlib_module_id.is_none());
        self.stdlib_module_id = Some(module_id);
    }

    pub fn set_stdlib_package_id(&mut self, package_id: PackageDefinitionId) {
        assert!(self.stdlib_package_id.is_none());
        self.stdlib_package_id = Some(package_id);
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
