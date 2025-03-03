use std::{cell::RefCell, path::PathBuf};

use arkc_hir::{hir::HirId, ty};
use parser::Span;

use crate::{
    compilation::{Compilation, Module, ModuleId},
    error::{diag::Diagnostic, msg::ErrorMessage},
    interner::Interner,
    operator_registry::OperatorRegistry,
};

pub struct SemaArgs {
    pub module_name: Option<String>,
    pub files: Option<Vec<PathBuf>>,
}

pub struct Sema {
    pub args: SemaArgs,
    pub interner: Interner,
    pub operators: OperatorRegistry,
    pub diag: RefCell<Diagnostic>,
    pub compilation: Compilation,
}

/*pub fn fn_by_name(sa: &Sema, name: &str) -> FnDefinitionId {
    let name = sa.interner.intern(name);
    ModuleSymTable::new(sa, sa.program_module_id())
        .get(name)
        .expect("symbol not found")
        .to_fn()
        .expect("function expected")
}*/

impl Sema {
    pub fn new(args: SemaArgs) -> Sema {
        Sema {
            args,
            interner: Interner::new(),
            operators: OperatorRegistry::new(),
            diag: RefCell::new(Diagnostic::new()),
            compilation: Compilation::new(),
        }
    }

    pub fn module(&self, id: ModuleId) -> &Module {
        self.compilation.module(id)
    }

    pub fn compilation(&self) -> &Compilation {
        &self.compilation
    }

    pub fn report(&self, span: Span, msg: ErrorMessage) {
        self.diag.borrow_mut().report(span, msg);
    }

    pub fn report_without_location(&self, msg: ErrorMessage) {
        self.diag.borrow_mut().report_without_location(msg);
    }

    pub fn warn(&self, span: Span, msg: ErrorMessage) {
        self.diag.borrow_mut().warn(span, msg);
    }
}
