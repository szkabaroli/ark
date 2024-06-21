pub mod sema;
pub mod hir;
mod typeck;
mod program_parser;
mod error;
mod sym;
mod interner;

use std::rc::Rc;

use error::msg::ErrorMessage;
use hir::SourceFileId;
pub use interner::Name;
use parser::Span;
pub use sym::{SymTable, Symbol, SymbolKind};
pub use sema::{Sema, SemaArgs};

pub fn check_program(sa: &mut Sema) -> bool {
    // This phase loads and parses all files. Also creates top-level-elements.
    let module_symtables = program_parser::parse(sa);

    println!("{:#?}", module_symtables);

    for (module_id, table) in module_symtables {
        assert!(sa.module(module_id).table.set(Rc::new(table)).is_ok());
    }


    println!("{:?}", sa.program_module_id());
    println!("{:?}", sa.interner);
    
    true
}

pub fn report_sym_shadow_span(sa: &Sema, name: Name, file: SourceFileId, span: Span, sym: Symbol) {
    let name = sa.interner.str(name).to_string();

    let msg = match sym.kind() {
        //SymbolKind::Class(_) => ErrorMessage::ShadowClass(name),
        //SymbolKind::Struct(_) => ErrorMessage::ShadowStruct(name),
        //SymbolKind::Trait(_) => ErrorMessage::ShadowTrait(name),
        //SymbolKind::Enum(_) => ErrorMessage::ShadowEnum(name),
        //SymbolKind::Global(_) => ErrorMessage::ShadowGlobal(name),
        //SymbolKind::Const(_) => ErrorMessage::ShadowConst(name),
        //SymbolKind::Var(_) => ErrorMessage::ShadowParam(name),
        SymbolKind::Module(_) => ErrorMessage::ShadowModule(name),
        //SymbolKind::TypeParam(_) => ErrorMessage::ShadowTypeParam(name),
        _ => unreachable!(),
    };

    sa.report(file, span, msg);
}