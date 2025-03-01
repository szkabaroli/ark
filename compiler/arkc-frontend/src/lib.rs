pub mod compilation;
pub mod sema;

mod error;
mod fndefcheck;
mod interner;
mod known;
mod program_parser;
mod readty;
mod resolve;
mod stdlib_lookup;
mod strudefcheck;
mod sym;
mod typecheck;

use arkc_ast_lowering::lower_file;
use arkc_hir::hir;
use error::msg::ErrorMessage;
use program_parser::ProgramParser;
use resolve::SymbolResolver;
use std::rc::Rc;
use sym::{Symbol, SymbolKind};
use typecheck::TypecheckingContext;

pub use interner::Name;
pub use readty::{check_type, replace_type};
pub use sema::{Sema, SemaArgs};

macro_rules! return_on_error {
    ($sa: ident) => {{
        if $sa.diag.borrow().has_errors() {
            return false;
        }
    }};
}

pub const CONFIG: bincode::config::Configuration = bincode::config::standard();

pub fn check_program(sa: &mut Sema) -> bool {
    // This phase loads and parses all files.

    let ast = {
        let mut parser = ProgramParser::new(sa);
        parser.parse_all()
    };

    return_on_error!(sa);

    let hir: Vec<hir::File> = ast.iter().map(|file| lower_file(file)).collect();
    sa.compilation.ast.set(ast).unwrap();

    let module_id = sa.compilation.program_module_id();

    {
        let mut resolver = SymbolResolver::new(sa);
        let module_symtables = resolver.scan_file(module_id, &hir[0]);

        for (module_id, table) in module_symtables {
            assert!(sa
                .module(module_id)
                .table
                .set(Rc::new(table.clone()))
                .is_ok());
        }
    }

    {
        *sa.compilation.hir.borrow_mut() = hir;
    }

    // Define internal types.
    stdlib_lookup::lookup_known_fundamental_types(sa);

    strudefcheck::check(sa);
    fndefcheck::check(sa);
    return_on_error!(sa);

    let mut typecheck = TypecheckingContext::new(sa, module_id);
    typecheck.check_file();

    //let mut f = std::fs::File::create("./test.arkmodule").unwrap();
    //let _ = encode_into_std_write(sa.compilation.hir.take(), &mut f, CONFIG);

    /*for (module_id, table) in discoverer.module_symtables {
        assert!(sa
            .compilation
            .module(module_id)
            .table
            .set(Rc::new(table))
            .is_ok());
    }*/

    true
}

pub fn report_sym_shadow_span(sa: &Sema, name: Name, sym: Symbol /*span: Span*/) {
    let name = sa.interner.str(name).to_string();

    let msg = match sym.kind() {
        //SymbolKind::Class(_) => ErrorMessage::ShadowClass(name),
        SymbolKind::Struct(_) => ErrorMessage::ShadowStruct(name),
        //SymbolKind::Trait(_) => ErrorMessage::ShadowTrait(name),
        //SymbolKind::Enum(_) => ErrorMessage::ShadowEnum(name),
        SymbolKind::FnDecl(_) => ErrorMessage::ShadowFunction(name),
        //SymbolKind::Global(_) => ErrorMessage::ShadowGlobal(name),
        //SymbolKind::Const(_) => ErrorMessage::ShadowConst(name),
        //SymbolKind::Var(_) => ErrorMessage::ShadowParam(name),
        SymbolKind::Module(_) => ErrorMessage::ShadowModule(name),
        //SymbolKind::TypeParam(_) => ErrorMessage::ShadowTypeParam(name),
        _ => unreachable!(),
    };

    panic!("{:?}", msg);
    //sa.report(span, msg);
}

fn function_pattern_match(name: &str, pattern: &str) -> bool {
    if pattern == "all" {
        return true;
    }

    for part in pattern.split(',') {
        if name.contains(part) {
            return true;
        }
    }

    false
}

pub fn emit_ast(sa: &Sema, filter: &str) {
    //for (_, function) in sa.compilation.functions.iter() {
    //let function_name = function.display_name(sa);

    //if function_pattern_match(&function_name, filter) {
    //parser::ast::dump::dump_function(&function.ast);
    //}
    //}
}
