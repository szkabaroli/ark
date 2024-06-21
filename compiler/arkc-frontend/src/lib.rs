use error::msg::ErrorMessage;
use parser::{ast, Span};

pub use interner::Name;
pub use sema::{Sema, SourceFileId};
pub use sym::{SymTable, Symbol, SymbolKind};
pub use ty::{SourceType, SourceTypeArray};

pub mod return_check;
pub mod sema;
pub mod specialize;
pub mod sym;
pub mod test;
pub mod ty;
pub mod typecheck;
//pub mod hir;
//pub mod hir_id;

mod error;
mod fctdef_check;
mod import_check;
mod interner;
mod program_parser;
mod readty;
mod stdlib_lookup;

pub const STDLIB: &[(&str, &str)] = &include!(concat!(env!("OUT_DIR"), "/dora_stdlib_bundle.rs"));

macro_rules! return_on_error {
    ($vm: ident) => {{
        if $vm.diag.borrow().has_errors() {
            return false;
        }
    }};
}

pub fn check_program(sa: &mut Sema) -> bool {
    // This phase loads and parses all files. Also creates top-level-elements.
    let module_symtables = program_parser::parse(sa);

    // Discover all imported types.
    import_check::check(sa, module_symtables);
    return_on_error!(sa);

    // Fill prelude with important types and functions.
    stdlib_lookup::setup_prelude(sa);

    // Define internal types.
    //stdlib_lookup::lookup_known_fundamental_types(sa);

    // Now all types are known and we can start parsing types/type bounds.
    //typedefck::parse_type_params(sa);
    // Find all trait implementations for types.
    //impldefck::check_definition(sa);
    // Check types/type bounds for type params.
    //typedefck::check_type_bounds(sa);
    //return_on_error!(sa);

    // Checks class/struct/trait/enum definitions.
    //aliasck::check(sa);
    //clsdefck::check(sa);
    //structdefck::check(sa);
    //traitdefck::check(sa);
    //enumck::check(sa);
    //impldefck::check_type_aliases(sa);
    //return_on_error!(sa);

    //globaldefck::check(sa);
    //constdefck::check(sa);
    //extensiondefck::check(sa);
    //return_on_error!(sa);

    // Check type definitions of params and return types in functions.
    fctdef_check::check(sa);
    return_on_error!(sa);

    // Check impl methods against trait definition.
    //impldefck::check_definition_against_trait(sa);
    //return_on_error!(sa);

    // Define internal functions & methods.
    //stdlib_lookup::resolve_internal_functions(sa);
    //stdlib_lookup::lookup_known_methods(sa);
    //stdlib_lookup::create_lambda_class(sa);

    // Check for internal functions, methods or types.
    //internalck(sa);
    //return_on_error!(sa);

    // Check function body.
    typecheck::check(sa);
    return_on_error!(sa);

    true
}

pub fn report_sym_shadow_span(sa: &Sema, name: Name, file: SourceFileId, span: Span, sym: Symbol) {
    let name = sa.interner.str(name).to_string();

    let msg = match sym.kind() {
        //SymbolKind::Class(_) => ErrorMessage::ShadowClass(name),
        //SymbolKind::Struct(_) => ErrorMessage::ShadowStruct(name),
        //SymbolKind::Trait(_) => ErrorMessage::ShadowTrait(name),
        //SymbolKind::Enum(_) => ErrorMessage::ShadowEnum(name),
        SymbolKind::Fct(_) => ErrorMessage::ShadowFunction(name),
        //SymbolKind::Global(_) => ErrorMessage::ShadowGlobal(name),
        //SymbolKind::Const(_) => ErrorMessage::ShadowConst(name),
        //SymbolKind::Var(_) => ErrorMessage::ShadowParam(name),
        SymbolKind::Module(_) => ErrorMessage::ShadowModule(name),
        //SymbolKind::TypeParam(_) => ErrorMessage::ShadowTypeParam(name),
        _ => unreachable!(),
    };

    sa.report(file, span, msg);
}

pub fn always_returns(s: &ast::StmtData) -> bool {
    return_check::returns_value(s).is_ok()
}

pub fn expr_always_returns(e: &ast::ExprKind) -> bool {
    todo!()
    //returnck::expr_returns_value(e).is_ok()
}

pub fn expr_block_always_returns(e: &ast::ExprBlockType) -> bool {
    todo!()
    // returnck::expr_block_returns_value(e).is_ok()
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
    for (_id, function) in sa.functions.iter() {
        let function_name = function.display_name(sa);

        if function_pattern_match(&function_name, filter) {
            ast::dump::dump_function(&function.ast);
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::error::msg::{ErrorDescriptor, ErrorMessage};
    use crate::sema::Sema;
    use crate::test;
    use parser::{compute_line_column, compute_line_starts};

    pub fn ok(code: &'static str) {
        test::check(code, |vm| {
            let diag = vm.diag.borrow();
            let errors = diag.errors();

            for e in errors {
                println!("{}", e.message(vm));
                println!("{:?}", e);
                println!();
            }

            assert!(!diag.has_errors(), "program should not have errors.");
        });
    }

    pub fn err(code: &'static str, loc: (u32, u32), msg: ErrorMessage) {
        test::check(code, |vm| {
            let diag = vm.diag.borrow();
            let errors = diag.errors();

            let error_loc = if errors.len() == 1 {
                compute_pos(code, &errors[0])
            } else {
                None
            };

            if errors.len() != 1 || error_loc != Some(loc) || errors[0].msg != msg {
                println!("expected:");
                println!("\t{:?} at {}:{}", msg, loc.0, loc.1);
                println!();
                if errors.is_empty() {
                    println!("but got no error.");
                    println!();
                } else {
                    println!("but got:");
                    for error in errors {
                        println!("\t{:?} at {:?}", error.msg, compute_pos(code, error));
                        println!();
                    }
                }
            }

            assert_eq!(1, errors.len(), "found {} errors instead", errors.len());
            assert_eq!(Some(loc), error_loc);
            assert_eq!(msg, errors[0].msg);
        });
    }

    fn compute_pos(code: &str, error: &ErrorDescriptor) -> Option<(u32, u32)> {
        if let Some(span) = error.span {
            let line_starts = compute_line_starts(code);
            Some(compute_line_column(&line_starts, span.start()))
        } else {
            None
        }
    }
}
