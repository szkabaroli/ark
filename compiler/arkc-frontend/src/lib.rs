use error::msg::ErrorMessage;
pub use interner::Name;
use parser::{ast, Span};
use sema::{Sema, SourceFileId};
pub use sym::{SymTable, Symbol, SymbolKind};
pub use ty::{SourceType, SourceTypeArray};

#[cfg(test)]
pub mod test;

pub mod generator;
pub mod typecheck;
mod error;
mod fctdef_check;
mod import_check;
mod interner;
mod program_parser;
mod sema;
mod specialize;
mod stdlib_lookup;
mod sym;
mod ty;
mod readty;

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
    println!("{:#?}", module_symtables);

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
    todo!()
    //returnck::returns_value(s).is_ok()
}

pub fn expr_always_returns(e: &ast::ExprData) -> bool {
    todo!()
    //returnck::expr_returns_value(e).is_ok()
}

pub fn expr_block_always_returns(e: &ast::ExprBlockType) -> bool {
    todo!()
    // returnck::expr_block_returns_value(e).is_ok()
}
