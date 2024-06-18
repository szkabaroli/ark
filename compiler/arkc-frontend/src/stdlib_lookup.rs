use std::rc::Rc;

use crate::{sema::{ModuleDefinition, ModuleDefinitionId, Sema}, Name, SymTable, SymbolKind};

fn resolve_name(sa: &Sema, name: &str, module_id: ModuleDefinitionId) -> SymbolKind {
    let path = name.split("::");
    let mut sym = SymbolKind::Module(module_id);

    for name in path {
        let module_id = sym.to_module().expect("module expected");
        let table = sa.module(module_id).table();

        let interned_name = sa.interner.intern(name);

        if let Some(current_sym) = table.get(interned_name) {
            sym = current_sym;
        } else {
            let module = sa.module(module_id);
            panic!("{} not found in module {}.", name, module.name(sa));
        }
    }

    sym
}

pub fn setup_prelude(sa: &mut Sema) {
    let stdlib_id = sa.stdlib_module_id();

    //let symbols = [
        //"primitives::Bool",
        //"primitives::UInt8",
        //"primitives::Char",
        //"primitives::Int32",
        //"primitives::Int64",
        //"primitives::Int",
        //"primitives::Float32",
        //"primitives::Float64",
        //"string::String",
        //"collections::Array",
        //"collections::Vec",
        //"print",
        //"println",
        //"primitives::Option",
        //"unimplemented",
        //"unreachable",
        //"assert",
        //"primitives::Result",
    //];

    let mut prelude_table = SymTable::new();

    //for name in &symbols {
    //    let sym = resolve_name(sa, name, stdlib_id);
    //    let name = final_path_name(sa, name);
    //    let old_sym = prelude_table.insert(name, sym);
    //    assert!(old_sym.is_none());
    //}

    /*{
        // include None and Some from Option
        let enum_id = resolve_name(sa, "primitives::Option", stdlib_id)
            .to_enum()
            .expect("enum expected");

        let enum_ = sa.enum_(enum_id);

        for variant in enum_.variants() {
            let old_sym =
                prelude_table.insert(variant.name, SymbolKind::EnumVariant(enum_id, variant.id));
            assert!(old_sym.is_none());
        }
    }*/ 

    /*{
        // include Ok and Err from Result
        let enum_id = resolve_name(sa, "primitives::Result", stdlib_id)
            .to_enum()
            .expect("enum expected");

        let enum_ = sa.enum_(enum_id);

        for variant in enum_.variants() {
            let old_sym =
                prelude_table.insert(variant.name, SymbolKind::EnumVariant(enum_id, variant.id));
            assert!(old_sym.is_none());
        }
    }*/

    let stdlib_name = sa.interner.intern("std");
    prelude_table.insert(stdlib_name, SymbolKind::Module(stdlib_id));

    let module = ModuleDefinition::new_top_level(None);
    assert!(module.table.set(Rc::new(prelude_table)).is_ok());
    let module_id = sa.modules.alloc(module);
    sa.set_prelude_module_id(module_id);
}

fn final_path_name(sa: &mut Sema, path: &str) -> Name {
    let name = path.split("::").last().expect("name missing");
    sa.interner.intern(name)
}