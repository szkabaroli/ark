use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use parser::NodeId;

use crate::{
    sema::{ModuleDefinitionId, Sema, SourceFileId},
    SymTable,
};

pub fn check<'a>(sa: &Sema, mut module_symtables: HashMap<ModuleDefinitionId, SymTable>) {
    // let mut processed_uses = HashSet::<(SourceFileId, NodeId)>::new();

    /*while {
        let mut did_resolve_symbol = false;

        for (_id, use_definition) in &sa.uses {
            let _ = check_use(
                sa,
                &mut module_symtables,
                &use_definition.ast,
                use_definition.module_id,
                use_definition.file_id,
                use_definition.visibility,
                None,
                true,
                &mut processed_uses,
                &mut did_resolve_symbol,
            );
        }

        did_resolve_symbol
    } {}*/

    /*for (_id, use_definition) in &sa.uses {
        let _ = check_use(
            sa,
            &mut module_symtables,
            &use_definition.ast,
            use_definition.module_id,
            use_definition.file_id,
            use_definition.visibility,
            None,
            false,
            &mut processed_uses,
            &mut false,
        );
    }*/

    for (module_id, table) in module_symtables {
        assert!(sa.module(module_id).table.set(Rc::new(table)).is_ok());
    }
}
