use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

use crate::sym::SymTable;
use crate::Name;
use id_arena::Id;

pub type ModuleId = Id<Module>;

#[derive(Debug)]
pub struct Module {
    pub id: Option<ModuleId>,
    pub name: ModuleName,
    pub dependencies: Vec<ModuleDependency>,
    pub dependency_names: HashMap<Name, ModuleId>,
    pub table: OnceCell<Rc<SymTable>>,
}

impl Module {
    pub fn new(name: ModuleName) -> Module {
        Module {
            id: None,
            name,
            dependencies: Vec::new(),
            dependency_names: HashMap::new(),
            table: OnceCell::new(),
        }
    }

    pub fn id(&self) -> ModuleId {
        self.id.expect("missing id")
    }

    pub fn table(&self) -> Rc<SymTable> {
        self.table.get().cloned().expect("missing table")
    }

    pub fn name(&self) -> ModuleName {
        self.name.clone()
    }

    /*pub fn add_dependency(&mut self, name: Name, module_id: ModuleId) -> bool {
        let table = self.table.get().unwrap();

        if table.get(name).is_some() {
            false
        } else {
            //let old_value = self
            //    .table
            //    .write()
            //    .insert(name, SymbolKind::Module(top_level_module_id));
            //assert!(old_value.is_none());
            self.dependencies.push(ModuleDependency { name, module_id });
            true
        }
    }*/
}

#[derive(Debug)]
pub struct ModuleDependency {
    pub name: Name,
    pub module_id: ModuleId,
}

#[derive(Debug, Clone)]
pub enum ModuleName {
    Program,
    External(String),
}
