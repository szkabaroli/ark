use arkc_hir::hir::HirId;
use arkc_hir::hir::NestedVarId;
use std::collections::HashMap;
use std::rc::Rc;

use self::SymbolKind::*;

use crate::compilation::ModuleId;
use crate::interner::Name;
use crate::Sema;

pub struct ModuleSymTable {
    module_id: ModuleId,
    levels: Vec<SymTable>,
    outer: Rc<SymTable>,
    // dependencies: Arc<RwLock<SymTable>>,
    // prelude: Rc<SymTable>,
}

impl ModuleSymTable {
    pub fn new(sa: &Sema, module_id: ModuleId) -> ModuleSymTable {
        let module = sa.compilation.module(module_id);
        let outer = module.table();
        // let dependencies = sa.compilation.modules[module_id].table.clone();
        // let prelude = sa.module(sa.prelude_module_id()).table();

        ModuleSymTable {
            module_id,
            levels: Vec::new(),
            outer,
            // dependencies,
            // prelude,
        }
    }

    pub fn module_id(&self) -> ModuleId {
        self.module_id
    }

    pub fn push_level(&mut self) {
        self.levels.push(SymTable::new());
    }

    pub fn pop_level(&mut self) {
        assert!(self.levels.len() >= 1);
        self.levels.pop();
    }

    pub fn levels(&mut self) -> usize {
        self.levels.len()
    }

    pub fn get_string(&self, sa: &Sema, name: &str) -> Option<SymbolKind> {
        let interned_name = sa.interner.intern(name);
        self.get(interned_name)
    }

    pub fn get(&self, name: Name) -> Option<SymbolKind> {
        for level in self.levels.iter().rev() {
            if let Some(val) = level.get(name) {
                return Some(val.clone());
            }
        }

        if let Some(sym) = self.outer.get(name) {
            return Some(sym.clone());
        }

        //if let Some(sym) = self.dependencies.read().get(name) {
        //    return Some(sym.clone());
        //}

        //if let Some(sym) = self.prelude.get(name) {
        //    return Some(sym.clone());
        //}

        None
    }

    pub fn insert(&mut self, name: Name, sym: SymbolKind) -> Option<Symbol> {
        self.levels.last_mut().unwrap().insert(name, sym)
    }
}

#[derive(Debug, Clone)]
pub struct SymTable {
    table: HashMap<Name, Symbol>,
}

impl SymTable {
    // creates a new table
    pub fn new() -> SymTable {
        SymTable {
            table: HashMap::new(),
        }
    }

    pub fn get(&self, name: Name) -> Option<SymbolKind> {
        self.table.get(&name).map(|sym| sym.kind.clone())
    }

    pub fn get_sym(&self, name: Name) -> Option<&Symbol> {
        self.table.get(&name)
    }

    pub fn insert(&mut self, name: Name, kind: SymbolKind) -> Option<Symbol> {
        let symbol = Symbol {
            //visibility: None,
            kind,
        };
        self.table.insert(name, symbol)
    }

    pub fn insert_use(
        &mut self,
        name: Name,
        //visibility: Visibility,
        kind: SymbolKind,
    ) -> Option<Symbol> {
        let symbol = Symbol {
            //visibility: Some(visibility),
            kind,
        };
        self.table.insert(name, symbol)
    }

    pub fn dump(&self, sa: &Sema) {
        for (key, symbol) in &self.table {
            println!("{} -> {:?}", sa.interner.str(*key), symbol.kind);
        }
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    // visibility: Option<Visibility>,
    kind: SymbolKind,
}

impl Symbol {
    //pub fn visibility(&self) -> Option<&Visibility> {
    //    self.visibility.as_ref()
    //}

    pub fn kind(&self) -> &SymbolKind {
        &self.kind
    }
}

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Module(ModuleId),
    Struct(HirId),
    FnDecl(HirId),
    Var(NestedVarId),
    //Field(HirId),
    //Class(HirId),
    //Trait(HirId),
    //TypeParam(HirId),
    //Enum(HirId),
    //Global(NodeId),
    //Const(NodeId),
    //EnumVariant(NodeId, u32),
    //TypeAlias(NodeId),
}

impl SymbolKind {
    pub fn to_fn(&self) -> Option<HirId> {
        match *self {
            FnDecl(id) => Some(id),
            _ => None,
        }
    }

    pub fn to_struct(&self) -> Option<HirId> {
        match *self {
            Struct(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_module(&self) -> bool {
        match *self {
            Module(_) => true,
            _ => false,
        }
    }

    pub fn to_module(&self) -> Option<ModuleId> {
        match *self {
            Module(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_var(&self) -> bool {
        match *self {
            Var(_) => true,
            _ => false,
        }
    }
}
