use std::collections::HashMap;
use std::collections::HashSet;

use arkc_hir::hir;
use arkc_hir::hir::visit;
use arkc_hir::hir::visit::Visitor;

use crate::compilation::ModuleId;
use crate::report_sym_shadow_span;
use crate::sym::SymTable;
use crate::sym::Symbol;
use crate::sym::SymbolKind;
use crate::Name;
use crate::Sema;

pub struct SymbolResolver<'a> {
    sa: &'a mut Sema,
    module_symtables: HashMap<ModuleId, SymTable>
}

impl<'a> SymbolResolver<'a> {
    pub fn new(sa: &'a mut Sema) -> Self {
        SymbolResolver {
            sa,
            module_symtables: HashMap::new()
        }
    }

    pub fn scan_file(&mut self, module_id: ModuleId, root: &hir::File) -> HashMap<ModuleId, SymTable> {
        let module_table = {
            let mut decl_discovery = TopLevelDeclaration {
                sa: self.sa,
                module_id,
                module_table: SymTable::new(),
                //external_modules: Vec::new(),
                //module_symtables: &mut self.module_symtables,
            };

            decl_discovery.visit_file(root);

            let module_table = decl_discovery.module_table;

            /*if !decl_discovery.external_modules.is_empty() {
                for external_module_id in decl_discovery.external_modules {
                    self.add_module_files(
                        external_module_id,
                        module_path.clone(),
                        file_lookup,
                    );
                }
            }*/

            module_table
        };

        assert!(self
            .module_symtables
            .insert(module_id, module_table)
            .is_none());

        self.module_symtables.clone()
    }
}

fn ensure_name(sa: &Sema, ident: Option<&hir::Identifier>) -> Name {
    if let Some(ident) = ident {
        sa.interner.intern(&ident.name)
    } else {
        sa.interner.intern("<missing name>")
    }
}

fn check_if_symbol_exists(sa: &Sema, used_names: &mut HashSet<Name>, name: Name, /*span: Span*/) {
    if !used_names.insert(name) {
        let name = sa.interner.str(name).to_string();
        panic!("shadow field error: {}", name)
        //sa.report(span, ErrorMessage::ShadowField(name));
    }
}

struct TopLevelDeclaration<'a> {
    sa: &'a mut Sema,
    module_id: ModuleId,
    module_table: SymTable,
    //external_modules: Vec<ModuleId>,
    //module_symtables: &'a mut HashMap<ModuleId, SymTable>,
}

impl<'a> TopLevelDeclaration<'a> {
    fn insert(&mut self, name: Name, sym: SymbolKind) -> Option<Symbol> {
        self.module_table.insert(name, sym)
    }

    fn insert_optional(
        &mut self,
        ident: Option<&hir::Identifier>,
        sym: SymbolKind,
    ) -> Option<(Name, Symbol)> {
        if let Some(ident) = ident {
            let name = self.sa.interner.intern(&ident.name);
            self.insert(name, sym).map(|sym| (name, sym))
        } else {
            None
        }
    }
}

impl<'a> visit::Visitor for TopLevelDeclaration<'a> {
    fn visit_struct(&mut self, node: &hir::Struct) {
        //let mut fields = Vec::with_capacity(node.fields.len());
        let mut used_names: HashSet<Name> = HashSet::new();

        for field in node.fields.iter() {
            let name = ensure_name(self.sa, Some(&field.name));

            check_if_symbol_exists(self.sa, &mut used_names, name);

            //fields.push(StructField {
            //    id: StructFieldId(idx),
            //    name,
            //    span: field.span,
            //    ty: OnceCell::new(),
            //    visibility: modifiers.visibility(),
            //});
        }

        let _ = ensure_name(self.sa, Some(&node.name));

        let sym = SymbolKind::Struct(node.hir_id);
        if let Some((name, sym)) = self.insert_optional(Some(&node.name), sym) {
            report_sym_shadow_span(self.sa, name, sym);
        }
    }

    fn visit_fn_decl(&mut self, node: &hir::FnDeclaration) {
        let _ = ensure_name(self.sa, node.name.as_ref());
        let sym = SymbolKind::FnDecl(node.hir_id);
        if let Some((name, sym)) = self.insert_optional(node.name.as_ref(), sym) {
            report_sym_shadow_span(self.sa, name, sym);
        }
    }
}
