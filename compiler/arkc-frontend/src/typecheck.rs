mod expr;
mod function;
mod literals;
mod stmt;

use crate::sym::SymbolKind;
use crate::{compilation::ModuleId, sym::ModuleSymTable};
use crate::{report_sym_shadow_span, Sema};
use arkc_hir::hir::NodeMap;
use arkc_hir::{hir, ty};
use function::VarManager;
use std::collections::BTreeMap;

pub use function::TypeCheck;

pub fn returns_value(statement: &hir::Statement) -> Result<(), ()> {
    match *statement.kind {
        hir::StatementKind::Let(_) => Err(()),
        hir::StatementKind::Expr(ref expr) => expr_returns_value(&expr),
    }
}

pub(super) fn add_local(
    sa: &Sema,
    symtable: &mut ModuleSymTable,
    vars: &VarManager,
    id: hir::NestedVarId,
) {
    let ident = &vars.get_var(id).name;
    let name = sa.interner.intern(ident.name.as_str());
    let existing_symbol = symtable.insert(name, SymbolKind::Var(id));

    if let Some(existing_symbol) = existing_symbol {
        if !existing_symbol.kind().is_var() {
            report_sym_shadow_span(sa, name, existing_symbol)
        }
    }
}

pub fn expr_returns_value(expr: &hir::Expr) -> Result<(), ()> {
    match *expr.kind {
        hir::ExprKind::Block(ref block) => block_returns_value(block),
        //hir::ExprKind::If(ref expr) => expr_if_returns_value(expr),
        //hir::ExprKind::For(ref expr) => Err(expr.span),
        //hir::ExprKind::While(ref expr) => Err(expr.span),
        //hir::ExprKind::Break(ref stmt) => Err(stmt.span),
        //hir::ExprKind::Continue(ref stmt) => Err(stmt.span),
        hir::ExprKind::Return(..) => Ok(()),
        _ => Err(()),
    }
}

pub fn block_returns_value(blk: &hir::Block) -> Result<(), ()> {
    for stmt in &blk.stmts {
        match returns_value(stmt) {
            Ok(_) => return Ok(()),
            Err(err_pos) => todo!(),
        }
    }

    if let Some(ref expr) = blk.expr {
        expr_returns_value(expr)
    } else {
        Err(())
    }
}

pub struct TypecheckingContext<'a> {
    pub sa: &'a Sema,
    module_id: ModuleId,
}

impl<'a> TypecheckingContext<'a> {
    pub fn new(sa: &'a Sema, module_id: ModuleId) -> Self {
        Self { sa, module_id }
    }

    pub fn check_file(&mut self) {
        let (mut types, mut analysis_map) = {
            let root = &self.sa.compilation.hir.borrow()[0];
            let mut types = BTreeMap::new();
            let mut analysis_map = NodeMap::new();

            for item in root.elements.iter() {
                match &item.kind {
                    hir::ElemKind::Function(func) => {
                        let mut analysis = hir::AnalysisData {
                            idents: hir::NodeMap::new(),
                            map_vars: hir::NodeMap::new(),
                            int_literals: hir::NodeMap::new(),
                            vars: hir::VarAccess::new(vec![]),
                        };

                        let func_ty = root.node_types.get(&func.hir_id).expect("to be defined");
                        self.check_fn_declaration(&mut analysis, &mut types, func_ty, &func);
                        analysis_map.insert(func.hir_id, analysis);
                    }
                    _ => (),
                }
            }

            (types, analysis_map)
        };

        // Update hir::File with typechecking results
        let root = &mut self.sa.compilation.hir.borrow_mut()[0];
        root.node_types.append(&mut types);
        root.func_analysis.extend(analysis_map)
    }

    fn check_fn_declaration(
        &mut self,
        analysis: &mut hir::AnalysisData,
        types: &mut BTreeMap<hir::HirId, ty::Type>,
        func_ty: &ty::Type,
        func: &hir::FnDeclaration,
    ) {
        let mut symtable = ModuleSymTable::new(self.sa, self.module_id);

        let func_ty = match func_ty {
            ty::Type::Function(ty) => ty,
            _ => unreachable!(),
        };

        let hir = &self.sa.compilation.hir.borrow()[0];

        let mut typeck = TypeCheck {
            sa: self.sa,
            ctx: self,
            hir,
            types,
            analysis,
            symtable: &mut symtable,
            param_types: vec![], // func.params_with_self().to_owned(),
            return_type: func_ty.output.clone(),
            vars: &mut VarManager::new(),
        };

        typeck.check_function(&func);
    }
}
