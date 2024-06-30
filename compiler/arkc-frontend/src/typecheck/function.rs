use std::collections::BTreeMap;

use arkc_hir::hir;
use arkc_hir::hir::HirId;
use arkc_hir::hir::NodeMap;
use arkc_hir::ty;
use serde::Deserialize;
use serde::Serialize;

use crate::check_type;
use crate::error::ErrorMessage;
use crate::readty::AliasReplacement;
use crate::readty::AllowSelf;
use crate::replace_type;
use crate::report_sym_shadow_span;
use crate::sym::ModuleSymTable;
use crate::sym::SymbolKind;
use crate::Name;
use crate::Sema;

use super::expr::check_expr;
use super::expr_returns_value;
use super::returns_value;
use super::stmt::check_stmt;
use super::TypecheckingContext;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash, Serialize, Deserialize)]
pub struct NestedVarId(pub usize);

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Var {
    pub id: NestedVarId,
    pub name: hir::Identifier,
    pub ty: ty::Type,
    pub mutable: bool,
    //pub location: VarLocation,
    //pub scope_id: NestedScopeId,
    pub function_id: HirId,
}

pub struct VarManager {
    // Stack of variables of all nested functions.
    vars: Vec<Var>,
    // Stack of all nested scopes. Mostly functions but also
    // loop bodies have scopes.
    // scopes: Vec<VarAccessPerScope>,

    // Start of functions.
    // functions: Vec<VarAccessPerFunction>,
}

impl VarManager {
    pub fn new() -> Self {
        Self { vars: vec![] }
    }

    pub(super) fn get_var(&self, idx: NestedVarId) -> &Var {
        &self.vars[idx.0]
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VarLocation {
    Stack,
    // TODO: closures Context(ScopeId, ContextFieldId),
}

pub(super) fn add_local(
    sa: &Sema,
    symtable: &mut ModuleSymTable,
    vars: &VarManager,
    id: NestedVarId,
) {
    let name = &vars.get_var(id).name;
    //let existing_symbol = symtable.insert(name.name, SymbolKind::Var(id));

    //if let Some(existing_symbol) = existing_symbol {
    //    if !existing_symbol.kind().is_var() {
    //        report_sym_shadow_span(sa, name, existing_symbol)
    //    }
    //}
}

pub struct TypeCheck<'a> {
    pub sa: &'a Sema,
    pub hir: &'a hir::File,
    pub ctx: &'a TypecheckingContext<'a>,
    pub symtable: &'a mut ModuleSymTable,
    pub int_literals: &'a mut NodeMap<i64>,
    pub types: &'a mut BTreeMap<hir::HirId, ty::Type>,
    pub vars: &'a mut VarManager,
    pub param_types: Vec<ty::Type>,
    pub return_type: Option<Box<ty::Type>>,
}

impl<'a> TypeCheck<'a> {
    pub fn check_function(&mut self, func: &hir::FnDeclaration) {
        let root = &self.sa.compilation.hir.borrow()[0];
        let start_level = self.symtable.levels();

        self.symtable.push_level();

        //self.add_params(func);
        self.check_body(root.get_body(&func.body_id).expect("missing block"));

        self.symtable.pop_level();
        assert_eq!(self.symtable.levels(), start_level);
    }

    fn check_body(&mut self, func_body: &hir::FnBody) {
        let fn_return_type = self
            .return_type
            .as_ref()
            .expect("missing return type")
            .clone();

        let mut returns = false;

        for stmt in &func_body.body.stmts {
            check_stmt(self, stmt);

            if returns_value(stmt).is_ok() {
                returns = true;
            }
        }

        let return_type = if let Some(ref value) = &func_body.body.expr {
            if expr_returns_value(value).is_ok() {
                returns = true;
            }

            check_expr(self, value, fn_return_type.clone())
        } else {
            ty::Type::Primitive(ty::PrimitiveType::Unit)
        };

        if !returns {
            self.check_fn_return_type(fn_return_type, return_type);
        }
    }

    pub(super) fn check_fn_return_type(
        &mut self,
        fn_return_type: Box<ty::Type>,
        //span: Span,
        expr_type: ty::Type,
    ) {
        if !fn_return_type.allows(expr_type.clone()) {
            let fn_type = self.ty_name(&fn_return_type);
            let expr_type = self.ty_name(&expr_type);
            let msg = ErrorMessage::ReturnType(fn_type, expr_type);
            panic!("error {:?}", msg);

            //self.sa.report(span, msg);
        }
    }

    pub(super) fn read_type(&mut self, ty_ctor: &hir::TypeData) -> ty::Type {
        let allow_self = AllowSelf::No; /*if self.self_ty.is_some() {
                                            AllowSelf::Yes
                                        } else {
                                            AllowSelf::No
                                        };*/

        let ty = check_type(self.sa, &self.hir, &self.symtable, ty_ctor, allow_self);

        replace_type(
            self.sa,
            ty,
            None, // self.self_ty.clone(),
            AliasReplacement::ReplaceWithActualType,
        )
    }

    pub(super) fn ty_name(&self, ty: &ty::Type) -> String {
        ty.name()
    }
}
