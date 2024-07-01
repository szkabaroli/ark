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

#[derive(Debug)]
struct VarAccessPerFunction {
    id: usize,
    //start_scope_id: usize,
    start_var_id: usize,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Var {
    pub id: hir::NestedVarId,
    pub name: hir::Identifier,
    pub ty: ty::Type,
    pub mutable: bool,
    //pub location: VarLocation,
    //pub scope_id: NestedScopeId,
    pub function_id: usize,
}

#[derive(Debug)]
pub struct VarManager {
    // Stack of variables of all nested functions.
    vars: Vec<Var>,
    // Stack of all nested scopes. Mostly functions but also
    // loop bodies have scopes.
    // scopes: Vec<VarAccessPerScope>,

    // Start of functions.
    functions: Vec<VarAccessPerFunction>,
}

impl VarManager {
    fn enter_function_scope(&mut self) {
        /*let scope_id = self.scopes.len();

        self.scopes.push(VarAccessPerScope {
            id: NestedScopeId(scope_id),
            next_field_id: 0,
            vars: Vec::new(),
        });*/
        self.functions.push(VarAccessPerFunction {
            id: self.functions.len(),
            //start_scope_id: scope_id,
            start_var_id: self.vars.len(),
        });
    }

    pub fn new() -> Self {
        Self { vars: vec![], functions: vec![] }
    }

    pub fn get_var(&self, idx: hir::NestedVarId) -> &Var {
        &self.vars[idx.0]
    }

    pub fn add_var(
        &mut self,
        name: hir::Identifier,
        ty: ty::Type,
        mutable: bool,
    ) -> hir::NestedVarId {
        let id = hir::NestedVarId(self.vars.len());

        let var = Var {
            id,
            name,
            ty,
            mutable,
            //location: VarLocation::Stack,
            //scope_id: self.current_scope().id,
            function_id: self.current_function().id,
        };

        self.vars.push(var);
        //self.current_scope_mut().vars.push(id);

        id
    }

    fn current_function(&self) -> &VarAccessPerFunction {
        self.functions.last().expect("missing function")
    }

    fn leave_function_scope(&mut self) -> Vec<Var> {
        //let _ = self.scopes.pop().expect("missing scope");
        let function = self.functions.pop().expect("missing function");

        self.vars.drain(function.start_var_id..).collect()
    }

    pub(super) fn local_var_id(&self, var_id: hir::NestedVarId) -> hir::VarId {
        assert!(var_id.0 >= self.current_function().start_var_id);
        hir::VarId(var_id.0 - self.current_function().start_var_id)
    }

    pub(super) fn maybe_allocate_in_context(&mut self, var_id: hir::NestedVarId) -> hir::IdentType {
        hir::IdentType::Var(self.local_var_id(var_id))

        //if var_id.0 < self.current_function().start_var_id {
        //let field_id = self.ensure_context_allocated(var_id);
        //let NestedScopeId(level) = self.scope_for_var(var_id).id;
        //IdentType::Context(OuterContextIdx(level), field_id)
        //} else {
        //    IdentType::Var(self.local_var_id(var_id))
        //}
    }
}

pub struct TypeCheck<'a> {
    pub sa: &'a Sema,
    pub hir: &'a hir::File,
    pub ctx: &'a TypecheckingContext<'a>,
    pub symtable: &'a mut ModuleSymTable,
    pub vars: &'a mut VarManager,
    pub analysis: &'a mut hir::AnalysisData,
    pub types: &'a mut BTreeMap<hir::HirId, ty::Type>,
    pub param_types: Vec<ty::Type>,
    pub return_type: Option<Box<ty::Type>>,
}

impl<'a> TypeCheck<'a> {
    pub fn check_function(&mut self, func: &hir::FnDeclaration) {
        let file = self.sa.compilation.hir.borrow();
        let body = file[0].get_body(&func.body_id).expect("missing block");

        let start_level = self.symtable.levels();
        self.enter_function_scope();
        self.symtable.push_level();

        //self.add_params(func);
        self.check_body(body);

        self.symtable.pop_level();
        assert_eq!(self.symtable.levels(), start_level);

        self.leave_function_scope();
    }

    fn enter_function_scope(&mut self) {
        //self.start_context_id = self.context_classes.len();
        //self.context_classes.push(LazyContextData::new());
        self.vars.enter_function_scope();
    }

    fn leave_function_scope(&mut self) {
        // Store var definitions for all local and context vars defined in this function.
        let vars = self.vars.leave_function_scope();

        let vars = vars
            .into_iter()
            .map(|vd| hir::Var {
                ty: vd.ty.clone(),
                //location: vd.location,
            })
            .collect();

        self.analysis.vars = hir::VarAccess::new(vars);
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

    pub(super) fn maybe_allocate_in_context(&mut self, var_id: hir::NestedVarId) -> hir::IdentType {
        let ident_type = self.vars.maybe_allocate_in_context(var_id);

        match ident_type {
            /*IdentType::Context(context_id, _field_id) => {
                // We need parent slots from the context of the variable up to (not including)
                // the first context of this function.
                // There is no need for parent slots for contexts within this function because
                // we can always load that context out of the lambda object which is passed as
                // the first argument.
                let indices = context_id.0 + 1..self.start_context_id;
                let range = &self.context_classes[indices];
                for context_class in range {
                    context_class.require_parent_slot();
                }
                // This lambda needs the caller context.
                assert!(self.is_lambda);
                self.needs_context_slot_in_lambda_object = true;
                ident_type
            }*/
            hir::IdentType::Var(..) => ident_type,
            _ => unreachable!(),
        }
    }

    pub(super) fn ty_name(&self, ty: &ty::Type) -> String {
        ty.name()
    }
}
