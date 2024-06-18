use parser::{ast, Span};

use crate::error::msg::ErrorMessage;
use crate::readty::{check_type, AllowSelf};
use crate::specialize::AliasReplacement;
use crate::{
    always_returns, expr_always_returns, report_sym_shadow_span,
    sema::{
        AnalysisData, ContextFieldId, IdentType, InnerContextId, LazyContextData,
        ModuleDefinitionId, NestedScopeId, NestedVarId, PackageDefinitionId, ScopeId, Sema,
        SourceFileId, Var, VarAccess, VarId, VarLocation,
    },
    specialize::replace_type,
    sym::ModuleSymTable,
    Name, SourceType, SymbolKind,
};

use super::{expr::check_expr, stmt::check_stmt};

pub(super) fn add_local(
    sa: &Sema,
    symtable: &mut ModuleSymTable,
    vars: &VarManager,
    id: NestedVarId,
    file_id: SourceFileId,
    span: Span,
) {
    let name = vars.get_var(id).name;
    let existing_symbol = symtable.insert(name, SymbolKind::Var(id));

    if let Some(existing_symbol) = existing_symbol {
        if !existing_symbol.kind().is_var() {
            report_sym_shadow_span(sa, name, file_id, span, existing_symbol)
        }
    }
}

fn parse_lit_int(mut value: &str) -> (u32, String, String) {
    let base = if value.starts_with("0b") {
        value = &value[2..];
        2
    } else if value.starts_with("0x") {
        value = &value[2..];
        16
    } else {
        10
    };

    let mut it = value.chars().peekable();
    let mut filtered_value = String::new();

    while let Some(&ch) = it.peek() {
        if ch.is_digit(base) {
            filtered_value.push(ch);
            it.next();
        } else if ch == '_' {
            it.next();
        } else {
            break;
        }
    }

    let mut suffix = String::new();

    for ch in it {
        suffix.push(ch);
    }

    (base, filtered_value, suffix)
}

fn parse_lit_float(mut value: &str) -> (u32, String, String) {
    let base = if value.starts_with("0b") {
        value = &value[2..];
        2
    } else if value.starts_with("0x") {
        value = &value[2..];
        16
    } else {
        10
    };

    let mut it = value.chars().peekable();
    let mut filtered_value = String::new();
    let mut allow_scientific = true;

    while let Some(&ch) = it.peek() {
        if ch.is_digit(base) || ch == '.' || ch == '-' || ch == '+' {
            filtered_value.push(ch);
            it.next();
        } else if ch == '_' {
            it.next();
        } else if (ch == 'e' || ch == 'E') && allow_scientific {
            filtered_value.push(ch);
            it.next();
            allow_scientific = false;
        } else {
            break;
        }
    }

    let mut suffix = String::new();

    for ch in it {
        suffix.push(ch);
    }

    (base, filtered_value, suffix)
}

fn determine_suffix_type_int_literal(
    sa: &Sema,
    file: SourceFileId,
    span: Span,
    suffix: &str,
) -> Option<SourceType> {
    match suffix {
        "u8" => Some(SourceType::UInt8),
        "i32" => Some(SourceType::Int32),
        "i64" => Some(SourceType::Int64),
        "f32" => Some(SourceType::Float32),
        "f64" => Some(SourceType::Float64),
        "" => None,
        _ => {
            sa.report(file, span, ErrorMessage::UnknownSuffix);
            None
        }
    }
}

pub fn check_lit_int(
    sa: &Sema,
    file: SourceFileId,
    e: &ast::ExprLitIntType,
    negate: bool,
    expected_type: SourceType,
) -> (SourceType, i64, f64) {
    let (base, value, suffix) = parse_lit_int(&e.value);
    let suffix_type = determine_suffix_type_int_literal(sa, file, e.span, &suffix);

    let ty = suffix_type.unwrap_or_else(|| match expected_type {
        SourceType::UInt8 if !negate => SourceType::UInt8,
        SourceType::Int32 => SourceType::Int32,
        SourceType::Int64 => SourceType::Int64,
        _ => SourceType::Int64,
    });

    if ty.is_float() {
        let value = value.parse::<f64>().expect("unparsable float");
        let value = if negate { -value } else { value };

        if base != 10 {
            sa.report(file, e.span, ErrorMessage::InvalidNumberFormat);
        }

        return (ty, 0, value);
    }

    if negate && ty == SourceType::UInt8 {
        sa.report(file, e.span, ErrorMessage::NegativeUnsigned);
    }

    let ty_name = ty.name(sa);
    let parsed_value = u64::from_str_radix(&value, base);

    let value = match parsed_value {
        Ok(value) => value,
        Err(_) => {
            sa.report(file, e.span, ErrorMessage::NumberLimitOverflow);
            return (ty, 0, 0.0);
        }
    };

    if base == 10 {
        let max = match ty {
            SourceType::UInt8 => 256,
            SourceType::Int32 => 1u64 << 31,
            SourceType::Int64 => 1u64 << 63,
            _ => unreachable!(),
        };

        if (negate && value > max) || (!negate && value >= max) {
            sa.report(file, e.span, ErrorMessage::NumberOverflow(ty_name.into()));
        }

        let value = if negate {
            (value as i64).wrapping_neg()
        } else {
            value as i64
        };

        (ty, value, 0.0)
    } else {
        assert!(!negate);

        let max = match ty {
            SourceType::UInt8 => 256 as u64,
            SourceType::Int32 => u32::max_value() as u64,
            SourceType::Int64 => u64::max_value() as u64,
            _ => unreachable!(),
        };

        if value > max {
            sa.report(file, e.span, ErrorMessage::NumberOverflow(ty_name.into()));
        }

        (ty, value as i64, 0.0)
    }
}

pub fn check_lit_float(
    sa: &Sema,
    file: SourceFileId,
    e: &ast::ExprLitFloatType,
    negate: bool,
) -> (SourceType, f64) {
    let (base, value, suffix) = parse_lit_float(&e.value);

    if base != 10 {
        sa.report(file, e.span, ErrorMessage::InvalidNumberFormat);
    }

    let ty = match suffix.as_str() {
        "f32" => SourceType::Float32,
        "f64" => SourceType::Float64,
        "" => SourceType::Float64,
        _ => {
            sa.report(file, e.span, ErrorMessage::UnknownSuffix);
            SourceType::Float64
        }
    };

    let (min, max) = match ty {
        SourceType::Float32 => (f32::MIN as f64, f32::MAX as f64),
        SourceType::Float64 => (f64::MIN, f64::MAX),
        _ => unreachable!(),
    };

    let value = value.parse::<f64>().expect("unparsable float");
    let value = if negate { -value } else { value };

    if value < min || value > max {
        let name = match ty {
            SourceType::Float32 => "Float32",
            SourceType::Float64 => "Float64",
            _ => unreachable!(),
        };
        sa.report(file, e.span, ErrorMessage::NumberOverflow(name.into()));
    }

    (ty, value)
}

pub struct TypeCheck<'a> {
    pub sa: &'a Sema,
    //pub type_param_defs: &'a TypeParamDefinition,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub analysis: &'a mut AnalysisData,
    pub symtable: &'a mut ModuleSymTable,
    pub param_types: Vec<SourceType>,
    pub return_type: Option<SourceType>,
    pub in_loop: bool,
    pub is_lambda: bool,
    pub has_hidden_self_argument: bool,
    pub is_self_available: bool,
    pub self_ty: Option<SourceType>,
    pub vars: &'a mut VarManager,
    // All nested contexts. There will be entries for all nested function/lambda
    // and block scopes even when we eventually learn that we don't need
    // a context class for some of them.
    pub context_classes: &'a mut Vec<LazyContextData>,
    pub start_context_id: usize,
    pub needs_context_slot_in_lambda_object: bool,
    // Lazily create contexts and lambdas discovered while checking functions.
    //pub lazy_context_class_creation: &'a mut Vec<LazyContextClassCreationData>,
    //pub lazy_lambda_creation: &'a mut Vec<LazyLambdaCreationData>,
}

impl<'a> TypeCheck<'a> {
    pub fn check_fct(&mut self, ast: &ast::Function) {
        self.check_common(|self_| {
            //self_.add_type_params();
            self_.add_params(ast);
            self_.check_body(ast);
        })
    }

    fn check_common<F>(&mut self, fct: F)
    where
        F: FnOnce(&mut TypeCheck<'a>),
    {
        let start_level = self.symtable.levels();
        self.enter_function_scope();
        self.symtable.push_level();

        fct(self);

        self.symtable.pop_level();
        assert_eq!(self.symtable.levels(), start_level);

        self.leave_function_scope();
    }

    fn add_params(&mut self, ast: &ast::Function) {
        self.add_hidden_parameter_self();

        let self_count = if self.has_hidden_self_argument { 1 } else { 0 };
        assert_eq!(ast.params.len() + self_count, self.param_types.len());

        for (ind, (param, ty)) in ast
            .params
            .iter()
            .zip(self.param_types.iter().skip(self_count))
            .enumerate()
        {
            // is this last argument of function with variadic arguments?
            let ty = if ind == ast.params.len() - 1
                && ast.params.last().expect("missing param").variadic
            {
                todo!();
                // type of variable is Array<T>
                // self.sa.known.array_ty(ty.clone())
            } else {
                ty.clone()
            };

            let name = self
                .sa
                .interner
                .intern(&param.name.as_ref().expect("missing name").name_as_string);

            let var_id = self.vars.add_var(name, ty, param.mutable);
            self.analysis
                .map_vars
                .insert(param.id, self.vars.local_var_id(var_id));

            // params are only allowed to replace functions, vars cannot be replaced
            let replaced_sym = self.symtable.insert(name, SymbolKind::Var(var_id));
            if let Some(replaced_sym) = replaced_sym {
                report_sym_shadow_span(self.sa, name, self.file_id, param.span, replaced_sym)
            }
        }
    }

    fn add_hidden_parameter_self(&mut self) {
        self.analysis.set_has_self(self.has_hidden_self_argument);

        if !self.has_hidden_self_argument {
            return;
        }

        // Only functions can use `self`.
        let self_ty = self.param_types[0].clone();
        let name = self.sa.interner.intern("self");

        assert!(!self.vars.has_vars());
        self.vars.add_var(name, self_ty, false);
    }

    pub(super) fn read_type(&mut self, t: &ast::TypeData) -> SourceType {
        let allow_self = if self.self_ty.is_some() {
            AllowSelf::Yes
        } else {
            AllowSelf::No
        };

        let ty = check_type(
            self.sa,
            &self.symtable,
            self.file_id,
            t,
            //self.type_param_defs,
            allow_self,
        );

        replace_type(
            self.sa,
            ty,
            None,
            self.self_ty.clone(),
            AliasReplacement::ReplaceWithActualType,
        )
    }

    fn check_body(&mut self, ast: &ast::Function) {
        let block = ast.block.as_ref().expect("missing block");
        let fct_return_type = self
            .return_type
            .as_ref()
            .expect("missing return type")
            .clone();

        if let ast::ExprData::Block(ref block) = block.as_ref() {
            let mut returns = false;

            for stmt in &block.stmts {
                check_stmt(self, stmt);

                if always_returns(stmt) {
                    returns = true;
                }
            }

            let return_type = if let Some(ref value) = &block.expr {
                if expr_always_returns(value) {
                    returns = true;
                }

                check_expr(self, value, fct_return_type.clone())
            } else {
                SourceType::Unit
            };

            if !returns {
                self.check_fct_return_type(fct_return_type, block.span, return_type);
            }
        } else {
            unreachable!()
        }
    }

    pub(super) fn maybe_allocate_in_context(&mut self, var_id: NestedVarId) -> IdentType {
        let ident_type = self.vars.maybe_allocate_in_context(var_id);

        match ident_type {
            //IdentType::Context(context_id, _field_id) => {
            // We need parent slots from the context of the variable up to (not including)
            // the first context of this function.
            // There is no need for parent slots for contexts within this function because
            // we can always load that context out of the lambda object which is passed as
            // the first argument.
            //    let indices = context_id.0 + 1..self.start_context_id;
            //    let range = &self.context_classes[indices];
            //    for context_class in range {
            //        context_class.require_parent_slot();
            //    }
            // This lambda needs the caller context.
            //    assert!(self.is_lambda);
            //    self.needs_context_slot_in_lambda_object = true;
            //    ident_type
            //}
            IdentType::Var(..) => ident_type,

            _ => unreachable!(),
        }
    }

    fn enter_function_scope(&mut self) {
        self.start_context_id = self.context_classes.len();
        self.context_classes.push(LazyContextData::new());
        self.vars.enter_function_scope();
    }

    fn leave_function_scope(&mut self) {
        let lazy_context_data = self.context_classes.pop().expect("missing context class");

        //if self.vars.has_context_vars() {
        //   self.setup_context_class(lazy_context_data.clone());
        //}

        let needs_context_slot_in_lambda_object =
            self.needs_context_slot_in_lambda_object || lazy_context_data.has_parent_slot();

        if needs_context_slot_in_lambda_object {
            assert!(self.is_lambda);
        }

        //assert!(self
        //    .analysis
        //    .needs_context_slot_in_lambda_object
        //    .set(needs_context_slot_in_lambda_object)
        //   .is_ok());

        assert!(self
            .analysis
            .function_context_data
            .set(lazy_context_data)
            .is_ok());

        // Store var definitions for all local and context vars defined in this function.
        let vars = self.vars.leave_function_scope();

        let vars = vars
            .into_iter()
            .map(|vd| Var {
                ty: vd.ty.clone(),
                location: vd.location,
            })
            .collect();

        self.analysis.vars = VarAccess::new(vars);
    }

    pub(super) fn check_fct_return_type(
        &mut self,
        return_type: SourceType,
        span: Span,
        expr_type: SourceType,
    ) {
        if !expr_type.is_unknown() && !return_type.allows(self.sa, expr_type.clone()) {
            let fct_type = self.ty_name(&return_type);
            let expr_type = self.ty_name(&expr_type);

            let msg = ErrorMessage::ReturnType(fct_type, expr_type);

            self.sa.report(self.file_id, span, msg);
        }
    }

    pub(super) fn ty_name(&self, ty: &SourceType) -> String {
        ty.name(self.sa)
        //ty.name_with_type_params(self.sa, self.type_param_defs)
    }
}

struct VarAccessPerScope {
    id: NestedScopeId,
    next_field_id: usize,
    vars: Vec<NestedVarId>,
}

struct VarAccessPerFunction {
    id: usize,
    start_scope_id: usize,
    start_var_id: usize,
}

pub struct VarManager {
    // Stack of variables of all nested functions.
    vars: Vec<VarDefinition>,

    // Stack of all nested scopes. Mostly functions but also
    // loop bodies have scopes.
    scopes: Vec<VarAccessPerScope>,

    // Start of functions.
    functions: Vec<VarAccessPerFunction>,
}

impl VarManager {
    pub fn new() -> VarManager {
        VarManager {
            vars: Vec::new(),
            scopes: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub fn has_vars(&self) -> bool {
        self.vars.len() > self.current_function().start_var_id
    }

    pub fn has_context_vars(&self) -> bool {
        self.current_scope().next_field_id > 0
    }

    fn current_scope(&self) -> &VarAccessPerScope {
        self.scopes.last().expect("no scope entered")
    }

    fn current_scope_mut(&mut self) -> &mut VarAccessPerScope {
        self.scopes.last_mut().expect("no scope entered")
    }

    fn current_function(&self) -> &VarAccessPerFunction {
        self.functions.last().expect("missing function")
    }

    fn scope_for_var(&mut self, var_id: NestedVarId) -> &mut VarAccessPerScope {
        let NestedScopeId(idx) = self.get_var(var_id).scope_id;
        &mut self.scopes[idx]
    }

    pub(super) fn local_var_id(&self, var_id: NestedVarId) -> VarId {
        assert!(var_id.0 >= self.current_function().start_var_id);
        VarId(var_id.0 - self.current_function().start_var_id)
    }

    pub(super) fn maybe_allocate_in_context(&mut self, var_id: NestedVarId) -> IdentType {
        if var_id.0 < self.current_function().start_var_id {
            todo!()
            //let field_id = self.ensure_context_allocated(var_id);
            //let NestedScopeId(level) = self.scope_for_var(var_id).id;
            //IdentType::Context(OuterContextIdx(level), field_id)
        } else {
            IdentType::Var(self.local_var_id(var_id))
        }
    }

    fn ensure_context_allocated(&mut self, var_id: NestedVarId) -> ContextFieldId {
        match self.get_var(var_id).location {
            VarLocation::Context(_scope_id, field_id) => return field_id,
            VarLocation::Stack => {}
        }

        // Allocate slot in context class.
        let scope = self.scope_for_var(var_id);
        let field_idx = ContextFieldId(scope.next_field_id);
        scope.next_field_id += 1;
        let NestedScopeId(nested_id) = scope.id;
        let function_id = self.get_var(var_id).function_id;
        let function_scope_id = self.functions[function_id].start_scope_id;
        let scope_id = ScopeId(nested_id - function_scope_id);
        self.vars[var_id.0].location = VarLocation::Context(scope_id, field_idx);

        field_idx
    }

    pub(super) fn add_var(&mut self, name: Name, ty: SourceType, mutable: bool) -> NestedVarId {
        let id = NestedVarId(self.vars.len());
        let context_id = self.scopes.len() - self.current_function().start_scope_id;
        let context_id = InnerContextId(context_id);

        let var = VarDefinition {
            id,
            context_id,
            name,
            ty,
            mutable,
            location: VarLocation::Stack,
            scope_id: self.current_scope().id,
            function_id: self.current_function().id,
        };

        self.vars.push(var);
        self.current_scope_mut().vars.push(id);

        id
    }

    pub(super) fn get_var(&self, idx: NestedVarId) -> &VarDefinition {
        &self.vars[idx.0]
    }

    fn enter_function_scope(&mut self) {
        let scope_id = self.scopes.len();

        self.scopes.push(VarAccessPerScope {
            id: NestedScopeId(scope_id),
            next_field_id: 0,
            vars: Vec::new(),
        });
        self.functions.push(VarAccessPerFunction {
            id: self.functions.len(),
            start_scope_id: scope_id,
            start_var_id: self.vars.len(),
        });
    }

    fn enter_block_scope(&mut self) {
        self.scopes.push(VarAccessPerScope {
            id: NestedScopeId(self.scopes.len()),
            next_field_id: 0,
            vars: Vec::new(),
        });
    }

    fn leave_function_scope(&mut self) -> Vec<VarDefinition> {
        let _ = self.scopes.pop().expect("missing scope");
        let function = self.functions.pop().expect("missing function");

        self.vars.drain(function.start_var_id..).collect()
    }

    fn leave_block_scope(&mut self) {
        self.scopes.pop().expect("missing scope");
    }
}

#[derive(Clone, Debug)]
pub struct VarDefinition {
    pub id: NestedVarId,
    pub context_id: InnerContextId,
    pub name: Name,
    pub ty: SourceType,
    pub mutable: bool,
    pub location: VarLocation,
    pub scope_id: NestedScopeId,
    pub function_id: usize,
}
