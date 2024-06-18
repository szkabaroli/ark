use parser::ast;

use crate::{error::msg::ErrorMessage, Name, SourceType, SourceTypeArray, SymbolKind};

use super::{
    check_expr_call, check_expr_return, check_lit_float, check_lit_int, check_stmt, TypeCheck,
};

pub(super) fn check_expr(
    ck: &mut TypeCheck,
    e: &ast::ExprData,
    expected_ty: SourceType,
) -> SourceType {
    match *e {
        ast::ExprData::LitChar(ref expr) => check_expr_lit_char(ck, expr, expected_ty),
        ast::ExprData::LitInt(ref expr) => check_expr_lit_int(ck, expr, false, expected_ty),
        ast::ExprData::LitFloat(ref expr) => check_expr_lit_float(ck, expr, false, expected_ty),
        //ast::ExprData::LitStr(ref expr) => check_expr_lit_str(ck, expr, expected_ty),
        //ast::ExprData::Template(ref expr) => check_expr_template(ck, expr, expected_ty),
        ast::ExprData::LitBool(ref expr) => check_expr_lit_bool(ck, expr, expected_ty),
        ast::ExprData::Ident(ref expr) => check_expr_ident(ck, expr, expected_ty),
        ast::ExprData::Un(ref expr) => check_expr_un(ck, expr, expected_ty),
        ast::ExprData::Bin(ref expr) => check_expr_bin(ck, expr, expected_ty),
        ast::ExprData::Call(ref expr) => check_expr_call(ck, expr, expected_ty),
        //ast::ExprData::TypeParam(ref expr) => check_expr_type_param(ck, expr, expected_ty),
        ast::ExprData::Path(ref expr) => check_expr_path(ck, expr, expected_ty),
        ast::ExprData::Dot(ref expr) => todo!(),//check_expr_dot(ck, expr, expected_ty),
        //ast::ExprData::This(ref expr) => check_expr_this(ck, expr, expected_ty),
        //ast::ExprData::Conv(ref expr) => check_expr_conv(ck, expr, expected_ty),
        //ast::ExprData::Is(ref expr) => check_expr_is(ck, expr, expected_ty),
        //ast::ExprData::Lambda(ref expr) => check_expr_lambda(ck, expr, expected_ty),
        ast::ExprData::Block(ref expr) => check_expr_block(ck, expr, expected_ty),
        //ast::ExprData::If(ref expr) => check_expr_if(ck, expr, expected_ty),
        //ast::ExprData::Tuple(ref expr) => check_expr_tuple(ck, expr, expected_ty),
        ast::ExprData::Paren(ref expr) => check_expr_paren(ck, expr, expected_ty),
        //ast::ExprData::Match(ref expr) => check_expr_match(ck, expr, expected_ty),
        //ast::ExprData::For(ref expr) => check_expr_for(ck, expr, expected_ty),
        //ast::ExprData::While(ref expr) => check_expr_while(ck, expr, expected_ty),
        ast::ExprData::Return(ref expr) => check_expr_return(ck, expr, expected_ty),
        //ast::ExprData::Break(..) | ast::ExprData::Continue(..) => {
        //    check_expr_break_and_continue(ck, e, expected_ty)
        //}
        ast::ExprData::Error { .. } => SourceType::Unknown,
    }
}

pub(super) fn read_path_expr(
    ck: &mut TypeCheck,
    expr: &ast::ExprData,
) -> Result<Option<SymbolKind>, ()> {
    if let Some(expr_path) = expr.to_path() {
        let sym = read_path_expr(ck, &expr_path.lhs)?;

        let element_name = if let Some(ident) = expr_path.rhs.to_ident() {
            ident.name.clone()
        } else {
            let msg = ErrorMessage::ExpectedSomeIdentifier;
            ck.sa.report(ck.file_id, expr_path.rhs.span(), msg);
            return Err(());
        };

        let interned_element_name = ck.sa.interner.intern(&element_name);

        match sym {
            Some(SymbolKind::Module(module_id)) => {
                let module = ck.sa.module(module_id);
                let symtable = module.table();
                let sym = symtable.get(interned_element_name);

                Ok(sym)
            }

            _ => {
                let msg = ErrorMessage::ExpectedModule;
                ck.sa.report(ck.file_id, expr.span(), msg);
                Err(())
            }
        }
    } else if let Some(expr_ident) = expr.to_ident() {
        let sym = ck.symtable.get_string(ck.sa, &expr_ident.name);

        Ok(sym)
    } else {
        let msg = ErrorMessage::ExpectedSomeIdentifier;
        ck.sa.report(ck.file_id, expr.span(), msg);
        Err(())
    }
}

pub(super) fn check_expr_path(
    ck: &mut TypeCheck,
    e: &ast::ExprPathType,
    expected_ty: SourceType,
) -> SourceType {
    let (container_expr, type_params) = (&e.lhs, SourceTypeArray::empty());
    //let (container_expr, type_params) = if let Some(expr_type_params) = e.lhs.to_type_param() {
    //    let type_params: Vec<SourceType> = expr_type_params
    //        .args
    //        .iter()
    //        .map(|p| ck.read_type(p))
    //        .collect();
    //    let type_params: SourceTypeArray = SourceTypeArray::with(type_params);
    //
    //    (&expr_type_params.callee, type_params)
    //} else {
    //    (&e.lhs, SourceTypeArray::empty())
    //};

    let sym = match read_path_expr(ck, container_expr) {
        Ok(sym) => sym,
        Err(()) => {
            ck.analysis.set_ty(e.id, SourceType::Unknown);
            return SourceType::Unknown;
        }
    };

    let element_name = if let Some(ident) = e.rhs.to_ident() {
        ident.name.clone()
    } else {
        let msg = ErrorMessage::ExpectedSomeIdentifier;
        ck.sa.report(ck.file_id, e.rhs.span(), msg);
        return SourceType::Unknown;
    };

    match sym {
        //Some(SymbolKind::Enum(id)) => check_enum_value_without_args(
        //    ck,
        //     e.id,
        //    e.op_span,
        //    expected_ty,
        //    id,
        //    type_params,
        //    element_name,
        //),

        //Some(SymbolKind::Module(module_id)) => {
        //    check_expr_path_module(ck, e, expected_ty, module_id, element_name)
        //}
        _ => {
            let msg = ErrorMessage::InvalidLeftSideOfSeparator;
            ck.sa.report(ck.file_id, e.lhs.span(), msg);

            ck.analysis.set_ty(e.id, SourceType::Unknown);
            SourceType::Unknown
        }
    }
}

pub(super) fn check_expr_ident(
    ck: &mut TypeCheck,
    e: &ast::ExprIdentType,
    expected_ty: SourceType,
) -> SourceType {
    let interned_name: Name = ck.sa.interner.intern(&e.name);
    let sym = ck.symtable.get(interned_name);

    match sym {
        Some(SymbolKind::Var(var_id)) => {
            let ty = ck.vars.get_var(var_id).ty.clone();
            ck.analysis.set_ty(e.id, ty.clone());

            // Variable may have to be context-allocated.
            let ident = ck.maybe_allocate_in_context(var_id);
            ck.analysis.map_idents.insert(e.id, ident);

            ty
        }

        //Some(SymbolKind::Global(globalid)) => {
        //    let global_var = ck.sa.global(globalid);
        //    let ty = global_var.ty();
        //    ck.analysis.set_ty(e.id, ty.clone());

        //    ck.analysis
        //        .map_idents
        //        .insert(e.id, IdentType::Global(globalid));

        //    ty
        //}

        // Some(SymbolKind::Const(const_id)) => {
        //    let const_ = ck.sa.const_(const_id);
        //    ck.analysis.set_ty(e.id, const_.ty());

        //    ck.analysis
        //        .map_idents
        //        .insert(e.id, IdentType::Const(const_id));

        //    const_.ty()
        //}

        //Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => check_enum_value_without_args_id(
        //    ck,
        //    e.id,
        //    e.span,
        //    expected_ty,
        //    enum_id,
        //    SourceTypeArray::empty(),
        //    variant_idx,
        //),
        None => {
            ck.sa.report(
                ck.file_id,
                e.span,
                ErrorMessage::UnknownIdentifier(e.name.clone()),
            );
            SourceType::Unknown
        }

        _ => {
            ck.sa
                .report(ck.file_id, e.span, ErrorMessage::ValueExpected);
            SourceType::Unknown
        }
    }
}

fn check_expr_lit_bool(
    ck: &mut TypeCheck,
    e: &ast::ExprLitBoolType,
    _expected_ty: SourceType,
) -> SourceType {
    ck.analysis.set_ty(e.id, SourceType::Bool);

    SourceType::Bool
}

fn check_expr_lit_char(
    ck: &mut TypeCheck,
    e: &ast::ExprLitCharType,
    _expected_ty: SourceType,
) -> SourceType {
    todo!();
    //let value = check_lit_char(ck.sa, ck.file_id, e);

    //ck.analysis.set_ty(e.id, SourceType::Char);
    //ck.analysis.set_literal_char(e.id, value);

    SourceType::Char
}

pub(super) fn check_expr_lit_int(
    ck: &mut TypeCheck,
    e: &ast::ExprLitIntType,
    negate: bool,
    expected_ty: SourceType,
) -> SourceType {
    let (ty, value_i64, value_f64) = check_lit_int(ck.sa, ck.file_id, e, negate, expected_ty);

    ck.analysis.set_ty(e.id, ty.clone());
    ck.analysis.set_literal_value(e.id, value_i64, value_f64);

    ty
}

fn check_expr_lit_float(
    ck: &mut TypeCheck,
    e: &ast::ExprLitFloatType,
    negate: bool,
    _expected_ty: SourceType,
) -> SourceType {
    let (ty, value) = check_lit_float(ck.sa, ck.file_id, e, negate);

    ck.analysis.set_ty(e.id, ty.clone());
    ck.analysis.set_literal_value(e.id, 0, value);

    ty
}

pub(super) fn check_expr_block(
    ck: &mut TypeCheck,
    block: &ast::ExprBlockType,
    _expected_ty: SourceType,
) -> SourceType {
    ck.symtable.push_level();

    for stmt in &block.stmts {
        check_stmt(ck, stmt);
    }

    let ty = if let Some(ref expr) = block.expr {
        check_expr(ck, expr, SourceType::Any)
    } else {
        SourceType::Unit
    };

    ck.analysis.set_ty(block.id, ty.clone());
    ck.symtable.pop_level();

    ty
}

pub(super) fn check_expr_paren(
    ck: &mut TypeCheck,
    paren: &ast::ExprParenType,
    _expected_ty: SourceType,
) -> SourceType {
    let ty = check_expr(ck, &paren.expr, SourceType::Any);
    ck.analysis.set_ty(paren.id, ty.clone());

    ty
}

pub(super) fn check_expr_assign(ck: &mut TypeCheck, e: &ast::ExprBinType) {
    if e.lhs.is_ident() {
        check_expr_assign_ident(ck, e);
    //} else if e.lhs.is_dot() {
    //    check_expr_assign_field(ck, e);
    //} else if e.lhs.is_call() {
    //    check_expr_assign_call(ck, e);
    } else {
        ck.sa
            .report(ck.file_id, e.span, ErrorMessage::LvalueExpected);
    }

    ck.analysis.set_ty(e.id, SourceType::Unit);
}

fn check_expr_assign_ident(ck: &mut TypeCheck, e: &ast::ExprBinType) {
    ck.analysis.set_ty(e.id, SourceType::Unit);

    let lhs_ident = e.lhs.to_ident().unwrap();
    let sym = ck.symtable.get_string(ck.sa, &lhs_ident.name);

    let lhs_type = match sym {
        Some(SymbolKind::Var(var_id)) => {
            if !ck.vars.get_var(var_id).mutable {
                ck.sa
                    .report(ck.file_id, e.span, ErrorMessage::LetReassigned);
            }

            // Variable may have to be context-allocated.
            let ident = ck.maybe_allocate_in_context(var_id);
            ck.analysis.map_idents.insert(e.lhs.id(), ident);

            ck.vars.get_var(var_id).ty.clone()
        }

        //Some(SymbolKind::Global(global_id)) => {
        //    let global_var = ck.sa.global(global_id);

        //    if !e.initializer && !global_var.mutable {
        //        ck.sa
        //            .report(ck.file_id, e.span, ErrorMessage::LetReassigned);
        //    }

        //    ck.analysis
        //        .map_idents
        //        .insert(e.lhs.id(), IdentType::Global(global_id));
        //    global_var.ty()
        //}
        None => {
            ck.sa.report(
                ck.file_id,
                lhs_ident.span,
                ErrorMessage::UnknownIdentifier(lhs_ident.name.clone()),
            );

            return;
        }

        _ => {
            ck.sa
                .report(ck.file_id, lhs_ident.span, ErrorMessage::LvalueExpected);

            return;
        }
    };

    let rhs_type = check_expr(ck, &e.rhs, lhs_type.clone());

    if !lhs_type.is_unknown() && !rhs_type.is_unknown() && !lhs_type.allows(ck.sa, rhs_type.clone())
    {
        let ident = e.lhs.to_ident().unwrap();
        let lhs_type = ck.ty_name(&lhs_type);
        let rhs_type = ck.ty_name(&rhs_type);

        ck.analysis.set_ty(e.id, SourceType::Unit);

        let msg = ErrorMessage::AssignType(ident.name.clone(), lhs_type, rhs_type);
        ck.sa.report(ck.file_id, e.span, msg);
    }
}

/*fn check_expr_assign_call(ck: &mut TypeCheck, e: &ast::ExprBinType) {
    let call = e.lhs.to_call().unwrap();
    let expr_type = check_expr(ck, &call.callee, SourceType::Any);

    let mut arg_types: Vec<SourceType> = call
        .args
        .iter()
        .map(|arg| check_expr(ck, arg, SourceType::Any))
        .collect();

    let value_type = check_expr(ck, &e.rhs, SourceType::Any);

    let name = ck.sa.interner.intern("set");
    arg_types.push(value_type);

    if let Some(descriptor) = find_method(
        ck,
        e.span,
        expr_type.clone(),
        false,
        name,
        &arg_types,
        &SourceTypeArray::empty(),
    ) {
        let call_type = CallType::Expr(expr_type, descriptor.fct_id, descriptor.type_params);
        ck.analysis
            .map_calls
            .insert_or_replace(e.id, Arc::new(call_type));
    }
}*/

fn check_expr_bin_trait(
    ck: &mut TypeCheck,
    e: &ast::ExprBinType,
    op: ast::BinOp,
    trait_method_name: &str,
    lhs_type: SourceType,
    rhs_type: SourceType,
) -> SourceType {
    let lhs_type = ck.ty_name(&lhs_type);
    let rhs_type = ck.ty_name(&rhs_type);
    let msg = ErrorMessage::BinOpType(op.as_str().into(), lhs_type, rhs_type);

    ck.sa.report(ck.file_id, e.span, msg);

    ck.analysis.set_ty(e.id, SourceType::Unknown);

    SourceType::Unknown
}

pub(super) fn check_expr_bin(
    ck: &mut TypeCheck,
    e: &ast::ExprBinType,
    _expected_ty: SourceType,
) -> SourceType {
    if e.op.is_any_assign() {
        check_expr_assign(ck, e);
        return SourceType::Unit;
    }

    let lhs_type = check_expr(ck, &e.lhs, SourceType::Any);
    let rhs_type = check_expr(ck, &e.rhs, SourceType::Any);

    if lhs_type.is_unknown() || rhs_type.is_unknown() {
        ck.analysis.set_ty(e.id, SourceType::Unknown);
        return SourceType::Unknown;
    }

    match e.op {
        ast::BinOp::Or | ast::BinOp::And => check_expr_bin_bool(ck, e, e.op, lhs_type, rhs_type),
        ast::BinOp::Cmp(cmp) => check_expr_bin_cmp(ck, e, cmp, lhs_type, rhs_type),
        ast::BinOp::Add => check_expr_bin_trait(ck, e, e.op, "add", lhs_type, rhs_type),
        ast::BinOp::Sub => check_expr_bin_trait(ck, e, e.op, "sub", lhs_type, rhs_type),
        ast::BinOp::Mul => check_expr_bin_trait(ck, e, e.op, "mul", lhs_type, rhs_type),
        ast::BinOp::Div => check_expr_bin_trait(ck, e, e.op, "div", lhs_type, rhs_type),
        ast::BinOp::Mod => check_expr_bin_trait(ck, e, e.op, "modulo", lhs_type, rhs_type),
        ast::BinOp::BitOr => check_expr_bin_trait(ck, e, e.op, "bitor", lhs_type, rhs_type),
        ast::BinOp::BitAnd => check_expr_bin_trait(ck, e, e.op, "bitand", lhs_type, rhs_type),
        ast::BinOp::BitXor => check_expr_bin_trait(ck, e, e.op, "bitxor", lhs_type, rhs_type),
        ast::BinOp::ShiftL => check_expr_bin_trait(ck, e, e.op, "shl", lhs_type, rhs_type),
        ast::BinOp::ArithShiftR => check_expr_bin_trait(ck, e, e.op, "sar", lhs_type, rhs_type),
        ast::BinOp::LogicalShiftR => check_expr_bin_trait(ck, e, e.op, "shr", lhs_type, rhs_type),
        ast::BinOp::Assign => unreachable!(),
    }
}

fn check_expr_bin_cmp(
    ck: &mut TypeCheck,
    e: &ast::ExprBinType,
    cmp: ast::CmpOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
) -> SourceType {
    match cmp {
        ast::CmpOp::Is | ast::CmpOp::IsNot => {
            if lhs_type != rhs_type {
                let lhs_type = ck.ty_name(&lhs_type);
                let rhs_type = ck.ty_name(&rhs_type);
                ck.sa.report(
                    ck.file_id,
                    e.span,
                    ErrorMessage::TypesIncompatible(lhs_type, rhs_type),
                );
            }
            //else if !lhs_type.is_cls() && !lhs_type.is_lambda() && !lhs_type.is_trait() {
            //    let lhs_type = ck.ty_name(&lhs_type);
            //    ck.sa.report(
            //        ck.file_id,
            //        e.span,
            //        ErrorMessage::ExpectedIdentityType(lhs_type),
            //    );
            //}

            ck.analysis.set_ty(e.id, SourceType::Bool);
            return SourceType::Bool;
        }

        ast::CmpOp::Eq | ast::CmpOp::Ne => {
            //if is_simple_enum(ck.sa, lhs_type.clone()) {
            //    check_expr_cmp_enum(ck, e, cmp, lhs_type, rhs_type)
            //} else {
            check_expr_bin_trait(ck, e, e.op, "equals", lhs_type, rhs_type);
            //}
        }

        ast::CmpOp::Ge | ast::CmpOp::Gt | ast::CmpOp::Le | ast::CmpOp::Lt => {
            check_expr_bin_trait(ck, e, e.op, "cmp", lhs_type, rhs_type);
        }
    }

    ck.analysis.set_ty(e.id, SourceType::Bool);

    SourceType::Bool
}

fn check_expr_bin_bool(
    ck: &mut TypeCheck,
    e: &ast::ExprBinType,
    op: ast::BinOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
) -> SourceType {
    check_type(ck, e, op, lhs_type, rhs_type, SourceType::Bool);
    ck.analysis.set_ty(e.id, SourceType::Bool);

    SourceType::Bool
}

pub(super) fn check_expr_un(
    ck: &mut TypeCheck,
    e: &ast::ExprUnType,
    expected_ty: SourceType,
) -> SourceType {
    if e.op == ast::UnOp::Neg && e.opnd.is_lit_int() {
        let expr_type = check_expr_lit_int(ck, e.opnd.to_lit_int().unwrap(), true, expected_ty);
        ck.analysis.set_ty(e.id, expr_type.clone());
        return expr_type;
    }

    let opnd = check_expr(ck, &e.opnd, SourceType::Any);

    match e.op {
        ast::UnOp::Neg => check_expr_un_trait(ck, e, e.op, opnd),
        ast::UnOp::Not => check_expr_un_trait(ck, e, e.op, opnd),
    }
}

fn check_expr_un_trait(
    ck: &mut TypeCheck,
    e: &ast::ExprUnType,
    op: ast::UnOp,
    ty: SourceType,
) -> SourceType {
    let ty = ck.ty_name(&ty);
    let msg = ErrorMessage::UnOpType(op.as_str().into(), ty);
    ck.sa.report(ck.file_id, e.span, msg);

    ck.analysis.set_ty(e.id, SourceType::Unknown);
    SourceType::Unknown
}

pub(super) fn check_type(
    ck: &mut TypeCheck,
    e: &ast::ExprBinType,
    op: ast::BinOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
    expected_type: SourceType,
) {
    if !expected_type.allows(ck.sa, lhs_type.clone())
        || !expected_type.allows(ck.sa, rhs_type.clone())
    {
        let op = op.as_str().into();
        let lhs_type = ck.ty_name(&lhs_type);
        let rhs_type = ck.ty_name(&rhs_type);
        let msg = ErrorMessage::BinOpType(op, lhs_type, rhs_type);

        ck.sa.report(ck.file_id, e.span, msg);
    }
}
