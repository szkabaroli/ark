use crate::{error::ErrorMessage, operator_registry::Operator, sym::SymbolKind, Sema};
use arkc_hir::{hir, ty};

use super::{
    check_expr_call,
    literals::{check_lit_float, check_lit_int},
    TypeCheck,
};

pub(super) fn check_expr(
    ck: &mut TypeCheck,
    expr: &hir::Expr,
    expected_ty: Box<ty::Type>,
) -> ty::Type {
    let ty = match *expr.kind {
        hir::ExprKind::Lit(ref lit) => check_expr_literal(ck, lit, expected_ty),
        hir::ExprKind::Call(ref call) => check_expr_call(ck, call, expected_ty),
        hir::ExprKind::Struct(ref stru) => check_expr_struct(ck, stru, expected_ty),
        hir::ExprKind::Return(ref expr) => check_expr_return(ck, expr, expected_ty),
        hir::ExprKind::Ident(ref ident) => check_expr_ident(ck, ident, expected_ty),
        hir::ExprKind::Bin(ref op, ref lhs, ref rhs) => {
            check_expr_bin(ck, op, lhs, rhs, expected_ty)
        }
        hir::ExprKind::Block(_) => todo!(),
        hir::ExprKind::Dot(_) => todo!(),
        hir::ExprKind::Path(_) => todo!(),
    };

    ck.types.insert(expr.hir_id.clone(), ty.clone());

    ty
}

fn check_expr_bin_bool(
    ck: &mut TypeCheck,
    op: &hir::BinOp,
    lhs_type: ty::Type,
    rhs_type: ty::Type,
) -> ty::Type {
    check_type(
        ck,
        op,
        op.kind,
        lhs_type,
        rhs_type,
        ty::Type::Primitive(ty::PrimitiveType::Bool),
    );
    //ck.analysis.set_ty(e.id, ty::Type::Primitive(ty::PrimitiveType::Bool));

    ty::Type::Primitive(ty::PrimitiveType::Bool)
}

pub(super) fn check_expr_bin(
    ck: &mut TypeCheck,
    op: &hir::BinOp,
    lhs: &hir::Expr,
    rhs: &hir::Expr,
    _expected_ty: Box<ty::Type>,
) -> ty::Type {
    //if op.is_any_assign() {
    //    check_expr_assign(ck, e);
    //    return ty::Type::Primitive(ty::PrimitiveType::Unit);
    //}

    let lhs_type = check_expr(ck, lhs, Box::new(ty::Type::Any));
    let rhs_type = check_expr(ck, rhs, Box::new(ty::Type::Any));

    match op.kind {
        hir::BinOpKind::Or | hir::BinOpKind::And => check_expr_bin_bool(ck, op, lhs_type, rhs_type),
        //hir::BinOpKind::Cmp(cmp) => check_expr_bin_cmp(ck, e, cmp, lhs_type, rhs_type),
        hir::BinOpKind::Add => check_operator_overload(ck, op, lhs_type, rhs_type),
        hir::BinOpKind::Sub => check_operator_overload(ck, op, lhs_type, rhs_type),
        hir::BinOpKind::Mul => check_operator_overload(ck, op, lhs_type, rhs_type),
        hir::BinOpKind::Div => check_operator_overload(ck, op, lhs_type, rhs_type),
        _ => unimplemented!(),
    }
}

fn check_operator_overload(
    ck: &mut TypeCheck,
    op: &hir::BinOp,
    //trait_id: TraitDefinitionId,
    //trait_method_name: &str,
    lhs_type: ty::Type,
    rhs_type: ty::Type,
) -> ty::Type {
    if let Some(operator) = ck.sa.operators.lookup(&op.kind, &lhs_type, &rhs_type) {
        if let Operator::BuiltIn(return_ty) = operator {
            return return_ty;
        } else {
            todo!("operator overloads")
        }
    } else {
        let lhs_type = ck.ty_name(&lhs_type);
        let rhs_type = ck.ty_name(&rhs_type);
        let msg = ErrorMessage::BinOpType(op.kind.as_str().into(), lhs_type, rhs_type);

        panic!("error: {:?}", msg);
        //ck.sa.report(ck.file_id, e.span, msg);
        //ck.analysis.set_ty(e.id, ty::Type::Error);

        ty::Type::Error
    }
}

fn check_expr_ident(
    ck: &mut TypeCheck,
    id: &hir::Identifier,
    expected_ty: Box<ty::Type>,
) -> ty::Type {
    let interned_name = ck.sa.interner.intern(&id.name);
    let sym = ck.symtable.get(interned_name);

    let ty = match sym {
        Some(SymbolKind::Var(var_id)) => {
            let ty = ck.vars.get_var(var_id).ty.clone();

            // Variable may have to be context-allocated.
            let ident = ck.maybe_allocate_in_context(var_id);
            ck.analysis.idents.insert(id.hir_id, ident);

            ty
        }
        None => {
            panic!("{:?}", ErrorMessage::UnknownIdentifier(id.name.clone()));
            //ck.sa.report(
            //
            //    e.span,
            //    ErrorMessage::UnknownIdentifier(e.name.clone()),
            //);
            ty::Type::Error
        }
        _ => {
            panic!("{:?}", ErrorMessage::ValueExpected);

            //ck.sa.report(ck.file_id, e.span, ErrorMessage::ValueExpected);
            ty::Type::Error
        }
    };

    ck.types.insert(id.hir_id.clone(), ty.clone());

    ty
}

pub(super) fn check_expr_literal(
    ck: &mut TypeCheck,
    lit: &hir::Literal,
    expected_ty: Box<ty::Type>,
) -> ty::Type {
    match lit.kind {
        hir::LiteralKind::Int(ref value) => {
            let (ty, int, float) = check_lit_int(value, false, expected_ty);
            ck.analysis.int_literals.insert(lit.hir_id, int);
            ck.types.insert(lit.hir_id.clone(), ty.clone());

            ty
        }
        hir::LiteralKind::Float(ref value) => {
            let (ty, value) = check_lit_float(lit.hir_id, value, false);
            //ck.analysis.set_literal_value(lit.hir_id, 0, value);
            ty
        }
        hir::LiteralKind::String(_) => todo!(),
        hir::LiteralKind::Bool(value) => {
            //ck.analysis.set_literal_value(lit.hir_id, 0, value);
            ty::Type::Primitive(ty::PrimitiveType::Bool)
        }
        hir::LiteralKind::Char(_) => todo!(),
        hir::LiteralKind::Unit => todo!(),
    }
}

pub(super) fn check_field_value(ck: &mut TypeCheck, fv: &hir::FieldValue) -> ty::Type {
    // TODO: expect type based on struct defintion
    check_expr(ck, &fv.value, Box::from(ty::Type::Any))
}

pub(super) fn check_expr_struct(
    ck: &mut TypeCheck,
    stru: &hir::ExprStruct,
    expected_ty: Box<ty::Type>,
) -> ty::Type {
    if let Some(expr_ident) = stru.name.to_ident() {
        let sym = ck.symtable.get_string(ck.sa, &expr_ident.name);

        for field in stru.fields.iter() {
            let ty = check_field_value(ck, field);
            ck.types.insert(field.name.hir_id.clone(), ty);
        }

        match sym {
            Some(SymbolKind::Struct(struct_id)) => ty::Type::Struct(struct_id),
            _ => unreachable!(),
        }
    } else {
        panic!();
        //let expr_type = check_expr(ck, callee, ty::Type::Any);
        //check_expr_call_expr(ck, e, expr_type, &arg_types)
    }
}

pub(super) fn check_expr_return(
    ck: &mut TypeCheck,
    expr: &Option<hir::Expr>,
    _expected_ty: Box<ty::Type>,
) -> ty::Type {
    if let Some(ref return_type) = ck.return_type {
        let expected_ty = return_type.clone();

        let expr_type = expr
            .as_ref()
            .map(|expr| check_expr(ck, &expr, expected_ty.clone()))
            .unwrap_or(ty::Type::Primitive(ty::PrimitiveType::Unit));

        ck.check_fn_return_type(expected_ty, expr_type);
    } else {
        panic!("error {:?}", ErrorMessage::InvalidReturn);
        // ck.sa.report(expr.span, ErrorMessage::InvalidReturn);
        // check_expr(ck, expr.as_ref(), ty::Type::Any);
    }

    ty::Type::Primitive(ty::PrimitiveType::Unit)
}

pub(super) fn check_type(
    ck: &mut TypeCheck,
    e: &hir::BinOp,
    op: hir::BinOpKind,
    lhs_type: ty::Type,
    rhs_type: ty::Type,
    expected_type: ty::Type,
) {
    if !expected_type.allows(lhs_type.clone()) || !expected_type.allows(rhs_type.clone()) {
        let op = op.as_str().into();
        let lhs_type = ck.ty_name(&lhs_type);
        let rhs_type = ck.ty_name(&rhs_type);
        let msg = ErrorMessage::BinOpType(op, lhs_type, rhs_type);

        panic!("error {:?}", msg);
        //ck.sa.report(e.span, msg);
    }
}

pub(super) fn read_path_expr(
    ck: &mut TypeCheck,
    expr: &hir::Expr,
) -> Result<Option<SymbolKind>, ()> {
    if let Some(expr_path) = expr.to_path() {
        let sym = read_path_expr(ck, &expr_path.lhs)?;

        let element_name = if let Some(ident) = expr_path.rhs.to_ident() {
            ident.name.clone()
        } else {
            let msg = ErrorMessage::ExpectedSomeIdentifier;
            println!("{:?}", msg);
            //ck.sa.report(ck.file_id, expr_path.rhs.span(), msg);
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
                println!("{:?}", msg);
                //ck.sa.report(ck.file_id, expr.span(), msg);
                Err(())
            }
        }
    } else if let Some(expr_ident) = expr.to_ident() {
        let sym = ck.symtable.get_string(ck.sa, &expr_ident.name);

        Ok(sym)
    } else {
        let msg = ErrorMessage::ExpectedSomeIdentifier;
        println!("{:?}", msg);
        //ck.sa.report(ck.file_id, expr.span(), msg);
        Err(())
    }
}
