use crate::{error::ErrorMessage, sym::SymbolKind, Sema};
use arkc_hir::{
    hir::{self, HirId},
    ty,
};

use super::{
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
        hir::ExprKind::Call(ref call) => check_expr_call(ck, call),
        hir::ExprKind::Struct(ref stru) => check_expr_struct(ck, stru, expected_ty),
        hir::ExprKind::Return(ref expr) => check_expr_return(ck, expr, expected_ty),
        hir::ExprKind::Ident(ref ident) => check_expr_ident(ck, ident, expected_ty),
        hir::ExprKind::Block(_) => todo!(),
        hir::ExprKind::Dot(_) => todo!(),
    };

    ck.types.insert(expr.hir_id.clone(), ty.clone());

    ty
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
            ty::Type::Unknown
        }
        _ => {
            panic!("{:?}", ErrorMessage::ValueExpected);

            //ck.sa.report(ck.file_id, e.span, ErrorMessage::ValueExpected);
            ty::Type::Unknown
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

pub(super) fn check_expr_call(ck: &mut TypeCheck, expr: &hir::FnCall) -> ty::Type {
    ty::Type::Primitive(ty::PrimitiveType::Unit)
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
        panic!("error");
        // ck.sa.report(expr.span, ErrorMessage::InvalidReturn);
        // check_expr(ck, expr.as_ref(), ty::Type::Any);
    }

    ty::Type::Primitive(ty::PrimitiveType::Unit)
}
