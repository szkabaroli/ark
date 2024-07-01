use arkc_hir::{hir, ty};

use crate::error::ErrorMessage;

use super::{add_local, expr::check_expr, TypeCheck};

pub(super) fn check_stmt(ck: &mut TypeCheck, s: &hir::Statement) {
    match *s.kind {
        hir::StatementKind::Let(ref stmt) => check_stmt_let(ck, stmt),
        hir::StatementKind::Expr(ref expr) => {
            check_expr(ck, &expr, Box::from(ty::Type::Any));
        }
    }
}

fn check_stmt_let(ck: &mut TypeCheck, binding: &hir::LetBinding) {
    let defined_type = if let Some(ref data_type) = binding.ty {
        ck.read_type(data_type)
    } else {
        ty::Type::Any
    };

    let expr_type = binding
        .expr
        .as_ref()
        .map(|expr| check_expr(ck, &expr, Box::from(defined_type.clone())))
        .unwrap_or(ty::Type::Any);

    let defined_type = if binding.ty.is_some() {
        defined_type
    } else {
        expr_type.clone()
    };

    if !defined_type.is_unknown() && !defined_type.is_defined_type() {
        panic!("{:?}", ErrorMessage::VarNeedsTypeOrExpression);
        //ck.sa.report( s.span, ErrorMessage::VarNeedsTypeOrExpression);
        return;
    }

    // update type of variable, necessary when stmt has initializer expression but no type
    check_let_pattern(ck, &binding.pattern, defined_type.clone());

    if binding.expr.is_some() {
        if !expr_type.is_unknown()
            && !defined_type.is_unknown()
            && !defined_type.allows(expr_type.clone())
        {
            let name = binding.pattern.to_name().unwrap();
            let defined_type = ck.ty_name(&defined_type);
            let expr_type = ck.ty_name(&expr_type);
            let msg = ErrorMessage::AssignType(name, defined_type, expr_type);

            panic!("{:?}", msg);
            //ck.sa.report( s.span, msg);
        }

    // let variable binding needs to be assigned
    } else {
        panic!("{:?}", ErrorMessage::LetMissingInitialization);
        //ck.sa
        //    .report(ck.file_id, s.span, ErrorMessage::LetMissingInitialization);
    }
}

pub(super) fn check_let_pattern(ck: &mut TypeCheck, pattern: &hir::PatternKind, ty: ty::Type) {
    match pattern {
        hir::PatternKind::Ident(ref ident) => {
            // let name = ck.sa.interner.intern(&ident.name);
            let var_id = ck.vars.add_var(ident.clone(), ty, true);

            add_local(ck.sa, ck.symtable, ck.vars, var_id);
            ck.analysis
                .map_vars
                .insert(ident.hir_id, ck.vars.local_var_id(var_id));
        }
    }
}
