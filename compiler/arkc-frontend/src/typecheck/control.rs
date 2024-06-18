use parser::ast;

use crate::{error::msg::ErrorMessage, SourceType};

use super::{expr::check_expr, function::TypeCheck};

pub(super) fn check_expr_return(
    ck: &mut TypeCheck,
    expr: &ast::ExprReturnType,
    _expected_ty: SourceType,
) -> SourceType {
    if let Some(ref return_type) = ck.return_type {
        let expected_ty = return_type.clone();

        let expr_type = expr
            .expr
            .as_ref()
            .map(|expr| check_expr(ck, &expr, expected_ty.clone()))
            .unwrap_or(SourceType::Unit);

        ck.check_fct_return_type(expected_ty, expr.span, expr_type);
    } else {
        ck.sa
            .report(ck.file_id, expr.span, ErrorMessage::InvalidReturn);

        if let Some(ref expr) = expr.expr {
            check_expr(ck, expr.as_ref(), SourceType::Any);
        }
    }

    SourceType::Unit
}