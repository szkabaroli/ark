use std::sync::Arc;

use parser::ast;

use crate::{
    error::msg::ErrorMessage,
    sema::{CallType, FctDefinitionId},
    SourceType, SourceTypeArray, SymbolKind,
};

use super::{expr::check_expr, function::TypeCheck, lookup::MethodLookup};

pub(super) fn check_expr_call(
    ck: &mut TypeCheck,
    e: &ast::Call,
    expected_ty: SourceType,
) -> SourceType {
    let (callee, type_params) = (&e.callee, SourceTypeArray::empty());
    //let (callee, type_params) = if let Some(expr_type_params) = e.callee.to_type_param() {
    //    let type_params: Vec<SourceType> = expr_type_params
    //        .args
    //        .iter()
    //        .map(|p| ck.read_type(p))
    //        .collect();
    //    let type_params: SourceTypeArray = SourceTypeArray::with(type_params);
    //    (&expr_type_params.callee, type_params)
    //} else {
    //    (&e.callee, SourceTypeArray::empty())
    //};

    let arg_types: Vec<SourceType> = e
        .args
        .iter()
        .map(|arg| check_expr(ck, arg, SourceType::Any))
        .collect();

    if let Some(expr_ident) = callee.to_ident() {
        let sym = ck.symtable.get_string(ck.sa, &expr_ident.name);

        check_expr_call_sym(ck, e, expected_ty, callee, sym, type_params, &arg_types)
    //} else if let Some(expr_dot) = callee.to_dot() {
    //    let object_type = check_expr(ck, &expr_dot.lhs, SourceType::Any);

    //    let method_name = match expr_dot.rhs.to_ident() {
    //        Some(ident) => ident.name.clone(),

    //        None => {
    //            let msg = ErrorMessage::NameExpected;
    //            ck.sa.report(ck.file_id, e.span, msg);

    //            ck.analysis.set_ty(e.id, SourceType::Error);
    //            return SourceType::Error;
    //        }
    //    };
    //    check_expr_call_method(ck, e, object_type, method_name, type_params, &arg_types)
    } else if let Some(_expr_path) = callee.to_path() {
        todo!()
        // check_expr_call_path(ck, e, expected_ty, callee, type_params, &arg_types)
    } else {
        todo!()
        //if !type_params.is_empty() {
        //    let msg = ErrorMessage::NoTypeParamsExpected;
        //    ck.sa.report(ck.file_id, e.callee.span(), msg);
        //}

        //let expr_type = check_expr(ck, callee, SourceType::Any);
        //check_expr_call_expr(ck, e, expr_type, &arg_types)
    }
}

fn check_expr_call_sym(
    ck: &mut TypeCheck,
    e: &ast::Call,
    expected_ty: SourceType,
    callee: &ast::ExprKind,
    sym: Option<SymbolKind>,
    type_params: SourceTypeArray,
    arg_types: &[SourceType],
) -> SourceType {
    match sym {
        Some(SymbolKind::Fct(fct_id)) => {
            check_expr_call_fct(ck, e, fct_id, type_params, &arg_types)
        }

        //Some(SymbolKind::Class(cls_id)) => {
        //    check_expr_call_class(ck, e, expected_ty, cls_id, type_params, &arg_types)
        //}

        //Some(SymbolKind::Struct(struct_id)) => {
        //    check_expr_call_struct(ck, e, struct_id, type_params, &arg_types)
        // }

        //Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => check_enum_value_with_args(
        //    ck,
        //    e,
        //    expected_ty,
        //    enum_id,
        //    type_params,
        //    variant_idx,
        //    &arg_types,
        //),
        _ => {
            todo!()
            //if !type_params.is_empty() {
            //    let msg = ErrorMessage::NoTypeParamsExpected;
            //    ck.sa.report(ck.file_id, e.callee.span(), msg);
            //}

            //let expr_type = check_expr(ck, callee, SourceType::Any);
            //check_expr_call_expr(ck, e, expr_type, &arg_types)
        }
    }
}

fn check_expr_call_fct(
    ck: &mut TypeCheck,
    e: &ast::Call,
    fct_id: FctDefinitionId,
    type_params: SourceTypeArray,
    arg_types: &[SourceType],
) -> SourceType {
    //if !fct_accessible_from(ck.sa, fct_id, ck.module_id) {
    //    let fct = ck.sa.function(fct_id);
    //    let msg = ErrorMessage::NotAccessible(fct.display_name(ck.sa));
    //    ck.sa.report(ck.file_id, e.span, msg);
    //}

    let lookup = MethodLookup::new(ck.sa, ck.file_id /* , ck.type_param_defs*/)
        .span(e.span)
        .callee(fct_id)
        .args(&arg_types)
        .fct_type_params(&type_params)
        .find();

    let ty = if lookup.find() {
        let call_type = CallType::Function(fct_id, type_params.clone());
        ck.analysis.map_calls.insert(e.id, Arc::new(call_type));

        lookup.found_ret().unwrap()
    } else {
        SourceType::Unknown
    };

    ck.analysis.set_ty(e.id, ty.clone());

    ty
}
