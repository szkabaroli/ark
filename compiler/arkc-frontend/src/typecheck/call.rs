use std::sync::Arc;

use arkc_hir::{
    hir::{self, HirId},
    ty,
};

use crate::{access::func_accessible_from, error::ErrorMessage, sym::SymbolKind};

use super::{
    expr::{check_expr, read_path_expr},
    function::check_args_compatible_func,
    CallArguments, TypeCheck,
};

pub fn check_expr_call(
    ck: &mut TypeCheck,
    expr: &hir::FnCall,
    expected_ty: Box<ty::Type>,
) -> ty::Type {
    let type_params = ty::TypeArray::empty();

    let arguments = create_call_arguments(ck, expr);

    if let Some(expr_ident) = expr.callee.to_ident() {
        let sym = ck.symtable.get_string(ck.sa, &expr_ident.name);
        check_expr_call_sym(
            ck,
            expr,
            expected_ty,
            &expr.callee,
            sym,
            type_params,
            arguments,
        )
    } else if let Some(expr_dot) = expr.callee.to_dot() {
        todo!()
        //let object_type = check_expr(ck, &expr_dot.lhs, Box::new(ty::Type::Any));

        //let method_name = match expr_dot.rhs.to_ident() {
        //    Some(ident) => ident.name.clone(),

        //    None => {
        //        let msg = ErrorMessage::NameExpected;
        //        panic!("{:?}", msg);
        //ck.sa.report(ck.file_id, e.span, msg);
        //ck.analysis.set_ty(e.id, ty_error());
        //        return ty::Type::Error;
        //    }
        //};
        //check_expr_call_method(ck, expr, object_type, method_name, arguments)
    } else if let Some(_expr_path) = expr.callee.to_path() {
        check_expr_call_path(ck, expr, expected_ty, &expr.callee, type_params, arguments)
    } else {
        //if !type_params.is_empty() {
        //    let msg = ErrorMessage::NoTypeParamsExpected;
        //    panic!("{:?}", msg);
        //ck.sa.report(ck.file_id, e.callee.span(), msg);
        //}

        let expr_type = check_expr(ck, &expr.callee, Box::new(ty::Type::Any));
        check_expr_call_expr(ck, expr, expr_type, arguments)
    }
}

pub(super) fn create_call_arguments(ck: &mut TypeCheck, e: &hir::FnCall) -> CallArguments {
    let mut arguments = CallArguments {
        arguments: Vec::with_capacity(e.args.len()),
        //span: e.span,
    };

    for arg in e.args.iter() {
        let ty = check_expr(ck, &arg.expr, Box::new(ty::Type::Any));
        //ck.analysis.set_ty(arg.id, ty);
        arguments.arguments.push(arg.clone());
    }

    arguments
}

fn check_expr_call_path(
    ck: &mut TypeCheck,
    e: &hir::FnCall,
    expected_ty: Box<ty::Type>,
    callee: &hir::Expr,
    type_params: ty::TypeArray,
    arguments: CallArguments,
) -> ty::Type {
    let callee_as_path = callee.to_path().unwrap();

    let (container_expr, container_type_params) =
        //if let Some(expr_type_params) = callee_as_path.lhs.to_type_param() {
        //    let container_type_params = expr_type_params
        //        .args
        //        .iter()
        //        .map(|p| ck.read_type(p))
        //        .collect();

        //    let container_type_params = ty::TypeArray::with(container_type_params);

        //    (&expr_type_params.callee, container_type_params)
        //} else {
            (&callee_as_path.lhs, ty::TypeArray::empty());
    //};

    let method_expr = &callee_as_path.rhs;

    let sym = match read_path_expr(ck, container_expr) {
        Ok(sym) => sym,
        Err(()) => {
            //ck.analysis.set_ty(e.id, ty_error());
            return ty::Type::Error;
        }
    };

    let method_name = if let Some(method_name_expr) = method_expr.to_ident() {
        method_name_expr.name.clone()
    } else {
        let msg = ErrorMessage::ExpectedSomeIdentifier;
        //ck.sa.report(ck.file_id, method_expr.span(), msg);
        //ck.analysis.set_ty(e.id, ty_error());
        return ty::Type::Error;
    };

    let interned_method_name = ck.sa.interner.intern(&method_name);

    match sym {
        /*Some(SymbolKind::Class(cls_id)) => {
            let cls = ck.sa.class(cls_id);
            if check_type_params(
                ck.sa,
                ck.element,
                ck.type_param_definition,
                cls,
                &container_type_params,
                ck.file_id,
                e.span,
                |ty| specialize_type(ck.sa, ty, &container_type_params),
            ) {
                check_expr_call_static_method(
                    ck,
                    e,
                    SourceType::Class(cls_id, container_type_params),
                    method_name,
                    type_params,
                    arguments,
                )
            } else {
                ty_error()
            }
        }*/
        Some(SymbolKind::Struct(struct_id)) => {
            //let struct_ = ck.sa.struct_(struct_id);

            /*if check_type_params(
                ck.sa,
                ck.element,
                ck.type_param_definition,
                struct_,
                &container_type_params,
                ck.file_id,
                e.span,
                |ty| specialize_type(ck.sa, ty, &container_type_params),
            ) {
                let object_ty = if let Some(ref primitive_ty) = struct_.primitive_ty {
                    assert!(container_type_params.is_empty());
                    primitive_ty.clone()
                } else {
                    SourceType::Struct(struct_id, container_type_params)
                };

                check_expr_call_static_method(ck, e, object_ty, method_name, type_params, arguments)
            } else {
                ty::Type::Error
            }*/

            ty::Type::Error
        }

        /*Some(SymbolKind::Enum(enum_id)) => {
            let enum_ = ck.sa.enum_(enum_id);

            if let Some(&variant_idx) = enum_.name_to_value().get(&interned_method_name) {
                if !container_type_params.is_empty() && !type_params.is_empty() {
                    let msg = ErrorMessage::NoTypeParamsExpected;
                    ck.sa.report(ck.file_id, callee_as_path.lhs.span(), msg);
                }

                let used_type_params = if type_params.is_empty() {
                    container_type_params
                } else {
                    type_params
                };

                check_expr_call_enum_variant(
                    ck,
                    e,
                    expected_ty,
                    enum_id,
                    used_type_params,
                    variant_idx,
                    arguments,
                )
            } else {
                if check_type_params(
                    ck.sa,
                    ck.element,
                    ck.type_param_definition,
                    enum_,
                    &container_type_params,
                    ck.file_id,
                    e.span,
                    |ty| specialize_type(ck.sa, ty, &container_type_params),
                ) {
                    let object_ty = SourceType::Enum(enum_id, container_type_params);

                    check_expr_call_static_method(
                        ck,
                        e,
                        object_ty,
                        method_name,
                        type_params,
                        arguments,
                    )
                } else {
                    ty_error()
                }
            }
        }*/

        /*Some(SymbolKind::TypeParam(id)) => {
            if !container_type_params.is_empty() {
                let msg = ErrorMessage::NoTypeParamsExpected;
                ck.sa.report(ck.file_id, callee_as_path.lhs.span(), msg);
            }

            check_expr_call_generic_static_method(ck, e, id, method_name, type_params, arguments)
        }*/
        Some(SymbolKind::Module(module_id)) => {
            if !container_type_params.is_empty() {
                let msg = ErrorMessage::NoTypeParamsExpected;
                println!("{:?}", msg)
                //ck.sa.report(ck.file_id, callee_as_path.lhs.span(), msg);
            }

            let sym = {
                let module = ck.sa.module(module_id);
                let table = module.table();

                table.get(interned_method_name)
            };

            check_expr_call_sym(ck, e, expected_ty, callee, sym, type_params, arguments)
        }

        /*Some(SymbolKind::Alias(alias_id)) => {
            if !container_type_params.is_empty() {
                let msg = ErrorMessage::NoTypeParamsExpected;
                ck.sa.report(ck.file_id, callee_as_path.lhs.span(), msg);
            }

            let alias_ty = ck.sa.alias(alias_id).ty();

            check_expr_call_static_method(ck, e, alias_ty, method_name, type_params, arguments)
        }*/
        _ => {
            let msg = ErrorMessage::StaticMethodCallTargetExpected;
            panic!("{:?}", msg);
            //ck.sa.report(ck.file_id, e.span, msg);
            //ck.analysis.set_ty(e.id, ty_error());
            ty::Type::Error
        }
    }
}

fn check_expr_call_expr(
    ck: &mut TypeCheck,
    e: &hir::FnCall,
    expr_type: ty::Type,
    arguments: CallArguments,
) -> ty::Type {
    if expr_type.is_error() {
        //ck.analysis.set_ty(e.id, ty_error());
        return ty::Type::Error;
    }

    let ty = ck.ty_name(&expr_type);
    panic!("{:?}", ErrorMessage::IndexGetNotImplemented(ty));

    //if expr_type.is_lambda() {
    //    return check_expr_call_expr_lambda(ck, e, expr_type, arguments);
    //}

    //let trait_id = ck.sa.known.traits.index_get();
    //let trait_ty = TraitType::from_trait_id(trait_id);

    //let impl_match = find_impl(
    //    ck.sa,
    //    ck.element,
    //    expr_type.clone(),
    //    &ck.type_param_definition,
    //    trait_ty.clone(),
    //);
    /*
    if let Some(impl_match) = impl_match {
        let trait_method_name = ck.sa.interner.intern("subscript");
        let trait_ = ck.sa.trait_(trait_id);
        let trait_method_id = trait_
            .get_method(trait_method_name, false)
            .expect("missing method");
        let method_id = ck
            .sa
            .impl_(impl_match.id)
            .get_method_for_trait_method_id(trait_method_id)
            .expect("method not found");

        let call_type = CallType::Expr(expr_type.clone(), method_id, impl_match.bindings.clone());
        ck.analysis
            .map_calls
            .insert_or_replace(e.id, Arc::new(call_type));

        let method = ck.sa.fct(method_id);

        check_args_compatible_fct(ck, method, arguments, &impl_match.bindings, None, |ty| ty);

        let return_type = specialize_type(ck.sa, method.return_type(), &impl_match.bindings);
        //ck.analysis.set_ty(e.id, return_type.clone());

        return_type
    } else {
        let ty = ck.ty_name(&expr_type);
        panic!("{:?}", ErrorMessage::IndexGetNotImplemented(ty));
        //ck.sa.report(
        //    ck.file_id,
        //    e.callee.span(),
        //    ErrorMessage::IndexGetNotImplemented(ty),
        //);

        //ck.analysis.set_ty(e.id, ty_error());

        ty::Type::Error
    } */
}

fn check_expr_call_func(
    ck: &mut TypeCheck,
    expr: &hir::FnCall,
    func_id: HirId,
    type_params: ty::TypeArray,
    arguments: CallArguments,
) -> ty::Type {
    let func = ck.hir.get_fn(&func_id).expect("to be found");

    if !func_accessible_from(ck.hir, func_id, ck.ctx.module_id) {
        let msg = ErrorMessage::NotAccessible;
        panic!("{:?}", msg)
        //ck.sa.report(ck.file_id, expr.span, msg);
    }

    check_args_compatible_func(ck, func, arguments, &type_params, None, |ty| ty);
    let ty = func.return_type();

    /*let ty = if check_type_params(
        ck.sa,
        ck.element,
        &ck.type_param_definition,
        fct,
        &type_params,
        ck.file_id,
        e.span,
        |ty| specialize_type(ck.sa, ty, &type_params),
    ) {
        check_args_compatible_fct(ck, fct, arguments, &type_params, None, |ty| ty);
        specialize_type(ck.sa, fct.return_type(), &type_params)
    } else {
        ty_error()
    };*/

    //ck.analysis.set_ty(e.id, ty.clone());

    let call_type = hir::CallType::Func(func_id, type_params.clone());
    ck.analysis
        .map_calls
        .insert(expr.hir_id, Arc::new(call_type));

    ty
}

fn check_expr_call_sym(
    ck: &mut TypeCheck,
    e: &hir::FnCall,
    expected_ty: Box<ty::Type>,
    callee: &hir::Expr,
    sym: Option<SymbolKind>,
    type_params: ty::TypeArray,
    arguments: CallArguments,
) -> ty::Type {
    match sym {
        Some(SymbolKind::FnDecl(func_id)) => {
            check_expr_call_func(ck, e, func_id, type_params, arguments)
        }

        //Some(SymbolKind::Class(cls_id)) => {
        //    check_expr_call_class(ck, e, expected_ty, cls_id, type_params, arguments)
        //}
        Some(SymbolKind::Struct(struct_id)) => {
            //check_expr_call_struct(ck, e, struct_id, type_params, arguments)
            ty::Type::Error
        }

        //Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => check_expr_call_enum_variant(
        //    ck,
        //    e,
        //    expected_ty,
        //    enum_id,
        //    type_params,
        //    variant_idx,
        //    arguments,
        //),
        _ => {
            //if !type_params.is_empty() {
            //    let msg = ErrorMessage::NoTypeParamsExpected;
            //    ck.sa.report(ck.file_id, e.callee.span(), msg);
            //}

            let expr_type = check_expr(ck, callee, Box::new(ty::Type::Any));
            check_expr_call_expr(ck, e, expr_type, arguments)
        }
    }
}
