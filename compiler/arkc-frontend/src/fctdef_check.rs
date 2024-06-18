use std::sync::Arc;

use parser::ast;

use crate::{
    error::msg::ErrorMessage, readty::{check_type, AllowSelf}, sema::{FctDefinition, FctParent, Sema}, specialize::{replace_type, AliasReplacement}, sym::ModuleSymTable, SourceType, SymbolKind
};

pub fn check(sa: &Sema) {
    for (_id, fct) in sa.functions.iter() {
        let ast = fct.ast.clone();

        let mut sym_table = ModuleSymTable::new(sa, fct.module_id);
        sym_table.push_level();

        let mut param_types: Vec<SourceType> = Vec::new();

        //for (id, name) in fct.type_params().names() {
        //    sym_table.insert(name, SymbolKind::TypeParam(id));
        //}

        match fct.parent {
            //FctParent::Impl(impl_id) => {
            //    let impl_ = sa.impl_(impl_id);
            //
            //    if fct.has_hidden_self_argument() {
            //        param_types.push(impl_.extended_ty());
            //    }
            //
            //    for &alias_id in impl_.aliases() {
            //        let alias = sa.alias(alias_id);
            //        sym_table.insert(alias.name, SymbolKind::TypeAlias(alias_id));
            //    }
            //}

            //FctParent::Extension(extension_id) => {
            //    let extension = sa.extension(extension_id);
            //
            //    if fct.has_hidden_self_argument() {
            //        param_types.push(extension.ty().clone());
            //    }
            //}

            //FctParent::Trait(trait_id) => {
            //    let trait_ = sa.trait_(trait_id);
            //
            //    if fct.has_hidden_self_argument() {
            //        param_types.push(SourceType::This);
            //    }
            //
            //    for &alias_id in trait_.aliases() {
            //        let alias = sa.alias(alias_id);
            //        sym_table.insert(alias.name, SymbolKind::TypeAlias(alias_id));
            //    }
            //}
            FctParent::None => {}

            FctParent::Function => unreachable!(),
        }

        for p in &ast.params {
            if fct.is_variadic.get() {
                sa.report(
                    fct.file_id,
                    p.span,
                    ErrorMessage::VariadicParameterNeedsToBeLast,
                );
            }

            let ty = process_type(sa, fct, &sym_table, &p.data_type);

            param_types.push(ty);

            if p.variadic {
                fct.is_variadic.set(true);
            }
        }

        assert!(fct.param_types.set(param_types).is_ok());

        let return_type = if let Some(ret) = ast.return_type.as_ref() {
            process_type(sa, fct, &sym_table, ret)
        } else {
            SourceType::Unit
        };

        assert!(fct.return_type.set(return_type).is_ok());
        fct.initialized.set(true);

        //check_test(sa, &*fct);
    }
}

fn process_type(
    sa: &Sema,
    fct: &FctDefinition,
    sym_table: &ModuleSymTable,
    ast: &Arc<ast::TypeData>,
) -> SourceType {
    let allow_self = if fct.is_self_allowed() {
        AllowSelf::Yes
    } else {
        AllowSelf::No
    };

    let ty = check_type(
        sa,
        &sym_table,
        fct.file_id,
        ast,
        // fct.type_params(),
        allow_self,
    );

    match fct.parent {
        //FctParent::Impl(id) => {
        //    let impl_ = sa.impl_(id);
        //    replace_type(
        //        sa,
        //        ty,
        //        None,
        //        Some(impl_.extended_ty()),
        //        AliasReplacement::ReplaceWithActualType,
        //    )
        //}

        FctParent::None => {
            replace_type(sa, ty, None, None, AliasReplacement::ReplaceWithActualType)
        }

        //FctParent::Extension(id) => {
        //    let ext = sa.extension(id);
        //    replace_type(
        //        sa,
        //        ty,
        //        None,
        //        Some(ext.ty().clone()),
        //        AliasReplacement::ReplaceWithActualType,
        //    )
        //}

        //FctParent::Trait(trait_id) => replace_type(
        //    sa,
        //    ty,
        //    None,
        //    None,
        //    AliasReplacement::ReplaceWithActualTypeKeepTrait(trait_id),
        //),

        FctParent::Function => unreachable!(),
    }
}
