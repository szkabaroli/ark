use std::sync::Arc;

use arkc_hir::hir;
use arkc_hir::ty;

use crate::check_type;
use crate::readty::AliasReplacement;
use crate::readty::AllowSelf;
use crate::replace_type;
use crate::{sema::Sema, sym::ModuleSymTable};

pub fn check(sa: &Sema) {
    let root = &mut sa.compilation.hir.borrow_mut()[0];

    for elem in root.elements.iter() {
        match elem.kind {
            hir::ElemKind::Function(ref func) => {
                let mut sym_table = ModuleSymTable::new(sa, sa.compilation.program_module_id());
                sym_table.push_level();

                let mut param_types: Vec<ty::Type> = Vec::new();

                //for (id, name) in fct.type_params().names() {
                //    sym_table.insert(name, SymbolKind::TypeParam(id));
                //}

                /*match fct.parent {
                    FctParent::Impl(impl_id) => {
                        let impl_ = sa.impl_(impl_id);

                        if fct.has_hidden_self_argument() {
                            param_types.push(impl_.extended_ty());
                        }

                        for &alias_id in impl_.aliases() {
                            let alias = sa.alias(alias_id);
                            sym_table.insert(alias.name, SymbolKind::TypeAlias(alias_id));
                        }
                    }

                    FctParent::Extension(extension_id) => {
                        let extension = sa.extension(extension_id);

                        if fct.has_hidden_self_argument() {
                            param_types.push(extension.ty().clone());
                        }
                    }

                    FctParent::Trait(trait_id) => {
                        let trait_ = sa.trait_(trait_id);

                        if fct.has_hidden_self_argument() {
                            param_types.push(SourceType::This);
                        }

                        for &alias_id in trait_.aliases() {
                            let alias = sa.alias(alias_id);
                            sym_table.insert(alias.name, SymbolKind::TypeAlias(alias_id));
                        }
                    }

                    FctParent::None => {}

                    FctParent::Function => unreachable!(),
                }*/

                /*for p in &ast.params {
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
                */

                let return_type = if let Some(ret) = &func.signature.ret {
                    //ty::Type::Primitive(ty::PrimitiveType::Unit)
                    process_type(sa, root, &func, &sym_table, &ret)
                } else {
                    ty::Type::Primitive(ty::PrimitiveType::Unit)
                };

                root.node_types.insert(
                    func.hir_id.clone(),
                    ty::Type::Function(ty::FnType {
                        inputs: param_types,
                        output: Some(Box::from(return_type)),
                        is_varargs: false,
                    }),
                );
            }
            _ => {
                // do nothing
            }
        }
    }
}

fn process_type(
    sa: &Sema,
    root: &hir::File,
    func: &hir::FnDeclaration,
    sym_table: &ModuleSymTable,
    hir: &hir::TypeData,
) -> ty::Type {
    let allow_self = AllowSelf::No;
    //if func.is_self_allowed() {
    //    AllowSelf::Yes
    //} else {
    //    AllowSelf::No
    //};

    let ty = check_type(
        sa,
        root,
        &sym_table,
        hir,
        allow_self,
    );

    replace_type(sa, ty, None, AliasReplacement::ReplaceWithActualType)
    
    /*let ty = check_type(
        sa,
        &sym_table,
        fct.file_id,
        ast,
        fct.type_params(),
        allow_self,
    );*/

    /*match fct.parent {
        FctParent::Impl(id) => {
            let impl_ = sa.impl_(id);
            replace_type(
                sa,
                ty,
                None,
                Some(impl_.extended_ty()),
                AliasReplacement::ReplaceWithActualType,
            )
        }

        FctParent::None => {
            replace_type(sa, ty, None, None, AliasReplacement::ReplaceWithActualType)
        }

        FctParent::Extension(id) => {
            let ext = sa.extension(id);
            replace_type(
                sa,
                ty,
                None,
                Some(ext.ty().clone()),
                AliasReplacement::ReplaceWithActualType,
            )
        }

        FctParent::Trait(trait_id) => replace_type(
            sa,
            ty,
            None,
            None,
            AliasReplacement::ReplaceWithActualTypeKeepTrait(trait_id),
        ),

        FctParent::Function => unreachable!(),
    }*/
}