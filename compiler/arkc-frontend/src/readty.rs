use std::{borrow::Borrow, cell::RefCell, rc::Rc};

use crate::{
    compilation::ModuleId,
    error::msg::ErrorMessage,
    sym::{ModuleSymTable, SymTable, SymbolKind},
    Sema,
};
use arkc_hir::{
    hir,
    ty::{FnType, PrimitiveType, Type},
};

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum AllowSelf {
    Yes,
    No,
}

pub fn parse_type(sa: &Sema, root: &hir::File, table: &ModuleSymTable, t: &hir::TypeData) -> Type {
    match *t {
        hir::TypeData::_Self => todo!(),
        hir::TypeData::Basic(ref node) => read_type_basic_unchecked(sa, root, table, node),
        hir::TypeData::Fn(ref node) => read_type_fn_unchecked(sa, root, table, node),
        //ast::TypeData::Tuple(ref node) => read_type_tuple_unchecked(sa, table, file_id, node),
        //ast::TypeData::Generic(..) | ast::TypeData::Path(..) => unreachable!(),
        hir::TypeData::Unknown { .. } => Type::Unknown,
    }
}

fn read_type_fn_unchecked(sa: &Sema, root: &hir::File, table: &ModuleSymTable, node: &hir::FnType) -> Type {
    let mut params = vec![];

    for param in &node.params {
        let ty = parse_type(sa, root, table, param);
        params.push(ty);
    }

    //let params = SourceTypeArray::with(params);

    let return_type = if let Some(ref ret) = node.ret {
        parse_type(sa, root, table, ret)
    } else {
        Type::Primitive(PrimitiveType::Unit)
    };

    Type::Function(FnType {
        inputs: vec![],
        output: Some(Box::new(return_type)),
        is_varargs: false,
    })
}

fn read_type_basic_unchecked(sa: &Sema, root: &hir::File, table: &ModuleSymTable, basic: &hir::BasicType) -> Type {
    let sym = read_type_path(sa, table, basic);

    if sym.is_err() {
        return Type::Unknown;
    }

    let sym = sym.unwrap();

    match sym {
        Some(SymbolKind::Struct(struct_id)) => {
            let stru = root.get_struct(&struct_id).unwrap();

            if let Some(ref primitive_ty) = stru.primitive_ty {
                Type::Primitive(primitive_ty.clone())
            } else {
                Type::Struct(struct_id)
            }
        }
        Some(_) => {
            let name = basic.path.names.last().cloned().unwrap().name.clone();
            let msg = ErrorMessage::UnknownType(name);
            panic!("{:?}", msg);
            // sa.report(node.span, msg);
            Type::Unknown
        }

        None => {
            let name = basic.path.names.last().cloned().unwrap().name.clone();
            let msg = ErrorMessage::UnknownIdentifier(name);
            panic!("{:?}", msg);
            // sa.report(node.span, msg);
            Type::Unknown
        }
    }
}

fn read_type_path(
    sa: &Sema,
    table: &ModuleSymTable,
    basic: &hir::BasicType,
) -> Result<Option<SymbolKind>, ()> {
    let names = &basic.path.names;

    if names.len() > 1 {
        let first_name = sa.interner.intern(&names.first().cloned().unwrap().name);

        let last_name = sa.interner.intern(&names.last().cloned().unwrap().name);

        let mut module_table = table_for_module(sa, basic, table.get(first_name))?;

        for ident in &names[1..names.len() - 1] {
            let name = sa.interner.intern(&ident.name);
            let sym = module_table.get(name);
            module_table = table_for_module(sa, basic, sym)?;
        }

        let sym = module_table.get(last_name);
        Ok(sym)
    } else {
        let name = &names.last().cloned().unwrap().name;
        Ok(table.get_string(sa, name))
    }
}

fn table_for_module(
    sa: &Sema,
    basic: &hir::BasicType,
    sym: Option<SymbolKind>,
) -> Result<Rc<SymTable>, ()> {
    match sym {
        Some(SymbolKind::Module(module_id)) => Ok(sa.compilation.module(module_id).table()),
        _ => {
            let msg = ErrorMessage::ExpectedModule;
            panic!("{:?}", msg);
            //sa.report(basic.span, msg);
            Err(())
        }
    }
}

pub fn verify_type(
    sa: &Sema,
    module_id: ModuleId,
    t: &hir::TypeData,
    ty: Type,
    allow_self: AllowSelf,
) -> bool {
    match t {
        &hir::TypeData::Basic(ref node) => {
            if !verify_type_basic(sa, module_id, node, ty, allow_self) {
                return false;
            }
        }

        &hir::TypeData::Unknown { .. } => {}
        _ => todo!(),
    }

    true
}

fn verify_type_basic(
    sa: &Sema,
    module_id: ModuleId,
    basic: &hir::BasicType,
    ty: Type,
    allow_self: AllowSelf,
) -> bool {
    match ty {
        Type::Primitive(PrimitiveType::Unit) => todo!(),
        Type::Primitive(PrimitiveType::Bool)
        | Type::Primitive(PrimitiveType::Char)
        | Type::Primitive(PrimitiveType::Int32)
        | Type::Primitive(PrimitiveType::Int64)
        | Type::Primitive(PrimitiveType::Float64) => {
            return true;
            //let struct_id = ty
            //    .primitive_struct_id(sa)
            //    .expect("primitive struct expected");

            //if !struct_accessible_from(sa, struct_id, module_id) {
            //    let struct_ = sa.struct_(struct_id);
            //    let msg = ErrorMessage::NotAccessible(struct_.name(sa));
            //    sa.report(file_id, node.span, msg);
            //    return false;
            //}
        }
        Type::Any => todo!(),
        Type::This => todo!(),
        Type::Function(_) => todo!(),
        Type::Struct(_) => {
            // let struct_ = sa.struct_(struct_id);

            //if !struct_accessible_from(sa, struct_id, module_id) {
            //    let msg = ErrorMessage::NotAccessible(struct_.name(sa));
            //    sa.report(file_id, node.span, msg);
            //    return false;
            //}
        }
        Type::Unknown => return false,
    }

    true
}

pub fn check_type(
    sa: &Sema,
    root: &hir::File,
    table: &ModuleSymTable,
    t: &hir::TypeData,
    allow_self: AllowSelf,
) -> Type {
    let ty = parse_type(sa, root, table, t);

    let is_good = verify_type(
        sa,
        table.module_id(),
        t,
        ty.clone(),
        allow_self,
    );

    if is_good {
        ty
    } else {
        Type::Unknown
    }
}

pub fn expand_type(
    sa: &Sema,
    root: &hir::File,
    table: &ModuleSymTable,
    t: &hir::TypeData,
    allow_self: AllowSelf,
) -> Type {
    let ty = parse_type(sa, root, table, t);

    let is_good = verify_type(sa, table.module_id(), t, ty.clone(), allow_self);

    if is_good {
        replace_type(sa, ty, None, AliasReplacement::ReplaceWithActualType)
    } else {
        Type::Unknown
    }
}

#[derive(Clone)]
pub enum AliasReplacement {
    None,
    // Map(&'a HashMap<AliasDefinitionId, SourceType>),
    ReplaceWithActualType,
    // ReplaceWithActualTypeKeepTrait(TraitDefinitionId),
}

pub fn replace_type(
    sa: &Sema,
    ty: Type,
    self_ty: Option<Type>,
    alias_map: AliasReplacement,
) -> Type {
    match ty {
        Type::This => {
            if let Some(self_ty) = self_ty {
                self_ty
            } else {
                ty
            }
        }
        Type::Function(_) => todo!(),
        Type::Primitive(PrimitiveType::Unit)
        | Type::Primitive(PrimitiveType::Bool)
        | Type::Primitive(PrimitiveType::Char)
        | Type::Primitive(PrimitiveType::Int32)
        | Type::Primitive(PrimitiveType::Int64)
        | Type::Primitive(PrimitiveType::Float64)
        | Type::Unknown => ty,
        Type::Struct(id) => Type::Struct(id),
        Type::Any => {
            panic!("unexpected type = {:?}", ty);
        }
    }
}
