use arkc_hir::{
    hir::{self, HirId},
    ty::PrimitiveType,
};

use crate::{compilation::ModuleId, sym::SymbolKind, Sema};

pub fn lookup_known_fundamental_types(sa: &mut Sema) {
    let stdlib_id = sa.compilation.program_module_id(); // TODO: move it to separate std module

    sa.compilation.known.structs.bool = Some(internal_struct(
        sa,
        stdlib_id,
        "Bool",
        Some(PrimitiveType::Bool),
    ));

    sa.compilation.known.structs.bool = Some(internal_struct(
        sa,
        stdlib_id,
        "Int32",
        Some(PrimitiveType::Int32),
    ));

    sa.compilation.known.structs.bool = Some(internal_struct(
        sa,
        stdlib_id,
        "Int64",
        Some(PrimitiveType::Int64),
    ));
}

fn resolve_name(sa: &Sema, name: &str, module_id: ModuleId) -> SymbolKind {
    let path = name.split(".");
    let mut sym = SymbolKind::Module(module_id);

    for name in path {
        let module_id = sym.to_module().expect("module expected");
        let table = sa.module(module_id).table();

        let interned_name = sa.interner.intern(name);

        if let Some(current_sym) = table.get(interned_name) {
            sym = current_sym;
        } else {
            let module = sa.module(module_id);
            panic!("{} not found in module {:?}.", name, module.name());
        }
    }

    sym
}

fn internal_struct(
    sa: &mut Sema,
    module_id: ModuleId,
    name: &str,
    ty: Option<PrimitiveType>,
) -> HirId {
    let struct_id = resolve_name(sa, name, module_id)
        .to_struct()
        .expect("struct expected");

    let file = &mut sa.compilation.hir.borrow_mut()[0];
    let stru = file.get_struct_mut(&struct_id).unwrap();

    assert!(stru.is_internal);
    stru.primitive_ty = ty;

    struct_id
}
