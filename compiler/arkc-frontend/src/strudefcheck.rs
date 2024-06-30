use std::borrow::Borrow;
use std::collections::BTreeMap;

use arkc_hir::hir;
use arkc_hir::ty;

use crate::readty::expand_type;
use crate::{sema::Sema, sym::ModuleSymTable};

pub fn check(sa: &Sema) {
    let mut types = {
        let root = &sa.compilation.hir.borrow()[0];
        let mut types = BTreeMap::new();

        for elem in root.elements.iter() {
            match elem.kind {
                hir::ElemKind::Struct(ref hir) => {
                    let mut structck = StructCheck {
                        sa,
                        //struct_id: id,
                        //file_id: stru.file_id,
                        hir,
                        symtable: ModuleSymTable::new(sa, sa.compilation.program_module_id()),
                    };

                    structck.check(root, &mut types);
                }
                _ => (),
            }
        }
        types
    };

    let root = &mut sa.compilation.hir.borrow_mut()[0];
    root.node_types.append(&mut types);
}

struct StructCheck<'a> {
    sa: &'a Sema,
    //struct_id: StructDefinitionId,
    //file_id: SourceFileId,
    hir: &'a hir::Struct,
    symtable: ModuleSymTable,
}

impl<'a> StructCheck<'a> {
    fn check(&mut self, root: &hir::File, types: &mut BTreeMap<hir::HirId, ty::Type>) {
        self.symtable.push_level();

        for (idx, field) in self.hir.fields.iter().enumerate() {
            self.visit_struct_field(root, types, idx, field);
        }

        self.symtable.pop_level();
    }

    fn visit_struct_field(
        &mut self,
        root: &hir::File,
        types: &mut BTreeMap<hir::HirId, ty::Type>,
        idx: usize,
        f: &'a hir::StructField,
    ) {
        let stru = self.hir;

        let ty = expand_type(self.sa, root, &self.symtable, &f.ty, crate::readty::AllowSelf::No);

        types.insert(stru.fields[idx].hir_id, ty);
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    /*use crate::tests::*;

    #[test]
    fn struct_field() {
        ok("struct Foo { a: Int32 }");
        ok("struct Foo { a: Int32, b: Int32 }");
        ok("struct Foo { a: Int32 } struct Bar { a: Int32 }");
        ok("struct Foo { a: Int32, bar: Bar } struct Bar { a: Int32 }");
        err(
            "struct Bar { a: Unknown }",
            (1, 17),
            ErrorMessage::UnknownIdentifier("Unknown".into()),
        );
        err(
            "struct Foo { a: Int32, a: Int32 }",
            (1, 24),
            ErrorMessage::ShadowField("a".into()),
        );
    }*/
}
