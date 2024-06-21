use crate::hir;
use parser::ast;

struct Context;

impl Context {
    fn lower_ty(&mut self, ast: &ast::Type) -> hir::Ty {
        hir::Ty {
            kind: todo!(),
            span: todo!(),
        }
    }

    fn lower_signature(&mut self, ast: &ast::FnSig) -> hir::FnSig {
        for input in ast.inputs {
            self.lower_ty(&input.data_type);
        }

        self.lower_ty(&ast.output);

        hir::FnSig {
            span: ast.span,
            inputs: todo!(),
            output: todo!(),
            c_variadic: todo!(),
        }
    }
}
