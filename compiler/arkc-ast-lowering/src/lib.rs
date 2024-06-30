mod lowering;

use parser::ast;
use lowering::LoweringContext;
use arkc_hir::hir;

pub fn lower_file(root: &ast::File) -> hir::File {
    LoweringContext::new().lower_file(root)
}