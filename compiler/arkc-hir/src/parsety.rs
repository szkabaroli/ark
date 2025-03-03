use std::cell::RefCell;

use parser::ast;
use serde::{Deserialize, Serialize};

use crate::ty;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ParsedType {
    #[serde(skip_deserializing, skip_serializing)]
    ast: Option<ast::Type>,
    //parsed_ast: OnceCell<Box<ParsedTypeAst>>,
    ty: RefCell<Option<ty::Type>>,
}

impl ParsedType {
    pub fn new_ty(ty: ty::Type) -> ParsedType {
        ParsedType {
            ast: None,
            //parsed_ast: OnceCell::new(),
            ty: RefCell::new(Some(ty)),
        }
    }

    pub fn new_ast(ast: ast::Type) -> ParsedType {
        ParsedType {
            ast: Some(ast),
            //parsed_ast: OnceCell::new(),
            ty: RefCell::new(None),
        }
    }

    pub fn ty(&self) -> ty::Type {
        self.ty.borrow().as_ref().cloned().expect("missing type")
    }

    pub fn set_ty(&self, ty: ty::Type) {
        *self.ty.borrow_mut() = Some(ty);
    }
}
