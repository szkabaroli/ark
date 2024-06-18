use std::sync::Arc;

use crate::token::Position;
#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    True,
    False,
    Null,
    This,
    Int(i64),
    Float(f64),
    Str(String),
    Builtin(String),
    Ident(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub pos: Position,
    pub decl: ExprDecl,
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum WhileFlag {
    NormalWhile,
    DoWhile,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprDecl {
    Assign(Arc<Expr>, Arc<Expr>),
    Const(Constant),
    Block(Vec<Arc<Expr>>),
    Paren(Arc<Expr>),
    Field(Arc<Expr>, String),
    Call(Arc<Expr>, Vec<Arc<Expr>>),
    Array(Arc<Expr>, Arc<Expr>),

    Vars(Vec<(String, Option<Arc<Expr>>)>),
    For(Arc<Expr>, Arc<Expr>, Arc<Expr>, Arc<Expr>),
    ForIn(String, Arc<Expr>, Arc<Expr>),
    If(Arc<Expr>, Arc<Expr>, Option<Arc<Expr>>),
    Function(Vec<String>, Arc<Expr>),
    Binop(String, Arc<Expr>, Arc<Expr>),
    Return(Option<Arc<Expr>>),
    Break(Option<Arc<Expr>>),
    Var(bool, String, Option<Arc<Expr>>),
    Continue,
    Next(Arc<Expr>, Arc<Expr>),
    Object(Vec<(String, Arc<Expr>)>),
    Label(String),
    Switch(Arc<Expr>, Vec<(Arc<Expr>, Arc<Expr>)>, Option<Arc<Expr>>),
    Unop(String, Arc<Expr>),
    Yield(Arc<Expr>),
    Jazz(String),
    Goto(String),
}

pub fn make_call(v: Arc<Expr>, args: Vec<Arc<Expr>>, pos: Position) -> Expr {
    Expr {
        pos,
        decl: ExprDecl::Call(v, args),
    }
}

pub fn make_ident(i: String, pos: Position) -> Expr {
    Expr {
        pos,
        decl: ExprDecl::Const(Constant::Ident(i)),
    }
}

pub fn make_builtin(b: String, pos: Position) -> Expr {
    Expr {
        pos,
        decl: ExprDecl::Const(Constant::Builtin(b)),
    }
}

pub fn make_int(i: i64, pos: Position) -> Expr {
    Expr {
        pos,
        decl: ExprDecl::Const(Constant::Int(i)),
    }
}

pub fn make_str(s: String, pos: Position) -> Expr {
    Expr {
        pos,
        decl: ExprDecl::Const(Constant::Str(s)),
    }
}
pub fn make_bin(op: String, e1: Arc<Expr>, e2: Arc<Expr>, pos: Position) -> Expr {
    Expr {
        pos,
        decl: ExprDecl::Binop(op, e1, e2),
    }
}

impl Expr {
    pub fn iter(&self, mut f: impl FnMut(&Arc<Expr>)) {
        match &self.decl {
            ExprDecl::Block(el) => {
                for e in el.iter() {
                    f(e);
                }
            }
            ExprDecl::Paren(e) => f(e),
            ExprDecl::Field(e, _) => f(e),
            ExprDecl::Call(e, el) => {
                f(e);
                for x in el.iter() {
                    f(x);
                }
            }
            ExprDecl::Array(e1, e2) => {
                f(e1);
                f(e2);
            }
            ExprDecl::Var(_, _, e) => match e {
                Some(e) => f(e),
                _ => (),
            },
            ExprDecl::If(e1, e2, e3) => {
                f(e1);
                f(e2);
                match e3 {
                    Some(e) => f(e),
                    _ => (),
                }
            }
            ExprDecl::Function(_, e) => f(e),
            ExprDecl::Binop(_, e1, e2) => {
                f(e1);
                f(e2)
            }
            ExprDecl::Return(Some(e)) => f(e),
            ExprDecl::Break(Some(e)) => f(e),
            ExprDecl::Next(e1, e2) => {
                f(e1);
                f(e2);
            }
            _ => (),
        }
    }
}
