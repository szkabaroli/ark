mod file;
mod r#fn;
mod id;
mod modules;
mod packages;
//mod lower;

use std::sync::Arc;
use id::HirId;
use parser::Span;

pub use file::{SourceFile, SourceFileId};
pub use modules::{ModuleDefinition, ModuleDefinitionId};
pub use packages::{PackageDefinition, PackageDefinitionId, PackageName};
pub use r#fn::{FnDefinition, FnDefinitionId};

type P<T> = Arc<T>;

#[derive(Debug, Clone)]
pub struct Ty {
    //pub hir_id: HirId,
    pub kind: TyKind,
    pub span: Span,
}

/// The various kinds of types recognized by the compiler.
#[derive(Debug, Clone)]
pub enum TyKind {
    /// The never type (`!`).
    Never,
    /// A tuple (`(A, B, C, D, ...)`).
    Tup(Vec<Ty>),
}

/// Represents a function's signature in a free function.
#[derive(Debug, Clone)]
pub struct FnSig {
    /// The types of the function's parameters.
    ///
    /// Additional argument data is stored in the function's [body](Body::params).
    pub inputs: Vec<Ty>,
    pub output: Ty,
    pub c_variadic: bool,
    pub span: Span,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct BodyId {
    pub hir_id: HirId,
}

/// An expression.
///
/// For more details, see the [rust lang reference].
/// Note that the reference does not document nightly-only features.
/// There may be also slight differences in the names and representation of AST nodes between
/// the compiler and the reference.
///
/// [rust lang reference]: https://doc.rust-lang.org/reference/expressions.html
#[derive(Debug, Clone)]
pub struct Expr {
    pub hir_id: HirId,
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// A function call.
    ///
    /// The first field resolves to the function itself (usually an `ExprKind::Path`),
    /// and the second field is the list of arguments.
    /// This also represents calling the constructor of
    /// tuple-like ADTs such as tuple structs and enum variants.
    Call(P<Expr>, P<Expr>),
}

/// Represents a parameter in a function header.
#[derive(Debug, Clone, Copy)]
pub struct Param {
    pub hir_id: HirId,
    pub ty_span: Span,
    pub span: Span,
}

/// The body of a function, closure, or constant value. In the case of
/// a function, the body contains not only the function body itself
/// (which is an expression), but also the argument patterns, since
/// those are something that the caller doesn't really care about.
///
/// # Examples
///
/// ```
/// fn foo((x, y): (u32, u32)) -> u32 {
///     x + y
/// }
/// ```
///
/// Here, the `Body` associated with `foo()` would contain:
///
/// - an `params` array containing the `(x, y)` pattern
/// - a `value` containing the `x + y` expression (maybe wrapped in a block)
/// - `coroutine_kind` would be `None`
///
/// All bodies have an **owner**, which can be accessed via the HIR
/// map using `body_owner_def_id()`.
#[derive(Debug, Clone)]
pub struct Body {
    pub params: Vec<Param>,
    pub value: P<Expr>,
}

#[derive(Debug, Clone)]
pub struct FnItem {
    pub sig: FnSig,
    pub body_id: BodyId,
}
