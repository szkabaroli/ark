use parser::{
    ast::{BinOp, BinOpKind, Ident},
    Span,
};

use crate::hir_id::HirId;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct BodyId {
    pub hir_id: HirId,
}

#[derive(Debug, Clone, Copy)]
pub enum ItemKind<'hir> {
    /// A function declaration.
    Fn(FnSig<'hir>, BodyId),
}

/// An expression.
#[derive(Debug, Clone, Copy)]
pub struct Expr<'hir> {
    pub hir_id: HirId,
    pub kind: ExprKind<'hir>,
    pub span: Span,
}

/// A block of statements `{ .. }`, which may have a label (in this case the
/// `targeted_by_break` field will be `true`) and may be `unsafe` by means of
/// the `rules` being anything but `DefaultBlock`.
#[derive(Debug, Clone, Copy)]
pub struct Block<'hir> {
    pub hir_id: HirId,
    /// Statements in a block.
    pub stmts: &'hir [Stmt<'hir>],
    /// An expression at the end of the block
    /// without a semicolon, if any.
    pub expr: Option<&'hir Expr<'hir>>,
    /// The span includes the curly braces `{` and `}` around the block.
    pub span: Span,
}

/// This type is used within both `ast::MetaItemLit` and `hir::Lit`.
///
/// Note that the entire literal (including the suffix) is considered when
/// deciding the `LitKind`. This means that float literals like `1f32` are
/// classified by this type as `Float`. This is different to `token::LitKind`
/// which does *not* consider the suffix.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum LitKind {
    /// A character literal (`'a'`).
    Char(char),
    /// An integer literal (`1`).
    Int(String),
    /// A float literal (`1.0`, `1f64` or `1E10f64`). The pre-suffix part is
    /// stored as a symbol rather than `f64` so that `LitKind` can impl `Eq`
    /// and `Hash`.
    Float(String),
    /// A boolean literal (`true`, `false`).
    Bool(bool),
    // Placeholder for a literal that wasn't well-formed in some way.
    // Err(ErrorGuaranteed),
}

/// A literal.
#[derive(Debug, Clone)]
pub struct Lit {
    pub kind: LitKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'hir> {
    /// A function call.
    ///
    /// The first field resolves to the function itself (usually an `ExprKind::Path`),
    /// and the second field is the list of arguments.
    /// This also represents calling the constructor of
    /// tuple-like ADTs such as tuple structs and enum variants.
    Call(&'hir Expr<'hir>, &'hir [Expr<'hir>]),
    /// A literal (e.g., `1`, `"foo"`).
    Lit(&'hir Lit),
    /// A block (e.g., `'label: { ... }`).
    Block(&'hir Block<'hir>),
    /// A `return`, with an optional value to be returned.
    Ret(Option<&'hir Expr<'hir>>),
    /// Path to a definition, possibly containing lifetime or type parameters.
    Path(Path<'hir>),
    // A binary operation (e.g., `a + b`, `a * b`).
    Binary(BinOp, &'hir Expr<'hir>, &'hir Expr<'hir>),
    // A unary operation (e.g., `!x`, `*x`).
    // Unary(UnOp, &'hir Expr<'hir>),
}

/// The resolution of a path or export.
///
/// For every path or identifier in Rust, the compiler must determine
/// what the path refers to. This process is called name resolution,
/// and `Res` is the primary result of name resolution.
///
/// For example, everything prefixed with `/* Res */` in this example has
/// an associated `Res`:
///
/// ```
/// fn str_to_string(s: & /* Res */ str) -> /* Res */ String {
///     /* Res */ String::from(/* Res */ s)
/// }
///
/// /* Res */ str_to_string("hello");
/// ```
///
/// The associated `Res`s will be:
///
/// - `str` will resolve to [`Res::PrimTy`];
/// - `String` will resolve to [`Res::Def`], and the `Res` will include the [`DefId`]
///   for `String` as defined in the standard library;
/// - `String::from` will also resolve to [`Res::Def`], with the [`DefId`]
///   pointing to `String::from`;
/// - `s` will resolve to [`Res::Local`];
/// - the call to `str_to_string` will resolve to [`Res::Def`], with the [`DefId`]
///   pointing to the definition of `str_to_string` in the current crate.
//
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Res<Id = HirId> {
    // Definition having a unique ID (`DefId`), corresponds to something defined in user code.
    //
    // **Not bound to a specific namespace.**
    // Def(DefKind, DefId),

    // Type namespace
    // A primitive type such as `i32` or `str`.
    //
    // **Belongs to the type namespace.**
    // PrimTy(PrimTy),
    /// A local variable or function parameter.
    ///
    /// **Belongs to the value namespace.**
    Local(Id),

    // All namespaces
    /// Name resolution failed. We use a dummy `Res` variant so later phases
    /// of the compiler won't crash and can instead report more errors.
    ///
    /// **Not bound to a specific namespace.**
    Err,
}

/// A segment of a path: an identifier, an optional lifetime, and a set of
/// types.
#[derive(Debug, Clone)]
pub struct PathSegment {
    /// The identifier portion of this path segment.
    pub ident: Ident,
    pub hir_id: HirId,
    pub res: Res,
}

/// A `Path` is essentially Rust's notion of a name; for instance,
/// `std::cmp::PartialEq`. It's represented as a sequence of identifiers,
/// along with a bunch of supporting information.
#[derive(Debug, Clone, Copy)]
pub struct Path<'hir, R = Res> {
    pub span: Span,
    /// The resolution for the path.
    pub res: R,
    /// The segments in the path: the things separated by `::`.
    pub segments: &'hir [PathSegment],
}

/// The various kinds of types recognized by the compiler.
#[derive(Debug, Clone, Copy, HashStable_Generic)]
pub enum TyKind<'hir> {
    
}

/// A statement.
#[derive(Debug, Clone, Copy)]
pub struct Stmt<'hir> {
    pub hir_id: HirId,
    pub kind: StmtKind<'hir>,
    pub span: Span,
}

/// The contents of a statement.
#[derive(Debug, Clone, Copy)]
pub enum StmtKind<'hir> {
    // A local (`let`) binding.
    //Let(&'hir LetStmt<'hir>),
    /// An expression with a trailing semi-colon (may have any type).
    Semi(&'hir Expr<'hir>),
}
