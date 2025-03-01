pub mod dump;
pub mod visit;

use std::fmt;
use std::slice::Iter;
use std::sync::Arc;

use crate::{green::GreenNode, source_file::SourceFileId, span::Span};

#[derive(Clone, Debug)]
pub struct File {
    pub source_id: SourceFileId,
    pub green: GreenNode,
    pub elements: Vec<Elem>,
}

impl File {
    #[cfg(test)]
    pub fn fct0(&self) -> &FnItem {
        self.elements[0].to_function().unwrap()
    }

    #[cfg(test)]
    pub fn struct0(&self) -> &StructItem {
        self.elements[0].to_struct().unwrap()
    }

    #[cfg(test)]
    pub fn fct(&self, index: usize) -> &FnItem {
        self.elements[index].to_function().unwrap()
    }

    #[cfg(test)]
    pub fn flow(&self, index: usize) -> &FlowItem {
        self.elements[index].to_flow().unwrap()
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Hash)]
pub struct NodeId(pub usize);

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

pub type Ident = Arc<IdentData>;

#[derive(Clone, Debug)]
pub struct IdentData {
    pub id: NodeId,
    pub span: Span,
    pub name_as_string: String,
}

pub type Path = Arc<PathData>;

#[derive(Clone, Debug)]
pub struct PathData {
    pub id: NodeId,
    pub span: Span,
    pub names: Vec<Ident>,
}

pub type Type = Arc<TypeData>;

#[derive(Clone, Debug)]
pub enum TypeData {
    This(TypeSelfType),
    Basic(TypeBasicType),
    //Tuple(TypeTupleType),
    //Lambda(TypeLambdaType),
    //Path(TypePathType),
    //Generic(TypeGenericType),
    Unknown { id: NodeId, span: Span },
}

impl TypeData {
    pub fn span(&self) -> Span {
        match *self {
            TypeData::This(ref ty) => ty.span,
            TypeData::Basic(ref ty) => ty.span,
            TypeData::Unknown { span, .. } => span,
        }
    }

    pub fn id(&self) -> NodeId {
        match *self {
            TypeData::This(ref ty) => ty.id,
            TypeData::Basic(ref ty) => ty.id,
            TypeData::Unknown { id, .. } => id,
        }
    }

    pub fn create_self(id: NodeId, span: Span, green: GreenNode) -> TypeData {
        TypeData::This(TypeSelfType { id, span, green })
    }

    pub fn create_basic(
        id: NodeId,
        span: Span,
        green: GreenNode,
        path: Path,
        params: Vec<Type>,
    ) -> TypeData {
        TypeData::Basic(TypeBasicType {
            id,
            span,
            green,
            path,
            params,
        })
    }

    pub fn to_basic(&self) -> Option<&TypeBasicType> {
        match *self {
            TypeData::Basic(ref val) => Some(val),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeSelfType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
}

#[derive(Clone, Debug)]
pub struct TypeBasicType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub path: Path,
    pub params: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct TypePrimitiveType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
}

impl TypeBasicType {
    #[cfg(test)]
    pub fn name(&self) -> String {
        assert_eq!(self.path.names.len(), 1);
        self.path
            .names
            .last()
            .cloned()
            .unwrap()
            .name_as_string
            .clone()
    }
}

#[derive(Clone, Debug)]
pub struct Import {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub path: Arc<ImportPath>,
}

#[derive(Clone, Debug)]
pub struct ImportPath {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub path: Vec<ImportAtom>,
    pub target: ImportPathDescriptor,
}

#[derive(Clone, Debug)]
pub struct ImportAtom {
    pub green: GreenNode,
    pub span: Span,
    pub value: ImportPathComponentValue,
}

#[derive(Clone, Debug)]
pub enum ImportPathComponentValue {
    Package,
    Name(Ident),
    Error,
}

#[derive(Clone, Debug)]
pub enum ImportPathDescriptor {
    Default,
    Error,
}

pub type Elem = Arc<ElemData>;

#[derive(Clone, Debug)]
pub enum ElemData {
    Global(Arc<GlobalItem>),
    Function(Arc<FnItem>),
    Flow(Arc<FlowItem>),
    Struct(Arc<StructItem>),
    Interface(Arc<InterfaceItem>),
    Import(Arc<Import>),
    Error { id: NodeId, span: Span },
}

impl ElemData {
    pub fn span(&self) -> Span {
        match self {
            ElemData::Global(ref node) => node.span,
            ElemData::Interface(ref node) => node.span,
            ElemData::Function(ref node) => node.span,
            ElemData::Struct(ref node) => node.span,
            ElemData::Flow(ref node) => node.span,
            ElemData::Import(ref node) => node.span,
            ElemData::Error { span, .. } => span.clone(),
        }
    }

    pub fn to_function(&self) -> Option<&FnItem> {
        match self {
            &ElemData::Function(ref fct) => Some(fct),
            _ => None,
        }
    }

    pub fn to_flow(&self) -> Option<&FlowItem> {
        match self {
            &ElemData::Flow(ref flow) => Some(flow),
            _ => None,
        }
    }

    pub fn to_struct(&self) -> Option<&StructItem> {
        match self {
            &ElemData::Struct(ref v) => Some(v),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Module {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub name: Option<Ident>,
    pub elements: Option<Vec<Elem>>,
}

// remove in next step
#[derive(Clone, Debug)]
pub struct ModifierList {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Vec<Modifier>,
}

impl ModifierList {
    pub fn iter(&self) -> Iter<Modifier> {
        self.modifiers.iter()
    }
}

#[derive(Clone, Debug)]
pub struct Modifier {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
}

#[derive(Clone, Debug)]
pub struct Const {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    // pub modifiers: Option<ModifierList>,
    pub name: Option<Ident>,
    pub data_type: Type,
    pub expr: Expr,
}

pub type FlowStmt = Arc<FlowStmtData>;

#[derive(Clone, Debug)]
pub enum FlowStmtData {
    Node(FlowStmtNodeType),
    Expr(FlowExprType),
}

impl FlowStmtData {
    pub fn create_expr(id: NodeId, span: Span, expr: FlowExpr) -> FlowStmtData {
        FlowStmtData::Expr(FlowExprType { id, span, expr })
    }

    pub fn create_flow_node(
        id: NodeId,
        span: Span,
        name: Option<Ident>,
        data_type: Option<Type>,
    ) -> FlowStmtData {
        FlowStmtData::Node(FlowStmtNodeType {
            id,
            span,
            name,
            data_type,
        })
    }
}

#[derive(Clone, Debug)]
pub struct FlowStmtNodeType {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,

    // pub pattern: Box<LetPattern>,
    pub data_type: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct FlowExprType {
    pub id: NodeId,
    pub span: Span,
    pub expr: FlowExpr,
}

#[derive(Clone, Debug)]
pub struct FlowExprBlockType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub stmts: Vec<FlowStmt>,
    pub expr: Option<FlowExpr>,
}

#[derive(Clone, Debug)]
pub struct FlowExprParenType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub args: Vec<FlowExpr>,
}

#[derive(Clone, Debug)]
pub struct FlowItem {
    pub id: NodeId,
    pub declaration_span: Span,
    pub span: Span,
    pub green: GreenNode,
    pub name: Option<Ident>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub block: Option<FlowExpr>,
}

/// Represents a function's signature.
#[derive(Clone, Debug)]
pub struct FnSignature {
    pub inputs: Vec<Param>,
    pub output: Option<Type>,
    pub span: Span,
}

/// A block (`{ .. }`).
///
/// E.g., `{ .. }` as in `fn foo() { .. }`.
#[derive(Clone, Debug)]
pub struct Block {
    pub id: NodeId,
    /// The statements in the block.
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug)]
pub struct StructField {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub green: GreenNode,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct StructItem {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub green: GreenNode,
    pub fields: Vec<StructField>,
}

#[derive(Clone, Debug)]
pub struct InterfaceItem {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub green: GreenNode
}

#[derive(Clone, Debug)]
pub struct GlobalItem {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub green: GreenNode
}

#[derive(Clone, Debug)]
pub struct FnItem {
    pub id: NodeId,
    pub declaration_span: Span,
    pub span: Span,
    pub green: GreenNode,
    pub name: Option<Ident>,
    // pub kind: FunctionKind,
    // pub type_params: Option<TypeParams>,
    pub signature: FnSignature,
    //pub where_bounds: Option<WhereBounds>,
    pub body: Option<Expr>,
}

impl FnItem {
    pub fn body(&self) -> &Expr {
        self.body.as_ref().unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct Param {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub mutable: bool,
    pub data_type: Type,
    pub variadic: bool,
}

pub type Stmt = Arc<StmtData>;

#[derive(Clone, Debug)]
pub enum StmtData {
    Let(StmtLetType),
    Expr(StmtExprType),
}

impl StmtData {
    pub fn create_expr(id: NodeId, span: Span, expr: Expr) -> StmtData {
        StmtData::Expr(StmtExprType { id, span, expr })
    }

    pub fn create_let(
        id: NodeId,
        span: Span,
        pattern: Box<PatternKind>,
        data_type: Option<Type>,
        expr: Option<Expr>,
    ) -> StmtData {
        StmtData::Let(StmtLetType {
            id,
            span,
            pattern,
            data_type,
            expr,
        })
    }

    pub fn span(&self) -> Span {
        match *self {
            StmtData::Let(ref stmt) => stmt.span,
            StmtData::Expr(ref stmt) => stmt.span,
        }
    }

    pub fn to_let(&self) -> Option<&StmtLetType> {
        match *self {
            StmtData::Let(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_expr(&self) -> Option<&StmtExprType> {
        match *self {
            StmtData::Expr(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_expr(&self) -> bool {
        match *self {
            StmtData::Expr(_) => true,
            _ => false,
        }
    }
}

pub type FlowExpr = Arc<FlowExprData>;

#[derive(Clone, Debug)]
pub enum FlowExprData {
    FlowBlock(FlowExprBlockType),
    Paren(FlowExprParenType),
    Path(ExprPathType),
    Dot(FlowExprDotType),
    Wire(FlowExprWireType),
    Lit(Literal),
    Ident(ExprIdentType),
    Error { id: NodeId, span: Span },
}

impl FlowExprData {
    pub fn needs_semicolon(&self) -> bool {
        match self {
            &FlowExprData::FlowBlock(_) => false,
            _ => true,
        }
    }

    pub fn id(&self) -> NodeId {
        match *self {
            FlowExprData::Lit(ref val) => val.id,
            FlowExprData::Dot(ref val) => val.id,
            FlowExprData::Ident(ref val) => val.id,
            FlowExprData::Path(ref val) => val.id,
            FlowExprData::FlowBlock(ref val) => val.id,
            FlowExprData::Paren(ref val) => val.id,
            FlowExprData::Wire(ref val) => val.id,
            FlowExprData::Error { id, .. } => id,
        }
    }

    pub fn span(&self) -> Span {
        match *self {
            FlowExprData::Lit(ref val) => val.span,
            FlowExprData::Dot(ref val) => val.span,
            FlowExprData::Path(ref val) => val.span,
            FlowExprData::FlowBlock(ref val) => val.span,
            FlowExprData::Paren(ref val) => val.span,
            FlowExprData::Ident(ref val) => val.span,
            FlowExprData::Wire(ref val) => val.span,
            FlowExprData::Error { span, .. } => span,
        }
    }

    pub fn create_dot(
        id: NodeId,
        span: Span,
        op_span: Span,
        lhs: FlowExpr,
        rhs: FlowExpr,
    ) -> FlowExprData {
        FlowExprData::Dot(FlowExprDotType {
            id,
            span,
            op_span,
            lhs,
            rhs,
        })
    }

    pub fn create_wire(id: NodeId, span: Span, lhs: FlowExpr, rhs: FlowExpr) -> FlowExprData {
        FlowExprData::Wire(FlowExprWireType { id, span, lhs, rhs })
    }
}

#[derive(Clone, Debug)]
pub struct FlowExprWireType {
    pub id: NodeId,
    pub span: Span,

    pub lhs: FlowExpr,
    pub rhs: FlowExpr,
}

#[derive(Clone, Debug)]
pub struct FlowExprDotType {
    pub id: NodeId,
    pub span: Span,
    pub op_span: Span,

    pub lhs: FlowExpr,
    pub rhs: FlowExpr,
}

pub type Expr = Arc<ExprKind>;

#[derive(Clone, Debug)]
pub struct FieldValue {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub name: Option<Ident>,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprStruct {
    pub id: NodeId,
    pub span: Span,
    pub name: Expr,
    pub field_values: Vec<FieldValue>,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Un(ExprUnOpType),
    Bin(BinOp, Expr, Expr),
    Literal(Literal),
    //Template(ExprTemplateType),
    Ident(ExprIdentType),
    Call(Call),
    //TypeParam(ExprTypeParamType),
    Path(ExprPathType),
    Dot(Dot),
    Struct(ExprStruct),
    //This(ExprSelfType),
    //Conv(ExprConvType),
    //Is(ExprIsType),
    //Lambda(Arc<Function>),
    Block(ExprBlockType),
    //If(ExprIfType),
    //For(ExprForType),
    //While(ExprWhileType),
    //Tuple(ExprTupleType),
    Paren(Paren),
    //Match(ExprMatchType),
    //Break(ExprBreakType),
    //Continue(ExprContinueType),
    Return(ExprReturnType),
    Error { id: NodeId, span: Span },
}

impl ExprKind {
    pub fn id(&self) -> NodeId {
        match *self {
            ExprKind::Un(ref val) => val.id,
            ExprKind::Bin(ref val, _, _) => val.id,
            //ExprData::LitStr(ref val) => val.id,
            //ExprData::Template(ref val) => val.id,
            ExprKind::Literal(ref val) => val.id,
            ExprKind::Ident(ref val) => val.id,
            ExprKind::Call(ref val) => val.id,
            //ExprData::TypeParam(ref val) => val.id,
            ExprKind::Path(ref val) => val.id,
            ExprKind::Dot(ref val) => val.id,
            ExprKind::Struct(ref val) => val.id,
            //ExprData::This(ref val) => val.id,
            //ExprData::Conv(ref val) => val.id,
            //ExprData::Is(ref val) => val.id,
            //ExprData::Lambda(ref val) => val.id,
            ExprKind::Block(ref val) => val.id,
            //ExprData::If(ref val) => val.id,
            //ExprData::Tuple(ref val) => val.id,
            ExprKind::Paren(ref val) => val.id,
            //ExprData::Match(ref val) => val.id,
            //ExprData::For(ref val) => val.id,
            //ExprData::While(ref val) => val.id,
            //ExprData::Break(ref val) => val.id,
            //ExprData::Continue(ref val) => val.id,
            ExprKind::Return(ref val) => val.id,
            ExprKind::Error { id, .. } => id,
        }
    }

    pub fn span(&self) -> Span {
        match *self {
            ExprKind::Un(ref val) => val.span,
            ExprKind::Bin(ref val, _, _) => val.span,
            //ExprData::LitStr(ref val) => val.span,
            //ExprData::Template(ref val) => val.span,
            ExprKind::Literal(ref val) => val.span,
            ExprKind::Ident(ref val) => val.span,
            ExprKind::Call(ref val) => val.span,
            //ExprData::TypeParam(ref val) => val.span,
            ExprKind::Path(ref val) => val.span,
            ExprKind::Dot(ref val) => val.span,
            ExprKind::Struct(ref val) => val.span,
            //ExprData::This(ref val) => val.span,
            //ExprData::Conv(ref val) => val.span,
            //ExprData::Is(ref val) => val.span,
            //ExprData::Lambda(ref val) => val.span,
            ExprKind::Block(ref val) => val.span,
            //ExprData::If(ref val) => val.span,
            //ExprData::Tuple(ref val) => val.span,
            ExprKind::Paren(ref val) => val.span,
            //ExprData::Match(ref val) => val.span,
            //ExprData::For(ref val) => val.span,
            //ExprData::While(ref val) => val.span,
            //ExprData::Break(ref val) => val.span,
            //ExprData::Continue(ref val) => val.span,
            ExprKind::Return(ref val) => val.span,
            ExprKind::Error { span, .. } => span,
        }
    }

    pub fn needs_semicolon(&self) -> bool {
        match self {
            &ExprKind::Block(_) => false,
            //&ExprData::If(_) => false,
            //&ExprData::Match(_) => false,
            //&ExprData::For(_) => false,
            //&ExprData::While(_) => false,
            _ => true,
        }
    }

    pub fn create_block(
        id: NodeId,
        span: Span,
        green: GreenNode,
        stmts: Vec<Stmt>,
        expr: Option<Expr>,
    ) -> ExprKind {
        ExprKind::Block(ExprBlockType {
            id,
            span,
            green,
            stmts,
            expr,
        })
    }

    pub fn create_dot(id: NodeId, span: Span, op_span: Span, lhs: Expr, rhs: Expr) -> ExprKind {
        ExprKind::Dot(Dot {
            id,
            span,
            op_span,
            lhs,
            rhs,
        })
    }

    pub fn create_return(id: NodeId, span: Span, green: GreenNode, expr: Option<Expr>) -> ExprKind {
        ExprKind::Return(ExprReturnType {
            id,
            span,
            green,
            expr,
        })
    }

    pub fn create_un(id: NodeId, span: Span, green: GreenNode, op: UnOp, opnd: Expr) -> ExprKind {
        ExprKind::Un(ExprUnOpType {
            id,
            span,
            green,
            op,
            opnd,
        })
    }

    pub fn create_bin(id: NodeId, span: Span, op: BinOpKind, lhs: Expr, rhs: Expr) -> ExprKind {
        ExprKind::Bin(
            BinOp {
                id,
                span,
                op,
                initializer: false,
            },
            lhs,
            rhs,
        )
    }

    pub fn create_lit_bool(id: NodeId, span: Span, green: GreenNode, value: bool) -> ExprKind {
        ExprKind::Literal(Literal {
            id,
            span,
            green,
            kind: LitKind::Bool(value),
        })
    }

    pub fn create_lit_int(id: NodeId, span: Span, green: GreenNode, value: String) -> ExprKind {
        ExprKind::Literal(Literal {
            id,
            span,
            green,
            kind: LitKind::Int(value),
        })
    }

    pub fn create_ident(id: NodeId, span: Span, green: GreenNode, name: String) -> ExprKind {
        ExprKind::Ident(ExprIdentType {
            id,
            span,
            green,
            name,
        })
    }

    pub fn create_path(id: NodeId, span: Span, op_span: Span, lhs: Expr, rhs: Expr) -> ExprKind {
        ExprKind::Path(ExprPathType {
            id,
            span,
            op_span,
            lhs,
            rhs,
        })
    }

    pub fn create_call(id: NodeId, span: Span, callee: Expr, args: Vec<Expr>) -> ExprKind {
        ExprKind::Call(Call {
            id,
            span,
            callee,
            args,
        })
    }

    pub fn create_paren(id: NodeId, span: Span, green: GreenNode, expr: Expr) -> ExprKind {
        ExprKind::Paren(Paren {
            id,
            span,
            green,
            expr,
        })
    }

    pub fn to_block(&self) -> Option<&ExprBlockType> {
        match *self {
            ExprKind::Block(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_return(&self) -> Option<&ExprReturnType> {
        match *self {
            ExprKind::Return(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_lit(&self) -> Option<&Literal> {
        match *self {
            ExprKind::Literal(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_path(&self) -> Option<&ExprPathType> {
        match *self {
            ExprKind::Path(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_un(&self) -> Option<&ExprUnOpType> {
        match *self {
            ExprKind::Un(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_bin(&self) -> Option<(&BinOp, &Expr, &Expr)> {
        match *self {
            ExprKind::Bin(ref val, ref lhs, ref rhs) => Some((val, lhs, rhs)),
            _ => None,
        }
    }

    pub fn to_ident(&self) -> Option<&ExprIdentType> {
        match *self {
            ExprKind::Ident(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_call(&self) -> Option<&Call> {
        match *self {
            ExprKind::Call(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_paren(&self) -> Option<&Paren> {
        match *self {
            ExprKind::Paren(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit(&self) -> bool {
        match *self {
            ExprKind::Literal(_) => true,
            _ => false,
        }
    }

    pub fn is_lit_int(&self) -> bool {
        match *self {
            ExprKind::Literal(ref lit) => lit.is_int(),
            _ => false,
        }
    }

    pub fn is_ident(&self) -> bool {
        match *self {
            ExprKind::Ident(_) => true,
            _ => false,
        }
    }

    pub fn is_bin(&self) -> bool {
        match *self {
            ExprKind::Bin(_, _, _) => true,
            _ => false,
        }
    }

    pub fn is_return(&self) -> bool {
        match *self {
            ExprKind::Return(_) => true,
            _ => false,
        }
    }

    pub fn is_path(&self) -> bool {
        match *self {
            ExprKind::Path(_) => true,
            _ => false,
        }
    }

    pub fn is_call(&self) -> bool {
        match *self {
            ExprKind::Call(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct LetIdentType {
    pub id: NodeId,
    pub span: Span,
    pub mutable: bool,
    pub name: Option<Ident>,
}

#[derive(Clone, Debug)]
pub enum PatternKind {
    Ident(LetIdentType),
}

#[derive(Clone, Debug)]
pub struct StmtLetType {
    pub id: NodeId,
    pub span: Span,
    pub pattern: Box<PatternKind>,
    pub data_type: Option<Type>,
    pub expr: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct StmtExprType {
    pub id: NodeId,
    pub span: Span,

    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprUnOpType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub op: UnOp,
    pub opnd: Expr,
}

#[derive(Clone, Copy, Debug)]
pub struct BinOp {
    pub id: NodeId,
    pub span: Span,

    pub op: BinOpKind,
    pub initializer: bool,
}

#[derive(Clone, Debug)]
pub struct Paren {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct Dot {
    pub id: NodeId,
    pub span: Span,
    pub op_span: Span,

    pub lhs: Expr,
    pub rhs: Expr,
}

/// This type is used within both `ast::MetaItemLit` and `hir::Lit`.
///
/// Note that the entire literal (including the suffix) is considered when
/// deciding the `LitKind`. This means that float literals like `1f32` are
/// classified by this type as `Float`. This is different to `token::LitKind`
/// which does *not* consider the suffix.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum LitKind {
    /// A struct literal `{ test: 10 }`.
    Struct,
    /// A character literal `'a'`.
    Char(char),
    /// An integer literal `1`.
    Int(String),
    /// A float literal `1.0`, `1f64` or `1E10f64`. The pre-suffix part is
    /// stored as a symbol rather than `f64` so that `LitKind` can impl `Eq`
    /// and `Hash`.
    Float(String),
    /// A boolean literal `true` or `false`.
    Bool(bool),
    /// A unit literal `()`.
    Unit,
    // Placeholder for a literal that wasn't well-formed in some way.
    // Err(ErrorGuaranteed),
}

#[derive(Clone, Debug)]
pub struct Literal {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub kind: LitKind,
}

impl Literal {
    pub fn is_int(&self) -> bool {
        match self.kind {
            LitKind::Int(_) => true,
            _ => false,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self.kind {
            LitKind::Bool(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&String> {
        match self.kind {
            LitKind::Int(ref value) => Some(value),
            LitKind::Float(ref value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprLitCharType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub value: String,
}

#[derive(Clone, Debug)]
pub struct ExprLitBoolType {
    pub id: NodeId,
    pub span: Span,

    pub value: bool,
}

#[derive(Clone, Debug)]
pub struct ExprLitIntType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub value: String,
}

#[derive(Clone, Debug)]
pub struct ExprLitFloatType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub value: String,
}

#[derive(Clone, Debug)]
pub struct ExprReturnType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub expr: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprIdentType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct ExprBlockType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub stmts: Vec<Stmt>,
    pub expr: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprPathType {
    pub id: NodeId,
    pub span: Span,
    pub op_span: Span,

    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Clone, Debug)]
pub struct Call {
    pub id: NodeId,
    pub span: Span,

    pub callee: Expr,
    pub args: Vec<Expr>,
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum UnOp {
    Neg,
    Not,
}

impl UnOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            UnOp::Neg => "-",
            UnOp::Not => "!",
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Is,
    IsNot,
}

impl CmpOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            CmpOp::Eq => "==",
            CmpOp::Ne => "!=",
            CmpOp::Lt => "<",
            CmpOp::Le => "<=",
            CmpOp::Gt => ">",
            CmpOp::Ge => ">=",
            CmpOp::Is => "===",
            CmpOp::IsNot => "!==",
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum BinOpKind {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Cmp(CmpOp),
    Or,
    And,
    BitOr,
    BitAnd,
    BitXor,
    ShiftL,
    ArithShiftR,
    LogicalShiftR,
}

impl BinOpKind {
    pub fn as_str(&self) -> &'static str {
        match *self {
            BinOpKind::Assign => "=",
            BinOpKind::Add => "+",
            BinOpKind::Sub => "-",
            BinOpKind::Mul => "*",
            BinOpKind::Div => "/",
            BinOpKind::Mod => "%",
            BinOpKind::Cmp(op) => op.as_str(),
            BinOpKind::Or => "||",
            BinOpKind::And => "&&",
            BinOpKind::BitOr => "|",
            BinOpKind::BitAnd => "&",
            BinOpKind::BitXor => "^",
            BinOpKind::ShiftL => "<<",
            BinOpKind::ArithShiftR => ">>",
            BinOpKind::LogicalShiftR => ">>>",
        }
    }

    pub fn is_any_assign(&self) -> bool {
        match *self {
            BinOpKind::Assign => true,
            _ => false,
        }
    }
}
