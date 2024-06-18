pub mod visit;

use std::fmt;
use std::slice::Iter;
use std::sync::Arc;

use crate::{green::GreenNode, span::Span};

#[derive(Clone, Debug)]
pub struct File {
    pub green: GreenNode,
    pub elements: Vec<Elem>,
}

impl File {
    #[cfg(test)]
    pub fn fct0(&self) -> &Function {
        self.elements[0].to_function().unwrap()
    }

    #[cfg(test)]
    pub fn fct(&self, index: usize) -> &Function {
        self.elements[index].to_function().unwrap()
    }

    #[cfg(test)]
    pub fn flow(&self, index: usize) -> &Flow {
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

pub type Elem = Arc<ElemData>;

#[derive(Clone, Debug)]
pub enum ElemData {
    Function(Arc<Function>),
    Flow(Arc<Flow>),
    Const(Arc<Const>),
    Error { id: NodeId, span: Span },
}

impl ElemData {
    pub fn to_function(&self) -> Option<&Function> {
        match self {
            &ElemData::Function(ref fct) => Some(fct),
            _ => None,
        }
    }

    pub fn to_flow(&self) -> Option<&Flow> {
        match self {
            &ElemData::Flow(ref flow) => Some(flow),
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
pub struct Flow {
    pub id: NodeId,
    pub declaration_span: Span,
    pub span: Span,
    pub green: GreenNode,
    pub name: Option<Ident>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub block: Option<FlowExpr>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: NodeId,
    pub declaration_span: Span,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    // pub kind: FunctionKind,
    pub name: Option<Ident>,
    // pub type_params: Option<TypeParams>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    //pub where_bounds: Option<WhereBounds>,
    pub block: Option<Expr>,
}

impl Function {
    pub fn block(&self) -> &Expr {
        self.block.as_ref().unwrap()
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
    // Let(StmtLetType),
    Expr(StmtExprType),
}

impl StmtData {
    pub fn create_expr(id: NodeId, span: Span, expr: Expr) -> StmtData {
        StmtData::Expr(StmtExprType { id, span, expr })
    }

    pub fn span(&self) -> Span {
        match *self {
            // StmtData::Let(ref stmt) => stmt.span,
            StmtData::Expr(ref stmt) => stmt.span,
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
    LitChar(ExprLitCharType),
    LitInt(ExprLitIntType),
    Ident(ExprIdentType),
    LitFloat(ExprLitFloatType),
    LitBool(ExprLitBoolType),
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
            FlowExprData::LitChar(ref val) => val.id,
            FlowExprData::LitInt(ref val) => val.id,
            FlowExprData::LitFloat(ref val) => val.id,
            FlowExprData::LitBool(ref val) => val.id,
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
            FlowExprData::LitChar(ref val) => val.span,
            FlowExprData::LitInt(ref val) => val.span,
            FlowExprData::LitFloat(ref val) => val.span,
            FlowExprData::LitBool(ref val) => val.span,
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

pub type Expr = Arc<ExprData>;

#[derive(Clone, Debug)]
pub enum ExprData {
    Un(ExprUnType),
    Bin(ExprBinType),
    LitChar(ExprLitCharType),
    LitInt(ExprLitIntType),
    LitFloat(ExprLitFloatType),
    LitBool(ExprLitBoolType),
    //LitStr(ExprLitStrType),
    //Template(ExprTemplateType),
    Ident(ExprIdentType),
    Call(ExprCallType),
    //TypeParam(ExprTypeParamType),
    Path(ExprPathType),
    Dot(ExprDotType),
    //This(ExprSelfType),
    //Conv(ExprConvType),
    //Is(ExprIsType),
    //Lambda(Arc<Function>),
    Block(ExprBlockType),
    //If(ExprIfType),
    //For(ExprForType),
    //While(ExprWhileType),
    //Tuple(ExprTupleType),
    Paren(ExprParenType),
    //Match(ExprMatchType),
    //Break(ExprBreakType),
    //Continue(ExprContinueType),
    Return(ExprReturnType),
    Error { id: NodeId, span: Span },
}

impl ExprData {
    pub fn id(&self) -> NodeId {
        match *self {
            ExprData::Un(ref val) => val.id,
            ExprData::Bin(ref val) => val.id,
            ExprData::LitChar(ref val) => val.id,
            ExprData::LitInt(ref val) => val.id,
            ExprData::LitFloat(ref val) => val.id,
            //ExprData::LitStr(ref val) => val.id,
            //ExprData::Template(ref val) => val.id,
            ExprData::LitBool(ref val) => val.id,
            ExprData::Ident(ref val) => val.id,
            ExprData::Call(ref val) => val.id,
            //ExprData::TypeParam(ref val) => val.id,
            ExprData::Path(ref val) => val.id,
            ExprData::Dot(ref val) => val.id,
            //ExprData::This(ref val) => val.id,
            //ExprData::Conv(ref val) => val.id,
            //ExprData::Is(ref val) => val.id,
            //ExprData::Lambda(ref val) => val.id,
            ExprData::Block(ref val) => val.id,
            //ExprData::If(ref val) => val.id,
            //ExprData::Tuple(ref val) => val.id,
            ExprData::Paren(ref val) => val.id,
            //ExprData::Match(ref val) => val.id,
            //ExprData::For(ref val) => val.id,
            //ExprData::While(ref val) => val.id,
            //ExprData::Break(ref val) => val.id,
            //ExprData::Continue(ref val) => val.id,
            ExprData::Return(ref val) => val.id,
            ExprData::Error { id, .. } => id,
        }
    }

    pub fn span(&self) -> Span {
        match *self {
            ExprData::Un(ref val) => val.span,
            ExprData::Bin(ref val) => val.span,
            ExprData::LitChar(ref val) => val.span,
            ExprData::LitInt(ref val) => val.span,
            ExprData::LitFloat(ref val) => val.span,
            //ExprData::LitStr(ref val) => val.span,
            //ExprData::Template(ref val) => val.span,
            ExprData::LitBool(ref val) => val.span,
            ExprData::Ident(ref val) => val.span,
            ExprData::Call(ref val) => val.span,
            //ExprData::TypeParam(ref val) => val.span,
            ExprData::Path(ref val) => val.span,
            ExprData::Dot(ref val) => val.span,
            //ExprData::This(ref val) => val.span,
            //ExprData::Conv(ref val) => val.span,
            //ExprData::Is(ref val) => val.span,
            //ExprData::Lambda(ref val) => val.span,
            ExprData::Block(ref val) => val.span,
            //ExprData::If(ref val) => val.span,
            //ExprData::Tuple(ref val) => val.span,
            ExprData::Paren(ref val) => val.span,
            //ExprData::Match(ref val) => val.span,
            //ExprData::For(ref val) => val.span,
            //ExprData::While(ref val) => val.span,
            //ExprData::Break(ref val) => val.span,
            //ExprData::Continue(ref val) => val.span,
            ExprData::Return(ref val) => val.span,
            ExprData::Error { span, .. } => span,
        }
    }

    pub fn needs_semicolon(&self) -> bool {
        match self {
            &ExprData::Block(_) => false,
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
    ) -> ExprData {
        ExprData::Block(ExprBlockType {
            id,
            span,
            green,
            stmts,
            expr,
        })
    }

    pub fn create_dot(id: NodeId, span: Span, op_span: Span, lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Dot(ExprDotType {
            id,
            span,
            op_span,
            lhs,
            rhs,
        })
    }

    pub fn create_return(id: NodeId, span: Span, green: GreenNode, expr: Option<Expr>) -> ExprData {
        ExprData::Return(ExprReturnType {
            id,
            span,
            green,
            expr,
        })
    }

    pub fn create_un(id: NodeId, span: Span, green: GreenNode, op: UnOp, opnd: Expr) -> ExprData {
        ExprData::Un(ExprUnType {
            id,
            span,
            green,
            op,
            opnd,
        })
    }

    pub fn create_bin(id: NodeId, span: Span, op: BinOp, lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Bin(ExprBinType {
            id,
            span,
            op,
            initializer: false,
            lhs,
            rhs,
        })
    }

    pub fn create_lit_bool(id: NodeId, span: Span, value: bool) -> ExprData {
        ExprData::LitBool(ExprLitBoolType { id, span, value })
    }

    pub fn create_lit_int(id: NodeId, span: Span, green: GreenNode, value: String) -> ExprData {
        ExprData::LitInt(ExprLitIntType {
            id,
            span,
            green,
            value,
        })
    }

    pub fn create_ident(id: NodeId, span: Span, green: GreenNode, name: String) -> ExprData {
        ExprData::Ident(ExprIdentType {
            id,
            span,
            green,
            name,
        })
    }

    pub fn create_path(id: NodeId, span: Span, op_span: Span, lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Path(ExprPathType {
            id,
            span,
            op_span,
            lhs,
            rhs,
        })
    }

    pub fn create_call(id: NodeId, span: Span, callee: Expr, args: Vec<Expr>) -> ExprData {
        ExprData::Call(ExprCallType {
            id,
            span,
            callee,
            args,
        })
    }

    pub fn create_paren(id: NodeId, span: Span, green: GreenNode, expr: Expr) -> ExprData {
        ExprData::Paren(ExprParenType {
            id,
            span,
            green,
            expr,
        })
    }

    pub fn to_block(&self) -> Option<&ExprBlockType> {
        match *self {
            ExprData::Block(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_return(&self) -> Option<&ExprReturnType> {
        match *self {
            ExprData::Return(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_lit_int(&self) -> Option<&ExprLitIntType> {
        match *self {
            ExprData::LitInt(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_lit_bool(&self) -> Option<&ExprLitBoolType> {
        match *self {
            ExprData::LitBool(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_path(&self) -> Option<&ExprPathType> {
        match *self {
            ExprData::Path(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_un(&self) -> Option<&ExprUnType> {
        match *self {
            ExprData::Un(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_bin(&self) -> Option<&ExprBinType> {
        match *self {
            ExprData::Bin(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_ident(&self) -> Option<&ExprIdentType> {
        match *self {
            ExprData::Ident(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_call(&self) -> Option<&ExprCallType> {
        match *self {
            ExprData::Call(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_paren(&self) -> Option<&ExprParenType> {
        match *self {
            ExprData::Paren(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_int(&self) -> bool {
        match *self {
            ExprData::LitInt(_) => true,
            _ => false,
        }
    }

    pub fn is_ident(&self) -> bool {
        match *self {
            ExprData::Ident(_) => true,
            _ => false,
        }
    }

    pub fn is_bin(&self) -> bool {
        match *self {
            ExprData::Bin(_) => true,
            _ => false,
        }
    }

    pub fn is_return(&self) -> bool {
        match *self {
            ExprData::Return(_) => true,
            _ => false,
        }
    }

    pub fn is_path(&self) -> bool {
        match *self {
            ExprData::Path(_) => true,
            _ => false,
        }
    }

    pub fn is_call(&self) -> bool {
        match *self {
            ExprData::Call(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct StmtExprType {
    pub id: NodeId,
    pub span: Span,

    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprUnType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub op: UnOp,
    pub opnd: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprBinType {
    pub id: NodeId,
    pub span: Span,

    pub op: BinOp,
    pub initializer: bool,
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprParenType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprDotType {
    pub id: NodeId,
    pub span: Span,
    pub op_span: Span,

    pub lhs: Expr,
    pub rhs: Expr,
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
pub struct ExprCallType {
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
pub enum BinOp {
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

impl BinOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            BinOp::Assign => "=",
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Cmp(op) => op.as_str(),
            BinOp::Or => "||",
            BinOp::And => "&&",
            BinOp::BitOr => "|",
            BinOp::BitAnd => "&",
            BinOp::BitXor => "^",
            BinOp::ShiftL => "<<",
            BinOp::ArithShiftR => ">>",
            BinOp::LogicalShiftR => ">>>",
        }
    }

    pub fn is_any_assign(&self) -> bool {
        match *self {
            BinOp::Assign => true,
            _ => false,
        }
    }
}
