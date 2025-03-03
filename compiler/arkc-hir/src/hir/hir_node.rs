use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::hir_map::HirMap;
use crate::parsety;
use crate::ty::{self, PrimitiveType, Type};

use super::{FnBodyId, HirId};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct NodeMap<V>
where
    V: Clone,
{
    map: HashMap<HirId, V>,
}

impl<V: Clone> NodeMap<V> {
    pub fn new() -> NodeMap<V> {
        NodeMap {
            map: HashMap::new(),
        }
    }

    pub fn extend(&mut self, rhs: NodeMap<V>) {
        self.map.extend(rhs.map);
    }

    pub fn get(&self, id: HirId) -> Option<&V> {
        self.map.get(&id)
    }

    pub fn insert(&mut self, id: HirId, data: V) {
        let old = self.map.insert(id, data);
        assert!(old.is_none());
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash, Serialize, Deserialize)]
pub struct NestedVarId(pub usize);

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash, Serialize, Deserialize)]
pub struct VarId(pub usize);

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum IdentType {
    /// Name of local variable.
    Var(VarId),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CallType {
    // Function calls, e.g. func(<args>) or Class.static_func(<args>).
    Func(HirId, ty::TypeArray),
}

impl CallType {
    pub fn func_id(&self) -> Option<HirId> {
        match *self {
            CallType::Func(func_id, _) => Some(func_id),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Var {
    pub ty: Type,
    // pub location: VarLocation,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VarAccess {
    vars: Vec<Var>,
}

impl VarAccess {
    pub fn new(vars: Vec<Var>) -> VarAccess {
        VarAccess { vars }
    }

    fn empty() -> VarAccess {
        VarAccess { vars: Vec::new() }
    }

    pub fn get_var(&self, id: VarId) -> &Var {
        &self.vars[id.0]
    }

    pub fn get_self(&self) -> &Var {
        &self.vars[0]
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalysisData {
    pub idents: NodeMap<IdentType>,
    pub map_vars: NodeMap<VarId>,
    pub map_calls: NodeMap<Arc<CallType>>,
    pub int_literals: NodeMap<i64>,
    pub vars: VarAccess,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct File {
    #[serde(skip)]
    pub hir_map: Option<HirMap>,
    pub node_types: BTreeMap<HirId, Type>,
    pub func_analysis: NodeMap<AnalysisData>,
    pub bodies: BTreeMap<FnBodyId, FnBody>,
    pub elements: Vec<Elem>,
}

impl File {
    pub fn get_body(&self, body_id: &FnBodyId) -> Option<&FnBody> {
        self.bodies.get(body_id)
    }

    pub fn get_fn_analisis(&self, hir_id: &HirId) -> Option<&AnalysisData> {
        self.func_analysis.get(*hir_id)
    }

    pub fn get_fn(&self, hir_id: &HirId) -> Option<&FnDeclaration> {
        self.elements
            .iter()
            .find(|elem| elem.kind.get_hir_id() == *hir_id)
            .and_then(|top| {
                if let ElemKind::Function(f) = &top.kind {
                    Some(f)
                } else {
                    None
                }
            })
    }

    pub fn get_struct_mut(&mut self, hir_id: &HirId) -> Option<&mut Struct> {
        self.elements
            .iter_mut()
            .find(|elem| elem.kind.get_hir_id() == *hir_id)
            .and_then(|top| {
                if let ElemKind::Struct(f) = &mut top.kind {
                    Some(f)
                } else {
                    None
                }
            })
    }

    pub fn get_struct(&self, hir_id: &HirId) -> Option<&Struct> {
        self.elements
            .iter()
            .find(|elem| elem.kind.get_hir_id() == *hir_id)
            .and_then(|top| {
                if let ElemKind::Struct(f) = &top.kind {
                    Some(f)
                } else {
                    None
                }
            })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BasicType {
    pub path: Path,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FnType {
    pub params: Vec<TypeData>,
    pub ret: Option<Box<TypeData>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeData {
    _Self,
    Unknown,
    Basic(BasicType),
    Fn(FnType),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Elem {
    pub kind: ElemKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ElemKind {
    Function(FnDeclaration),
    Struct(Struct),
    Extern(FnDefinition),
    Signature(FnDefinition),
}

impl ElemKind {
    pub fn get_hir_id(&self) -> HirId {
        match &self {
            ElemKind::Extern(elem) => elem.hir_id.clone(),
            ElemKind::Function(elem) => elem.hir_id.clone(),
            ElemKind::Signature(elem) => elem.hir_id.clone(),
            ElemKind::Struct(elem) => elem.hir_id.clone(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FnDefinition {
    pub name: Identifier,
    pub hir_id: HirId,
    pub signature: FnType,
}

impl std::ops::Deref for Identifier {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.name
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FnDeclaration {
    pub name: Option<Identifier>,
    pub mangled_name: Option<Identifier>,
    pub analysis: Option<AnalysisData>,
    //pub arguments: Vec<ArgumentDecl>,
    pub body_id: FnBodyId,
    pub hir_id: HirId,
    pub signature: FnType,
    pub return_type: parsety::ParsedType
}

impl FnDeclaration {
    pub fn get_name(&self) -> String {
        match &self.mangled_name {
            Some(ref name) => name.name.clone(),
            None => {
                if let Some(ref name) = self.name {
                    name.name.clone()
                } else {
                    String::from("")
                }
            }
        }
    }

    pub fn return_type(&self) -> ty::Type {
        self.parsed_return_type().ty()
    }

    pub fn parsed_return_type(&self) -> &parsety::ParsedType {
        &self.return_type
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructField {
    pub hir_id: HirId,
    pub name: Identifier,
    pub ty: TypeData,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Struct {
    pub hir_id: HirId,
    pub name: Identifier,
    pub fields: Vec<StructField>,
    pub primitive_ty: Option<PrimitiveType>,
    pub is_internal: bool,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Identifier {
    pub hir_id: HirId,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Path {
    pub names: Vec<Identifier>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FnBody {
    pub id: FnBodyId,
    pub fn_id: HirId,
    pub name: Option<Identifier>,
    pub mangled_name: Option<Identifier>,
    pub body: Block,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Block {
    pub stmts: Vec<Statement>,
    pub expr: Option<Expr>,
}

pub enum TypeKind {
    Any,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Statement {
    pub kind: Box<StatementKind>,
}

impl Statement {
    pub fn is_return(&self) -> bool {
        match &*self.kind {
            StatementKind::Expr(expr) => expr.is_return(),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PatternKind {
    Ident(Identifier),
}

impl PatternKind {
    pub fn to_name(&self) -> Option<String> {
        match self {
            PatternKind::Ident(ref ident) => Some(ident.name.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LetBinding {
    pub hir_id: HirId,
    pub pattern: PatternKind,
    pub ty: Option<TypeData>,
    pub expr: Option<Expr>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum StatementKind {
    Expr(Expr),
    Let(LetBinding),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Expr {
    pub hir_id: HirId,
    pub kind: Box<ExprKind>,
}

impl Expr {
    pub fn to_struct(&self) -> Option<&ExprStruct> {
        match *self.kind {
            ExprKind::Struct(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_ident(&self) -> Option<&Identifier> {
        match *self.kind {
            ExprKind::Ident(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_dot(&self) -> Option<&Dot> {
        match *self.kind {
            ExprKind::Dot(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_path(&self) -> Option<&ExprPath> {
        match *self.kind {
            ExprKind::Path(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_return(&self) -> bool {
        if let ExprKind::Return(_) = &*self.kind {
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldValue {
    pub hir_id: HirId,
    pub name: Identifier,
    pub value: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExprStruct {
    pub hir_id: HirId,
    pub name: Expr,
    pub fields: Vec<FieldValue>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExprPath {
    pub hir_id: HirId,
    pub rhs: Expr,
    pub lhs: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExprKind {
    Block(Block),
    Struct(ExprStruct),
    Lit(Literal),
    Ident(Identifier),
    Call(FnCall),
    Dot(Dot),
    Path(ExprPath),
    Bin(BinOp, Expr, Expr),
    Return(Option<Expr>),
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash, Serialize, Deserialize)]
pub enum BinOpKind {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    //Cmp(CmpOp),
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
            //BinOpKind::AddAssign => "+=",
            BinOpKind::Sub => "-",
            //BinOpKind::SubAssign => "-=",
            BinOpKind::Mul => "*",
            //BinOpKind::MulAssign => "*=",
            BinOpKind::Div => "/",
            //BinOpKind::DivAssign => "/=",
            BinOpKind::Mod => "%",
            //BinOpKind::ModAssign => "%=",
            //BinOpKind::Cmp(op) => op.as_str(),
            BinOpKind::Or => "||",
            BinOpKind::And => "&&",
            BinOpKind::BitOr => "|",
            //BinOpKind::BitOrAssign => "|=",
            BinOpKind::BitAnd => "&",
            //BinOpKind::BitAndAssign => "&=",
            BinOpKind::BitXor => "^",
            //BinOpKind::BitXorAssign => "^=",
            BinOpKind::ShiftL => "<<",
            //BinOpKind::ShiftLAssign => "<<=",
            BinOpKind::ArithShiftR => ">>",
            //BinOpKind::ArithShiftRAssign => ">>=",
            BinOpKind::LogicalShiftR => ">>>",
            //BinOpKind::LogicalShiftRAssign => ">>>=",
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BinOp {
    pub hir_id: HirId,
    pub kind: BinOpKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dot {
    pub hir_id: HirId,
    pub op: Expr,
    pub value: Identifier,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Argument {
    pub hir_id: HirId,
    pub name: Option<Identifier>,
    pub expr: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FnCall {
    pub hir_id: HirId,
    pub callee: Expr,
    pub args: Vec<Arc<Argument>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Literal {
    pub hir_id: HirId,
    pub kind: LiteralKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LiteralKind {
    Int(String),
    Float(String),
    String(String),
    Bool(bool),
    Char(char),
    Unit,
}
