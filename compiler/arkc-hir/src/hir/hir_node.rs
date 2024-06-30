use std::collections::{BTreeMap, HashMap};

use serde::{Deserialize, Serialize};

use crate::hir_map::HirMap;
use crate::ty::{PrimitiveType, Type};

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

    pub fn extend(&mut self, rhs: NodeMap<V>){
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

#[derive(Debug, Deserialize, Serialize)]
pub struct File {
    #[serde(skip)]
    pub hir_map: Option<HirMap>,
    pub node_types: BTreeMap<HirId, Type>,
    pub int_literals: NodeMap<i64>,
    pub bodies: BTreeMap<FnBodyId, FnBody>,
    pub elements: Vec<Elem>,
}

impl File {
    pub fn get_body(&self, body_id: &FnBodyId) -> Option<&FnBody> {
        self.bodies.get(body_id)
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
    //pub arguments: Vec<ArgumentDecl>,
    pub body_id: FnBodyId,
    pub hir_id: HirId,
    pub signature: FnType,
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
    pub value: Expr
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExprStruct {
    pub hir_id: HirId,
    pub name: Expr,
    pub fields: Vec<FieldValue>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExprKind {
    Block(Block),
    Struct(ExprStruct),
    Lit(Literal),
    Ident(Identifier),
    Call(FnCall),
    Dot(Dot),
    Return(Option<Expr>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dot {
    pub hir_id: HirId,
    pub op: Expr,
    pub value: Identifier,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FnCall {
    pub hir_id: HirId,
    pub op: Expr,
    pub args: Vec<Expr>,
    pub ty: Option<Type>,
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
