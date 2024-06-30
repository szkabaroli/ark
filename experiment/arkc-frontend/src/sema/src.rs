use std::{
    cell::{Cell, OnceCell},
    collections::{hash_map::Iter, HashMap},
    rc::Rc,
    sync::Arc,
};

use bytecode::Intrinsic;
use parser::ast;

use crate::ty::{SourceType, SourceTypeArray};

use super::FctDefinitionId;

#[derive(Debug)]
pub struct AnalysisData {
    pub has_self: Option<bool>,
    pub map_calls: NodeMap<Arc<CallType>>, // maps function call to FctId
    pub map_idents: NodeMap<IdentType>,
    pub map_tys: NodeMap<SourceType>,
    pub map_vars: NodeMap<VarId>,
    pub map_literals: NodeMap<(i64, f64)>,

    // All variables defined in this function (including
    // context allocated ones).
    pub vars: VarAccess,

    pub function_context_data: OnceCell<LazyContextData>,
}

impl AnalysisData {
    pub fn new() -> AnalysisData {
        AnalysisData {
            has_self: None,
            // map_templates: NodeMap::new(),
            map_calls: NodeMap::new(),
            map_idents: NodeMap::new(),
            map_tys: NodeMap::new(),
            map_vars: NodeMap::new(),
            // map_cls: NodeMap::new(),
            // map_fors: NodeMap::new(),
            // map_lambdas: NodeMap::new(),
            map_literals: NodeMap::new(),
            // map_char_literals: NodeMap::new(),
            // map_string_literals: NodeMap::new(),
            // map_block_contexts: NodeMap::new(),
            vars: VarAccess::empty(),
            function_context_data: OnceCell::new(),
            // needs_context_slot_in_lambda_object: OnceCell::new(),
            // outer_contexts: Vec::new(),
        }
    }

    pub fn function_context_data(&self) -> LazyContextData {
        self.function_context_data
            .get()
            .cloned()
            .expect("missing context")
    }

    pub fn set_literal_value(&mut self, id: ast::NodeId, value_i64: i64, value_f64: f64) {
        self.map_literals.insert(id, (value_i64, value_f64));
    }

    pub fn literal_value(&self, id: ast::NodeId) -> (i64, f64) {
        self.map_literals.get(id).expect("no literal found").clone()
    }


    pub fn set_has_self(&mut self, value: bool) {
        self.has_self = Some(value);
    }

    pub fn has_self(&self) -> bool {
        self.has_self.expect("has_self uninitialized")
    }

    pub fn set_ty(&mut self, id: ast::NodeId, ty: SourceType) {
        self.map_tys.insert_or_replace(id, ty);
    }

    pub fn ty(&self, id: ast::NodeId) -> SourceType {
        self.map_tys.get(id).expect("no type found").clone()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct InnerContextId(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ContextFieldId(pub usize);

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct NestedScopeId(pub usize);

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct ScopeId(pub usize);

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct NestedVarId(pub usize);

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct VarId(pub usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VarLocation {
    Stack,
    Context(ScopeId, ContextFieldId),
}

impl VarLocation {
    pub fn is_stack(&self) -> bool {
        match self {
            VarLocation::Stack => true,
            VarLocation::Context(..) => false,
        }
    }

    pub fn is_context(&self) -> bool {
        match self {
            VarLocation::Context(..) => true,
            VarLocation::Stack => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Var {
    pub ty: SourceType,
    pub location: VarLocation,
}

#[derive(Debug, Clone)]
pub enum CallType {
    // Function calls, e.g. fct(<args>) or Class::static_fct(<args>).
    Function(FctDefinitionId, SourceTypeArray),

    // Used for internal functions (those are not exposed to Dora as Fct). Used for enum comparisons.
    Intrinsic(Intrinsic),
}

impl CallType {
    pub fn to_intrinsic(&self) -> Option<Intrinsic> {
        match *self {
            CallType::Intrinsic(intrinsic) => Some(intrinsic),
            _ => None,
        }
    }

    pub fn fct_id(&self) -> Option<FctDefinitionId> {
        match *self {
            CallType::Function(fctid, _) => Some(fctid),
            //CallType::Method(_, fctid, _) => Some(fctid),
            //CallType::Expr(_, fctid, _) => Some(fctid),
            //CallType::TraitObjectMethod(_, fctid) => Some(fctid),
            //CallType::GenericMethod(_, _, fctid, _) => Some(fctid),
            //CallType::GenericStaticMethod(_, _, fctid, _) => Some(fctid),

            //CallType::NewClass(..)
            //| CallType::NewStruct(..)
            //| CallType::NewEnum(..)
            //| CallType::Lambda(..)
            CallType::Intrinsic(..) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum IdentType {
    /// Name of local variable.
    Var(VarId),

    // Context variable.
    // Context(OuterContextIdx, ContextFieldId),

    // Name of a global variable.
    //Global(GlobalDefinitionId),

    // Field expression: <expr>.<field_name>
    //Field(SourceType, FieldId),

    // Field expression: <expr>.<field_name>
    //StructField(SourceType, StructDefinitionFieldId),

    // Name of structure.
    //Struct(StructDefinitionId),

    // Name of constant.
    //Const(ConstDefinitionId),

    // Name of function with type params: some_function[T1, T2, ...].
    Function(FctDefinitionId, SourceTypeArray),
    // Name of class with type params: SomeClass[T1, T2, ...].
    //Class(ClassDefinitionId, SourceTypeArray),

    // Specific enum variant.
    //EnumVariant(EnumDefinitionId, SourceTypeArray, u32),
}

#[derive(Clone, Debug)]
pub struct LazyContextData(Rc<ContextData>);

impl LazyContextData {
    pub fn new() -> LazyContextData {
        LazyContextData(Rc::new(ContextData {
            has_parent_slot: Cell::new(false),
            //class_id: OnceCell::new(),
        }))
    }

    pub fn has_parent_slot(&self) -> bool {
        self.0.has_parent_slot.get()
    }
}

#[derive(Clone, Debug)]
pub struct ContextData {
    pub has_parent_slot: Cell<bool>,
    //pub class_id: OnceCell<ClassDefinitionId>,
}

#[derive(Clone, Debug)]
pub struct NodeMap<V>
where
    V: Clone,
{
    map: HashMap<ast::NodeId, V>,
}

impl<V> NodeMap<V>
where
    V: Clone,
{
    pub fn new() -> NodeMap<V> {
        NodeMap {
            map: HashMap::new(),
        }
    }

    pub fn get(&self, id: ast::NodeId) -> Option<&V> {
        self.map.get(&id)
    }

    pub fn get_mut(&mut self, id: ast::NodeId) -> Option<&mut V> {
        self.map.get_mut(&id)
    }

    pub fn insert(&mut self, id: ast::NodeId, data: V) {
        let old = self.map.insert(id, data);
        assert!(old.is_none());
    }

    pub fn replace(&mut self, id: ast::NodeId, data: V) {
        let old = self.map.insert(id, data);
        assert!(old.is_some());
    }

    pub fn insert_or_replace(&mut self, id: ast::NodeId, data: V) {
        self.map.insert(id, data);
    }

    pub fn clear(&mut self) {
        self.map.clear();
    }

    pub fn iter(&self) -> Iter<ast::NodeId, V> {
        self.map.iter()
    }
}

#[derive(Debug)]
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

    pub fn get_var(&self, idx: VarId) -> &Var {
        &self.vars[idx.0]
    }

    pub fn get_self(&self) -> &Var {
        &self.vars[0]
    }
}
