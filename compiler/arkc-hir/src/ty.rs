use std::{ops::Index, sync::Arc};

use serde::{Deserialize, Serialize};

use crate::hir::HirId;

/// Function types.
#[derive(Clone, Eq, PartialEq, Debug, Hash, Serialize, Deserialize)]
pub struct FnType {
    pub inputs: Vec<Type>,
    pub output: Option<Box<Type>>,
    pub is_varargs: bool,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, Serialize, Deserialize)]
pub enum Type {
    Any,
    This,

    Struct(HirId),

    /// int, char, bool, etc
    Primitive(PrimitiveType),

    /// Any function type (including closures)
    /// Note that all functions in ante take at least 1 argument.
    Function(FnType),

    // TODO: Struct(StructType),
    Error,
}

impl Type {
    pub fn name(&self) -> String {
        match self {
            Type::Error => "<error>".into(),
            Type::Any => "Any".into(),
            Type::Primitive(PrimitiveType::Unit) => "()".into(),
            Type::Primitive(PrimitiveType::Bool) => "bool".into(),
            Type::Primitive(PrimitiveType::Char) => "char".into(),
            Type::Primitive(PrimitiveType::Int32) => "int32".into(),
            Type::Primitive(PrimitiveType::Int64) => "int64".into(),
            Type::Primitive(PrimitiveType::Float32) => "float32".into(),
            Type::Primitive(PrimitiveType::Float64) => "float64".into(),
            Type::Struct(sid) => {
                "Struct".into()
                /*let struct_ = self.sa.struct_(sid);
                let name = struct_.name;
                let name = self.sa.interner.str(name).to_string();

                if type_params.len() == 0 {
                    name
                } else {
                    let params = type_params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", name, params)
                } */
            }
            Type::This => todo!(),
            Type::Function(_) => todo!(),
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            Type::Primitive(_) => true,
            _ => false,
        }
    }

    pub fn as_primitive(&self) -> Option<&PrimitiveType> {
        match self {
            Type::Primitive(prim) => Some(prim),
            _ => None,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Type::Primitive(PrimitiveType::Float64) => true,
            _ => false,
        }
    }

    pub fn is_error(&self) -> bool {
        match self {
            Type::Error => true,
            _ => false,
        }
    }

    pub fn is_defined_type(&self) -> bool {
        match self {
            Type::Error | Type::This | Type::Any /*| Type::Ptr*/ => false,
            Type::Primitive(PrimitiveType::Unit)
            | Type::Primitive(PrimitiveType::Bool)
            | Type::Primitive(PrimitiveType::Char)
            | Type::Primitive(PrimitiveType::Int32)
            | Type::Primitive(PrimitiveType::Int64)
            | Type::Primitive(PrimitiveType::Float32)
            | Type::Primitive(PrimitiveType::Float64)
            | Type::Function(_) => true,
            Type::Struct(_) => {
                //for param in params.iter() {
                //    if !param.is_defined_type(sa) {
                //        return false;
                //    }
                //}

                true
            }
        }
    }
}

/// Primitive types are the easy cases when unifying types.
/// They're equal simply if the other type is also the same PrimitiveType variant,
/// there is no recursion needed like with other Types. If the `Type`
/// enum forms a tree, then these are the leaf nodes.
#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub enum PrimitiveType {
    Unit,
    Bool,
    Int32,
    Int64,
    Float32,
    Float64,
    Char,
}

impl Type {
    pub fn allows(&self, other: Type) -> bool {
        match self {
            Type::This => true,
            Type::Error => true,
            // Any allows all other types
            Type::Any => true,
            Type::Primitive(PrimitiveType::Unit)
            | Type::Primitive(PrimitiveType::Bool)
            | Type::Primitive(PrimitiveType::Char)
            | Type::Struct(_) => *self == other,
            Type::Primitive(PrimitiveType::Int32)
            | Type::Primitive(PrimitiveType::Int64)
            | Type::Primitive(PrimitiveType::Float32)
            | Type::Primitive(PrimitiveType::Float64) => *self == other,
            Type::Function(_) => *self == other,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeArray {
    Empty,
    List(Arc<Vec<Type>>),
}

impl TypeArray {
    pub fn empty() -> TypeArray {
        TypeArray::Empty
    }

    pub fn new(types: Arc<Vec<Type>>) -> TypeArray {
        TypeArray::List(types)
    }

    pub fn single(ty: Type) -> TypeArray {
        TypeArray::List(Arc::new(vec![ty]))
    }

    pub fn with(type_params: Vec<Type>) -> TypeArray {
        if type_params.len() == 0 {
            TypeArray::Empty
        } else {
            TypeArray::List(Arc::new(type_params))
        }
    }

    pub fn connect(&self, other: &TypeArray) -> TypeArray {
        if self.is_empty() {
            return other.clone();
        }

        if other.is_empty() {
            return self.clone();
        }

        let mut params = self.types().to_vec();
        params.extend_from_slice(other.types());

        TypeArray::List(Arc::new(params))
    }

    pub fn types(&self) -> &[Type] {
        match self {
            TypeArray::Empty => &[],
            TypeArray::List(ref params) => (**params).as_slice(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        match self {
            &TypeArray::Empty => 0,
            &TypeArray::List(ref params) => params.len(),
        }
    }
}

impl Index<usize> for TypeArray {
    type Output = Type;

    fn index(&self, idx: usize) -> &Type {
        match self {
            &TypeArray::Empty => panic!("type list index out-of-bounds"),
            &TypeArray::List(ref params) => &params[idx],
        }
    }
}

impl From<Vec<Type>> for TypeArray {
    fn from(value: Vec<Type>) -> Self {
        TypeArray::with(value)
    }
}

impl From<Type> for TypeArray {
    fn from(value: Type) -> Self {
        TypeArray::single(value)
    }
}

impl From<()> for TypeArray {
    fn from(_value: ()) -> Self {
        TypeArray::empty()
    }
}

pub struct TypeArrayIter<'a> {
    params: &'a TypeArray,
    idx: usize,
}

impl<'a> Iterator for TypeArrayIter<'a> {
    type Item = Type;

    fn next(&mut self) -> Option<Type> {
        match self.params {
            &TypeArray::Empty => None,
            &TypeArray::List(ref params) => {
                if self.idx < params.len() {
                    let ret = params[self.idx].clone();
                    self.idx += 1;

                    Some(ret)
                } else {
                    None
                }
            }
        }
    }
}
