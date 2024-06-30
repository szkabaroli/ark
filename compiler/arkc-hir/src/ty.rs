use serde::{Deserialize, Serialize};

use crate::hir::HirId;

/// Function types.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct FnType {
    pub inputs: Vec<Type>,
    pub output: Option<Box<Type>>,
    pub is_varargs: bool,
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
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
    Unknown,
}

impl Type {
    pub fn name(&self) -> String {
        match self {
            Type::Unknown => "<unknown>".into(),
            Type::Any => "Any".into(),
            Type::Primitive(PrimitiveType::Unit) => "()".into(),
            Type::Primitive(PrimitiveType::Bool) => "Bool".into(),
            Type::Primitive(PrimitiveType::Char) => "Char".into(),
            Type::Primitive(PrimitiveType::Int32) => "Int32".into(),
            Type::Primitive(PrimitiveType::Int64) => "Int64".into(),
            Type::Primitive(PrimitiveType::Float64) => "Float64".into(),
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

    pub fn is_float(&self) -> bool {
        match self {
            Type::Primitive(PrimitiveType::Float64) => true,
            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            Type::Unknown => true,
            _ => false,
        }
    }
    pub fn is_defined_type(&self) -> bool {
        match self {
            Type::Unknown | Type::This | Type::Any /*| Type::Ptr*/ => false,
            Type::Primitive(PrimitiveType::Unit)
            | Type::Primitive(PrimitiveType::Bool)
            | Type::Primitive(PrimitiveType::Char)
            | Type::Primitive(PrimitiveType::Int32)
            | Type::Primitive(PrimitiveType::Int64)
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
    // Uint8,
    Int32,
    Int64,
    Float64,
    Char,
}

impl Type {
    pub fn allows(&self, other: Type) -> bool {
        match self {
            Type::This => true,
            Type::Unknown => true,
            // Any allows all other types
            Type::Any => true,
            Type::Primitive(PrimitiveType::Unit)
            | Type::Primitive(PrimitiveType::Bool)
            // | Type::Primitive(PrimitiveType::Uint8)
            | Type::Primitive(PrimitiveType::Char)
            | Type::Struct(_)
            //| Type::Enum(_, _)
            //| Type::Trait(_, _) 
            => *self == other,
            Type::Primitive(PrimitiveType::Int32) | Type::Primitive(PrimitiveType::Int64) | Type::Primitive(PrimitiveType::Float64) => {
                *self == other
            }
            // Type::Primitive(PrimitiveType::Ptr) => panic!("ptr does not allow any other types"),
            Type::Function(_) => *self == other,
            //Type::This | Type::TypeAlias(..) => unreachable!(),
            /*Type::Class(self_cls_id, self_list) => {
                if *self == other {
                    return true;
                }

                let (other_cls_id, other_list) = match other {
                    Type::Class(cls_id, ref other_list) => (cls_id, other_list.clone()),
                    _ => {
                        return false;
                    }
                };

                *self_cls_id == other_cls_id && self_list == &other_list
            }*/
            /*Type::Tuple(subtypes) => match other {
                Type::Tuple(other_subtypes) => {
                    if subtypes.len() != other_subtypes.len() {
                        return false;
                    }

                    let len = subtypes.len();

                    for idx in 0..len {
                        let ty = subtypes[idx].clone();
                        let other_ty = other_subtypes[idx].clone();

                        if !ty.allows(sa, other_ty) {
                            return false;
                        }
                    }

                    true
                }

                _ => false,
            },*/

            //Type::TypeParam(_) => *self == other,

            // Type::Lambda(_, _) => {
                // for now expect the exact same params and return types
                // possible improvement: allow super classes for params,
                //                             sub class for return type
              //  *self == other
            //}
        }
    }
}
