use std::{ops::Index, sync::Arc};

use crate::sema::Sema;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum SourceType {
    // couldn't determine type because of error
    Unknown,

    // Allow any type here, used for type inference
    Any,

    // type with only one value: ()
    Unit,

    // primitives
    Bool,
    Char,
    UInt8,
    Int32,
    Int64,
    Float32,
    Float64,

    // pointer to object, only used internally
    Ptr,

    // self type
    This,
    // some class
    // Class(ClassDefinitionId, SourceTypeArray),

    // some struct
    // Struct(StructDefinitionId, SourceTypeArray),

    // some tuple
    // Tuple(SourceTypeArray),

    // some trait object
    // Trait(TraitDefinitionId, SourceTypeArray),

    // some type variable
    // TypeParam(TypeParamId),

    // Type alias.
    // TypeAlias(AliasDefinitionId),

    // some lambda
    Function(SourceTypeArray, Box<SourceType>),
    // some enum
    // Enum(EnumDefinitionId, SourceTypeArray),
}

impl SourceType {
    pub fn is_unit(&self) -> bool {
        match self {
            SourceType::Unit => true,
            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            SourceType::Unknown => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            &SourceType::Float32 | &SourceType::Float64 => true,
            _ => false,
        }
    }

    pub fn is_int32(&self) -> bool {
        match self {
            &SourceType::Int32 => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            &SourceType::Bool => true,
            _ => false,
        }
    }

    pub fn name(&self, sa: &Sema) -> String {
        let writer = SourceTypePrinter {
            sa,
            //type_params: None,
        };

        writer.name(self.clone())
    }

    pub fn allows(&self, sa: &Sema, other: SourceType) -> bool {
        match self {
            // allow all types for Unknown, there is already an error,
            // don't report too many messages for the same error
            SourceType::Unknown => true,

            // Any allows all other types
            SourceType::Any => true,

            SourceType::Unit
            | SourceType::Bool
            | SourceType::UInt8
            | SourceType::Char
            //| SourceType::Struct(_, _)
            //| SourceType::Enum(_, _)
            //| SourceType::Trait(_, _) 
            => *self == other,
            SourceType::Int32 | SourceType::Int64 | SourceType::Float32 | SourceType::Float64 => {
                *self == other
            }
            SourceType::Ptr => panic!("ptr does not allow any other types"),
            SourceType::This /*| SourceType::TypeAlias(..)*/ => unreachable!(),
            //SourceType::Class(self_cls_id, self_list) => {
            //    if *self == other {
            //        return true;
            //    }

            //    let (other_cls_id, other_list) = match other {
            //        SourceType::Class(cls_id, ref other_list) => (cls_id, other_list.clone()),
            //        _ => {
            //            return false;
            //        }
            //    };

            //    *self_cls_id == other_cls_id && self_list == &other_list
            //}
            //SourceType::Tuple(subtypes) => match other {
            //    SourceType::Tuple(other_subtypes) => {
            //        if subtypes.len() != other_subtypes.len() {
            //            return false;
            //        }

            //        let len = subtypes.len();

            //        for idx in 0..len {
            //            let ty = subtypes[idx].clone();
            //            let other_ty = other_subtypes[idx].clone();

            //            if !ty.allows(sa, other_ty) {
            //                return false;
            //            }
            //        }

            //        true
            //    }

            //    _ => false,
            //},

            //SourceType::TypeParam(_) => *self == other,

            SourceType::Function(_, _) => {
                // for now expect the exact same params and return types
                // possible improvement: allow super classes for params,
                //                             sub class for return type
                *self == other
            }
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SourceTypeArray {
    Empty,
    List(Arc<Vec<SourceType>>),
}

impl SourceTypeArray {
    pub fn empty() -> Self {
        Self::Empty
    }

    pub fn with(val: Vec<SourceType>) -> Self {
        Self::List(Arc::from(val))
    }

    pub fn is_empty(&self) -> bool {
        match self {
            SourceTypeArray::Empty => true,
            _ => false,
        }
    }

    pub fn iter(&self) -> SourceTypeArrayIter {
        SourceTypeArrayIter {
            params: self,
            idx: 0,
        }
    }
}

impl Index<usize> for SourceTypeArray {
    type Output = SourceType;

    fn index(&self, idx: usize) -> &SourceType {
        match self {
            &SourceTypeArray::Empty => panic!("type list index out-of-bounds"),
            &SourceTypeArray::List(ref params) => &params[idx],
        }
    }
}

pub struct SourceTypeArrayIter<'a> {
    params: &'a SourceTypeArray,
    idx: usize,
}

impl<'a> Iterator for SourceTypeArrayIter<'a> {
    type Item = SourceType;

    fn next(&mut self) -> Option<SourceType> {
        match self.params {
            &SourceTypeArray::Empty => None,

            &SourceTypeArray::List(ref params) => {
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

struct SourceTypePrinter<'a> {
    sa: &'a Sema,
    // type_params: Option<&'a TypeParamDefinition>,
}

impl<'a> SourceTypePrinter<'a> {
    pub fn name(&self, ty: SourceType) -> String {
        match ty {
            SourceType::Unknown => "<unknown>".into(),
            SourceType::Any => "Any".into(),
            SourceType::Unit => "()".into(),
            SourceType::UInt8 => "UInt8".into(),
            SourceType::Char => "Char".into(),
            SourceType::Int32 => "Int32".into(),
            SourceType::Int64 => "Int64".into(),
            SourceType::Float32 => "Float32".into(),
            SourceType::Float64 => "Float64".into(),
            SourceType::Bool => "Bool".into(),
            SourceType::Ptr => panic!("type Ptr only for internal use."),
            SourceType::This => "Self".into(),
            /*SourceType::Class(id, type_params) => {
                let cls = self.sa.class(id);
                let base = self.sa.interner.str(cls.name);

                if type_params.len() == 0 {
                    base.to_string()
                } else {
                    let params = type_params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", base, params)
                }
            }*/
            /*SourceType::Struct(sid, type_params) => {
                let struct_ = self.sa.struct_(sid);
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
                }
            }*/
            /*SourceType::Trait(trait_id, type_params) => {
                let trait_ = self.sa.trait_(trait_id);
                let name = self.sa.interner.str(trait_.name).to_string();

                if type_params.len() == 0 {
                    name
                } else {
                    let params = type_params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", name, params)
                }
            }*/
            /*SourceType::Enum(id, type_params) => {
                let enum_ = self.sa.enum_(id);
                let name = self.sa.interner.str(enum_.name).to_string();

                if type_params.len() == 0 {
                    name
                } else {
                    let params = type_params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", name, params)
                }
            }*/
            /*SourceType::TypeParam(idx) => {
                if let Some(type_params) = self.type_params {
                    self.sa.interner.str(type_params.name(idx)).to_string()
                } else {
                    format!("TypeParam({})", idx.to_usize())
                }
            }*/
            SourceType::Function(params, return_type) => {
                let params = params
                    .iter()
                    .map(|ty| self.name(ty.clone()))
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret = self.name(*return_type);

                format!("({}) -> {}", params, ret)
            }
            /*SourceType::Tuple(subtypes) => {
                let types = subtypes
                    .iter()
                    .map(|ty| self.name(ty.clone()))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("({})", types)
            }*/
            /*SourceType::TypeAlias(id) => {
                let alias = self.sa.alias(id);

                if let Some(ty) = alias.ty.get() {
                    format!(
                        "{}={}",
                        self.sa.interner.str(alias.name),
                        self.name(ty.clone())
                    )
                } else {
                    format!("{}", self.sa.interner.str(alias.name))
                }
            }*/
        }
    }
}
