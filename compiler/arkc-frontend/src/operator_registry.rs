use std::collections::HashMap;

use arkc_hir::{
    hir::{self, HirId},
    ty,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct OperatorSig {
    pub op: hir::BinOpKind,
    pub lhs: ty::Type,
    pub rhs: ty::Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operator {
    BuiltIn(ty::Type),
    Overload(HirId, ty::Type),
}

#[derive(Clone, Debug)]
pub struct OperatorRegistry {
    overloads: HashMap<OperatorSig, Operator>,
}

impl OperatorRegistry {
    pub fn new() -> Self {
        Self {
            overloads: HashMap::new(),
        }
    }

    pub fn register_builtin(
        &mut self,
        op: hir::BinOpKind,
        lhs: ty::Type,
        rhs: ty::Type,
        result: ty::Type,
    ) {
        self.overloads
            .insert(OperatorSig { op, lhs, rhs }, Operator::BuiltIn(result));
    }

    pub fn register_overload(
        &mut self,
        func_id: HirId,
        op: hir::BinOpKind,
        lhs: ty::Type,
        rhs: ty::Type,
        result: ty::Type,
    ) {
        self.overloads.insert(
            OperatorSig { op, lhs, rhs },
            Operator::Overload(func_id, result),
        );
    }

    pub fn lookup(&self, op: &hir::BinOpKind, lhs: &ty::Type, rhs: &ty::Type) -> Option<Operator> {
        self.overloads
            .get(&OperatorSig {
                op: op.clone(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            })
            .cloned()
    }
}
