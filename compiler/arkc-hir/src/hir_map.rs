use std::collections::HashMap;

use bincode::{Decode, Encode};
use parser::NodeId;
use crate::hir::{FnBodyId, HirId};

#[derive(Clone, Debug)]
pub struct HirMap {
    map: HashMap<HirId, NodeId>,
    rev_map: HashMap<NodeId, HirId>,
    pub hir_id_next: u32,
    pub body_id_next: u32,
}

impl HirMap {
    pub fn new() -> Self {
        HirMap {
            hir_id_next: 1,
            body_id_next: 1,
            map: HashMap::new(),
            rev_map: HashMap::new(),
        }
    }

    pub fn next_hir_id(&mut self, node_id: NodeId) -> HirId {
        let hir_id = HirId(self.hir_id_next);
        self.hir_id_next += 1;

        self.add_hir_mapping(hir_id.clone(), node_id);

        hir_id
    }

    pub fn add_hir_mapping(&mut self, hir_id: HirId, node_id: NodeId) {
        self.map.insert(hir_id.clone(), node_id);
        self.rev_map.insert(node_id, hir_id);
    }

    pub fn next_body_id(&mut self) -> FnBodyId {
        let body_id = FnBodyId(self.body_id_next);

        self.body_id_next += 1;

        body_id
    }
}