use std::fmt::{self, Debug};

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct OwnerId {
    pub def_id: usize,
}

impl Debug for OwnerId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Example: DefId(0:1 ~ aa[7697]::{use#0})
        Debug::fmt(&self.def_id, f)
    }
}

impl From<OwnerId> for HirId {
    fn from(owner: OwnerId) -> HirId {
        HirId {
            owner,
            local_id: ItemLocalId(0),
        }
    }
}

/// Uniquely identifies a node in the HIR of the current crate. It is
/// composed of the `owner`, which is the `LocalDefId` of the directly enclosing
/// `hir::Item`, `hir::TraitItem`, or `hir::ImplItem` (i.e., the closest "item-like"),
/// and the `local_id` which is unique within the given owner.
///
/// This two-level structure makes for more stable values: One can move an item
/// around within the source code, or add or remove stuff before it, without
/// the `local_id` part of the `HirId` changing, which is a very useful property in
/// incremental compilation where we have to persist things through changes to
/// the code base.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct HirId {
    pub owner: OwnerId,
    pub local_id: ItemLocalId,
}

impl Debug for HirId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Example: HirId(DefId(0:1 ~ aa[7697]::{use#0}).10)
        // Don't use debug_tuple to always keep this on one line.
        write!(f, "HirId({:?}.{:?})", self.owner, self.local_id)
    }
}

impl fmt::Display for HirId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ItemLocalId(usize);

impl ItemLocalId {
    /// Signal local id which should never be used.
    pub const INVALID: ItemLocalId = ItemLocalId(usize::max_value());
}
