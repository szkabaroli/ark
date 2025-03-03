use arkc_hir::hir;

use crate::{compilation::ModuleId, Sema};

pub fn func_accessible_from(file: &hir::File, func_id: hir::HirId, module_id: ModuleId) -> bool {
    return true;
    //accessible_from(sa, func.module_id, func.visibility, module_id)
}

/*fn accessible_from(
    sa: &Sema,
    target_module_id: ModuleDefinitionId,
    element_visibility: Visibility,
    user_module_id: ModuleDefinitionId,
) -> bool {
    // Each module can access stuff in itself.
    if target_module_id == user_module_id {
        return true;
    }

    // Modules can always access all their parents.
    if module_contains(sa, target_module_id, user_module_id) {
        return true;
    }

    // Find the common parent of both modules.
    let common_parent_id = common_parent(sa, target_module_id, user_module_id);

    let target_module = sa.module(target_module_id);

    {
        let target_module = sa.module(target_module_id);
        let user_module = sa.module(user_module_id);

        if target_module.package_id == user_module.package_id {
            assert!(common_parent_id.is_some());
        } else {
            assert!(common_parent_id.is_none());
        }
    }

    if let Some(common_parent_id) = common_parent_id {
        let common_parent_depth = sa.modules[common_parent_id].depth;

        // The common parent module is an ancestor of the user module, which means
        // the user module has access to everything along that path including the
        // common parent modules direct children.
        if common_parent_depth + 1 == target_module.depth {
            element_visibility.is_public()
        } else {
            let start_depth = common_parent_depth + 2;
            for &ns_id in &target_module.parents[start_depth..] {
                if !sa.modules[ns_id].visibility.is_public() {
                    return false;
                }
            }

            target_module.visibility.is_public() && element_visibility.is_public()
        }
    } else {
        // No common parent: means we try to access another package
        // the whole path needs to be public

        for &ns_id in &target_module.parents {
            let ns = sa.module(ns_id);
            if !ns.visibility.is_public() {
                return false;
            }
        }

        target_module.visibility.is_public() && element_visibility.is_public()
    }
}

fn common_parent(sa: &Sema, lhs_id: ModuleId, rhs_id: ModuleId) -> Option<ModuleId> {
    if lhs_id == rhs_id {
        return Some(lhs_id);
    }

    let lhs = sa.module(lhs_id);
    let rhs = sa.module(rhs_id);

    if lhs.depth > rhs.depth {
        if lhs.parents[rhs.depth] == rhs_id {
            return Some(rhs_id);
        } else {
            // do nothing
        }
    } else if rhs.depth > lhs.depth {
        if rhs.parents[lhs.depth] == lhs_id {
            return Some(lhs_id);
        } else {
            // do nothing
        }
    }

    let start = std::cmp::min(lhs.depth, rhs.depth);

    for depth in (0..start).rev() {
        if lhs.parents[depth] == rhs.parents[depth] {
            return Some(lhs.parents[depth]);
        }
    }

    None
}*/
