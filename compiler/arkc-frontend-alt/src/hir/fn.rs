use std::sync::Arc;

use crate::hir;
use id_arena::Id;
use parser::ast;

use crate::Name;

use super::{ModuleDefinitionId, PackageDefinitionId, SourceFileId};

pub type FnDefinitionId = Id<FnDefinition>;

pub struct FnDefinition {
    pub id: Option<FnDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::FnItem>,
    pub name: Name,
}
