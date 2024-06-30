use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;

use parser::SourceFileId;
use serde::Deserialize;
use serde::Serialize;

use crate::hir_map::HirMap;
use crate::ty::Type;

use super::Elem;
use super::FnBody;
use super::FnBodyId;
use super::HirId;

#[derive(Debug, Deserialize, Serialize)]
pub struct FileInfo {
    path: PathBuf
}

#[derive(Clone, Copy, Eq, Hash, PartialEq, PartialOrd, Ord, Deserialize, Serialize)]
pub struct FileId(pub u32);

impl fmt::Debug for FileId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "FileId({})", self.0)
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Module {
    #[serde(skip)]
    pub(crate) hir_map: Option<HirMap>,
    #[serde(skip)]
    pub(crate) spans: Option<HashMap<HirId, parser::Span>>,

    //pub files: HashMap<FileId, FileInfo>,
    pub node_types: HashMap<HirId, Type>,
    pub bodies: HashMap<FnBodyId, FnBody>,
    pub elements: HashMap<FileId, Elem>,
}
