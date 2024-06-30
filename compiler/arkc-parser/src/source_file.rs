use std::cell::OnceCell;
use std::path::PathBuf;
use std::sync::Arc;

use id_arena::Id;

use crate::compute_line_starts;

pub type SourceFileId = Id<SourceFile>;

#[derive(Debug)]
pub struct SourceFile {
    pub id: OnceCell<SourceFileId>,
    pub path: PathBuf,
    pub content: Arc<String>,
    pub line_starts: Vec<u32>,
}

impl SourceFile {
    pub fn new(path: PathBuf, code: &'static str) -> Self {
        let content = Arc::new(String::from(code));
        SourceFile::new_shared(path, content)
    }

    pub fn new_shared(path: PathBuf, content: Arc<String>) -> Self {
        let line_starts = compute_line_starts(&content);

        SourceFile {
            id: OnceCell::new(),
            path,
            line_starts,
            content,
        }
    }
}
