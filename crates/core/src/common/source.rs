use std::{
    collections::HashMap,
    path::PathBuf,
    sync::atomic::{AtomicU32, Ordering},
    time::SystemTime,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

impl NodeId {
    pub fn explicit(id: u32) -> Self {
        NodeId(id)
    }

    pub fn new() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        NodeId(COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(pub u32);

impl FileId {
    pub fn explicit(id: u32) -> Self {
        FileId(id)
    }

    pub fn new() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        FileId(COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
    pub file_id: FileId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceMap {
    files: Vec<FileInfo>,
    spans: HashMap<NodeId, Span>,
    path_to_id: HashMap<PathBuf, FileId>,
}

impl SourceMap {
    pub fn new() -> Self {
        SourceMap {
            files: Vec::new(),
            spans: HashMap::new(),
            path_to_id: HashMap::new(),
        }
    }

    pub fn add_file(&mut self, path: impl Into<PathBuf>, content: String) -> FileId {
        let path = path.into();

        if let Some(&existing_id) = self.path_to_id.get(&path) {
            return existing_id;
        }

        let file_id = FileId::new();
        let file_info = FileInfo {
            path: path.clone(),
            content,
            last_modified: SystemTime::now(),
            encoding: "UTF-8".to_string(),
        };

        self.files.push(file_info);
        self.path_to_id.insert(path, file_id);
        file_id
    }

    pub fn get_line(&self, file_id: FileId, line_num: u32) -> Option<&str> {
        let file = &self.files.get(file_id.0 as usize)?;
        file.content.lines().nth(line_num as usize)
    }
}

impl SourceMap {
    pub fn insert(&mut self, node_id: NodeId, span: Span) {
        self.spans.insert(node_id, span);
    }

    pub fn get_span(&self, node_id: NodeId) -> Option<&Span> {
        self.spans.get(&node_id)
    }

    pub fn get_file_info(&self, file_id: FileId) -> Option<&FileInfo> {
        self.files.get(file_id.0 as usize)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileInfo {
    /// File path for the file, useful for error reporting and other purposes.
    pub path: PathBuf,
    /// Source text content of the file.
    pub content: String,
    /// Last modified time of the file, useful for caching and file watching.
    pub last_modified: SystemTime,
    /// The encoding of the file, useful for parsing.
    pub encoding: String,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseContext<'a> {
    pub current_file_id: FileId,
    pub source_map: &'a mut SourceMap,
}

impl ParseContext<'_> {
    pub fn add_span(&mut self, node_id: NodeId, start: Position, end: Position) {
        let span = Span {
            start,
            end,
            file_id: self.current_file_id,
        };
        self.source_map.spans.insert(node_id, span);
    }

    pub fn start_span(&mut self, start: Position) -> impl FnMut(Position) {
        let node_id = NodeId::new();
        let file_id = self.current_file_id;
        let source_map = &mut self.source_map;
        move |end: Position| {
            let span = Span {
                start,
                end,
                file_id,
            };
            source_map.insert(node_id, span);
        }
    }
}
