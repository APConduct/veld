use std::{
    collections::HashMap,
    sync::atomic::{AtomicU32, Ordering},
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

pub struct SourceMap {
    spans: HashMap<NodeId, Span>,
}

impl SourceMap {
    pub fn new() -> Self {
        SourceMap {
            spans: HashMap::new(),
        }
    }
}

impl SourceMap {
    pub fn insert(&mut self, node_id: NodeId, span: Span) {
        self.spans.insert(node_id, span);
    }
}

// pub struct ParseContext {
//     pub file_id: FileId,  // Set once per file
//     // other parsing state...
// }
