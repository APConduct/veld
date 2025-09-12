//! This module defines the core data structures for source code mapping and parsing context.
//! It includes identifiers for AST nodes and files, structures for representing source
//! positions and spans, and a `SourceMap` to manage all source file information.

use std::{
    collections::HashMap,
    path::PathBuf,
    sync::atomic::{AtomicU32, Ordering},
    time::SystemTime,
};

/// A unique identifier for an Abstract Syntax Tree (AST) node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

impl NodeId {
    /// Creates a `NodeId` with a specific `u32` value.
    /// This is useful for testing or when specific IDs are needed.
    ///
    /// # Example
    /// ```
    /// use veld_common::source::NodeId;
    ///
    /// let node_id = NodeId::explicit(42);
    /// assert_eq!(node_id.0, 42);
    /// ```
    pub fn explicit(id: u32) -> Self {
        NodeId(id)
    }

    /// Creates a new, unique `NodeId`.
    ///
    /// This uses a global atomic counter to ensure uniqueness across threads.
    ///
    /// # Example
    /// ```
    /// use veld_common::source::NodeId;
    ///
    /// let node_id1 = NodeId::new();
    /// let node_id2 = NodeId::new();
    /// assert_ne!(node_id1, node_id2);
    /// ```
    pub fn new() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        NodeId(COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

/// A unique identifier for a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(pub u32);

impl FileId {
    /// Creates a `FileId` with a specific `u32` value.
    ///
    /// # Example
    /// ```
    /// use veld_common::source::FileId;
    ///
    /// let file_id = FileId::explicit(7);
    /// assert_eq!(file_id.0, 7);
    /// ```
    pub fn explicit(id: u32) -> Self {
        FileId(id)
    }

    /// Creates a new, unique `FileId`.
    ///
    /// This uses a global atomic counter to ensure uniqueness.
    ///
    /// # Example
    /// ```
    /// use veld_common::source::FileId;
    ///
    /// let file_id1 = FileId::new();
    /// let file_id2 = FileId::new();
    /// assert_ne!(file_id1, file_id2);
    /// ```
    pub fn new() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        FileId(COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

/// Represents a location in a source file, specified by line and column.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Position {
    /// The 1-based line number.
    pub line: u32,
    /// The 1-based column number.
    pub column: u32,
}

/// Represents a region of source code within a specific file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// The starting position of the span (inclusive).
    pub start: Position,
    /// The ending position of the span (inclusive).
    pub end: Position,
    /// The ID of the file this span belongs to.
    pub file_id: FileId,
}

/// Manages all source files and their corresponding AST node spans.
///
/// It stores the content of each file and maps `NodeId`s to `Span`s, allowing
/// for error reporting and other source-aware tooling.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceMap {
    /// A list of all source files, indexed by their `FileId`.
    files: Vec<FileInfo>,
    /// A mapping from `NodeId`s to their corresponding `Span`s.
    spans: HashMap<NodeId, Span>,
    /// A mapping from file paths to their corresponding `FileId`s.
    path_to_id: HashMap<PathBuf, FileId>,
}

impl SourceMap {
    /// Creates a new, empty `SourceMap`.
    ///
    /// # Example
    /// ```
    /// use veld_common::source::SourceMap;
    /// let source_map = SourceMap::new();
    /// assert!(source_map.files().is_empty());
    /// ```
    pub fn new() -> Self {
        SourceMap {
            files: Vec::new(),
            spans: HashMap::new(),
            path_to_id: HashMap::new(),
        }
    }

    /// Adds a file to the `SourceMap`, returning a unique `FileId` for it.
    ///
    /// If a file with the same path already exists, its existing `FileId` is returned.
    ///
    /// # Example
    /// ```
    /// use veld_common::source::SourceMap;
    /// let mut source_map = SourceMap::new();
    /// let file_id = source_map.add_file("example.veld", "let x = 42;".to_string());
    /// assert_eq!(file_id.0, 0);
    /// let same_file_id = source_map.add_file("example.veld", "let x = 42;".to_string());
    /// assert_eq!(file_id, same_file_id);
    /// ```
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

    /// Retrieves the content of a specific line from a file.
    ///
    /// Line numbers are 0-based.
    ///
    /// # Example
    /// ```
    /// use veld_common::source::SourceMap;
    ///
    /// let mut source_map = SourceMap::new();
    /// let file_id = source_map.add_file(
    ///     "example.veld",
    ///     "let x = 42;\nlet y = x + 1".to_string()
    /// );
    /// let line = source_map.get_line(file_id, 1);
    /// assert_eq!(line, Some("let y = x + 1"));
    /// ```
    pub fn get_line(&self, file_id: FileId, line_num: u32) -> Option<&str> {
        let file = &self.files.get(file_id.0 as usize)?;
        file.content.lines().nth(line_num as usize)
    }

    /// Returns a reference to the list of all `FileInfo` entries in the `SourceMap`.
    ///
    /// This can be useful for iterating over all files or for debugging purposes.
    ///
    /// # Example
    /// ```
    /// use veld_common::source::SourceMap;
    /// let mut source_map = SourceMap::new();
    /// source_map.add_file("file1.veld", "content1".to_string());
    /// source_map.add_file("file2.veld", "content2".to_string());
    /// let files = source_map.files();
    /// assert_eq!(files.len(), 2);
    /// ```
    pub fn files(&self) -> &Vec<FileInfo> {
        &self.files
    }
}

impl SourceMap {
    /// Associates a `Span` with a `NodeId`.
    ///
    /// # Example
    /// ```
    /// use veld_common::source::{SourceMap, NodeId, Span, Position, FileId};
    /// let mut source_map = SourceMap::new();
    /// let file_id = source_map.add_file("example.veld", "let x = 42;".to_string());
    /// let node_id = NodeId::new();
    /// let span = Span {
    ///     start: Position { line: 1, column: 1 },
    ///     end: Position { line: 1, column: 10 },
    ///     file_id,
    /// };
    /// source_map.insert(node_id, span);
    /// assert_eq!(source_map.get_span(node_id), Some(&span));
    /// ```
    pub fn insert(&mut self, node_id: NodeId, span: Span) {
        self.spans.insert(node_id, span);
    }

    /// Retrieves the `Span` associated with a `NodeId`.
    ///
    /// # Example
    /// ```
    /// use veld_common::source::{SourceMap, NodeId, Span, Position, FileId};
    /// let mut source_map = SourceMap::new();
    /// let file_id = source_map.add_file("example.veld", "let x = 42;".to_string());
    /// let node_id = NodeId::new();
    /// let span = Span {
    ///     start: Position { line: 1, column: 1 },
    ///     end: Position { line: 1, column: 10 },
    ///     file_id,
    /// };
    /// source_map.insert(node_id, span);
    /// assert_eq!(source_map.get_span(node_id), Some(&span));
    /// ```
    pub fn get_span(&self, node_id: NodeId) -> Option<&Span> {
        self.spans.get(&node_id)
    }

    /// Retrieves the `FileInfo` for a given `FileId`.
    ///
    /// # Example
    /// ```
    /// use veld_common::source::SourceMap;
    /// let mut source_map = SourceMap::new();
    /// let file_id = source_map.add_file("example.veld", "let x = 42;".to_string());
    /// let file_info = source_map.get_file_info(file_id);
    /// assert!(file_info.is_some());
    /// ```
    pub fn get_file_info(&self, file_id: FileId) -> Option<&FileInfo> {
        self.files.get(file_id.0 as usize)
    }
}

/// Contains metadata and content for a single source file.
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

/// Holds contextual information needed during the parsing process.
#[derive(Debug, PartialEq, Eq)]
pub struct ParseContext<'a> {
    /// The `FileId` of the source file currently being parsed.
    pub current_file_id: FileId,
    /// A mutable reference to the `SourceMap` to store file and span information.
    pub source_map: &'a mut SourceMap,
}

impl ParseContext<'_> {
    /// Creates and adds a `Span` to the `SourceMap` for a given `NodeId`. The span is constructed from the provided start and end positions and the context's `current_file_id` and is then associated with the `NodeId` in the `SourceMap`.
    ////
    /// # Example
    /// ```
    /// use veld_common::source::{ParseContext, SourceMap, NodeId, Position, FileId};
    /// let mut source_map = SourceMap::new();
    /// let file_id = source_map.add_file("example.veld", "let x = 42;".to_string());
    /// let mut parse_context = ParseContext {
    ///     current_file_id: file_id,
    ///     source_map: &mut source_map,
    /// };
    /// let node_id = NodeId::new();
    /// parse_context.add_span(
    ///     node_id,
    ///     Position { line: 1, column: 1 },
    ///     Position { line: 1, column: 10 },
    /// );
    /// let span = parse_context.source_map.get_span(node_id);
    /// assert!(span.is_some());
    /// ```
    pub fn add_span(&mut self, node_id: NodeId, start: Position, end: Position) {
        let span = Span {
            start,
            end,
            file_id: self.current_file_id,
        };
        self.source_map.spans.insert(node_id, span);
    }

    /// Begins tracking a new span at the given start position.
    ///
    /// This returns a closure that should be called with the end position when the
    /// corresponding AST node has been fully parsed. A new `NodeId` is generated internally.
    ///
    /// # Example
    /// ```
    /// use veld_common::source::{ParseContext, SourceMap, Position};
    /// let mut source_map = SourceMap::new();
    /// let file_id = source_map.add_file("example.veld", "let x = 42;".to_string());
    /// let mut parse_context = ParseContext {
    ///     current_file_id: file_id,
    ///     source_map: &mut source_map,
    /// };
    /// let start = Position { line: 1, column: 1 };
    /// let mut end_span = parse_context.start_span(start);
    /// let end = Position { line: 1, column: 10 };
    /// end_span(end);
    /// // The span is now recorded in the source map.
    /// ```
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
