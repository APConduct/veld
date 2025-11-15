use serde_json::{self, Value};
use std::collections::HashMap;
use std::io::{self, BufReader, Read, Write};
use tracing;
use tracing_subscriber;

use crate::rpc::{decode_message, encode_message, RPCError};

pub mod analysis;
pub mod rpc;

use analysis::{diagnostics_to_lsp, AnalysisResult, Analyzer};

#[derive(Debug, Clone)]
struct TextDocument {
    uri: String,
    content: String,
    version: i32,
    analysis: Option<AnalysisResult>,
}

pub struct LSPServer {
    documents: HashMap<String, TextDocument>,
    next_id: i32,
    analyzer: Analyzer,
}

impl LSPServer {
    fn new() -> Self {
        Self {
            documents: HashMap::new(),
            next_id: 1,
            analyzer: Analyzer::new(),
        }
    }

    pub fn url(&self) -> String {
        format!("https://localhost:{}", self.next_id)
    }

    pub fn get_next_id(&mut self) -> i32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

fn main() {
    // Initialize logging to a file for debugging
    // Ignore errors if logger is already set
    let _ = get_logger("lsp_server.log".to_string());

    tracing::info!("LSP Server starting...");

    let mut server = LSPServer::new();
    let mut buffer = Vec::new();
    let stdin = io::stdin();
    let mut reader = BufReader::new(stdin);
    let mut consecutive_zero_reads = 0;

    loop {
        // Read more data into buffer
        let mut temp_buffer = [0; 1024];
        match reader.read(&mut temp_buffer) {
            Ok(0) => {
                consecutive_zero_reads += 1;

                // If we have buffered data, try to process it
                if !buffer.is_empty() {
                    tracing::debug!(
                        "Read 0 bytes but buffer has {} bytes, processing",
                        buffer.len()
                    );
                    // Continue to message processing below
                } else if consecutive_zero_reads > 3 {
                    // Multiple consecutive zero reads with no data = EOF
                    tracing::info!(
                        "EOF reached after {} zero reads, shutting down",
                        consecutive_zero_reads
                    );
                    break;
                } else {
                    // First few zero reads - might be connection delay, wait a bit
                    tracing::debug!(
                        "Zero read #{}, waiting for connection",
                        consecutive_zero_reads
                    );
                    std::thread::sleep(std::time::Duration::from_millis(10));
                    continue;
                }
            }
            Ok(n) => {
                consecutive_zero_reads = 0; // Reset counter on successful read
                tracing::debug!("Read {} bytes from stdin", n);
                buffer.extend_from_slice(&temp_buffer[..n]);
            }
            Err(e) => {
                tracing::error!("Error reading input: {}", e);
                eprintln!("Error reading input: {}", e);
                break;
            }
        }

        // Try to extract complete messages
        while let Ok((message_length, message_data)) = split(&buffer, false) {
            // Process the message
            match decode_message(message_data) {
                Ok((method, content)) => {
                    tracing::info!("Received method: {}", method);

                    // Handle different LSP methods
                    let response = handle_message(&mut server, &method, content);

                    // Send response back
                    if let Some(resp) = response {
                        let encoded = encode_message(resp);
                        if let Err(e) = io::stdout().write_all(&encoded) {
                            tracing::error!("Failed to write response: {}", e);
                        } else {
                            io::stdout().flush().unwrap_or_else(|e| {
                                tracing::error!("Failed to flush stdout: {}", e);
                            });
                        }
                    }
                }
                Err(e) => {
                    tracing::error!("Failed to decode message: {}", e.message);
                }
            }

            // Remove processed message from buffer
            buffer.drain(..message_length);
        }
    }
}

fn handle_message(
    server: &mut LSPServer,
    method: &str,
    content: &[u8],
) -> Option<serde_json::Value> {
    // Parse the content as JSON to extract request details
    let request: Value = if content.is_empty() {
        serde_json::json!({})
    } else {
        match serde_json::from_slice(content) {
            Ok(json) => json,
            Err(e) => {
                tracing::error!("Failed to parse JSON content: {}", e);
                return None;
            }
        }
    };

    let request_id = request.get("id").and_then(|id| id.as_i64()).unwrap_or(1) as i32;

    match method {
        "initialize" => {
            tracing::info!("Handling initialize request");
            Some(serde_json::json!({
                "jsonrpc": "2.0",
                "id": request_id,
                "result": {
                    "capabilities": {
                        "textDocumentSync": {
                            "openClose": true,
                            "change": 1,
                            "save": {
                                "includeText": true
                            }
                        },
                        "hoverProvider": true,
                        "completionProvider": {
                            "resolveProvider": false,
                            "triggerCharacters": [".", ":"]
                        },
                        "definitionProvider": true,
                        "documentSymbolProvider": true,
                        "workspaceSymbolProvider": true,
                        "codeActionProvider": true,
                        "documentFormattingProvider": true,
                        "documentRangeFormattingProvider": true,
                        "renameProvider": {
                            "prepareProvider": true
                        },
                        "foldingRangeProvider": true,
                        "semanticTokensProvider": {
                            "legend": {
                                "tokenTypes": ["keyword", "string", "comment", "number", "operator", "function", "variable"],
                                "tokenModifiers": ["declaration", "definition", "readonly"]
                            },
                            "range": true,
                            "full": true
                        }
                    },
                    "serverInfo": {
                        "name": "veld-lsp",
                        "version": "0.1.0"
                    }
                }
            }))
        }
        "initialized" => {
            tracing::info!("Client initialized");
            None
        }
        "textDocument/didOpen" => {
            tracing::info!("Handling didOpen notification");
            if let Some(params) = request.get("params") {
                if let Some(text_document) = params.get("textDocument") {
                    let uri = text_document
                        .get("uri")
                        .and_then(|u| u.as_str())
                        .unwrap_or("")
                        .to_string();
                    let text = text_document
                        .get("text")
                        .and_then(|t| t.as_str())
                        .unwrap_or("")
                        .to_string();
                    let version = text_document
                        .get("version")
                        .and_then(|v| v.as_i64())
                        .unwrap_or(0) as i32;

                    // Analyze the document
                    let analysis = server.analyzer.analyze(&text);

                    // Store the document with analysis
                    server.documents.insert(
                        uri.clone(),
                        TextDocument {
                            uri: uri.clone(),
                            content: text,
                            version,
                            analysis: Some(analysis),
                        },
                    );

                    tracing::info!("Document opened and analyzed");

                    // Send diagnostics
                    if let Some(doc) = server.documents.get(&uri) {
                        if let Some(ref analysis) = doc.analysis {
                            let diagnostics = diagnostics_to_lsp(&uri, &analysis.diagnostics);
                            let encoded = encode_message(diagnostics);
                            if let Err(e) = io::stdout().write_all(&encoded) {
                                tracing::error!("Failed to write diagnostics: {}", e);
                            } else {
                                io::stdout().flush().unwrap_or_else(|e| {
                                    tracing::error!("Failed to flush stdout: {}", e);
                                });
                            }
                        }
                    }
                }
            }
            None
        }
        "textDocument/didChange" => {
            tracing::info!("Handling didChange notification");
            if let Some(params) = request.get("params") {
                if let Some(text_document) = params.get("textDocument") {
                    let uri = text_document
                        .get("uri")
                        .and_then(|u| u.as_str())
                        .unwrap_or("")
                        .to_string();
                    let version = text_document
                        .get("version")
                        .and_then(|v| v.as_i64())
                        .unwrap_or(0) as i32;

                    if let Some(changes) = params.get("contentChanges").and_then(|c| c.as_array()) {
                        if let Some(change) = changes.first() {
                            if let Some(text) = change.get("text").and_then(|t| t.as_str()) {
                                if let Some(doc) = server.documents.get_mut(&uri) {
                                    doc.content = text.to_string();
                                    doc.version = version;

                                    // Re-analyze the document
                                    let analysis = server.analyzer.analyze(&text);
                                    doc.analysis = Some(analysis);

                                    tracing::info!("Document content updated and re-analyzed");

                                    // Send updated diagnostics
                                    if let Some(ref analysis) = doc.analysis {
                                        let diagnostics =
                                            diagnostics_to_lsp(&uri, &analysis.diagnostics);
                                        let encoded = encode_message(diagnostics);
                                        if let Err(e) = io::stdout().write_all(&encoded) {
                                            tracing::error!("Failed to write diagnostics: {}", e);
                                        } else {
                                            io::stdout().flush().unwrap_or_else(|e| {
                                                tracing::error!("Failed to flush stdout: {}", e);
                                            });
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            None
        }
        "textDocument/didSave" => {
            tracing::info!("Handling didSave notification");
            None
        }
        "textDocument/didClose" => {
            tracing::info!("Handling didClose notification");
            if let Some(params) = request.get("params") {
                if let Some(text_document) = params.get("textDocument") {
                    let uri = text_document
                        .get("uri")
                        .and_then(|u| u.as_str())
                        .unwrap_or("")
                        .to_string();
                    server.documents.remove(&uri);
                    tracing::info!("Document removed from memory");
                }
            }
            None
        }
        "shutdown" => {
            tracing::info!("Shutdown requested");
            Some(serde_json::json!({
                "jsonrpc": "2.0",
                "id": request_id,
                "result": null
            }))
        }
        "exit" => {
            tracing::info!("Exit requested");
            std::process::exit(0);
        }
        "textDocument/hover" => {
            tracing::info!("Handling hover request");

            let hover_content = if let Some(params) = request.get("params") {
                if let Some(text_document) = params.get("textDocument") {
                    let uri = text_document
                        .get("uri")
                        .and_then(|u| u.as_str())
                        .unwrap_or("");
                    if let Some(doc) = server.documents.get(uri) {
                        if let Some(position) = params.get("position") {
                            let line =
                                position.get("line").and_then(|l| l.as_i64()).unwrap_or(0) as usize;
                            let character = position
                                .get("character")
                                .and_then(|c| c.as_i64())
                                .unwrap_or(0) as usize;

                            // Try to get hover info from analysis
                            if let Some(ref analysis) = doc.analysis {
                                if let Some(ref ast) = analysis.ast {
                                    if let Some(ref type_checker) = analysis.type_checker {
                                        if let Some(info) = server.analyzer.get_hover_info(
                                            &doc.content,
                                            ast,
                                            type_checker,
                                            line,
                                            character,
                                        ) {
                                            info
                                        } else {
                                            format!(
                                                "No type information available at {}:{}",
                                                line, character
                                            )
                                        }
                                    } else {
                                        "Type checker not available".to_string()
                                    }
                                } else {
                                    "Document has syntax errors".to_string()
                                }
                            } else {
                                "Document not analyzed".to_string()
                            }
                        } else {
                            "Veld Language Server - Hover Information".to_string()
                        }
                    } else {
                        "Document not found".to_string()
                    }
                } else {
                    "No text document specified".to_string()
                }
            } else {
                "Hello from veld LSP!".to_string()
            };

            Some(serde_json::json!({
                "jsonrpc": "2.0",
                "id": request_id,
                "result": {
                    "contents": {
                        "kind": "markdown",
                        "value": hover_content
                    }
                }
            }))
        }
        "textDocument/completion" => {
            tracing::info!("Handling completion request");

            let mut completion_items = Vec::new();

            if let Some(params) = request.get("params") {
                if let Some(text_document) = params.get("textDocument") {
                    let uri = text_document
                        .get("uri")
                        .and_then(|u| u.as_str())
                        .unwrap_or("");

                    if let Some(doc) = server.documents.get(uri) {
                        if let Some(position) = params.get("position") {
                            let line =
                                position.get("line").and_then(|l| l.as_i64()).unwrap_or(0) as usize;
                            let character = position
                                .get("character")
                                .and_then(|c| c.as_i64())
                                .unwrap_or(0) as usize;

                            // Get completions from analyzer
                            if let Some(ref analysis) = doc.analysis {
                                if let Some(ref ast) = analysis.ast {
                                    if let Some(ref type_checker) = analysis.type_checker {
                                        let completions = server.analyzer.get_completions(
                                            ast,
                                            type_checker,
                                            line,
                                            character,
                                        );

                                        // Convert to LSP format
                                        for item in completions {
                                            completion_items.push(serde_json::json!({
                                                "label": item.label,
                                                "kind": item.kind as i32,
                                                "detail": item.detail,
                                                "documentation": item.documentation,
                                            }));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            Some(serde_json::json!({
                "jsonrpc": "2.0",
                "id": request_id,
                "result": {
                    "isIncomplete": false,
                    "items": completion_items
                }
            }))
        }
        "textDocument/definition" => {
            tracing::info!("Handling goto definition request");

            if let Some(params) = request.get("params") {
                if let Some(text_document) = params.get("textDocument") {
                    let uri = text_document
                        .get("uri")
                        .and_then(|u| u.as_str())
                        .unwrap_or("");

                    if let Some(doc) = server.documents.get(uri) {
                        if let Some(position) = params.get("position") {
                            let line =
                                position.get("line").and_then(|l| l.as_i64()).unwrap_or(0) as usize;
                            let character = position
                                .get("character")
                                .and_then(|c| c.as_i64())
                                .unwrap_or(0) as usize;

                            // Try to find definition
                            if let Some(ref analysis) = doc.analysis {
                                if let Some(ref ast) = analysis.ast {
                                    if let Some((def_line, def_col)) = server
                                        .analyzer
                                        .find_definition(&doc.content, ast, line, character)
                                    {
                                        return Some(serde_json::json!({
                                            "jsonrpc": "2.0",
                                            "id": request_id,
                                            "result": {
                                                "uri": uri,
                                                "range": {
                                                    "start": {
                                                        "line": def_line,
                                                        "character": def_col
                                                    },
                                                    "end": {
                                                        "line": def_line,
                                                        "character": def_col + 10
                                                    }
                                                }
                                            }
                                        }));
                                    }
                                }
                            }
                        }
                    }
                }
            }

            Some(serde_json::json!({
                "jsonrpc": "2.0",
                "id": request_id,
                "result": null
            }))
        }
        "textDocument/documentSymbol" => {
            tracing::info!("Handling document symbol request");
            Some(serde_json::json!({
                "jsonrpc": "2.0",
                "id": request_id,
                "result": []
            }))
        }
        "workspace/symbol" => {
            tracing::info!("Handling workspace symbol request");
            Some(serde_json::json!({
                "jsonrpc": "2.0",
                "id": request_id,
                "result": []
            }))
        }
        "textDocument/codeAction" => {
            tracing::info!("Handling code action request");
            Some(serde_json::json!({
                "jsonrpc": "2.0",
                "id": request_id,
                "result": []
            }))
        }
        "textDocument/formatting" => {
            tracing::info!("Handling formatting request");
            Some(serde_json::json!({
                "jsonrpc": "2.0",
                "id": request_id,
                "result": []
            }))
        }
        "textDocument/rangeFormatting" => {
            tracing::info!("Handling range formatting request");
            Some(serde_json::json!({
                "jsonrpc": "2.0",
                "id": request_id,
                "result": []
            }))
        }
        "textDocument/prepareRename" => {
            tracing::info!("Handling prepare rename request");
            Some(serde_json::json!({
                "jsonrpc": "2.0",
                "id": request_id,
                "result": null
            }))
        }
        "textDocument/rename" => {
            tracing::info!("Handling rename request");
            Some(serde_json::json!({
                "jsonrpc": "2.0",
                "id": request_id,
                "result": null
            }))
        }
        "textDocument/foldingRange" => {
            tracing::info!("Handling folding range request");
            Some(serde_json::json!({
                "jsonrpc": "2.0",
                "id": request_id,
                "result": []
            }))
        }
        "textDocument/semanticTokens/full" => {
            tracing::info!("Handling semantic tokens request");
            Some(serde_json::json!({
                "jsonrpc": "2.0",
                "id": request_id,
                "result": {
                    "data": []
                }
            }))
        }
        "textDocument/semanticTokens/range" => {
            tracing::info!("Handling semantic tokens range request");
            Some(serde_json::json!({
                "jsonrpc": "2.0",
                "id": request_id,
                "result": {
                    "data": []
                }
            }))
        }
        _ => {
            tracing::warn!("Unhandled method: {}", method);
            // Send method not found error for requests (those with IDs)
            if request.get("id").is_some() {
                Some(serde_json::json!({
                    "jsonrpc": "2.0",
                    "id": request_id,
                    "error": {
                        "code": -32601,
                        "message": "Method not found",
                        "data": format!("The method '{}' is not supported by this server", method)
                    }
                }))
            } else {
                None
            }
        }
    }
}

fn split(data: &[u8], _: bool) -> Result<(usize, &[u8]), RPCError> {
    let pattern = b"\r\n\r\n";
    let (header, content, found) = if let Some(pos) = data
        .windows(pattern.len())
        .position(|window| window == pattern)
    {
        (&data[..pos], &data[pos + pattern.len()..], true)
    } else {
        (data, &[][..], false)
    };
    if !found {
        return Err(RPCError {
            message: "Header separator not found".to_string(),
        });
    }
    // Content-Length: <number>
    let content_length_bytes = &header[String::from("Content-Length: ").len()..];
    let content_length = std::str::from_utf8(content_length_bytes)
        .map_err(|_| RPCError {
            message: "Invalid UTF-8 in content length".to_string(),
        })?
        .parse::<usize>()
        .map_err(|_| RPCError {
            message: "Failed to parse content length".to_string(),
        })?;

    if content.len() < content_length {
        return Err(RPCError {
            message: "Incomplete message".to_string(),
        });
    }

    let total_length = header.len() + 4 + content_length;

    return Ok((total_length, &data[..total_length]));
}

fn get_logger(filename: String) -> Result<(), RPCError> {
    let file = std::fs::File::create(filename).map_err(|e| RPCError {
        message: format!("Failed to create log file: {}", e),
    })?;
    let subscriber = tracing_subscriber::fmt()
        .with_writer(std::sync::Arc::new(file))
        .finish();
    tracing::subscriber::set_global_default(subscriber).map_err(|e| RPCError {
        message: format!("Failed to set global default subscriber: {}", e),
    })?;
    Ok(())
}
