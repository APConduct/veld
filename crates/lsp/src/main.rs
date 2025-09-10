use serde_json::{self, Value};
use std::collections::HashMap;
use std::io::{self, BufReader, Read, Write};
use tracing;
use tracing_subscriber;

use crate::rpc::{decode_message, encode_message, RPCError};

pub mod rpc;

#[derive(Debug, Clone)]
struct TextDocument {
    uri: String,
    content: String,
    version: i32,
}

struct LSPServer {
    documents: HashMap<String, TextDocument>,
    next_id: i32,
}

impl LSPServer {
    fn new() -> Self {
        Self {
            documents: HashMap::new(),
            next_id: 1,
        }
    }

    fn get_next_id(&mut self) -> i32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

fn main() {
    // Initialize logging to a file for debugging
    if let Err(e) = get_logger("lsp_server.log".to_string()) {
        eprintln!("Failed to initialize logger: {}", e.message);
    } else {
        tracing_subscriber::fmt::init();
    }

    tracing::info!("LSP Server starting...");

    let mut server = LSPServer::new();
    let mut buffer = Vec::new();
    let stdin = io::stdin();
    let mut reader = BufReader::new(stdin);

    loop {
        // Read more data into buffer
        let mut temp_buffer = [0; 1024];
        match reader.read(&mut temp_buffer) {
            Ok(0) => {
                tracing::info!("EOF reached, shutting down");
                break; // EOF
            }
            Ok(n) => {
                buffer.extend_from_slice(&temp_buffer[..n]);
                tracing::debug!("Read {} bytes, buffer size: {}", n, buffer.len());
            }
            Err(e) => {
                tracing::error!("Error reading input: {}", e);
                eprintln!("Error reading input: {}", e);
                break;
            }
        }

        // Try to extract complete messages
        while let Ok((message_length, message_data)) = split(&buffer, false) {
            tracing::debug!("Processing message of length: {}", message_length);

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

                    server.documents.insert(
                        uri.clone(),
                        TextDocument {
                            uri,
                            content: text,
                            version,
                        },
                    );

                    tracing::info!("Document opened and stored");
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
                                    tracing::info!("Document content updated");
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
                            let line = position.get("line").and_then(|l| l.as_i64()).unwrap_or(0);
                            let character = position
                                .get("character")
                                .and_then(|c| c.as_i64())
                                .unwrap_or(0);

                            // Simple hover based on position
                            format!("Hover info for position {}:{} in {}", line, character, uri)
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

            let mut completion_items = vec![
                serde_json::json!({
                    "label": "fn",
                    "kind": 3,
                    "detail": "Function declaration",
                    "documentation": "Define a new function",
                    "insertText": "fn ${1:name}(${2:params}) {\n\t$0\n}"
                }),
                serde_json::json!({
                    "label": "let",
                    "kind": 14,
                    "detail": "Variable binding",
                    "documentation": "Create a new variable binding",
                    "insertText": "let ${1:name} = ${2:value};"
                }),
                serde_json::json!({
                    "label": "if",
                    "kind": 14,
                    "detail": "Conditional statement",
                    "documentation": "Conditional execution",
                    "insertText": "if ${1:condition} {\n\t$0\n}"
                }),
                serde_json::json!({
                    "label": "for",
                    "kind": 14,
                    "detail": "For loop",
                    "documentation": "Iterate over a collection",
                    "insertText": "for ${1:item} in ${2:collection} {\n\t$0\n}"
                }),
                serde_json::json!({
                    "label": "while",
                    "kind": 14,
                    "detail": "While loop",
                    "documentation": "Loop while condition is true",
                    "insertText": "while ${1:condition} {\n\t$0\n}"
                }),
                serde_json::json!({
                    "label": "struct",
                    "kind": 22,
                    "detail": "Struct definition",
                    "documentation": "Define a new struct",
                    "insertText": "struct ${1:Name} {\n\t${2:field}: ${3:Type},\n}"
                }),
                serde_json::json!({
                    "label": "impl",
                    "kind": 14,
                    "detail": "Implementation block",
                    "documentation": "Implement methods for a type",
                    "insertText": "impl ${1:Type} {\n\t$0\n}"
                }),
                serde_json::json!({
                    "label": "match",
                    "kind": 14,
                    "detail": "Pattern matching",
                    "documentation": "Pattern match expression",
                    "insertText": "match ${1:value} {\n\t${2:pattern} => ${3:result},\n\t_ => ${4:default},\n}"
                }),
            ];

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
