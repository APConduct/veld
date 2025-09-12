use serde::{Deserialize, Serialize};
pub use serde_json::json;

pub fn encode_message<T: Serialize>(msg: T) -> Vec<u8> {
    let json_string = serde_json::to_string(&msg).expect("Failed to serialize message");

    format!(
        "Content-Length: {}\r\n\r\n{}",
        json_string.len(),
        json_string
    )
    .into_bytes()
}

#[derive(Debug, PartialEq, Eq)]
pub struct RPCError {
    // code: i32,
    pub message: String,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BaseMessage {
    pub method: String,
}

pub fn decode_message(msg: &[u8]) -> Result<(String, &[u8]), RPCError> {
    let pattern = b"\r\n\r\n";
    let (header, content, found) = if let Some(pos) = msg
        .windows(pattern.len())
        .position(|window| window == pattern)
    {
        (&msg[..pos], &msg[pos + pattern.len()..], true)
    } else {
        // (msg, &msg[pos + pattern.len()..], true)
        (msg, &[][..], false)
    };
    if !found {
        return Err(RPCError {
            message: "Did now find Seperator.".to_string(),
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
            message: format!(
                "Content length mismatch: declared {}, actual {}",
                content_length,
                content.len()
            ),
        });
    }

    let base_message =
        serde_json::from_slice::<BaseMessage>(&content[..content_length]).map_err(|err| {
            RPCError {
                message: format!("Failed to unmarshal base message: {}", err),
            }
        })?;

    Ok((base_message.method, &content[..content_length]))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Serialize;

    #[test]
    fn test_encode() {
        #[derive(Serialize)]
        struct EncodingExample {
            testing: bool,
        }
        let expected = "Content-Length: 16\r\n\r\n{\"testing\":true}";
        let actual = encode_message(EncodingExample { testing: true });
        assert_eq!(
            actual,
            expected.as_bytes(),
            "Expected: {}, actual: {:?}",
            expected,
            actual
        );
    }

    #[test]
    fn test_encode_complex_message() {
        #[derive(Serialize)]
        struct ComplexMessage {
            method: String,
            id: i32,
            params: Vec<String>,
        }
        let msg = ComplexMessage {
            method: "test_method".to_string(),
            id: 42,
            params: vec!["param1".to_string(), "param2".to_string()],
        };
        let result = encode_message(msg);
        let result_str = String::from_utf8(result).unwrap();
        assert!(result_str.starts_with("Content-Length: "));
        assert!(result_str.contains("\r\n\r\n"));
        assert!(result_str.contains("test_method"));
    }

    #[test]
    fn test_encode_empty_struct() {
        #[derive(Serialize)]
        struct EmptyMessage {}
        let expected = "Content-Length: 2\r\n\r\n{}";
        let actual = encode_message(EmptyMessage {});
        assert_eq!(actual, expected.as_bytes());
    }

    #[test]
    fn test_decode() {
        let incoming_message = "Content-Length: 15\r\n\r\n{\"method\":\"hi\"}";

        let (method, content) = decode_message(incoming_message.as_bytes()).unwrap();
        let content_length = content.len();

        if method != "hi" {
            panic!("Expected method 'hi', got {:?}", method);
        }

        assert_eq!(content_length, 15, "Expected 15, got {}", content_length);
    }

    #[test]
    fn test_decode_different_method() {
        let incoming_message = "Content-Length: 23\r\n\r\n{\"method\":\"initialize\"}";
        let (method, content) = decode_message(incoming_message.as_bytes()).unwrap();
        assert_eq!(method, "initialize");
        assert_eq!(content.len(), 23);
    }

    #[test]
    fn test_decode_no_separator() {
        let incoming_message = "Content-Length: 15{\"method\":\"hi\"}";
        let result = decode_message(incoming_message.as_bytes());
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().message, "Did now find Seperator.");
    }

    #[test]
    fn test_decode_invalid_content_length() {
        let incoming_message = "Content-Length: abc\r\n\r\n{\"method\":\"hi\"}";
        let result = decode_message(incoming_message.as_bytes());
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().message,
            "Failed to parse content length"
        );
    }

    #[test]
    fn test_decode_invalid_json() {
        let incoming_message = "Content-Length: 13\r\n\r\n{\"method\":hi}";
        let result = decode_message(incoming_message.as_bytes());
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .message
            .contains("Failed to unmarshal base message"));
    }

    #[test]
    fn test_decode_missing_method_field() {
        let incoming_message = "Content-Length: 11\r\n\r\n{\"id\":\"hi\"}";
        let result = decode_message(incoming_message.as_bytes());
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .message
            .contains("Failed to unmarshal base message"));
    }

    #[test]
    fn test_encode_decode_roundtrip() {
        #[derive(Serialize)]
        struct TestMessage {
            method: String,
        }
        let original_method = "test_roundtrip";
        let msg = TestMessage {
            method: original_method.to_string(),
        };

        let encoded = encode_message(msg);
        let (decoded_method, _) = decode_message(&encoded).unwrap();

        assert_eq!(decoded_method, original_method);
    }

    #[test]
    fn test_rpc_error_debug() {
        let error = RPCError {
            message: "Test error".to_string(),
        };
        let debug_str = format!("{:?}", error);
        assert!(debug_str.contains("Test error"));
    }

    #[test]
    fn test_base_message_serialize_deserialize() {
        let original = BaseMessage {
            method: "test_method".to_string(),
        };
        let json = serde_json::to_string(&original).unwrap();
        let deserialized: BaseMessage = serde_json::from_str(&json).unwrap();
        assert_eq!(original, deserialized);
    }
}
