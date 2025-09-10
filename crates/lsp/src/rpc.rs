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

pub fn decode_message(msg: &[u8]) -> Result<(String, usize), RPCError> {
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

    let _ = content;

    let base_message =
        serde_json::from_slice::<BaseMessage>(&content[..content_length]).map_err(|err| {
            RPCError {
                message: format!("Failed to unmarshal base message: {}", err),
            }
        })?;

    Ok((base_message.method, content_length))
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
    fn test_decode() {
        let incoming_message = "Content-Length: 15\r\n\r\n{\"method\":\"hi\"}";

        let (method, content_length) = decode_message(incoming_message.as_bytes()).unwrap();

        if method != "hi" {
            panic!("Expected method 'hi', got {:?}", method);
        }

        assert_eq!(content_length, 15, "Expected 16, got {}", content_length);
    }
}
