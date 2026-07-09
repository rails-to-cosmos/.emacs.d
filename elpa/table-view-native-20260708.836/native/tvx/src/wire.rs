//! JSON-RPC stdio transport: Content-Length framing (like jsonrpc.el / LSP)
//! and the small serde_json coercion helpers the rest of the crate shares.

use serde_json::Value;
use std::io::{self, Read, Write};

/// Read one Content-Length-framed JSON-RPC message, or None at EOF.
pub fn read_frame<R: Read>(reader: &mut R) -> Option<Value> {
    let mut header = Vec::new();
    let mut byte = [0u8; 1];
    loop {
        if reader.read_exact(&mut byte).is_err() {
            return None;
        }
        header.push(byte[0]);
        if header.ends_with(b"\r\n\r\n") {
            break;
        }
    }
    let mut content_length = None;
    for line in String::from_utf8_lossy(&header).split("\r\n") {
        if let Some(v) = line.strip_prefix("Content-Length:") {
            content_length = v.trim().parse().ok();
        }
    }
    let len: usize = content_length?;
    let mut buf = vec![0u8; len];
    reader.read_exact(&mut buf).ok()?;
    serde_json::from_slice(&buf).ok()
}

/// Write one Content-Length-framed JSON-RPC message to stdout.
pub fn write_frame(v: &Value) {
    let body = serde_json::to_vec(v).unwrap();
    let mut out = io::stdout().lock();
    let _ = write!(out, "Content-Length: {}\r\n\r\n", body.len());
    let _ = out.write_all(&body);
    let _ = out.flush();
}

/// A JSON value as a display string (strings verbatim, everything else via Display).
pub fn json_to_string(v: &Value) -> String {
    match v {
        Value::String(s) => s.clone(),
        other => other.to_string(),
    }
}

/// A JSON value coerced to i64 (numbers direct, numeric strings parsed, else 0).
pub fn json_i64(v: &Value) -> i64 {
    match v {
        Value::Number(n) => n.as_i64().unwrap_or_else(|| n.as_f64().unwrap_or(0.0) as i64),
        Value::String(s) => s.parse().unwrap_or(0),
        _ => 0,
    }
}

pub fn cell_str(row: &Value, key: &str) -> String {
    row.get("cells").and_then(|c| c.get(key)).map(json_to_string).unwrap_or_default()
}
pub fn cell_i64(row: &Value, key: &str) -> i64 {
    row.get("cells").and_then(|c| c.get(key)).map(json_i64).unwrap_or(0)
}

/// Parse a `[[key, asc], ...]` sort chain into (column, ascending) pairs.
pub fn parse_sort(v: &Value) -> Vec<(String, bool)> {
    v.as_array()
        .map(|a| {
            a.iter()
                .filter_map(|p| {
                    let p = p.as_array()?;
                    Some((
                        p.first()?.as_str()?.to_string(),
                        p.get(1).and_then(Value::as_bool).unwrap_or(true),
                    ))
                })
                .collect()
        })
        .unwrap_or_default()
}
