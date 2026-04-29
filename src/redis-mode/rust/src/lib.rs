//! redis-mod — native Emacs module for redis-mode.
//!
//! Defuns:
//!   (redis-mod-exec    URL CMD)                  -> String  pretty-printed RESP
//!   (redis-mod-scan    URL CURSOR PATTERN COUNT) -> (NEW-CURSOR . (KEY ...))
//!   (redis-mod-type    URL KEY)                  -> String  ("string", "list", …)
//!   (redis-mod-preview URL KEY MAX-ITEMS)        -> String  small preview
//!   (redis-mod-ping    URL)                      -> String  ("PONG" on success)
//!   (redis-mod-disconnect URL)                   -> nil     drops the cached conn
//!
//! Connections are cached per URL inside a `Mutex<HashMap>`; a connection-error
//! retry-once policy reconnects transparently so REPL turns survive the server
//! dropping idle sockets.

use emacs::{defun, Env, IntoLisp, Result, Value};
use once_cell::sync::Lazy;
use redis::{Client, Commands, Connection, ErrorKind, RedisError, RedisResult, Value as RV};
use std::collections::HashMap;
use std::fmt::Write as _;
use std::sync::Mutex;

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "redis-mod", defun_prefix = "redis-mod")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

/* -------------------------------------------------------------------------
 * connection cache */

static CONNS: Lazy<Mutex<HashMap<String, Connection>>> = Lazy::new(Default::default);

fn is_conn_err(e: &RedisError) -> bool {
    matches!(
        e.kind(),
        ErrorKind::IoError | ErrorKind::ClientError | ErrorKind::AuthenticationFailed
    ) || e.is_connection_dropped()
        || e.is_connection_refusal()
}

/// Borrow (creating if necessary) the cached connection for URL and run F.
/// On a connection-class error, drop the cached entry, reconnect once, retry.
fn with_conn<F, R>(url: &str, mut f: F) -> RedisResult<R>
where
    F: FnMut(&mut Connection) -> RedisResult<R>,
{
    let mut map = CONNS.lock().unwrap();
    if !map.contains_key(url) {
        let client = Client::open(url)?;
        map.insert(url.to_string(), client.get_connection()?);
    }
    {
        let conn = map.get_mut(url).unwrap();
        match f(conn) {
            Ok(r) => return Ok(r),
            Err(e) if is_conn_err(&e) => {
                // fall through to reconnect
                map.remove(url);
            }
            Err(e) => return Err(e),
        }
    }
    let client = Client::open(url)?;
    let mut fresh = client.get_connection()?;
    let r = f(&mut fresh)?;
    map.insert(url.to_string(), fresh);
    Ok(r)
}

fn to_emacs_err<E: std::fmt::Display>(e: E) -> emacs::Error {
    emacs::Error::msg(format!("redis-mod: {e}"))
}

/* -------------------------------------------------------------------------
 * defuns */

#[defun]
fn exec(_: &Env, url: String, cmd: String) -> Result<String> {
    let parts = shlex::split(&cmd)
        .ok_or_else(|| emacs::Error::msg("redis-mod: unparseable command (mismatched quotes?)"))?;
    if parts.is_empty() {
        return Err(emacs::Error::msg("redis-mod: empty command"));
    }
    let (name, args) = parts.split_first().unwrap();

    let val: RV = with_conn(&url, |c| {
        let mut command = redis::cmd(name);
        for a in args {
            command.arg(a.as_str());
        }
        command.query::<RV>(c)
    })
    .map_err(to_emacs_err)?;

    Ok(format_value(&val, 0))
}

#[defun]
fn scan<'e>(
    env: &'e Env,
    url: String,
    cursor: String,
    pattern: String,
    count: i64,
) -> Result<Value<'e>> {
    let (next_cursor, keys): (String, Vec<String>) = with_conn(&url, |c| {
        let mut cmd = redis::cmd("SCAN");
        cmd.arg(&cursor);
        if !pattern.is_empty() {
            cmd.arg("MATCH").arg(&pattern);
        }
        if count > 0 {
            cmd.arg("COUNT").arg(count);
        }
        cmd.query::<(String, Vec<String>)>(c)
    })
    .map_err(to_emacs_err)?;

    // Build (NEW-CURSOR KEY1 KEY2 ...) — a flat list. Elisp side does
    // (car/cdr) to split off the cursor.
    let mut elems: Vec<Value> = Vec::with_capacity(keys.len() + 1);
    elems.push(next_cursor.into_lisp(env)?);
    for k in keys {
        elems.push(k.into_lisp(env)?);
    }
    env.call("list", &elems)
}

#[defun]
fn type_of(_: &Env, url: String, key: String) -> Result<String> {
    with_conn(&url, |c| c.key_type(&key)).map_err(to_emacs_err)
}

#[defun]
fn preview(_: &Env, url: String, key: String, max_items: i64) -> Result<String> {
    let max = max_items.max(1) as isize;

    let kind: String = with_conn(&url, |c| c.key_type(&key)).map_err(to_emacs_err)?;

    match kind.as_str() {
        "none" => Ok("(no such key)\n".into()),
        "string" => {
            let val: Option<String> =
                with_conn(&url, |c| c.get(&key)).map_err(to_emacs_err)?;
            Ok(match val {
                Some(s) => format_string_value(&s),
                None => "(nil)".into(),
            })
        }
        "list" => {
            let len: isize =
                with_conn(&url, |c| c.llen(&key)).map_err(to_emacs_err)?;
            let stop = (max - 1).min(len.saturating_sub(1));
            let items: Vec<String> =
                with_conn(&url, |c| c.lrange(&key, 0, stop)).map_err(to_emacs_err)?;
            let mut out = format!("(list, len={})\n", len);
            for (i, v) in items.iter().enumerate() {
                writeln!(out, "{:>3}) {}", i, format_string_value(v)).ok();
            }
            if (len as usize) > items.len() {
                writeln!(out, "    … {} more", (len as usize) - items.len()).ok();
            }
            Ok(out)
        }
        "set" => {
            let len: isize =
                with_conn(&url, |c| c.scard(&key)).map_err(to_emacs_err)?;
            let items: Vec<String> = with_conn(&url, |c| {
                redis::cmd("SRANDMEMBER")
                    .arg(&key)
                    .arg(max as i64)
                    .query::<Vec<String>>(c)
            })
            .map_err(to_emacs_err)?;
            let mut out = format!("(set, len={})\n", len);
            for v in &items {
                writeln!(out, "  · {}", format_string_value(v)).ok();
            }
            if (len as usize) > items.len() {
                writeln!(out, "  … {} more", (len as usize) - items.len()).ok();
            }
            Ok(out)
        }
        "zset" => {
            let len: isize =
                with_conn(&url, |c| c.zcard(&key)).map_err(to_emacs_err)?;
            let stop = (max - 1).min(len.saturating_sub(1));
            let pairs: Vec<(String, f64)> = with_conn(&url, |c| {
                redis::cmd("ZRANGE")
                    .arg(&key)
                    .arg(0_isize)
                    .arg(stop)
                    .arg("WITHSCORES")
                    .query::<Vec<(String, f64)>>(c)
            })
            .map_err(to_emacs_err)?;
            let mut out = format!("(zset, len={})\n", len);
            for (m, s) in &pairs {
                writeln!(out, "  {:>12}  {}", s, format_string_value(m)).ok();
            }
            if (len as usize) > pairs.len() {
                writeln!(out, "  … {} more", (len as usize) - pairs.len()).ok();
            }
            Ok(out)
        }
        "hash" => {
            let len: isize =
                with_conn(&url, |c| c.hlen(&key)).map_err(to_emacs_err)?;
            let pairs: Vec<(String, String)> = with_conn(&url, |c| {
                redis::cmd("HSCAN")
                    .arg(&key)
                    .arg("0")
                    .arg("COUNT")
                    .arg(max as i64)
                    .query::<(String, Vec<(String, String)>)>(c)
                    .map(|(_, v)| v)
            })
            .map_err(to_emacs_err)?;
            let shown: Vec<&(String, String)> = pairs.iter().take(max as usize).collect();
            let mut out = format!("(hash, len={})\n", len);
            for (k, v) in &shown {
                writeln!(out, "  {} = {}", k, format_string_value(v)).ok();
            }
            if (len as usize) > shown.len() {
                writeln!(out, "  … {} more", (len as usize) - shown.len()).ok();
            }
            Ok(out)
        }
        "stream" => {
            let len: isize = with_conn(&url, |c| {
                redis::cmd("XLEN").arg(&key).query::<isize>(c)
            })
            .map_err(to_emacs_err)?;
            Ok(format!(
                "(stream, len={})\n  use XRANGE/XREVRANGE in the REPL to inspect entries\n",
                len
            ))
        }
        other => Ok(format!("(unsupported type: {})\n", other)),
    }
}

#[defun]
fn ping(_: &Env, url: String) -> Result<String> {
    with_conn(&url, |c| {
        redis::cmd("PING").query::<String>(c)
    })
    .map_err(to_emacs_err)
}

#[defun]
fn disconnect(_: &Env, url: String) -> Result<()> {
    CONNS.lock().unwrap().remove(&url);
    Ok(())
}

/* -------------------------------------------------------------------------
 * formatting */

fn format_value(v: &RV, indent: usize) -> String {
    let pad = "  ".repeat(indent);
    match v {
        RV::Nil => format!("{pad}(nil)"),
        RV::Int(n) => format!("{pad}(integer) {n}"),
        RV::BulkString(bytes) => format!("{pad}{}", format_bulk(bytes)),
        RV::SimpleString(s) => format!("{pad}{s}"),
        RV::Okay => format!("{pad}OK"),
        RV::Array(items) => {
            if items.is_empty() {
                return format!("{pad}(empty array)");
            }
            let mut out = String::new();
            for (i, it) in items.iter().enumerate() {
                let inner = format_value(it, indent + 1);
                let inner = inner.trim_start_matches(' ');
                writeln!(out, "{pad}{:>3}) {}", i + 1, inner).ok();
            }
            out.trim_end().to_string()
        }
        RV::Map(pairs) => {
            let mut out = String::new();
            for (k, val) in pairs {
                let kfmt = format_value(k, 0);
                let vfmt = format_value(val, 0);
                writeln!(out, "{pad}{} => {}", kfmt.trim(), vfmt.trim()).ok();
            }
            out.trim_end().to_string()
        }
        RV::Set(items) => {
            let mut out = String::new();
            for it in items {
                writeln!(out, "{pad}· {}", format_value(it, 0).trim()).ok();
            }
            out.trim_end().to_string()
        }
        RV::Double(d) => format!("{pad}(double) {d}"),
        RV::Boolean(b) => format!("{pad}(boolean) {b}"),
        RV::VerbatimString { format: _, text } => format!("{pad}{text}"),
        RV::BigNumber(n) => format!("{pad}(bignumber) {n}"),
        RV::Push { kind, data } => {
            let mut out = format!("{pad}(push: {kind})\n");
            for it in data {
                writeln!(out, "{pad}  {}", format_value(it, 0).trim()).ok();
            }
            out.trim_end().to_string()
        }
        RV::Attribute { data, attributes: _ } => format_value(data, indent),
        RV::ServerError(e) => format!("{pad}(error) {e:?}"),
    }
}

fn format_bulk(bytes: &[u8]) -> String {
    match std::str::from_utf8(bytes) {
        Ok(s) => format_string_value(s),
        Err(_) => format!("(binary, {} bytes)", bytes.len()),
    }
}

fn format_string_value(s: &str) -> String {
    if s.contains(['\n', '"', '\t']) || s.is_empty() {
        format!("\"{}\"", s.escape_default())
    } else {
        format!("\"{s}\"")
    }
}
