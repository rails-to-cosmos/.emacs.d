//! tvx — table-view native backend.
//! JSON-RPC 2.0 over stdio (Content-Length framed, like jsonrpc.el / LSP).
//! Requests: initialize, open, window, close, count, aggregate, patch.
//! Server notification: $/delta (a subscribed window changed after a patch).
//!
//! Modules: `wire` (framing + json helpers), `column` (dict-encoded columns),
//! `delta` (subscription + prefix/suffix diff), `table` (the store + views +
//! patch).  See docs/proposals/native-backend.org.

mod column;
mod delta;
mod table;
mod wire;

use crate::delta::Sub;
use crate::table::Table;
use crate::wire::{parse_sort, read_frame, write_frame};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::io;

const PROTOCOL: u32 = 1;
const VERSION: &str = env!("CARGO_PKG_VERSION");

fn handle_of<'a>(params: &Value, tables: &'a mut HashMap<u64, Table>) -> Result<&'a mut Table, (i64, String)> {
    let h = params.get("handle").and_then(Value::as_u64).unwrap_or(0);
    tables.get_mut(&h).ok_or((-32000, format!("no such handle {h}")))
}

fn main() {
    if std::env::args().any(|a| a == "--version" || a == "-V") {
        println!("tvx {VERSION} protocol {PROTOCOL}");
        return;
    }
    let mut tables: HashMap<u64, Table> = HashMap::new();
    let mut next_handle: u64 = 1;
    let stdin = io::stdin();
    let mut reader = stdin.lock();

    while let Some(msg) = read_frame(&mut reader) {
        let id = msg.get("id").cloned().unwrap_or(Value::Null);
        let method = msg.get("method").and_then(Value::as_str).unwrap_or("");
        let params = msg.get("params").cloned().unwrap_or(json!({}));
        let mut notify: Option<Value> = None; // a $/delta to push after the reply

        let result: Result<Value, (i64, String)> = match method {
            "initialize" => {
                let want = params.get("protocol");
                let min = want.and_then(|p| p.get("min")).and_then(Value::as_u64).unwrap_or(1) as u32;
                let max = want.and_then(|p| p.get("max")).and_then(Value::as_u64).unwrap_or(1) as u32;
                if PROTOCOL < min || PROTOCOL > max {
                    Err((-32100, format!("protocol mismatch: server {PROTOCOL}, client [{min},{max}]")))
                } else {
                    Ok(json!({"serverInfo": {"name": "tvx", "version": VERSION}, "protocol": PROTOCOL,
                              "wire": "json", "capabilities": {"delta": true, "aggregate": true,
                                                               "patch": true, "count": true, "progress": false}}))
                }
            }
            "open" => {
                let empty = vec![];
                let columns = params.get("columns").and_then(Value::as_array).unwrap_or(&empty);
                let source = params.get("source").cloned().unwrap_or(json!({}));
                match Table::build(columns, &source) {
                    Ok(t) => {
                        let h = next_handle; next_handle += 1;
                        let total = t.ids.len();
                        tables.insert(h, t);
                        Ok(json!({"handle": h, "gen": 0, "rev": 0, "total": total, "protocol": PROTOCOL}))
                    }
                    Err(e) => Err((-32602, e)),
                }
            }
            "window" => match handle_of(&params, &mut tables) {
                Err(e) => Err(e),
                Ok(t) => {
                    let offset = params.get("offset").and_then(Value::as_u64).unwrap_or(0) as usize;
                    let limit = params.get("limit").and_then(Value::as_u64).unwrap_or(50) as usize;
                    let sort = parse_sort(params.get("sort").unwrap_or(&Value::Null));
                    let filter = params.get("filter").and_then(Value::as_str).unwrap_or("").to_string();
                    let subscribe = params.get("subscribe").and_then(Value::as_bool).unwrap_or(false);
                    let total = t.total();
                    match t.view(&sort, &filter) {
                        Err(e) => Err((-32004, e)),
                        Ok(view) => {
                            let matched = view.len();
                            let end = (offset + limit).min(matched);
                            let snaps: Vec<_> = if offset < matched {
                                view[offset..end].iter().map(|&i| t.snap(i as usize)).collect()
                            } else { vec![] };
                            let rows: Vec<Value> = snaps.iter()
                                .map(|r| json!({"id": r.id, "cells": r.cells})).collect();
                            let gen = if subscribe {
                                t.sub_gen += 1;
                                t.sub = Some(Sub { offset, limit, sort, filter,
                                                   last: snaps, client_rev: t.rev, gen: t.sub_gen,
                                                   last_matched: matched, last_total: total });
                                t.sub_gen
                            } else { 0 };
                            Ok(json!({"gen": gen, "rev": t.rev, "offset": offset, "total": total,
                                      "matched": matched, "hasNext": end < matched, "rows": rows}))
                        }
                    }
                }
            },
            "count" => match handle_of(&params, &mut tables) {
                Err(e) => Err(e),
                Ok(t) => {
                    let filter = params.get("filter").and_then(Value::as_str).unwrap_or("");
                    Ok(json!({"rev": t.rev, "matched": t.filter_rows(filter).len()}))
                }
            },
            "aggregate" => match handle_of(&params, &mut tables) {
                Err(e) => Err(e),
                Ok(t) => {
                    let filter = params.get("filter").and_then(Value::as_str).unwrap_or("");
                    let column = params.get("column").and_then(Value::as_str).unwrap_or("");
                    let op = params.get("op").and_then(Value::as_str).unwrap_or("count");
                    match t.aggregate(column, op, filter) {
                        Ok(v) => Ok(json!({"rev": t.rev, "value": v})),
                        Err(e) => Err((-32004, e)),
                    }
                }
            },
            "patch" => {
                let handle = params.get("handle").and_then(Value::as_u64).unwrap_or(0);
                match handle_of(&params, &mut tables) {
                    Err(e) => Err(e),
                    Ok(t) => {
                        let empty = vec![];
                        let upserts = params.get("upserts").and_then(Value::as_array).unwrap_or(&empty).clone();
                        let deletes = params.get("deletes").and_then(Value::as_array).unwrap_or(&empty).clone();
                        t.patch(&upserts, &deletes);
                        if let Some(mut d) = t.delta_after_patch() {
                            d["handle"] = json!(handle);
                            notify = Some(json!({"jsonrpc": "2.0", "method": "$/delta", "params": d}));
                        }
                        Ok(json!({"rev": t.rev}))
                    }
                }
            }
            "close" => {
                tables.remove(&params.get("handle").and_then(Value::as_u64).unwrap_or(0));
                Ok(json!({"ok": true}))
            }
            "shutdown" => Ok(json!({})),
            other => Err((-32601, format!("unknown method: {other}"))),
        };

        // Push the $/delta first, then the request reply.
        if let Some(n) = notify {
            write_frame(&n);
        }
        if id != Value::Null {
            let resp = match result {
                Ok(r) => json!({"jsonrpc": "2.0", "id": id, "result": r}),
                Err((code, msg)) => json!({"jsonrpc": "2.0", "id": id, "error": {"code": code, "message": msg}}),
            };
            write_frame(&resp);
        }
    }
}

#[cfg(test)]
mod tests;
