//! The live layer's value types: a client's subscribed window (`Sub`), a row
//! snapshot for diffing (`RowSnap`), and the prefix/suffix diff that turns two
//! window states into minimal delete/insert ops.

use serde_json::{json, Value};

/// One window a client is watching, plus the rows last pushed to it and the
/// rev that client is currently consistent at (for `baseRev` self-healing).
/// `gen` identifies this subscription: it is bumped on every (re)subscribe, so
/// a delta for a newer window is rejected by a client still showing an older
/// one even when the revs happen to match.
pub struct Sub {
    pub offset: usize,
    pub limit: usize,
    pub sort: Vec<(String, bool, bool)>, // (column, ascending, nulls_first)
    pub filter: String,
    pub last: Vec<RowSnap>,
    pub client_rev: u64,
    pub gen: u64,
    pub last_matched: usize,
    pub last_total: usize,
}

#[derive(Clone, PartialEq)]
pub struct RowSnap {
    pub id: String,
    pub cells: Value,
}

/// Prefix/suffix diff of two row windows into delete/insert ops, mirroring the
/// elisp incremental renderer.  Deletes descend and inserts ascend, so applying
/// them left to right on the client is unambiguous.  A row whose content changed
/// falls into the rewritten middle (delete + insert); the client's id->cons pool
/// still recovers `eq` for an unchanged row that merely moved.
pub fn diff_ops(old: &[RowSnap], new: &[RowSnap]) -> Vec<Value> {
    let (no, nn) = (old.len(), new.len());
    let mut p = 0;
    while p < no && p < nn && old[p] == new[p] {
        p += 1;
    }
    let mut s = 0;
    while s < no - p && s < nn - p && old[no - 1 - s] == new[nn - 1 - s] {
        s += 1;
    }
    let mut ops = Vec::new();
    for i in (p..no - s).rev() {
        ops.push(json!({"op": "delete", "index": i}));
    }
    for i in p..nn - s {
        ops.push(json!({"op": "insert", "index": i,
                        "row": {"id": new[i].id, "cells": new[i].cells}}));
    }
    ops
}
