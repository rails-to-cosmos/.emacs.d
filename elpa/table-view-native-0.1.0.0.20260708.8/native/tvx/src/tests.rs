//! Backend unit tests: collation/order parity with elisp, patch + tombstones,
//! aggregates, and the delta diff / subscribe path.

use crate::delta::{diff_ops, RowSnap, Sub};
use crate::table::Table;
use serde_json::{json, Value};

fn cols() -> Vec<Value> {
    vec![json!({"key":"name","type":"text"}), json!({"key":"num","type":"number"})]
}
fn rows(specs: &[(&str, &str, i64)]) -> Value {
    json!({"kind":"rows","rows": specs.iter().map(|(id, name, num)|
        json!({"id": id, "cells": {"name": name, "num": num}})).collect::<Vec<_>>()})
}
fn ids(t: &mut Table, sort: &[(String, bool)], filter: &str) -> Vec<String> {
    t.view(sort, filter).unwrap().iter().map(|&i| t.ids[i as usize].clone()).collect()
}

#[test]
fn string_sort_codepoint() {
    let mut t = Table::build(&cols(), &rows(&[("a", "banana", 1), ("b", "apple", 2), ("c", "cherry", 3)])).unwrap();
    assert_eq!(ids(&mut t, &[("name".into(), true)], ""), ["b", "a", "c"]);
}
#[test]
fn numeric_sort_signed() {
    let mut t = Table::build(&cols(), &rows(&[("a", "x", 5), ("b", "y", -3), ("c", "z", 0)])).unwrap();
    assert_eq!(ids(&mut t, &[("num".into(), true)], ""), ["b", "c", "a"]);
}
#[test]
fn desc_ties_keep_insertion_order() {
    let mut t = Table::build(&cols(), &rows(&[("a", "p", 1), ("b", "q", 1), ("c", "r", 1)])).unwrap();
    assert_eq!(ids(&mut t, &[("num".into(), false)], ""), ["a", "b", "c"]);
}
#[test]
fn filter_numeric_skip() {
    let mut t = Table::build(&cols(), &rows(&[("a", "lib", 12), ("b", "core", 34)])).unwrap();
    assert_eq!(ids(&mut t, &[], "lib"), ["a"]);
    assert_eq!(ids(&mut t, &[], "34"), ["b"]);
    assert_eq!(ids(&mut t, &[], "zzz"), Vec::<String>::new());
}
#[test]
fn multi_key_stable() {
    let mut t = Table::build(&cols(), &rows(&[("a", "same", 2), ("b", "same", 1), ("c", "diff", 5)])).unwrap();
    assert_eq!(ids(&mut t, &[("name".into(), true), ("num".into(), true)], ""), ["c", "b", "a"]);
}

#[test]
fn patch_upsert_delete() {
    let mut t = Table::build(&cols(), &rows(&[("a", "lib", 1), ("b", "core", 2)])).unwrap();
    t.patch(&[json!({"id":"c","cells":{"name":"async","num":3}})], &[]);
    assert_eq!(ids(&mut t, &[("name".into(), true)], ""), ["c", "b", "a"]); // async<core<lib
    t.patch(&[json!({"id":"a","cells":{"name":"aaa","num":9}})], &[]); // update sorts first
    assert_eq!(ids(&mut t, &[("name".into(), true)], ""), ["a", "c", "b"]);
    t.patch(&[], &[json!("b")]); // delete
    assert_eq!(ids(&mut t, &[("name".into(), true)], ""), ["a", "c"]);
}
#[test]
fn aggregate_ops() {
    let mut t = Table::build(&cols(), &rows(&[("a", "x", 10), ("b", "y", 20), ("c", "z", 30)])).unwrap();
    assert_eq!(t.aggregate("num", "sum", "").unwrap(), json!(60));
    assert_eq!(t.aggregate("num", "max", "").unwrap(), json!(30));
    assert_eq!(t.aggregate("num", "count", "").unwrap(), json!(3));
    t.patch(&[], &[json!("c")]);
    assert_eq!(t.aggregate("num", "sum", "").unwrap(), json!(30)); // tombstoned c excluded
}
#[test]
fn delta_diff_prefix_suffix() {
    let old = vec![RowSnap { id: "a".into(), cells: json!({}) }, RowSnap { id: "b".into(), cells: json!({}) },
                   RowSnap { id: "d".into(), cells: json!({}) }];
    let new = vec![RowSnap { id: "a".into(), cells: json!({}) }, RowSnap { id: "c".into(), cells: json!({}) },
                   RowSnap { id: "d".into(), cells: json!({}) }];
    let ops = diff_ops(&old, &new); // replace middle b->c
    assert_eq!(ops, vec![json!({"op":"delete","index":1}),
                         json!({"op":"insert","index":1,"row":{"id":"c","cells":{}}})]);
}
fn subscribe(t: &mut Table, sort: Vec<(String, bool)>, filter: &str, limit: usize) {
    let view = t.view(&sort, filter).unwrap();
    let matched = view.len();
    let end = limit.min(matched);
    let snaps: Vec<RowSnap> = view[..end].iter().map(|&i| t.snap(i as usize)).collect();
    t.sub_gen += 1;
    t.sub = Some(Sub { offset: 0, limit, sort, filter: filter.into(), last: snaps,
                       client_rev: t.rev, gen: t.sub_gen, last_matched: matched, last_total: t.total() });
}

#[test]
fn subscribe_then_patch_emits_delta() {
    let mut t = Table::build(&cols(), &rows(&[("a", "lib", 1), ("b", "core", 2)])).unwrap();
    subscribe(&mut t, vec![("name".into(), true)], "", 50); // [core(b), lib(a)]
    t.patch(&[json!({"id":"c","cells":{"name":"aaa","num":9}})], &[]); // aaa sorts first
    let d = t.delta_after_patch().unwrap();
    assert_eq!(d["baseRev"], json!(0));
    assert_eq!(d["rev"], json!(1));
    assert_eq!(d["gen"], json!(1));
    assert_eq!(d["ops"][0]["op"], json!("insert"));
    assert_eq!(d["ops"][0]["index"], json!(0));
    assert_eq!(d["ops"][0]["row"]["id"], json!("c"));
}

#[test]
fn patch_outside_window_still_refreshes_counts() {
    // Window shows only the first row; a delete of a later row leaves the window
    // slice identical but must still emit an (empty-ops) delta with new counts.
    let mut t = Table::build(&cols(), &rows(&[("a", "aaa", 1), ("b", "bbb", 2), ("c", "ccc", 3)])).unwrap();
    subscribe(&mut t, vec![("name".into(), true)], "", 1); // window = [aaa(a)]
    t.patch(&[], &[json!("c")]); // delete a row outside the window
    let d = t.delta_after_patch().unwrap();
    assert_eq!(d["ops"].as_array().unwrap().len(), 0); // no visible change
    assert_eq!(d["matched"], json!(2));
    assert_eq!(d["total"], json!(2));
    assert_eq!(d["baseRev"], json!(0));
    assert_eq!(d["rev"], json!(1));
    // a second identical-count patch (no-op delete of the same id) emits nothing
    t.patch(&[], &[json!("c")]);
    assert!(t.delta_after_patch().is_none());
}

#[test]
fn append_missing_string_cell_is_empty_not_null() {
    let mut t = Table::build(&cols(), &rows(&[("a", "x", 1)])).unwrap();
    t.patch(&[json!({"id":"z","cells":{"num":5}})], &[]); // no "name"
    // "" sorts before "x"; the appended row's name is "" not the literal "null".
    let order = ids(&mut t, &[("name".into(), true)], "");
    assert_eq!(order, ["z", "a"]);
    assert_eq!(ids(&mut t, &[], "null"), Vec::<String>::new()); // no spurious "null"
}
