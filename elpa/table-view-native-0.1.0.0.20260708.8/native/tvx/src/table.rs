//! The in-memory table: columnar store with tombstones, a view (filter + sort)
//! cache, patch application, and post-patch delta computation for a subscribed
//! window.

use crate::column::{gen_col, rebuild_str_lookups, Col, StrCol};
use crate::delta::{diff_ops, RowSnap, Sub};
use crate::wire::{cell_i64, cell_str, json_to_string};
use serde_json::{json, Value};
use std::collections::HashMap;

pub struct Table {
    pub ids: Vec<String>,
    pub alive: Vec<bool>,
    pub keys: Vec<String>,
    id_index: HashMap<String, usize>,
    cols: Vec<Col>,
    pub rev: u64,
    pub sub_gen: u64,
    cache: Vec<(String, Vec<u32>)>,
    pub sub: Option<Sub>,
}

impl Table {
    pub fn build(columns: &[Value], source: &Value) -> Result<Table, String> {
        let keys: Vec<String> = columns.iter()
            .map(|c| c.get("key").and_then(Value::as_str).unwrap_or("").to_string())
            .collect();
        let numeric: Vec<bool> = columns.iter()
            .map(|c| c.get("type").and_then(Value::as_str) == Some("number"))
            .collect();

        let kind = source.get("kind").and_then(Value::as_str).unwrap_or("");
        let (ids, mut cols): (Vec<String>, Vec<Col>) = match kind {
            "gen" => {
                let n = source.get("n").and_then(Value::as_u64).unwrap_or(0) as usize;
                let ids = (0..n).map(|i| format!("r{i}")).collect();
                let cols = keys.iter().zip(&numeric).map(|(k, &num)| gen_col(k, num, n)).collect();
                (ids, cols)
            }
            "rows" => {
                let rows = source.get("rows").and_then(Value::as_array).ok_or("source.rows missing")?;
                let ids = rows.iter().map(|r| r.get("id").map(json_to_string).unwrap_or_default()).collect();
                let cols = keys.iter().zip(&numeric).map(|(k, &num)| {
                    if num {
                        Col::Int(rows.iter().map(|r| cell_i64(r, k)).collect())
                    } else {
                        Col::Str(StrCol::from(rows.iter().map(|r| cell_str(r, k)).collect()), HashMap::new())
                    }
                }).collect();
                (ids, cols)
            }
            other => return Err(format!("unknown source kind: {other}")),
        };
        let n = ids.len();
        let id_index = ids.iter().enumerate().map(|(i, s)| (s.clone(), i)).collect();
        rebuild_str_lookups(&mut cols);
        Ok(Table { ids, alive: vec![true; n], keys, id_index, cols, rev: 0,
                   sub_gen: 0, cache: Vec::new(), sub: None })
    }

    pub fn total(&self) -> usize {
        self.alive.iter().filter(|&&a| a).count()
    }

    fn col_index(&self, key: &str) -> Option<usize> {
        self.keys.iter().position(|k| k == key)
    }

    fn ensure_ranks(&mut self) {
        for c in &mut self.cols {
            if let Col::Str(s, _) = c {
                if s.dirty { s.rebuild_rank(); }
            }
        }
    }

    /// Filtered+sorted row indices for (SORT, FILTER); memoized per rev (LRU 4).
    pub fn view(&mut self, sort: &[(String, bool)], filter: &str) -> Result<Vec<u32>, String> {
        let cache_key = format!("{}|{sort:?}|{filter}", self.rev);
        if let Some((_, v)) = self.cache.iter().find(|(k, _)| *k == cache_key) {
            return Ok(v.clone());
        }
        let sort_ci: Vec<(usize, bool)> = sort.iter()
            .map(|(k, asc)| self.col_index(k).map(|ci| (ci, *asc)).ok_or_else(|| format!("no column: {k}")))
            .collect::<Result<_, _>>()?;
        self.ensure_ranks();
        let mut v = self.filter_rows(filter);
        if !sort_ci.is_empty() {
            // Stable sort_by + a final RowIx tiebreak => deterministic, load-order
            // stable (ties keep insertion order, matching the elisp stable sort);
            // desc is a reversed compare per key, not a whole-vector reverse.
            let keys: Vec<(&Col, bool)> = sort_ci.iter().map(|&(ci, asc)| (&self.cols[ci], asc)).collect();
            v.sort_by(|&a, &b| {
                for (col, asc) in &keys {
                    let (ka, kb) = (col.order_key(a as usize), col.order_key(b as usize));
                    let ord = if *asc { ka.cmp(&kb) } else { kb.cmp(&ka) };
                    if ord != std::cmp::Ordering::Equal { return ord; }
                }
                a.cmp(&b)
            });
        }
        self.cache.push((cache_key, v.clone()));
        if self.cache.len() > 4 { self.cache.remove(0); }
        Ok(v)
    }

    /// Live (non-tombstoned) rows matching FILTER (empty = all).  Substring is
    /// tested against string columns' dictionary uniques; numeric columns are
    /// consulted only for a numeric-looking needle (a non-numeric needle cannot
    /// match a decimal string), matching the elisp joined-cell filter.
    pub fn filter_rows(&self, filter: &str) -> Vec<u32> {
        let n = self.ids.len();
        let alive = &self.alive;
        if filter.is_empty() {
            return (0..n as u32).filter(|&i| alive[i as usize]).collect();
        }
        let needle = filter.to_lowercase();
        let numeric_needle = needle.bytes().all(|b| b.is_ascii_digit() || matches!(b, b'.' | b'-' | b'e'));
        let code_hit: Vec<Option<Vec<bool>>> = self.cols.iter().map(|c| match c {
            Col::Str(s, _) => Some(s.lower.iter().map(|u| u.contains(&needle)).collect()),
            Col::Int(_) => None,
        }).collect();
        (0..n as u32).filter(|&i| {
            let i = i as usize;
            alive[i] && self.cols.iter().enumerate().any(|(ci, c)| match c {
                Col::Str(s, _) => code_hit[ci].as_ref().unwrap()[s.codes[i] as usize],
                Col::Int(v) => numeric_needle && v[i].to_string().contains(&needle),
            })
        }).collect()
    }

    pub fn cells_of(&self, r: usize) -> Value {
        Value::Object(self.keys.iter().enumerate()
            .map(|(ci, k)| (k.clone(), self.cols[ci].cell(r))).collect())
    }
    pub fn snap(&self, r: usize) -> RowSnap {
        RowSnap { id: self.ids[r].clone(), cells: self.cells_of(r) }
    }

    /// Apply upserts (update by id, else append) and deletes (tombstone); bump
    /// rev and drop the view cache.
    pub fn patch(&mut self, upserts: &[Value], deletes: &[Value]) {
        let keys = self.keys.clone();
        for row in upserts {
            let id = row.get("id").map(json_to_string).unwrap_or_default();
            let cells = row.get("cells").cloned().unwrap_or(json!({}));
            match self.id_index.get(&id).copied() {
                Some(r) => {
                    for (ci, k) in keys.iter().enumerate() {
                        if let Some(v) = cells.get(k) { self.cols[ci].set(r, v); }
                    }
                    self.alive[r] = true;
                }
                None => {
                    let r = self.ids.len();
                    for (ci, k) in keys.iter().enumerate() {
                        self.cols[ci].push(cells.get(k).unwrap_or(&Value::Null));
                    }
                    self.ids.push(id.clone());
                    self.alive.push(true);
                    self.id_index.insert(id, r);
                }
            }
        }
        for d in deletes {
            let id = json_to_string(d);
            if let Some(&r) = self.id_index.get(&id) { self.alive[r] = false; }
        }
        self.rev += 1;
        self.cache.clear();
    }

    /// Recompute the subscribed window against what the client last saw and
    /// return a `$/delta` payload, or None when the window is unaffected (the
    /// client stays consistent at its current rev).
    pub fn delta_after_patch(&mut self) -> Option<Value> {
        let sub = self.sub.as_ref()?;
        let (offset, limit) = (sub.offset, sub.limit);
        let (sort, filter) = (sub.sort.clone(), sub.filter.clone());
        let base = sub.client_rev;
        let gen = sub.gen;
        let (last, last_matched, last_total) = (sub.last.clone(), sub.last_matched, sub.last_total);
        let view = self.view(&sort, &filter).ok()?;
        let matched = view.len();
        let total = self.total();
        let end = (offset + limit).min(matched);
        let new: Vec<RowSnap> = if offset < matched {
            view[offset..end].iter().map(|&i| self.snap(i as usize)).collect()
        } else { vec![] };
        let ops = diff_ops(&last, &new);
        let rev = self.rev;
        let sub = self.sub.as_mut().unwrap();
        sub.last = new;
        // Nothing visible changed *and* the counts are unchanged: the client
        // stays consistent, no delta needed.  But a patch outside the window can
        // move matched/total with no ops -- still push (empty ops) so the counts
        // refresh rather than going stale until a window-affecting patch.
        if ops.is_empty() && matched == last_matched && total == last_total {
            return None;
        }
        sub.client_rev = rev;
        sub.last_matched = matched;
        sub.last_total = total;
        Some(json!({"handle": Value::Null, "gen": gen, "baseRev": base, "rev": rev,
                    "matched": matched, "total": total, "ops": ops}))
    }

    pub fn aggregate(&mut self, column: &str, op: &str, filter: &str) -> Result<Value, String> {
        let ci = self.col_index(column).ok_or_else(|| format!("no column: {column}"))?;
        let rows = self.filter_rows(filter);
        if op == "count" {
            return Ok(json!(rows.len()));
        }
        let nums: Vec<i64> = match &self.cols[ci] {
            Col::Int(v) => rows.iter().map(|&r| v[r as usize]).collect(),
            Col::Str(_, _) => return Err(format!("column {column} is not numeric")),
        };
        if nums.is_empty() {
            return Ok(Value::Null);
        }
        Ok(match op {
            "sum" => json!(nums.iter().sum::<i64>()),
            "min" => json!(nums.iter().min().unwrap()),
            "max" => json!(nums.iter().max().unwrap()),
            "avg" => json!(nums.iter().sum::<i64>() as f64 / nums.len() as f64),
            other => return Err(format!("unknown aggregate op: {other}")),
        })
    }
}
