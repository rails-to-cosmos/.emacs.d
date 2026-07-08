//! Columnar storage.  String columns are dictionary-encoded with a precomputed
//! `rank[]` so sort/filter is u32 work over uniques, not per-row string work;
//! the rank rebuilds lazily after a patch introduces new uniques.

use crate::wire::{json_i64, json_to_string};
use serde_json::{json, Value};
use std::collections::HashMap;

const WORDS: [&str; 8] = ["core", "lib", "utils", "http", "json", "async", "test", "cli"];

/// Dictionary-encoded string column: rows hold a u32 code into the unique table.
pub struct StrCol {
    pub codes: Vec<u32>,
    pub values: Vec<String>, // unique display strings, by code
    pub lower: Vec<String>,  // lowercased uniques (substring filter)
    pub rank: Vec<u32>,      // rank[code] = codepoint-sorted position => cmp is u32
    pub dirty: bool,         // uniques changed since rank was built
}

impl StrCol {
    pub fn from(strings: Vec<String>) -> StrCol {
        let mut c = StrCol { codes: Vec::new(), values: Vec::new(), lower: Vec::new(),
                             rank: Vec::new(), dirty: true };
        let mut lookup: HashMap<String, u32> = HashMap::new();
        c.codes = strings.into_iter()
            .map(|s| intern(&mut lookup, &mut c.values, &mut c.lower, s))
            .collect();
        c.rebuild_rank();
        c
    }
    /// Code for VALUE, adding a new unique (and marking rank dirty) if unseen.
    pub fn code_of(&mut self, lookup: &mut HashMap<String, u32>, value: String) -> u32 {
        let before = self.values.len();
        let code = intern(lookup, &mut self.values, &mut self.lower, value);
        if self.values.len() != before {
            self.dirty = true;
        }
        code
    }
    /// rebuild rank[code] = position in codepoint-sorted order.  String Ord is
    /// bytewise UTF-8 = codepoint order = elisp `string<'.
    pub fn rebuild_rank(&mut self) {
        let mut order: Vec<u32> = (0..self.values.len() as u32).collect();
        order.sort_by(|&a, &b| self.values[a as usize].cmp(&self.values[b as usize]));
        self.rank = vec![0u32; self.values.len()];
        for (pos, &code) in order.iter().enumerate() {
            self.rank[code as usize] = pos as u32;
        }
        self.dirty = false;
    }
}

fn intern(lookup: &mut HashMap<String, u32>, values: &mut Vec<String>, lower: &mut Vec<String>, s: String) -> u32 {
    *lookup.entry(s.clone()).or_insert_with(|| {
        lower.push(s.to_lowercase());
        values.push(s);
        (values.len() - 1) as u32
    })
}

pub enum Col {
    Int(Vec<i64>),
    Str(StrCol, HashMap<String, u32>), // dict + value->code lookup, for patch interning
}

impl Col {
    pub fn cell(&self, r: usize) -> Value {
        match self {
            Col::Int(v) => json!(v[r]),
            Col::Str(c, _) => json!(c.values[c.codes[r] as usize]),
        }
    }
    /// Total-ordered u64 key so `cmp` matches the elisp comparator (signed ints,
    /// codepoint strings via rank).
    pub fn order_key(&self, r: usize) -> u64 {
        match self {
            Col::Int(v) => (v[r] as u64) ^ (1u64 << 63),
            Col::Str(c, _) => c.rank[c.codes[r] as usize] as u64,
        }
    }
    pub fn set(&mut self, r: usize, v: &Value) {
        match self {
            Col::Int(col) => col[r] = json_i64(v),
            Col::Str(c, lookup) => {
                let code = c.code_of(lookup, json_to_string(v));
                c.codes[r] = code;
            }
        }
    }
    pub fn push(&mut self, v: &Value) {
        match self {
            Col::Int(col) => col.push(json_i64(v)),
            Col::Str(c, lookup) => {
                // A missing (Null) string cell is "", matching the build path;
                // json_to_string(Null) would otherwise store the literal "null".
                let s = if v.is_null() { String::new() } else { json_to_string(v) };
                let code = c.code_of(lookup, s);
                c.codes.push(code);
            }
        }
    }
}

/// Build each string column's value->code reverse lookup (used to intern patches).
pub fn rebuild_str_lookups(cols: &mut [Col]) {
    for c in cols.iter_mut() {
        if let Col::Str(s, lookup) = c {
            lookup.clear();
            for (code, v) in s.values.iter().enumerate() {
                lookup.insert(v.clone(), code as u32);
            }
        }
    }
}

/// Synthetic column for the `gen` source; the elisp benchmark replicates the
/// same formula so the two can be compared fairly.
pub fn gen_col(key: &str, num: bool, n: usize) -> Col {
    if num {
        if key == "num" {
            Col::Int((0..n).map(|i| ((i as i64).wrapping_mul(2654435761) % 100_000).abs()).collect())
        } else {
            Col::Int((0..n).map(|i| i as i64).collect())
        }
    } else {
        Col::Str(StrCol::from((0..n).map(|i| format!("{}-{:05}", WORDS[i % 8], i)).collect()), HashMap::new())
    }
}
