//! parquet-mod — native Emacs module for parquet-mode.
//!
//! Exposes two functions to Emacs:
//!   (parquet-mod-info PATH)              -> String  (org-mode metadata)
//!   (parquet-mod-page PATH OFFSET LIMIT) -> String  (org-mode data table)
//!
//! Output format must match parquet-info.py exactly so the elisp
//! rendering/paginator layer can stay provider-agnostic.

use emacs::{defun, Env, Result};
use polars::prelude::*;
use std::fmt::Write as _;

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "parquet-mod", defun_prefix = "parquet-mod")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

/* -------------------------------------------------------------------------
 * public defuns */

#[defun]
fn info(_: &Env, path: String) -> Result<String> {
    let lf = scan(&path)?;
    let schema = lf.schema().map_err(to_emacs_err)?;
    let total_rows = row_count(&path)?;

    let mut out = String::new();
    writeln!(out, "#+TITLE: {path}").unwrap();
    writeln!(out, "#+PROPERTY: rows {total_rows}").unwrap();
    writeln!(out, "#+PROPERTY: columns {}", schema.len()).unwrap();
    writeln!(out).unwrap();
    writeln!(out, "* Schema").unwrap();
    writeln!(out).unwrap();
    writeln!(out, "| Column | Type |").unwrap();
    writeln!(out, "|--------|------|").unwrap();
    for (name, dtype) in schema.iter() {
        writeln!(out, "| {name} | {dtype} |").unwrap();
    }
    writeln!(out).unwrap();
    writeln!(out, "* Data").unwrap();
    writeln!(out).unwrap();
    Ok(out)
}

#[defun]
fn page(_: &Env, path: String, offset: i64, limit: i64) -> Result<String> {
    let lf = scan(&path)?;
    let df = lf
        .slice(offset, limit.max(0) as u32)
        .collect()
        .map_err(to_emacs_err)?;

    if df.is_empty() {
        return Ok("(no more rows)\n".to_string());
    }

    let cols: Vec<String> = df
        .get_column_names()
        .iter()
        .map(|s| s.to_string())
        .collect();

    let mut out = String::new();
    writeln!(out, "| {} |", cols.join(" | ")).unwrap();
    writeln!(
        out,
        "|{}|",
        std::iter::repeat("---")
            .take(cols.len())
            .collect::<Vec<_>>()
            .join("|")
    )
    .unwrap();

    let columns = df.get_columns();
    for i in 0..df.height() {
        let cells: Vec<String> = columns
            .iter()
            .map(|c| format_cell(&c.get(i).unwrap_or(AnyValue::Null)))
            .collect();
        writeln!(out, "| {} |", cells.join(" | ")).unwrap();
    }
    Ok(out)
}

/* -------------------------------------------------------------------------
 * helpers */

fn scan(path: &str) -> Result<LazyFrame> {
    LazyFrame::scan_parquet(path, ScanArgsParquet::default()).map_err(to_emacs_err)
}

/// Row count without materializing the data — pushed down to the parquet reader.
fn row_count(path: &str) -> Result<i64> {
    let df = scan(path)?
        .select([len().alias("n")])
        .collect()
        .map_err(to_emacs_err)?;
    let n = df
        .column("n")
        .map_err(to_emacs_err)?
        .get(0)
        .map_err(to_emacs_err)?;
    Ok(match n {
        AnyValue::UInt32(v) => v as i64,
        AnyValue::UInt64(v) => v as i64,
        AnyValue::Int64(v) => v,
        AnyValue::Int32(v) => v as i64,
        _ => 0,
    })
}

/// Format a polars cell for org-table insertion: stringify, escape `|`, truncate.
/// Matches parquet-info.py:format behaviour.
fn format_cell(v: &AnyValue<'_>) -> String {
    let mut s = match v {
        AnyValue::Null => String::new(),
        AnyValue::String(s) => s.to_string(),
        AnyValue::StringOwned(s) => s.to_string(),
        other => other.to_string(),
    };
    // Escape pipes — would otherwise break the org table cell boundary.
    if s.contains('|') {
        s = s.replace('|', "\\vert{}");
    }
    // Truncate long values so a stray blob doesn't balloon the line.
    if s.chars().count() > 60 {
        let truncated: String = s.chars().take(57).collect();
        s = format!("{truncated}...");
    }
    s
}

fn to_emacs_err<E: std::fmt::Display>(e: E) -> emacs::Error {
    emacs::Error::msg(format!("parquet-mod: {e}"))
}
