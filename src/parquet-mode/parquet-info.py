#!/usr/bin/env -S uv run --script
# /// script
# dependencies = ["polars", "fire"]
# ///

"""Parquet file inspector for Emacs parquet-mode."""

import fire
import polars as pl


class ParquetInfo:
    """Parquet file inspector."""

    def info(self, path: str) -> None:
        """Print parquet metadata in org format."""
        lf = pl.scan_parquet(path)
        schema = lf.collect_schema()
        total_rows = lf.select(pl.len()).collect().item()

        print(f"#+TITLE: {path}")
        print(f"#+PROPERTY: rows {total_rows}")
        print(f"#+PROPERTY: columns {len(schema)}")
        print()
        print("* Schema")
        print()
        print("| Column | Type |")
        print("|--------|------|")
        for name, dtype in schema.items():
            print(f"| {name} | {dtype} |")
        print()
        print("* Data")
        print()

    def page(self, path: str, offset: int = 0, limit: int = 50) -> None:
        """Print a page of rows as an org table."""
        lf = pl.scan_parquet(path)
        df = lf.slice(offset, limit).collect()

        if df.is_empty():
            print("(no more rows)")
            return

        columns = df.columns
        print("| " + " | ".join(columns) + " |")
        print("|" + "|".join(["---"] * len(columns)) + "|")
        for row in df.iter_rows():
            cells = []
            for val in row:
                s = str(val) if val is not None else ""
                s = s.replace("|", "\\vert{}")
                if len(s) > 60:
                    s = s[:57] + "..."
                cells.append(s)
            print("| " + " | ".join(cells) + " |")


if __name__ == "__main__":
    fire.Fire(ParquetInfo)
