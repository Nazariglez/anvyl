#!/usr/bin/env python3
"""Record active provider and source-extern fixture identities."""
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
BASELINE = ROOT / ".pi/tmp/provider-abi-simplification/fixture-manifest-before.json"


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def fixtures(root: Path) -> dict:
    rows = []
    for path in sorted(root.rglob("*.anv")):
        text = path.read_text()
        helper = "// @helper" in text
        skips = [line.strip() for line in text.splitlines() if "@skip:" in line]
        rows.append({
            "path": path.relative_to(ROOT).as_posix(),
            "sha256": sha256(path),
            "helper": helper,
            "skip_reasons": skips,
            "active": not helper and not skips,
        })
    return {
        "fixture_files": len(rows),
        "active": sum(row["active"] for row in rows),
        "helpers": sum(row["helper"] for row in rows),
        "skipped": sum(bool(row["skip_reasons"]) for row in rows),
        "fixtures": rows,
    }


def active_paths(manifest: dict, group: str) -> set[str]:
    return {row["path"] for row in manifest[group]["fixtures"] if row["active"]}


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    current = {
        "schema": 2,
        "generator": {
            "path": "scripts/measure_provider_abi_fixtures.py",
            "sha256": sha256(Path(__file__)),
        },
        "native_providers": fixtures(ROOT / "tests/run/native_providers"),
        "source_externs": fixtures(ROOT / "tests/syntax/externs"),
    }
    baseline = json.loads(BASELINE.read_text())
    for group in ("native_providers", "source_externs"):
        required = active_paths(baseline, group)
        actual = active_paths(current, group)
        current[group]["baseline_active_subset"] = required <= actual
        current[group]["baseline_missing"] = sorted(required - actual)
        current[group]["baseline_subset"] = required <= actual
        if current[group]["skipped"]:
            raise RuntimeError(f"{group} has skipped fixtures")
        if required - actual:
            raise RuntimeError(f"{group} lost baseline fixtures: {sorted(required - actual)}")
    if current["native_providers"]["active"] < 51:
        raise RuntimeError("fewer than 51 active native-provider fixtures")
    if current["source_externs"]["active"] < 136:
        raise RuntimeError("fewer than 136 active source-extern fixtures")
    output = args.output if args.output.is_absolute() else ROOT / args.output
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps(current, indent=2, sort_keys=True) + "\n")


if __name__ == "__main__":
    main()
