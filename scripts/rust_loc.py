#!/usr/bin/env python3

from __future__ import annotations

import subprocess
from pathlib import Path


def rust_files() -> list[Path]:
    files = subprocess.check_output(["git", "ls-files", "*.rs"], text=True).splitlines()
    return [Path(file) for file in files if "tests" not in Path(file).parts]


def is_cfg_test(line: str) -> bool:
    compact = "".join(line.split())
    return compact.startswith("#[cfg(test)]")


def is_test_attr(line: str) -> bool:
    return "".join(line.split()).startswith("#[test]")


def strip_item(lines: list[str], start: int) -> int:
    i = start
    depth = 0
    started = False

    while i < len(lines):
        line = lines[i]
        stripped = line.strip()
        if not started and (not stripped or stripped.startswith("#")):
            i += 1
            continue

        started = True
        depth += line.count("{") - line.count("}")
        i += 1

        if depth <= 0 and (";" in line or "{" in line or stripped.endswith("}")):
            return i

    return i


def production_lines(lines: list[str]) -> list[str]:
    out = []
    i = 0
    while i < len(lines):
        line = lines[i]
        if is_cfg_test(line) or is_test_attr(line):
            i = strip_item(lines, i + 1)
            continue
        out.append(line)
        i += 1
    return out


def main() -> None:
    total = 0
    nonblank = 0
    for path in rust_files():
        lines = production_lines(path.read_text(errors="ignore").splitlines())
        total += len(lines)
        nonblank += sum(1 for line in lines if line.strip())

    print(f"lines: {total}")
    print(f"nonblank: {nonblank}")


if __name__ == "__main__":
    main()
