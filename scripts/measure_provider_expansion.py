#!/usr/bin/env python3
"""Capture provider proc-macro expansion sizes and public helper identities."""
from __future__ import annotations

import argparse
import hashlib
import json
import re
import shutil
import subprocess
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
FIXTURES = {
    "local_provider": "tests/run/native_providers/local_provider/provider",
    "final_abi_values_ok": "tests/run/native_providers/final_abi_values_ok/provider",
    "resource_stored_callback_ok": "tests/run/native_providers/resource_stored_callback_ok/provider",
}
PUB_FN = re.compile(r"^pub\s+fn\s+([A-Za-z_][A-Za-z0-9_]*)\b", re.MULTILINE)


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def closure(path: Path) -> list[dict[str, str | int]]:
    files = [path / "Cargo.toml", *sorted((path / "src").rglob("*.rs"))]
    return [
        {"path": item.relative_to(ROOT).as_posix(), "bytes": item.stat().st_size, "sha256": sha256(item)}
        for item in files
    ]


def compiler_closure() -> list[dict[str, str | int]]:
    files = [ROOT / "Cargo.toml", ROOT / "Cargo.lock"]
    files.extend((ROOT / "crates").rglob("Cargo.toml"))
    files.extend((ROOT / "crates").rglob("*.rs"))
    return [
        {"path": item.relative_to(ROOT).as_posix(), "bytes": item.stat().st_size, "sha256": sha256(item)}
        for item in sorted(files)
    ]


def expand(path: Path) -> bytes:
    manifest = path / "Cargo.toml"
    original = manifest.read_bytes()
    lock = path / "Cargo.lock"
    target = path / "target"
    if lock.exists() or target.exists():
        raise RuntimeError(f"generated expansion state was not clean: {path}")
    try:
        isolated = original.replace(b"edition.workspace = true", b"edition = \"2024\"")
        if b"[workspace]" not in isolated:
            isolated += b"\n[workspace]\n"
        manifest.write_bytes(isolated)
        completed = subprocess.run(
            ["cargo", "+nightly", "rustc", "--manifest-path", str(manifest), "--lib", "--", "-Zunpretty=expanded"],
            cwd=ROOT,
            capture_output=True,
        )
        if completed.returncode:
            raise RuntimeError(completed.stderr.decode(errors="replace"))
        return completed.stdout
    finally:
        manifest.write_bytes(original)
        lock.unlink(missing_ok=True)
        shutil.rmtree(target, ignore_errors=True)


def functions(text: str) -> list[dict[str, int | str]]:
    rows = []
    for occurrence, match in enumerate(PUB_FN.finditer(text), 1):
        name = match.group(1)
        family = (
            "anvyx_generated_helper" if name.startswith("__anvyx_")
            else "package_metadata_helper" if name in {"provider_descriptor", "provider_descriptors", "rust_module_support", "rust_module_supports"}
            else "other_public_function"
        )
        rows.append({
            "name": name,
            "line": text.count("\n", 0, match.start()) + 1,
            "occurrence": occurrence,
            "qualified": name,
            "family": family,
        })
    return rows


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--label", required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    output_dir = args.output_dir if args.output_dir.is_absolute() else ROOT / args.output_dir
    output_dir.mkdir(parents=True, exist_ok=True)
    measured = {}
    for name, relative in FIXTURES.items():
        fixture = ROOT / relative
        output = output_dir / f"expanded-{name.replace('_provider', '').replace('_ok', '').replace('_values', '')}-{args.label}.rs"
        # Preserve the historical artifact names used by the frozen baseline.
        output = output_dir / {
            "local_provider": f"expanded-local-{args.label}.rs",
            "final_abi_values_ok": f"expanded-final-abi-{args.label}.rs",
            "resource_stored_callback_ok": f"expanded-callback-{args.label}.rs",
        }[name]
        source = expand(fixture)
        output.write_bytes(source)
        text = source.decode()
        public = functions(text)
        families: dict[str, int] = {}
        for row in public:
            families[row["family"]] = families.get(row["family"], 0) + 1
        measured[name] = {
            "manifest": f"{relative}/Cargo.toml",
            "manifest_sha256": sha256(fixture / "Cargo.toml"),
            "input_closure": closure(fixture),
            "output": output.relative_to(ROOT).as_posix(),
            "output_sha256": sha256(output),
            "bytes": len(source),
            "nonblank_lines": sum(bool(line.strip()) for line in text.splitlines()),
            "public_function_count": len(public),
            "families": families,
            "public_functions": public,
        }
    artifact = {
        "schema": 2,
        "label": args.label,
        "generator": {"path": "scripts/measure_provider_expansion.py", "sha256": sha256(Path(__file__))},
        "compiler_input_closure": compiler_closure(),
        "fixtures": measured,
    }
    (output_dir / f"expansion-metrics-{args.label}.json").write_text(json.dumps(artifact, indent=2, sort_keys=True) + "\n")


if __name__ == "__main__":
    main()
