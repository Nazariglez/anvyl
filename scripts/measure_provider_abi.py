#!/usr/bin/env python3
"""Reproducibly measure provider-ABI production and public surface."""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import subprocess
import sys
from collections import defaultdict
from datetime import datetime, timezone
from pathlib import Path, PurePosixPath
from typing import Any

AFFECTED = ("macros", "externs", "runtime", "project", "frontend", "backend", "core", "stdlib", "lang")
RAW_DTOS = ("RustProviderSupport", "RustProviderCargo", "RustModuleSupport", "RustTypeBinding", "RustMaterializerBinding", "RustExternBinding", "RustPath", "RustExternAbi", "RustAbiSupport", "RustWrapperCtx", "RustParamAbi", "RustReturnAbi")
RAW_OWNER_PREFIXES = ("crates/runtime/src/provider.rs", "crates/externs/src/")
RAW_CONSTRUCTOR_OWNERS = {
    "RustProviderSupport": ("crates/core/src/lib.rs", "crates/project/src/manifest.rs", "crates/stdlib/src/lib.rs"),
    "RustProviderCargo": ("crates/core/src/lib.rs", "crates/project/src/manifest.rs", "crates/stdlib/src/lib.rs"),
    "RustModuleSupport": ("crates/macros/src/module_macro.rs",),
    "RustTypeBinding": ("crates/runtime/src/provider.rs", "crates/externs/src/catalog.rs"),
    "RustMaterializerBinding": ("crates/runtime/src/provider.rs", "crates/externs/src/catalog.rs"),
    "RustExternBinding": ("crates/runtime/src/provider.rs", "crates/externs/src/catalog.rs"),
    "RustPath": ("crates/runtime/src/provider.rs", "crates/externs/src/catalog.rs"),
    "RustExternAbi": ("crates/macros/src/function_macro.rs", "crates/macros/src/methods_macro.rs"),
}
BASELINE_REPRESENTATIONS = {"ExternTypeExpr::": (262, 11), "RustParamAdapter::": (0, 0), "RustReturnAdapter::": (0, 0)}
SEMANTIC_GATES = (
    ("proc-macro wrapper construction", "crates/macros/src/boundary.rs", r"pub fn validate_callable_signature\s*\("),
    ("runtime materializer attestation", "crates/runtime/src/provider.rs", r"fn validate_inline_materialization\s*\("),
    ("catalog package validation", "crates/externs/src/catalog.rs", r"fn validate_package\s*\("),
    ("frontend provider resolution", "crates/frontend/src/externs/catalog.rs", r"pub\(crate\) fn build_catalog\s*\("),
    ("AIR verifier boundary", "crates/frontend/src/air/verify.rs", r"pub fn verify\s*\(program: &Program\)"),
    ("backend AIR/catalog attestation", "crates/backend/src/rust/native.rs", r"pub\(super\) fn attests_air\s*\("),
    ("RIR verifier boundary", "crates/backend/src/rust/rir.rs", r"pub\(super\) fn verify\s*\(program: &RirProgram\)"),
    ("runtime callback invocation gate", "crates/runtime/src/callback_registry.rs", r"pub unsafe fn begin_invocation\s*\("),
    ("runtime callback reentry gate", "crates/runtime/src/safepoint.rs", r"pub fn validate_reentry\s*\("),
    ("runtime resource access gate", "crates/runtime/src/resource.rs", r"pub fn with_erased<R>"),
)
BACKEND_LOOKUP_SCANS = (
    ("provider support lookup", "crates/backend/src/rust/native.rs", r"providers\s*\.\s*iter\s*\(\s*\)\s*\.\s*find"),
    ("module binding lookup", "crates/backend/src/rust/native.rs", r"support\s*\.\s*modules\s*\.\s*iter\s*\(\s*\)\s*\.\s*flat_map\s*\(\s*\|module\|\s*&module\.bindings\s*\)"),
    ("materializer support scan", "crates/backend/src/rust/rep_policy.rs", r"for support in supports\s*\{"),
    ("materializer module scan", "crates/backend/src/rust/rep_policy.rs", r"for module in &support\.modules\s*\{"),
    ("materializer type scan", "crates/backend/src/rust/rep_policy.rs", r"for ty in &module\.types\s*\{"),
    ("native type index lookup", "crates/backend/src/rust/rep_policy.rs", r"self\.native_types\.get\s*\("),
)
GENERATED_HELPERS = ("provider_descriptor", "provider_descriptors", "rust_module_support", "rust_module_supports", "__anvyx_module_support")

class MetricError(RuntimeError): pass

class Tree:
    def __init__(self, root: Path, requested: str):
        self.root, self.requested = root, requested
        self.worktree = requested == "worktree"
        self.resolved = "worktree" if self.worktree else self.git("rev-parse", "--verify", requested + "^{commit}")
    def git(self, *args: str) -> str:
        return subprocess.check_output(["git", *args], cwd=self.root, text=True).strip()
    def paths(self) -> list[PurePosixPath]:
        if self.worktree:
            tracked = self.git("ls-files").splitlines()
            candidates = self.git("ls-files", "--others", "--exclude-standard").splitlines()
            untracked = [
                name for name in candidates
                if Path(name).suffix in {".rs", ".py", ".toml", ".anv"}
                and "target" not in PurePosixPath(name).parts
                and ".anvyx" not in PurePosixPath(name).parts
            ]
            names = set(tracked) | set(untracked)
        else:
            names = self.git("ls-tree", "-r", "--name-only", self.resolved).splitlines()
        return sorted(PurePosixPath(name) for name in names)
    def read(self, path: PurePosixPath) -> str:
        if self.worktree: return (self.root / path).read_text(encoding="utf-8")
        return subprocess.check_output(["git", "show", f"{self.resolved}:{path}"], cwd=self.root, text=True)

def lex(source: str) -> list[tuple[str, int]]:
    """Rust-lite lexer for delimiters and identifiers; comments/literals are opaque."""
    out, i, size = [], 0, len(source)
    while i < size:
        c = source[i]
        if c.isspace(): i += 1; continue
        if source.startswith("//", i):
            j = source.find("\n", i + 2); i = size if j < 0 else j + 1; continue
        if source.startswith("/*", i):
            depth, i = 1, i + 2
            while i < size and depth:
                if source.startswith("/*", i): depth += 1; i += 2
                elif source.startswith("*/", i): depth -= 1; i += 2
                else: i += 1
            if depth: raise MetricError("unterminated block comment")
            continue
        if c == 'r' and i + 1 < size and source[i + 1] in '"#':
            q = i + 1
            while q < size and source[q] == '#': q += 1
            if q < size and source[q] == '"':
                end = source.find('"' + '#' * (q - i - 1), q + 1)
                if end < 0: raise MetricError("unterminated raw string")
                i = end + 1 + q - i - 1; continue
        if c == '"':
            i += 1
            while i < size:
                if source[i] == '\\': i += 2
                elif source[i] == '"': i += 1; break
                else: i += 1
            else: raise MetricError("unterminated string")
            continue
        if c == "'":
            # Character literals must win over the lifetime spelling in b'x'.
            if i + 2 < size and source[i + 2] == "'": i += 3; continue
            if i + 3 < size and source[i + 1] == '\\' and source[i + 3] == "'": i += 4; continue
            if i + 1 < size and (source[i + 1].isalpha() or source[i + 1] == '_'):
                j = i + 2
                while j < size and (source[j].isalnum() or source[j] == '_'): j += 1
                out.append((source[i:j], i)); i = j; continue
            # A bare apostrophe can be a lifetime punctuation in incomplete syntax.
            out.append(("'", i)); i += 1; continue
        if c.isalpha() or c == '_':
            j = i + 1
            while j < size and (source[j].isalnum() or source[j] == '_'): j += 1
            out.append((source[i:j], i)); i = j; continue
        if source.startswith("::", i) or source.startswith("->", i): out.append((source[i:i+2], i)); i += 2; continue
        out.append((c, i)); i += 1
    return out

def line(source: str, pos: int) -> int: return source.count("\n", 0, pos) + 1

def attached_start(source: str, start: int) -> int:
    result = source.rfind("\n", 0, start) + 1
    while result:
        end = result - 1; previous = source.rfind("\n", 0, end) + 1
        text = source[previous:end].strip()
        if not text or text.startswith("///") or text.startswith("//!") or text.startswith("#"):
            result = previous
        else: break
    return result

def cfg_end(source: str, after_attr: int) -> int:
    tokens = lex(source[after_attr:])
    if not tokens:
        raise MetricError("missing cfg(test) item")
    allowed = {"fn", "mod", "impl", "struct", "enum", "trait", "union", "const", "static", "type", "use", "extern", "macro_rules"}
    i = 0
    while i < len(tokens) and tokens[i][0] == "#":
        if i + 1 >= len(tokens) or tokens[i + 1][0] != "[":
            raise MetricError("malformed attribute after #[cfg(test)]")
        i += 2
        depth = 1
        while i < len(tokens) and depth:
            depth += (tokens[i][0] == "[") - (tokens[i][0] == "]")
            i += 1
        if depth:
            raise MetricError("unclosed attribute after #[cfg(test)]")
    if i < len(tokens) and tokens[i][0] == "pub":
        i += 1
        if i < len(tokens) and tokens[i][0] == "(":
            depth = 1
            i += 1
            while i < len(tokens) and depth:
                depth += (tokens[i][0] == "(") - (tokens[i][0] == ")")
                i += 1
            if depth:
                raise MetricError("unclosed visibility in #[cfg(test)] item")
    while i < len(tokens) and tokens[i][0] in {"unsafe", "async", "default", "auto"}:
        i += 1
    if i >= len(tokens) or tokens[i][0] not in allowed:
        raise MetricError("unsupported #[cfg(test)] item head")
    head = tokens[i][0]
    i += 1
    if head in {"fn", "mod", "struct", "enum", "trait", "union", "const", "static", "type"}:
        if i >= len(tokens) or not re.fullmatch(r"[A-Za-z_]\w*", tokens[i][0]):
            raise MetricError(f"malformed #[cfg(test)] {head} item")
    stack: list[str] = []
    pairs = {"{": "}", "[": "]", "(": ")"}
    semicolon_item = head in {"const", "static", "type", "use"}
    for token, pos in tokens[i:]:
        if token in pairs:
            stack.append(pairs[token])
        elif token in {"}", "]", ")"}:
            if not stack or stack.pop() != token:
                raise MetricError("unbalanced delimiter in #[cfg(test)] item")
            if not stack and token == "}" and not semicolon_item:
                end = after_attr + pos + 1
                trailing = re.match(r"[ \t\r\n]*;", source[end:])
                return end + trailing.end() if trailing else end
        elif token == ";" and not stack:
            return after_attr + pos + 1
    raise MetricError("unclosed #[cfg(test)] item")


def validate_cfg_syntax(items: list[str]) -> None:
    if not items:
        return
    check = subprocess.run(
        ["rustc", "+nightly", "-Z", "parse-crate-root-only", "--crate-type", "lib", "--emit", "metadata", "-o", os.devnull, "-"],
        input="\n".join(items),
        text=True,
        capture_output=True,
    )
    if check.returncode:
        detail = check.stderr.splitlines()[0] if check.stderr else "unknown parser error"
        raise MetricError(f"malformed #[cfg(test)] item: {detail}")


def production(source: str) -> tuple[str, int]:
    pattern = re.compile(r"(?m)^[ \t]*#\s*\[\s*cfg\s*\(\s*test\s*\)\s*\]")
    pieces, cursor, removed, cfg_items = [], 0, 0, []
    for match in pattern.finditer(source):
        if match.start() < cursor: continue
        start, end = attached_start(source, match.start()), cfg_end(source, match.end())
        cfg_items.append(source[start:end])
        pieces += [source[cursor:start], "\n" * source[start:end].count("\n")]
        removed += sum(bool(row.strip()) for row in source[start:end].splitlines())
        cursor = end
    pieces.append(source[cursor:])
    validate_cfg_syntax(cfg_items)
    return "".join(pieces), removed

def top_level_public(source: str) -> list[dict[str, Any]]:
    tokens, result, braces = lex(source), [], 0
    kinds = {"struct", "enum", "trait", "type", "fn", "const", "static"}
    i = 0
    while i < len(tokens):
        text, pos = tokens[i]
        if text == "{": braces += 1
        elif text == "}": braces -= 1
        elif braces == 0 and text == "pub" and i + 1 < len(tokens) and tokens[i + 1][0] != "(":
            j = i + 1
            if tokens[j][0] == "unsafe": j += 1
            if j < len(tokens) and tokens[j][0] in kinds and j + 1 < len(tokens):
                result.append({"kind": tokens[j][0], "name": tokens[j + 1][0], "line": line(source, pos)})
        i += 1
    return result

def pub_use_groups(source: str) -> list[tuple[str, list[tuple[str, str]]]]:
    groups = []
    pattern = re.compile(r"pub\s+use\s+([A-Za-z_]\w*(?:::[A-Za-z_]\w*)*)\s*::\s*\{(.*?)\}\s*;", re.S)
    for match in pattern.finditer(source):
        names = []
        for item in match.group(2).split(","):
            words = re.findall(r"[A-Za-z_]\w*", item)
            if not words:
                continue
            source_name = words[0]
            if source_name == "self":
                continue
            exported_name = words[-1] if "as" in words else source_name
            names.append((source_name, exported_name))
        groups.append((match.group(1), names))
    return groups


def public_surface(sources: dict[PurePosixPath, str]) -> dict[str, Any]:
    definitions: dict[str, list[dict[str, Any]]] = {}
    by_name: dict[str, list[dict[str, Any]]] = defaultdict(list)
    externs_source = sources[PurePosixPath("crates/externs/src/lib.rs")]
    modules = re.findall(r"(?m)^\s*pub\s+use\s+([A-Za-z_]\w*)\s*::\s*\*\s*;", externs_source)
    if not modules: raise MetricError("externs facade has no glob-effective owner modules")
    owner_paths = [f"crates/externs/src/{module}.rs" for module in modules] + ["crates/runtime/src/provider.rs"]
    for raw in owner_paths:
        path = PurePosixPath(raw)
        if path not in sources: raise MetricError(f"effective ABI owner `{path}` is missing")
        rows = top_level_public(sources[path]); definitions[raw] = rows
        for row in rows: by_name[row["name"]].append({"path": raw, **row})
    externs = []
    for module in modules:
        path = f"crates/externs/src/{module}.rs"
        externs.extend({"path": "crates/externs/src/lib.rs", "kind": "glob", "module": module, "name": row["name"], "owner": path} for row in definitions[path])
    externs_api: dict[str, list[dict[str, Any]]] = {}
    for module in modules:
        path = f"crates/externs/src/{module}.rs"
        for row in definitions[path]:
            externs_api[row["name"]] = [{"path": path, **row}]
    provider_path = "crates/runtime/src/provider.rs"
    provider_api: dict[str, list[dict[str, Any]]] = {
        row["name"]: [{"path": provider_path, **row}]
        for row in definitions[provider_path]
    }
    for module, names in pub_use_groups(sources[PurePosixPath(provider_path)]):
        source_api = externs_api if module == "anvyx_externs" else {}
        for source_name, exported_name in names:
            owners = source_api.get(source_name, [])
            if not owners:
                raise MetricError(f"provider facade reexports unknown `{module}::{source_name}`")
            provider_api[exported_name] = owners
    runtime_source = sources[PurePosixPath("crates/runtime/src/lib.rs")]
    runtime_groups = [group for group in pub_use_groups(runtime_source) if group[0] in {"provider", "anvyx_externs", "self"}]
    runtime_api: dict[str, list[dict[str, Any]]] = {}
    pending = [(module, source_name, exported_name) for module, names in runtime_groups for source_name, exported_name in names]
    resolved: dict[tuple[str, str, str], list[dict[str, Any]]] = {}
    while pending:
        next_pending = []
        progressed = False
        for module, source_name, exported_name in pending:
            source_api = {"provider": provider_api, "anvyx_externs": externs_api, "self": runtime_api}[module]
            owners = source_api.get(source_name)
            if owners is None:
                next_pending.append((module, source_name, exported_name))
                continue
            runtime_api[exported_name] = owners
            resolved[(module, source_name, exported_name)] = owners
            progressed = True
        if not progressed:
            unresolved = ", ".join(f"{module}::{source}" for module, source, _ in next_pending)
            raise MetricError(f"runtime facade has unresolved reexports: {unresolved}")
        pending = next_pending
    runtime = [
        {
            "path": "crates/runtime/src/lib.rs", "kind": "explicit",
            "source_module": module, "source_name": source_name, "exported_name": exported_name,
            "owners": resolved[(module, source_name, exported_name)],
        }
        for module, names in runtime_groups
        for source_name, exported_name in names
    ]
    runtime_owned_names = {
        owner["name"]
        for row in runtime
        for owner in row["owners"]
        if owner["path"] == provider_path
    }
    macro_source = sources[PurePosixPath("crates/macros/src/lib.rs")]
    macro_exports = []
    macro_pattern = re.compile(
        r"(?ms)^\s*#\[(proc_macro(?:_attribute|_derive)?)(?:\(([^\]]*)\))?\]\s*"
        r"pub\s+fn\s+([A-Za-z_]\w*)\s*\("
    )
    for match in macro_pattern.finditer(macro_source):
        attribute, args, implementation = match.groups()
        exported = implementation
        if attribute == "proc_macro_derive":
            derive = re.match(r"\s*([A-Za-z_]\w*)", args or "")
            if not derive:
                raise MetricError("malformed proc_macro_derive export")
            exported = derive.group(1)
        macro_exports.append({
            "path": "crates/macros/src/lib.rs",
            "attribute": attribute,
            "exported_name": exported,
            "implementation": implementation,
            "line": line(macro_source, match.start()),
        })
    abi_defs = [row for path, rows in definitions.items() for row in rows if path != "crates/runtime/src/provider.rs" and row["kind"] in {"struct", "enum", "trait"}]
    abi_defs += [row for row in definitions["crates/runtime/src/provider.rs"] if row["kind"] in {"struct", "enum", "trait"} and row["name"] in runtime_owned_names]
    raw_defs = [
        {"path": path, **row}
        for path, rows in definitions.items()
        for row in rows
        if row["name"] in RAW_DTOS
    ]
    for path, source in sources.items():
        for row in top_level_public(source):
            if row["name"] in RAW_DTOS and not any(str(path) == prefix or str(path).startswith(prefix) for prefix in RAW_OWNER_PREFIXES):
                raise MetricError(f"raw DTO `{row['name']}` moved to unapproved owner `{path}`")
    return {"owner_definitions": definitions, "effective_reexports": {"externs_glob": externs, "runtime_explicit": runtime}, "proc_macro_exports": macro_exports, "public_abi_type_definitions": len(abi_defs), "raw_host_support_types": raw_defs}

def constructors(sources: dict[PurePosixPath, str]) -> dict[str, list[dict[str, Any]]]:
    output: dict[str, list[dict[str, Any]]] = {}
    for name, allowed in RAW_CONSTRUCTOR_OWNERS.items():
        hits = []
        for path, source in sources.items():
            ts = lex(source)
            for i in range(len(ts) - 1):
                if ts[i][0] != name or ts[i + 1][0] != "{": continue
                before = ts[i - 1][0] if i else ""
                if before in {"struct", "enum", "trait", "union", "impl", "for"}:
                    continue
                raw = str(path)
                if raw not in allowed: raise MetricError(f"raw constructor `{name}` escaped owner at {raw}:{line(source, ts[i][1])}")
                hits.append({"path": raw, "line": line(source, ts[i][1])})
        output[name] = hits
    return output

def full_hits(sources: dict[PurePosixPath, str], token: str, roots: tuple[str, ...] | None = None) -> dict[str, Any]:
    rows = []
    for path, source in sources.items():
        if roots and not any(str(path).startswith(root) for root in roots): continue
        starts = [m.start() for m in re.finditer(re.escape(token), source)]
        if starts: rows.append({"path": str(path), "occurrences": len(starts), "lines": [line(source, start) for start in starts]})
    return {"token": token, "occurrences": sum(row["occurrences"] for row in rows), "files": len(rows), "per_file": rows}

def pattern_hits(sources: dict[PurePosixPath, str], label: str, path: str, pattern: str) -> dict[str, Any]:
    source = sources.get(PurePosixPath(path))
    if source is None:
        return {"label": label, "path": path, "pattern": pattern, "occurrences": 0, "lines": []}
    matches = list(re.finditer(pattern, source, re.S))
    return {"label": label, "path": path, "pattern": pattern, "occurrences": len(matches), "lines": [line(source, match.start()) for match in matches]}


def structural(sources: dict[PurePosixPath, str]) -> dict[str, Any]:
    representations = [full_hits(sources, token) for token in BASELINE_REPRESENTATIONS]
    fan_out = [full_hits(sources, token + "::") for token in ("RustParamAdapter", "RustReturnAdapter")]
    helpers = [full_hits(sources, token, ("crates/macros/src/", "crates/runtime/src/", "crates/project/src/", "crates/core/src/", "crates/stdlib/src/")) for token in GENERATED_HELPERS]
    gates = [pattern_hits(sources, *rule) for rule in SEMANTIC_GATES]
    scans = [pattern_hits(sources, *rule) for rule in BACKEND_LOOKUP_SCANS]
    return {
        "representation_families": representations,
        "adapter_fan_out": fan_out,
        "validation_boundaries": gates,
        "generated_helper_families": helpers,
        "backend_raw_lookup_scans": scans,
    }


def classify(path: PurePosixPath) -> str:
    parts = path.parts
    if path.name in {"tests.rs", "test_support.rs"} or "tests" in parts: return "fixtures_tests"
    if path == PurePosixPath("crates/backend/src/vm") or PurePosixPath("crates/backend/src/vm") in path.parents: return "orphan_vm"
    if path.name == "build.rs": return "build_script"
    if "examples" in parts or parts[:1] == ("examples",): return "examples"
    if "tests" in parts or parts[:1] == ("tests",): return "fixtures_tests"
    if parts[:1] == ("scripts",): return "measurement_scripts" if path.name.startswith(("measure_provider_abi", "measure_provider_expansion", "assert_provider_abi")) else "scripts"
    if len(parts) >= 4 and parts[0] == "crates" and parts[2] == "src" and path.suffix == ".rs": return "production"
    return "other"

def measure(tree: Tree, kind: str) -> dict[str, Any]:
    excluded: dict[str, dict[str, Any]] = defaultdict(lambda: {"files": 0, "nonblank_lines": 0, "paths": []})
    sources, files, cfg_removed = {}, [], 0
    for path in tree.paths():
        category = classify(path)
        try: raw = tree.read(path)
        except (UnicodeDecodeError, FileNotFoundError): continue
        lines = sum(bool(row.strip()) for row in raw.splitlines())
        if category != "production":
            excluded[category]["files"] += 1
            excluded[category]["nonblank_lines"] += lines
            excluded[category]["paths"].append({"path": str(path), "nonblank_lines": lines})
            continue
        clean, removed = production(raw); cfg_removed += removed; sources[path] = clean
        files.append({"path": str(path), "crate": path.parts[1], "nonblank_production_loc": sum(bool(row.strip()) for row in clean.splitlines())})
    crates: dict[str, dict[str, int]] = defaultdict(lambda: {"files": 0, "nonblank_production_loc": 0})
    for row in files:
        crates[row["crate"]]["files"] += 1; crates[row["crate"]]["nonblank_production_loc"] += row["nonblank_production_loc"]
    data: dict[str, Any] = {"schema": 4, "artifact_role": kind, "generated_at": datetime.now(timezone.utc).isoformat(), "tree": {"requested": tree.requested, "resolved_commit": tree.resolved}, "measurement_driver": {"path": "scripts/measure_provider_abi.py", "sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest()}, "measurement_scripts": script_hashes(tree.root), "affected_crates": list(AFFECTED), "files": sorted(files, key=lambda x:x["path"]), "crates": {name: crates[name] for name in AFFECTED}, "workspace_crates": dict(sorted(crates.items())), "affected_total": sum(crates[x]["nonblank_production_loc"] for x in AFFECTED), "workspace_total": sum(x["nonblank_production_loc"] for x in crates.values()), "excluded_accounting": {"categories": dict(sorted(excluded.items())), "cfg_test_nonblank_lines_removed": cfg_removed}, "forbidden_excluded_imports": forbidden_imports(sources)}
    if kind == "loc": data["structural_metrics"] = structural(sources)
    else:
        data["public_surface"] = {**public_surface(sources), "raw_public_field_constructor_sites": constructors(sources)}
    return data

def script_hashes(root: Path) -> dict[str, str]:
    return {path: hashlib.sha256((root / path).read_bytes()).hexdigest() for path in ("scripts/measure_provider_abi.py", "scripts/measure_provider_expansion.py", "scripts/assert_provider_abi_artifacts.py") if (root / path).is_file()}

def call_end(source: str, opening: int) -> int:
    depth, i = 1, opening + 1
    while i < len(source) and depth:
        if source.startswith("//", i):
            end = source.find("\n", i + 2); i = len(source) if end < 0 else end + 1
        elif source.startswith("/*", i):
            end = source.find("*/", i + 2)
            if end < 0: raise MetricError("unterminated include comment")
            i = end + 2
        elif source[i] == '"':
            i += 1
            while i < len(source):
                if source[i] == "\\": i += 2
                elif source[i] == '"': i += 1; break
                else: i += 1
        elif source[i] == "(": depth, i = depth + 1, i + 1
        elif source[i] == ")": depth, i = depth - 1, i + 1
        else: i += 1
    if depth: raise MetricError("unclosed include macro")
    return i


def forbidden_imports(sources: dict[PurePosixPath, str]) -> list[dict[str, Any]]:
    patterns = (
        r"#\s*\[\s*path\s*=\s*[^\]]*(?:(?:tests|examples|scripts)/|tests\.rs\b|test_support\.rs\b)",
        r"\bmod\s+tests\s*;",
        r"\buse\s+(?:crate|super|self)\s*::\s*(?:tests|examples|scripts)\b",
        r"\buse\s+(?:crate|super|self)\s*::\s*\{[^}]*\b(?:tests|examples|scripts)\b",
        r"\buse\s*\{[^}]*\b(?:crate|super|self)\s*::\s*(?:tests|examples|scripts)\b",
    )
    bad = []
    for path, source in sources.items():
        for match in re.finditer(r"include(?:_str|_bytes)?!\s*\(", source):
            opening = source.find("(", match.start(), match.end())
            end = call_end(source, opening)
            body = source[opening + 1:end - 1]
            joined = "".join(re.findall(r'"([^"\\]*(?:\\.[^"\\]*)*)"', body))
            if any(lane in joined for lane in ("tests", "examples", "scripts", "test_support.rs")):
                bad.append({"path": str(path), "line": line(source, match.start()), "pattern": "include path", "text": source[match.start():end]})
        for pattern in patterns:
            for match in re.finditer(pattern, source, re.S):
                bad.append({"path": str(path), "line": line(source, match.start()), "pattern": pattern, "text": match.group(0)})
    return bad


def self_test() -> None:
    good = {
        "array": "#[cfg(test)] const X: [u8; 1] = [0];\nfn keep() {}",
        "nested-generics": "#[cfg(test)] static X: Option<Vec<[u8; 1]>> = None;\nfn keep() {}",
        "braced:semicolon": "#[cfg(test)] const X: Option<u8> = { Some(1) };\nfn keep() {}",
        "docs-attrs": "/// doc\n#[allow(dead_code)]\n#[cfg(test)] fn x() {}\nfn keep() {}",
        "comparison": "#[cfg(test)] const X: bool = 1 < 2;\nfn keep() {}",
    }
    for name, source in good.items():
        if production(source)[0].strip() != "fn keep() {}": raise MetricError(f"self-test failed: {name}")
    for source in (
        "#[cfg(test)] garbage;",
        "#[cfg(test)] fn () {}",
        "#[cfg(test)] struct {}",
        "#[cfg(test)] type = u8;",
        "#[cfg(test)] type X = Vec<u8;",
        "#[cfg(test)] fn x<<>>() {}",
        "#[cfg(test)] fn x<T() {}",
        "#[cfg(test)] fn x<T: Fn(Vec<u8)>() {}",
        "#[cfg(test)] struct X<T {}",
        "#[cfg(test)] impl<T X {}",
        "#[cfg(test)] impl<T> {}",
        "#[cfg(test)] fn x() {",
        "#[cfg(test)] const X: [u8; 1] = [0;",
        "#[cfg(test)] /*",
    ):
        try: production(source)
        except MetricError: continue
        raise MetricError("self-test accepted malformed cfg(test) item")
    excluded = {
        PurePosixPath("crates/x/src/lib.rs"): (
            'include!(concat!("tests", "/fixture.rs"));\n'
            'include!(concat!("te", "sts/fixture.rs"));\n'
            'use crate::{tests, okay};\n'
            'use {crate::tests, crate::okay};\n'
            '#[path = "tests.rs"] mod hidden;\n'
            '#[path = "test_support.rs"] mod support;\n'
            'include!("test_support.rs");'
        )
    }
    if len(forbidden_imports(excluded)) != 7:
        raise MetricError("self-test missed forbidden excluded-lane imports")

def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--tree"); parser.add_argument("--output", type=Path); parser.add_argument("--kind", choices=("loc", "surface")); parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args()
    try:
        if args.self_test: self_test(); print("measure_provider_abi: self-test passed"); return 0
        if not args.tree or not args.output or not args.kind: parser.error("--tree, --output, and --kind are required")
        root = Path(__file__).resolve().parents[1]; data = measure(Tree(root, args.tree), args.kind)
        tmp = args.output.with_suffix(args.output.suffix + ".tmp"); args.output.parent.mkdir(parents=True, exist_ok=True); tmp.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n"); os.replace(tmp, args.output)
        print(json.dumps({"kind":args.kind, "affected_total":data["affected_total"], "workspace_total":data["workspace_total"], "output":str(args.output)}, sort_keys=True))
    except (MetricError, OSError, subprocess.CalledProcessError, UnicodeDecodeError) as error:
        print(f"measure_provider_abi: {error}", file=sys.stderr); return 1
    return 0
if __name__ == "__main__": raise SystemExit(main())
