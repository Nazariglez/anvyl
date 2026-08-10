#!/usr/bin/env python3
"""Compare frozen provider-ABI performance and probe artifacts to current measurements."""
from __future__ import annotations

import hashlib
import json
from collections import Counter
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
ART = ROOT / ".pi/tmp/provider-abi-simplification"
CASES = (
    "contracts/basic_dispatch_ok",
    "dataref/ref_param_projection_list_index_ok",
    "value_materialization/recursive_aggregates_ok",
    "lambdas/escaping_loop_mut_capture_fresh_cells_ok",
    "native_providers/final_abi_values_ok",
    "native_providers/retained_callback_provider_reentrant_fire_ok",
    "string_fmt/backend_specs_ok",
)


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def files(paths: list[Path]) -> list[dict[str, str | int]]:
    return [
        {"path": path.relative_to(ROOT).as_posix(), "bytes": path.stat().st_size, "sha256": sha256(path)}
        for path in sorted(paths)
    ]


def compiler_closure() -> list[dict[str, str | int]]:
    paths = [ROOT / "Cargo.toml", ROOT / "Cargo.lock", ROOT / "scripts/measure_rust_backend.py"]
    paths.extend(path for path in (ROOT / "crates").rglob("*.rs"))
    paths.extend(path for path in (ROOT / "crates").rglob("Cargo.toml"))
    return files(paths)


def case_closure(case: dict) -> list[dict[str, str | int]]:
    root = Path(case["root"])
    paths = [
        path for path in root.rglob("*")
        if path.is_file() and path.suffix in {".anv", ".rs", ".toml"}
        and ".anvyx" not in path.parts and "target" not in path.parts and path.name != "Cargo.lock"
    ]
    return files(paths)


def percent(before: int | float, after: int | float) -> float:
    return 100 * (after - before) / before


def artifacts(case: dict, key: str) -> int:
    values = [sample["artifacts"][key]["bytes"] for sample in case["samples"]]
    if len(set(values)) != 1:
        raise RuntimeError(f"{case['name']} generated {key} changed between samples")
    return values[0]


def compare_mode(before: dict, after: dict) -> tuple[dict, list[str]]:
    failures = []
    for key in ("mode", "runs", "warmup", "profile", "cache_policy"):
        if before[key] != after[key]:
            failures.append(f"{after['mode']}: {key} differs")
    for key in ("sha256",):
        if before["script"][key] != after["script"][key]:
            failures.append(f"{after['mode']}: benchmark script hash differs")
    for key in ("host", "rustc", "python"):
        if before["environment"][key] != after["environment"][key]:
            failures.append(f"{after['mode']}: {key} differs")
    old, new = {case["name"]: case for case in before["cases"]}, {case["name"]: case for case in after["cases"]}
    if tuple(old) != CASES or tuple(new) != CASES:
        failures.append(f"{after['mode']}: corpus differs")
    rows = {}
    for name in CASES:
        previous, current = old[name], new[name]
        if len(previous["samples"]) != 20 or len(current["samples"]) != 20:
            failures.append(f"{after['mode']}: {name} wrong sample count")
        metrics = {
            "source_to_completed_run.median_ns": (previous["summary"]["source_to_completed_run"]["median_ns"], current["summary"]["source_to_completed_run"]["median_ns"]),
            "source_to_completed_run.p95_ns": (previous["summary"]["source_to_completed_run"]["p95_ns"], current["summary"]["source_to_completed_run"]["p95_ns"]),
            "cached_binary.median_ns": (previous["summary"]["cached_binary"]["median_ns"], current["summary"]["cached_binary"]["median_ns"]),
            "cached_binary.p95_ns": (previous["summary"]["cached_binary"]["p95_ns"], current["summary"]["cached_binary"]["p95_ns"]),
            "generated_source_bytes": (artifacts(previous, "source"), artifacts(current, "source")),
            "generated_binary_bytes": (artifacts(previous, "binary"), artifacts(current, "binary")),
        }
        observable = {}
        for kind in ("invocation", "cached_binary"):
            keys = ("exit_status", "stdout_sha256", "stderr_sha256")
            observable[kind] = {key: all(a[kind][key] == b[kind][key] for a, b in zip(previous["samples"], current["samples"])) for key in keys}
            if not all(observable[kind].values()):
                failures.append(f"{after['mode']}: {name} observable {kind} changed")
        rows[name] = {
            "metrics": {key: {"before": old, "after": new, "percent_change": percent(old, new)} for key, (old, new) in metrics.items()},
            "observable_hashes_match": observable,
            "current_input_closure": case_closure(current),
        }
    return rows, failures


def probe() -> dict:
    trace = ART / "provider-probe-trace-after.tsv"
    baseline = json.loads((ART / "provider-probe-baseline.json").read_text())
    lines = [line.split("\t") for line in trace.read_text().splitlines() if line]
    if any(len(line) != 2 for line in lines):
        raise RuntimeError("invalid provider probe trace")
    paths = {Path(path).resolve() for path, _ in lines}
    if any(not path.is_file() or not path.is_relative_to(ROOT) for path in paths):
        raise RuntimeError("provider probe trace references a missing or external input")
    counts = Counter(path for path, _ in lines)
    profiles = sorted(set(profile for _, profile in lines))
    closure = files([*paths, ART / "fixture-manifest-after.json"])
    return {
        "schema": 3,
        "command": "ANVYX_PROVIDER_PROBE_TRACE=<path> cargo run --package test-runner -- <exact baseline active fixture paths> --quiet",
        "trace_sha256": sha256(trace),
        "trace_input_closure": closure,
        "total_invocations": len(lines),
        "per_package_invocations": dict(sorted(counts.items())),
        "semantic_profile": profiles[0] if len(profiles) == 1 else profiles,
        "baseline_counts_match": counts == Counter(baseline["per_package_invocations"]),
        "baseline_total_match": len(lines) == baseline["total_invocations"],
        "baseline_profile": baseline["semantic_profile"],
        "input_fixture_manifest_sha256": sha256(ART / "fixture-manifest-before.json"),
    }


def main() -> None:
    modes, failures = {}, []
    for mode in ("cold", "warm"):
        before = json.loads((ART / f"perf-{mode}-before.json").read_text())
        after = json.loads((ART / f"perf-{mode}-after.json").read_text())
        rows, mode_failures = compare_mode(before, after)
        modes[mode], failures = rows, failures + mode_failures
    regressions = [
        {"mode": mode, "case": case, "metric": metric, **values}
        for mode, cases in modes.items()
        for case, row in cases.items()
        for metric, values in row["metrics"].items()
        if values["percent_change"] > 5
    ]
    compiler_inputs = compiler_closure()
    report = {
        "schema": 4,
        "generator": {"path": "scripts/compare_provider_abi_artifacts.py", "sha256": sha256(Path(__file__))},
        "inputs": {
            mode: {
                "same_script_sha256": not any(failure.startswith(f"{mode}: benchmark script") for failure in failures),
                "same_host": not any(failure.startswith(f"{mode}: host") for failure in failures),
                "same_rustc": not any(failure.startswith(f"{mode}: rustc") for failure in failures),
                "same_python_full_version": not any(failure.startswith(f"{mode}: python") for failure in failures),
                "same_cache_policy": not any(failure.startswith(f"{mode}: cache_policy") for failure in failures),
                "same_corpus": not any(failure.startswith(f"{mode}: corpus") for failure in failures),
                "warmup_runs": [3, 20],
            }
            for mode in modes
        },
        "compiler_input_closure": compiler_inputs,
        "performance_input_closure_recorded": bool(compiler_inputs),
        "modes": modes,
        "regressions_over_5_percent": regressions,
        "validation_failures": failures,
        "probe": probe(),
    }
    (ART / "perf-comparison.json").write_text(json.dumps(report, indent=2, sort_keys=True) + "\n")
    if failures:
        raise RuntimeError("; ".join(failures))


if __name__ == "__main__":
    main()
