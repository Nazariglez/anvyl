#!/usr/bin/env python3

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import platform
import statistics
import subprocess
import sys
import time
import tomllib
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


@dataclass(frozen=True)
class CorpusCase:
    name: str
    source: str
    root: str
    provider_probe: bool = False
    cli_options: tuple[str, ...] = ()


CORPUS = (
    CorpusCase(
        "contracts/basic_dispatch_ok",
        "tests/run/contracts/basic_dispatch_ok.anv",
        "tests/run/contracts",
    ),
    CorpusCase(
        "dataref/ref_param_projection_list_index_ok",
        "tests/run/dataref/ref_param_projection_list_index_ok.anv",
        "tests/run/dataref",
    ),
    CorpusCase(
        "value_materialization/recursive_aggregates_ok",
        "tests/run/value_materialization/recursive_aggregates_ok.anv",
        "tests/run/value_materialization",
    ),
    CorpusCase(
        "lambdas/escaping_loop_mut_capture_fresh_cells_ok",
        "tests/run/lambdas/escaping_loop_mut_capture_fresh_cells_ok.anv",
        "tests/run/lambdas",
    ),
    CorpusCase(
        "native_providers/final_abi_values_ok",
        "tests/run/native_providers/final_abi_values_ok/src/main.anv",
        "tests/run/native_providers/final_abi_values_ok",
        provider_probe=True,
        cli_options=("--lint", "unused=allow"),
    ),
    CorpusCase(
        "native_providers/retained_callback_provider_reentrant_fire_ok",
        "tests/run/native_providers/retained_callback_provider_reentrant_fire_ok/src/main.anv",
        "tests/run/native_providers/retained_callback_provider_reentrant_fire_ok",
        provider_probe=True,
    ),
    CorpusCase(
        "string_fmt/backend_specs_ok",
        "tests/run/string_fmt/backend_specs_ok.anv",
        "tests/run/string_fmt",
    ),
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--runs", type=positive_int, required=True)
    parser.add_argument("--warmup", type=nonnegative_int, required=True)
    parser.add_argument("--mode", choices=("cold", "warm"), required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    if args.mode == "warm" and args.warmup == 0:
        parser.error("warm mode requires at least one warmup")
    return args


def positive_int(raw: str) -> int:
    value = int(raw)
    if value < 1:
        raise argparse.ArgumentTypeError("must be at least 1")
    return value


def nonnegative_int(raw: str) -> int:
    value = int(raw)
    if value < 0:
        raise argparse.ArgumentTypeError("must be nonnegative")
    return value


def sha256_bytes(content: bytes) -> str:
    return hashlib.sha256(content).hexdigest()


def file_record(path: Path) -> dict[str, Any]:
    content = path.read_bytes()
    return {
        "path": str(path),
        "bytes": len(content),
        "sha256": sha256_bytes(content),
    }


def command_output(command: list[str], cwd: Path) -> str:
    return subprocess.check_output(command, cwd=cwd, text=True).strip()


def run_command(command: list[str], cwd: Path) -> dict[str, Any]:
    started = time.perf_counter_ns()
    completed = subprocess.run(command, cwd=cwd, capture_output=True)
    elapsed_ns = time.perf_counter_ns() - started
    return {
        "elapsed_ns": elapsed_ns,
        "exit_status": completed.returncode,
        "stdout_bytes": len(completed.stdout),
        "stdout_sha256": sha256_bytes(completed.stdout),
        "stderr_bytes": len(completed.stderr),
        "stderr_sha256": sha256_bytes(completed.stderr),
    }


def clean(cli: Path, root: Path) -> None:
    completed = subprocess.run([cli, "clean"], cwd=root, capture_output=True)
    if completed.returncode != 0:
        raise RuntimeError(
            f"clean failed in {root}: {completed.stderr.decode(errors='replace')}"
        )


def executable_path(path: Path) -> Path | None:
    if path.is_file():
        return path
    windows = path.with_suffix(".exe")
    return windows if windows.is_file() else None


def discover_artifacts(cache_root: Path) -> tuple[dict[str, Any], Path | None]:
    sources = []
    matches = []
    for source in sorted(cache_root.glob("crates/*/src/main.rs")):
        sources.append(source)
        manifest = source.parents[1] / "Cargo.toml"
        try:
            cargo = tomllib.loads(manifest.read_text())
            binary_name = cargo["bin"][0]["name"]
        except (OSError, KeyError, IndexError, tomllib.TOMLDecodeError):
            continue
        binary = executable_path(cache_root / "target" / "release" / binary_name)
        if binary is not None:
            matches.append((source, binary))

    if len(matches) == 1:
        source, binary = matches[0]
        return (
            {
                "source": {"available": True, **file_record(source)},
                "binary": {"available": True, **file_record(binary)},
            },
            binary,
        )

    source = (
        {"available": True, **file_record(sources[0])}
        if len(sources) == 1
        else {
            "available": False,
            "reason": f"expected one generated source, found {len(sources)}",
        }
    )
    return (
        {
            "source": source,
            "binary": {
                "available": False,
                "reason": f"expected one generated binary, found {len(matches)}",
            },
        },
        None,
    )


def percentile_95(values: list[int]) -> int:
    ordered = sorted(values)
    return ordered[math.ceil(len(ordered) * 0.95) - 1]


def timing_summary(samples: list[dict[str, Any]], key: str) -> dict[str, Any]:
    values = [sample[key]["elapsed_ns"] for sample in samples if sample[key] is not None]
    if not values:
        return {"samples": 0, "median_ns": None, "p95_ns": None}
    return {
        "samples": len(values),
        "median_ns": statistics.median(values),
        "p95_ns": percentile_95(values),
    }


def measure_case(
    repo: Path,
    cli: Path,
    case: CorpusCase,
    mode: str,
    warmup: int,
    runs: int,
) -> dict[str, Any]:
    root = repo / case.root
    source = repo / case.source
    relative_source = source.relative_to(root)
    cache_root = root / ".anvyx" / "cache" / "rust"
    run = [str(cli), "run", "--release", *case.cli_options, str(relative_source)]

    if mode == "warm":
        clean(cli, root)
    for _ in range(warmup):
        if mode == "cold":
            clean(cli, root)
        warmup_result = run_command(run, root)
        if warmup_result["exit_status"] != 0:
            raise RuntimeError(f"warmup failed for {case.name}")

    samples = []
    for index in range(runs):
        if mode == "cold":
            clean(cli, root)
        invocation = run_command(run, root)
        artifacts, binary = discover_artifacts(cache_root)
        binary_run = run_command([str(binary)], root) if binary is not None else None
        samples.append(
            {
                "index": index,
                "invocation": invocation,
                "artifacts": artifacts,
                "cached_binary": binary_run,
            }
        )

    return {
        "name": case.name,
        "source": str(source),
        "root": str(root),
        "cache_root": str(cache_root),
        "command": run,
        "provider_probe_included": case.provider_probe,
        "provider_probe_profile": "dev" if case.provider_probe else None,
        "samples": samples,
        "summary": {
            "source_to_completed_run": timing_summary(samples, "invocation"),
            "cached_binary": timing_summary(samples, "cached_binary"),
        },
    }


def validate_result(result: dict[str, Any], runs: int) -> None:
    if result["schema_version"] != 1 or len(result["cases"]) != len(CORPUS):
        raise RuntimeError("invalid benchmark result shape")
    for case in result["cases"]:
        if len(case["samples"]) != runs:
            raise RuntimeError(f"wrong sample count for {case['name']}")
        for sample in case["samples"]:
            if sample["invocation"]["exit_status"] != 0:
                raise RuntimeError(f"benchmark invocation failed for {case['name']}")
            source = sample["artifacts"]["source"]
            if not source["available"]:
                raise RuntimeError(
                    f"generated source unavailable for {case['name']}: {source['reason']}"
                )
            binary = sample["cached_binary"]
            if binary is not None and binary["exit_status"] != 0:
                raise RuntimeError(f"cached binary failed for {case['name']}")


def main() -> None:
    args = parse_args()
    repo = Path(__file__).resolve().parents[1]
    unsupported_env = [
        name for name in ("ANVYX_CACHE_DIR", "CARGO_TARGET_DIR") if name in os.environ
    ]
    if unsupported_env:
        raise RuntimeError(
            f"benchmark requires unset environment variables: {', '.join(unsupported_env)}"
        )
    cli = repo / "target" / "release" / ("anvyx.exe" if os.name == "nt" else "anvyx")
    build = ["cargo", "build", "-q", "-p", "anvyx", "--release"]
    subprocess.run(build, cwd=repo, check=True)
    if not cli.is_file():
        raise RuntimeError(f"missing CLI binary: {cli}")

    output = args.output if args.output.is_absolute() else repo / args.output
    cargo_config = repo / ".cargo" / "config.toml"
    result = {
        "schema_version": 1,
        "created_at": datetime.now(timezone.utc).isoformat(),
        "mode": args.mode,
        "runs": args.runs,
        "warmup": args.warmup,
        "profile": "release",
        "cache_policy": (
            "run `anvyx clean` in each fixture root before every warmup and sample"
            if args.mode == "cold"
            else "run `anvyx clean` once per fixture, then reuse its generated Cargo cache"
        ),
        "invocation": {
            "argv": sys.argv,
            "cwd": str(Path.cwd()),
            "output": str(output),
        },
        "build_command": build,
        "script": file_record(Path(__file__).resolve()),
        "cli": file_record(cli),
        "environment": {
            "repo": str(repo),
            "head": command_output(["git", "rev-parse", "HEAD"], repo),
            "hostname": platform.node(),
            "host": platform.platform(),
            "machine": platform.machine(),
            "logical_cpus": os.cpu_count(),
            "python": sys.version,
            "rustc": command_output(["rustc", "--version", "--verbose"], repo),
            "cargo": command_output(["cargo", "--version"], repo),
            "cargo_home": os.environ.get("CARGO_HOME"),
            "cargo_config": file_record(cargo_config) if cargo_config.is_file() else None,
        },
        "cases": [
            measure_case(repo, cli, case, args.mode, args.warmup, args.runs)
            for case in CORPUS
        ],
    }
    validate_result(result, args.runs)
    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = output.with_name(f".{output.name}.tmp")
    temporary.write_text(json.dumps(result, indent=2) + "\n")
    temporary.replace(output)


if __name__ == "__main__":
    main()
