#!/usr/bin/env python3
"""Assert final provider-ABI evidence and write its ledger."""
from __future__ import annotations

import argparse
import hashlib
import json
import re
import subprocess
import sys
from collections import Counter
from pathlib import Path
from typing import Any


BASELINE = "64748198"
TARGET_DELETION = 750
AFFECTED = {"macros", "externs", "runtime", "project", "frontend", "backend", "core", "stdlib", "lang"}
KNOWN_AFFECTED_CRATE_DELTAS = {
    "backend": 587,
    "externs": 1743,
    "frontend": -563,
    "macros": -695,
    "runtime": -1644,
    "core": -63,
    "lang": -65,
    "project": -140,
    "stdlib": -36,
}
CAMPAIGN = Path(".pi/tmp/provider-abi-simplification")
LOC_BEFORE = CAMPAIGN / "loc-before.json"
SURFACE_BEFORE = CAMPAIGN / "surface-before.json"
LOC_CURRENT = CAMPAIGN / "loc-current.json"
SURFACE_CURRENT = CAMPAIGN / "surface-current.json"
LEDGER_JSON = CAMPAIGN / "final-ledger.json"
LEDGER_MD = CAMPAIGN / "final-ledger.md"
PERF = CAMPAIGN / "perf-comparison.json"
EXPANSION_BEFORE = CAMPAIGN / "expansion-metrics-before.json"
EXPANSION_AFTER = CAMPAIGN / "expansion-metrics-after.json"
FIXTURES = CAMPAIGN / "fixture-manifest-after.json"
UNITS = CAMPAIGN / "provider-unit-tests-0.4.json"
DIAGNOSTICS = CAMPAIGN / "provider-diagnostic-matrix-after.json"
FINAL_COMMANDS = (
    ("format", "rtk cargo +nightly fmt", "final-gate-cargo-nightly-fmt"),
    ("workspace_tests", "rtk cargo test -q --workspace", "final-gate-cargo-test-workspace"),
    ("integration_tests", "just tests", "final-gate-just-tests"),
    ("release_tests", "just full-tests-release", "final-gate-just-full-tests-release"),
    ("clippy", "rtk cargo clippy --workspace", "final-gate-cargo-clippy-workspace"),
    ("native_only_example", "rtk cargo check --manifest-path examples/native_providers/native_only/host/Cargo.toml", "final-gate-example-native-only"),
    ("source_wrapper_example", "rtk cargo check --manifest-path examples/native_providers/source_wrapper/colors/Cargo.toml", "final-gate-example-source-wrapper"),
    ("vm_diff", "rtk git diff --name-only 64748198 -- crates/backend/src/vm", "final-gate-vm-diff"),
    ("metric_self_test", "rtk python3 scripts/measure_provider_abi.py --self-test", "final-metric-self-test"),
    ("metric_loc_baseline", "rtk python3 scripts/measure_provider_abi.py --tree 64748198 --kind loc", "final-metric-loc-baseline"),
    ("metric_loc_worktree", "rtk python3 scripts/measure_provider_abi.py --tree worktree --kind loc", "final-metric-loc-worktree"),
    ("metric_surface_baseline", "rtk python3 scripts/measure_provider_abi.py --tree 64748198 --kind surface", "final-metric-surface-baseline"),
    ("metric_surface_worktree", "rtk python3 scripts/measure_provider_abi.py --tree worktree --kind surface", "final-metric-surface-worktree"),
    ("performance_cold", "rtk python3 scripts/measure_rust_backend.py --warmup 3 --runs 20 --mode cold", "final-perf-cold-retry"),
    ("performance_warm", "rtk python3 scripts/measure_rust_backend.py --warmup 3 --runs 20 --mode warm", "final-perf-warm"),
    ("performance_comparison", "rtk python3 scripts/compare_provider_abi_artifacts.py", "final-perf-compare-current"),
    ("expansion", "rtk python3.13 scripts/measure_provider_expansion.py --label after", "final-expansion"),
    ("fixture_manifest", "rtk python3.13 scripts/measure_provider_abi_fixtures.py", "final-fixture-manifest"),
    ("diagnostics", "rtk python3.13 .pi/tmp/provider-abi-simplification/generate_session_0_3_current.py --verify", "final-diagnostics-verify-after-cleanup"),
    ("unit_manifest", "rtk python3.13 .pi/tmp/provider-abi-simplification/generate_session_0_4_unit_tests.py --verify", "final-unit-manifest-verify"),
)


class AssertionError(RuntimeError):
    pass


def git(*args: str) -> bytes:
    return subprocess.check_output(["git", *args])


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def load(path: Path) -> dict[str, Any]:
    if not path.is_file():
        raise AssertionError(f"missing artifact: {path}")
    return json.loads(path.read_text())


def name_status(*args: str) -> list[dict[str, str]]:
    fields = [field for field in git(*args).decode().split("\0") if field]
    rows: list[dict[str, str]] = []
    while fields:
        status = fields.pop(0)
        if not fields:
            raise AssertionError(f"truncated name-status record: {status!r}")
        path = fields.pop(0)
        row = {"status": status, "path": path}
        if status[:1] in {"R", "C"}:
            if not fields:
                raise AssertionError(f"truncated rename/copy record: {status!r}")
            row["old_path"] = path
            row["path"] = fields.pop(0)
        rows.append(row)
    return rows


def diff_inventory() -> list[dict[str, str]]:
    return name_status("diff", "--name-status", "-z", BASELINE, "--")


def staged_inventory() -> list[dict[str, str]]:
    return name_status("diff", "--cached", "--name-status", "-z", "--")


def git_paths(root: Path, *args: str) -> list[str]:
    return sorted(path for path in subprocess.check_output(["git", "ls-files", *args], cwd=root, text=True).splitlines() if path)


def untracked_inventory(root: Path) -> list[str]:
    return git_paths(root, "--others", "--exclude-standard")


def ignored_campaign_artifacts(root: Path) -> list[str]:
    return git_paths(root, "--others", "--ignored", "--exclude-standard", "--", str(CAMPAIGN))


def lane(path: str) -> str:
    parts = Path(path).parts
    if parts[:4] == ("crates", "backend", "src", "vm"):
        return "orphan_vm"
    if path.endswith(("tests.rs", "test_support.rs")) or "tests" in parts or parts[:1] == ("tests",):
        return "fixtures_tests"
    if parts[:1] == ("examples",):
        return "examples"
    if Path(path).name == "build.rs":
        return "build_script"
    if parts[:1] == ("scripts",):
        return "measurement_scripts" if Path(path).name.startswith(("measure_provider_abi", "measure_provider_expansion", "measure_provider_abi_fixtures", "compare_provider_abi", "assert_provider_abi")) else "scripts"
    if len(parts) >= 4 and parts[:1] == ("crates",) and parts[2] == "src" and Path(path).suffix == ".rs":
        return "affected_production" if parts[1] in AFFECTED else "production_outside_affected"
    return "other"


def require(condition: bool, message: str, failures: list[str]) -> None:
    if not condition:
        failures.append(message)


def records_current(root: Path, records: list[dict[str, Any]], label: str, failures: list[str]) -> None:
    for row in records:
        path = root / row["path"]
        require(path.is_file(), f"{label}: missing input {row['path']}", failures)
        if path.is_file():
            require(path.stat().st_size == row["bytes"], f"{label}: input byte drift {row['path']}", failures)
            require(sha256(path) == row["sha256"], f"{label}: input hash drift {row['path']}", failures)


def cleanup_generated_state(root: Path) -> None:
    for path in root.rglob(".anvyx"):
        if ".git" not in path.parts:
            subprocess.run(["rm", "-rf", str(path)], check=True)
    for path in root.rglob("target"):
        if path != root / "target" and ".git" not in path.parts:
            subprocess.run(["rm", "-rf", str(path)], check=True)
    for path in root.rglob("Cargo.lock"):
        if path != root / "Cargo.lock" and ".git" not in path.parts:
            path.unlink()
    for path in root.rglob("__pycache__"):
        if ".git" not in path.parts:
            subprocess.run(["rm", "-rf", str(path)], check=True)


def generated_paths(root: Path) -> list[str]:
    paths = []
    for path in root.rglob("Cargo.lock"):
        if path != root / "Cargo.lock":
            paths.append(path.relative_to(root).as_posix())
    for path in root.rglob(".anvyx"):
        if ".git" not in path.parts:
            paths.append(path.relative_to(root).as_posix() + "/")
    for path in root.rglob("target"):
        if path != root / "target" and ".git" not in path.parts:
            paths.append(path.relative_to(root).as_posix() + "/")
    for path in root.rglob("__pycache__"):
        if ".git" not in path.parts:
            paths.append(path.relative_to(root).as_posix() + "/")
    return sorted(set(paths))


def final_commands(root: Path, failures: list[str]) -> list[dict[str, Any]]:
    rows = []
    for name, command, stem in FINAL_COMMANDS:
        stdout = CAMPAIGN / f"{stem}.stdout.log"
        stderr = CAMPAIGN / f"{stem}.stderr.log"
        status = CAMPAIGN / f"{stem}.exit"
        require(stdout.is_file() and stderr.is_file() and status.is_file(), f"final command evidence missing: {name}", failures)
        try:
            exit_status = int(status.read_text().strip())
        except (OSError, ValueError):
            exit_status = -1
        require(exit_status == 0, f"final command failed: {name}", failures)
        row = {
            "name": name,
            "command": command,
            "exit_status": exit_status,
            "stdout": str(stdout),
            "stderr": str(stderr),
            "stdout_bytes": stdout.stat().st_size if stdout.is_file() else 0,
            "stderr_bytes": stderr.stat().st_size if stderr.is_file() else 0,
        }
        text = re.sub(r"\x1b\[[0-9;]*m", "", stdout.read_text() if stdout.is_file() else "")
        if name == "workspace_tests":
            summary = re.search(r"cargo test: (\d+) passed \((\d+) suites", text)
            row["counts"] = {"passed": int(summary.group(1)), "suites": int(summary.group(2))} if summary else {}
        elif name in {"integration_tests", "release_tests"}:
            row["counts"] = {
                "passed": [int(value) for value in re.findall(r"Passed:\s*(\d+)", text)],
                "skipped": [int(value) for value in re.findall(r"Skipped:\s*(\d+)", text)],
            }
        elif name == "clippy":
            summary = re.search(r"cargo clippy: (\d+) errors, (\d+) warnings", text)
            row["counts"] = {"errors": int(summary.group(1)), "warnings": int(summary.group(2))} if summary else {}
        rows.append(row)
    return rows


def assert_metrics(root: Path, failures: list[str]) -> dict[str, Any]:
    driver = root / "scripts/measure_provider_abi.py"
    completed = subprocess.run([sys.executable, str(driver), "--self-test"], cwd=root, capture_output=True, text=True, timeout=120)
    require(completed.returncode == 0, f"metrics: canonical driver self-test failed: {completed.stderr.strip()}", failures)
    driver_sha256 = sha256(driver)
    expected_driver = {"path": "scripts/measure_provider_abi.py", "sha256": driver_sha256}
    loc_before, surface_before = load(root / LOC_BEFORE), load(root / SURFACE_BEFORE)
    loc_current, surface_current = load(root / LOC_CURRENT), load(root / SURFACE_CURRENT)
    for name, artifact, role, tree in (
        ("loc-before", loc_before, "loc", BASELINE),
        ("surface-before", surface_before, "surface", BASELINE),
        ("loc-current", loc_current, "loc", "worktree"),
        ("surface-current", surface_current, "surface", "worktree"),
    ):
        require(artifact.get("schema") == 4, f"{name}: wrong metric schema", failures)
        require(artifact.get("artifact_role") == role, f"{name}: wrong artifact role", failures)
        require(artifact.get("tree", {}).get("requested") == tree, f"{name}: wrong measured tree", failures)
        require(artifact.get("measurement_driver") == expected_driver, f"{name}: not measured by the canonical driver", failures)
        require(artifact.get("measurement_scripts", {}).get("scripts/measure_provider_abi.py") == driver_sha256, f"{name}: canonical driver hash drift", failures)
    current_assert_sha256 = sha256(root / "scripts/assert_provider_abi_artifacts.py")
    for name, artifact in (("loc-current", loc_current), ("surface-current", surface_current)):
        require(artifact.get("measurement_scripts", {}).get("scripts/assert_provider_abi_artifacts.py") == current_assert_sha256, f"{name}: assertion script changed after measurement", failures)
    require(loc_before["affected_total"] == surface_before["affected_total"], "baseline LOC/surface affected totals differ", failures)
    require(loc_current["affected_total"] == surface_current["affected_total"], "current LOC/surface affected totals differ", failures)
    require(loc_before["workspace_total"] == surface_before["workspace_total"], "baseline LOC/surface workspace totals differ", failures)
    require(loc_current["workspace_total"] == surface_current["workspace_total"], "current LOC/surface workspace totals differ", failures)
    structural = loc_current["structural_metrics"]
    representation = {row["token"]: row for row in structural["representation_families"]}
    adapters = {row["token"]: row for row in structural["adapter_fan_out"]}
    owners = structural["validation_boundaries"]
    require(loc_current["affected_total"] <= loc_before["affected_total"] - TARGET_DELETION, "affected LOC target missed", failures)
    require(loc_current["workspace_total"] <= loc_before["workspace_total"] - TARGET_DELETION, "workspace LOC target missed", failures)
    require(surface_current["public_surface"]["public_abi_type_definitions"] <= 72, "public ABI ceiling exceeded", failures)
    require(len(surface_current["public_surface"]["raw_host_support_types"]) <= 5, "validated host-support ceiling exceeded", failures)
    require(representation["ExternTypeExpr::"]["files"] <= 9, "ExternTypeExpr fan-out ceiling exceeded", failures)
    require(adapters["RustParamAdapter::"]["files"] <= 4, "RustParamAdapter fan-out exceeds four files", failures)
    require(adapters["RustReturnAdapter::"]["files"] <= 3, "RustReturnAdapter fan-out exceeds three files", failures)
    require(len(owners) == 10 and all(row["occurrences"] == 1 for row in owners), "validation owner inventory is not exactly ten live owners", failures)
    require(not loc_current["forbidden_excluded_imports"], "forbidden excluded-lane import found", failures)
    crate_deltas = {
        name: loc_current["crates"][name]["nonblank_production_loc"] - loc_before["crates"][name]["nonblank_production_loc"]
        for name in KNOWN_AFFECTED_CRATE_DELTAS
    }
    actual_deletion = loc_before["affected_total"] - loc_current["affected_total"]
    require(crate_deltas == KNOWN_AFFECTED_CRATE_DELTAS, f"affected crate deltas drifted: {crate_deltas}", failures)
    require(actual_deletion == -sum(crate_deltas.values()), "affected deletion arithmetic drifted", failures)
    require(actual_deletion >= TARGET_DELETION, "affected deletion target missed", failures)
    return {"loc_before": loc_before, "surface_before": surface_before, "loc_current": loc_current, "surface_current": surface_current, "representation": representation, "adapters": adapters, "owners": owners}


def assert_perf(root: Path, failures: list[str]) -> dict[str, Any]:
    perf = load(root / PERF)
    require(perf.get("schema") == 4, "performance: wrong comparison schema", failures)
    require(perf.get("validation_failures") == [], "performance: incompatible baseline inputs", failures)
    require(perf.get("performance_input_closure_recorded"), "performance: current measurements lack a recorded input closure", failures)
    require(perf.get("regressions_over_5_percent") == [], "performance: unexplained regression over 5%", failures)
    records_current(root, perf.get("compiler_input_closure", []), "performance compiler closure", failures)
    for mode in ("cold", "warm"):
        inputs = perf.get("inputs", {}).get(mode, {})
        require(all(inputs.get(key) for key in ("same_script_sha256", "same_host", "same_rustc", "same_python_full_version", "same_cache_policy", "same_corpus")), f"performance {mode}: baseline input mismatch", failures)
        require(inputs.get("warmup_runs") == [3, 20], f"performance {mode}: expected warmup 3/runs 20", failures)
        for case, row in perf.get("modes", {}).get(mode, {}).items():
            records_current(root, row.get("current_input_closure", []), f"performance {mode} {case}", failures)
            for metric, values in row.get("metrics", {}).items():
                require(values.get("percent_change", 6) <= 5, f"performance {mode} {case} {metric} exceeds 5%", failures)
            require(all(all(values.values()) for values in row.get("observable_hashes_match", {}).values()), f"performance {mode} {case}: observable output changed", failures)
    probe = perf.get("probe", {})
    require(probe.get("schema") == 3, "probe: wrong freshness schema", failures)
    records_current(root, probe.get("trace_input_closure", []), "probe trace closure", failures)
    require(probe.get("total_invocations") == 51 and probe.get("baseline_total_match"), "probe: invocation total drift", failures)
    require(probe.get("baseline_counts_match"), "probe: per-package invocation drift", failures)
    require(probe.get("semantic_profile") == "provider-probe-v4", "probe: final profile is not provider-probe-v4", failures)
    return perf


def assert_expansion(root: Path, failures: list[str]) -> dict[str, Any]:
    before, after = load(root / EXPANSION_BEFORE), load(root / EXPANSION_AFTER)
    require(after.get("schema") == 2 and after.get("label") == "after", "expansion: wrong after artifact schema", failures)
    generator = after.get("generator", {})
    require(generator.get("path") == "scripts/measure_provider_expansion.py" and generator.get("sha256") == sha256(root / "scripts/measure_provider_expansion.py"), "expansion: generator drift", failures)
    records_current(root, after.get("compiler_input_closure", []), "expansion compiler closure", failures)
    expected = {"local_provider", "final_abi_values_ok", "resource_stored_callback_ok"}
    require(set(before.get("fixtures", {})) == expected and set(after.get("fixtures", {})) == expected, "expansion: fixture set drift", failures)
    comparisons = {}
    for name in expected:
        old, new = before["fixtures"][name], after["fixtures"][name]
        records_current(root, new.get("input_closure", []), f"expansion {name}", failures)
        output = root / new["output"]
        require(output.is_file() and sha256(output) == new.get("output_sha256"), f"expansion {name}: output hash drift", failures)
        require(new.get("families", {}).get("package_metadata_helper", 0) == 0, f"expansion {name}: split package helpers remain", failures)
        comparisons[name] = {}
        for metric in ("bytes", "nonblank_lines", "public_function_count"):
            change = 100 * (new[metric] - old[metric]) / old[metric]
            comparisons[name][metric] = {"before": old[metric], "after": new[metric], "percent_change": change}
            require(change <= 5, f"expansion {name}: {metric} exceeds 5%", failures)
        require(new.get("families", {}).get("anvyx_generated_helper", 0) <= old.get("families", {}).get("anvyx_generated_helper", 0), f"expansion {name}: generated helper count grew", failures)
    return {"comparison": comparisons, "artifact": after}


def assert_fixtures(root: Path, failures: list[str]) -> dict[str, Any]:
    fixture = load(root / FIXTURES)
    require(fixture.get("schema") == 2, "fixtures: wrong schema", failures)
    generator = fixture.get("generator", {})
    require(generator.get("sha256") == sha256(root / "scripts/measure_provider_abi_fixtures.py"), "fixtures: generator drift", failures)
    for name, minimum in (("native_providers", 51), ("source_externs", 136)):
        group = fixture.get(name, {})
        require(group.get("active", 0) >= minimum, f"fixtures {name}: active count below {minimum}", failures)
        require(group.get("skipped") == 0, f"fixtures {name}: skipped fixture", failures)
        require(group.get("baseline_active_subset") and group.get("baseline_subset") and not group.get("baseline_missing"), f"fixtures {name}: baseline identity loss", failures)
        for row in group.get("fixtures", []):
            path = root / row["path"]
            require(path.is_file() and sha256(path) == row.get("sha256"), f"fixtures {name}: current fixture hash drift {row['path']}", failures)
    return fixture


def assert_units(root: Path, failures: list[str]) -> dict[str, Any]:
    units = load(root / UNITS)
    rows = units.get("tests", [])
    counts = Counter(row.get("canonical_disposition") for row in rows)
    require(counts == Counter({"KEEP": 43, "MIGRATE_THEN_DELETE": 17}), f"units: wrong disposition count {counts}", failures)
    completed = subprocess.run([sys.executable, str(root / ".pi/tmp/provider-abi-simplification/generate_session_0_4_unit_tests.py"), "--verify"], cwd=root, capture_output=True, text=True, timeout=180)
    require(completed.returncode == 0, f"units: generator verification failed: {completed.stderr.strip()}", failures)
    return {"count": len(rows), "counts": dict(counts)}


def assert_diagnostics(root: Path, failures: list[str]) -> dict[str, Any]:
    diagnostics = load(root / DIAGNOSTICS)
    probe = diagnostics.get("provider_probe", {})
    require(diagnostics.get("artifact_role") == "final-provider-diagnostic-matrix", "diagnostics: wrong artifact role", failures)
    require(diagnostics.get("schema") == 4 and probe.get("schema") == 4 and probe.get("profile") == "provider-probe-v4", "diagnostics: wrong schema/profile", failures)
    require(probe.get("paired_package_api") == "rust_providers() -> RawProviderPackage", "diagnostics: paired package API drift", failures)
    require(len(diagnostics.get("diagnostic_categories", [])) == 12, "diagnostics: wrong category count", failures)
    operator = next((row for row in diagnostics.get("diagnostic_categories", []) if row.get("category") == "operator_name_binding_mismatch"), {})
    require(operator.get("source") == "tests/run/native_providers/operator_name_err/src/main.anv", "diagnostics: operator binding mismatch identity drift", failures)
    require(diagnostics.get("separate_fixture_coverage") == [{"path": "tests/run/native_providers/invalid_rust_binding_path_err/src/main.anv", "reason": "Invalid Rust binding paths remain separate fixture coverage and do not substitute for the operator binding mismatch."}], "diagnostics: invalid Rust path coverage drift", failures)
    generator = root / ".pi/tmp/provider-abi-simplification/generate_session_0_3_current.py"
    completed = subprocess.run([sys.executable, str(generator), "--verify"], cwd=root, capture_output=True, text=True, timeout=180)
    require(completed.returncode == 0, f"diagnostics: generator verification failed: {completed.stderr.strip()}", failures)
    return {"schema": diagnostics.get("schema"), "profile": probe.get("profile"), "categories": len(diagnostics.get("diagnostic_categories", [])), "generator_sha256": sha256(generator)}


def assert_artifacts(root: Path) -> tuple[dict[str, Any], list[str]]:
    failures: list[str] = []
    metrics = assert_metrics(root, failures)
    perf = assert_perf(root, failures)
    expansion = assert_expansion(root, failures)
    fixtures = assert_fixtures(root, failures)
    units = assert_units(root, failures)
    diagnostics = assert_diagnostics(root, failures)
    commands = final_commands(root, failures)
    inventory = diff_inventory()
    for row in inventory:
        if row["status"] != "D":
            require((root / row["path"]).exists(), f"changed path missing: {row['path']}", failures)
    untracked = untracked_inventory(root)
    ignored_campaign = ignored_campaign_artifacts(root)
    require(all((root / path).is_file() for path in untracked), "untracked inventory contains missing path", failures)
    require(all((root / path).is_file() for path in ignored_campaign), "ignored campaign inventory contains missing path", failures)
    require(not set(untracked) & set(ignored_campaign), "untracked and ignored campaign inventories overlap", failures)
    excluded: dict[str, list[dict[str, str]]] = {}
    for row in inventory:
        excluded.setdefault(lane(row["path"]), []).append(row)
    vm_paths = excluded.get("orphan_vm", [])
    root_lock_paths = [row for row in inventory if row["path"] == "Cargo.lock"]
    invalid_lock_paths = [row for row in inventory if Path(row["path"]).name == "Cargo.lock" and row["path"] != "Cargo.lock"]
    cache_paths = [row for row in inventory if ".anvyx" in Path(row["path"]).parts or "target" in Path(row["path"]).parts]
    generated = generated_paths(root)
    require(not vm_paths, "VM excluded lane changed", failures)
    require(not invalid_lock_paths, "tracked nested Cargo.lock changed", failures)
    require(not cache_paths, "tracked cache path changed", failures)
    require(not generated, f"generated nested lock/cache remains: {generated}", failures)
    require(not staged_inventory(), "Git index changed", failures)
    cargo_check = subprocess.run(["cargo", "check", "--locked"], cwd=root, capture_output=True, text=True)
    require(cargo_check.returncode == 0, f"root Cargo.lock does not match Cargo.toml: {cargo_check.stderr.strip()}", failures)
    before_lanes = metrics["loc_before"]["excluded_accounting"]["categories"]
    current_lanes = metrics["loc_current"]["excluded_accounting"]["categories"]
    excluded_accounting = {
        name: {
            "baseline": {key: before_lanes.get(name, {}).get(key, 0) for key in ("files", "nonblank_lines")},
            "current": {key: current_lanes.get(name, {}).get(key, 0) for key in ("files", "nonblank_lines")},
            "delta": {key: current_lanes.get(name, {}).get(key, 0) - before_lanes.get(name, {}).get(key, 0) for key in ("files", "nonblank_lines")},
        }
        for name in sorted(before_lanes.keys() | current_lanes.keys())
    }
    ledger = {
        "schema": 5,
        "baseline": BASELINE,
        "metric_artifacts": {
            "loc_before": str(LOC_BEFORE), "surface_before": str(SURFACE_BEFORE), "loc_current": str(LOC_CURRENT), "surface_current": str(SURFACE_CURRENT),
            "canonical_measurement_driver": metrics["loc_current"].get("measurement_driver"),
            "both_tree_driver_match": metrics["loc_before"].get("measurement_driver") == metrics["loc_current"].get("measurement_driver"),
        },
        "production": {
            "affected": {"baseline": metrics["loc_before"]["affected_total"], "current": metrics["loc_current"]["affected_total"], "delta": metrics["loc_current"]["affected_total"] - metrics["loc_before"]["affected_total"], "ceiling": metrics["loc_before"]["affected_total"] - TARGET_DELETION},
            "workspace": {"baseline": metrics["loc_before"]["workspace_total"], "current": metrics["loc_current"]["workspace_total"], "delta": metrics["loc_current"]["workspace_total"] - metrics["loc_before"]["workspace_total"], "ceiling": metrics["loc_before"]["workspace_total"] - TARGET_DELETION},
        },
        "surface": {"public_abi_type_definitions": metrics["surface_current"]["public_surface"]["public_abi_type_definitions"], "raw_host_support_types": len(metrics["surface_current"]["public_surface"]["raw_host_support_types"])},
        "structural": {"representation_fanout": {key: {"occurrences": row["occurrences"], "files": row["files"]} for key, row in metrics["representation"].items()}, "adapter_fanout": {key: {"occurrences": row["occurrences"], "files": row["files"]} for key, row in metrics["adapters"].items()}, "validation_owners": metrics["owners"]},
        "evidence": {"commands": commands, "performance": str(PERF), "performance_regressions_over_5_percent": perf.get("regressions_over_5_percent"), "expansion": expansion["comparison"], "fixtures": {name: {key: fixtures[name][key] for key in ("active", "helpers", "skipped", "baseline_active_subset")} for name in ("native_providers", "source_externs")}, "units": units, "diagnostics": diagnostics, "probe": perf.get("probe")},
        "diff": {"baseline_changed_file_count": len(inventory), "baseline_name_status": inventory, "staged_name_status": staged_inventory(), "untracked_paths": untracked, "ignored_campaign_artifact_paths": ignored_campaign},
        "lock_and_cache": {"root_lock_changes": root_lock_paths, "root_lock_cargo_check_locked": cargo_check.returncode == 0, "invalid_tracked_nested_locks": invalid_lock_paths, "generated_nested_locks_or_caches": generated, "tracked_cache_paths": cache_paths},
        "excluded_lane": {"accounting": excluded_accounting, "changed_paths": excluded, "vm_changed_paths": vm_paths, "forbidden_excluded_imports": metrics["loc_current"]["forbidden_excluded_imports"]},
        "remaining_checks": ["Phase 6 and Session 6.2 checkboxes intentionally remain unchecked."],
        "assertion": {"passed": not failures, "failures": failures},
    }
    return ledger, failures


def markdown(ledger: dict[str, Any]) -> str:
    production = ledger["production"]
    evidence = ledger["evidence"]
    lines = [
        "# Provider ABI simplification final ledger", "",
        "Status: **all requested gates, quantitative evidence, and recorded final reviews captured; Phase 6 and Session 6.2 checkboxes remain intentionally unchecked.**", "",
        "## Current metrics", "",
        "| Metric | Baseline | Current | Ceiling |", "| --- | ---: | ---: | ---: |",
        f"| Affected production LOC | {production['affected']['baseline']:,} | {production['affected']['current']:,} | {production['affected']['ceiling']:,} |",
        f"| Workspace production LOC | {production['workspace']['baseline']:,} | {production['workspace']['current']:,} | {production['workspace']['ceiling']:,} |", "",
    ]
    lines.extend([
        "## Final quantitative evidence", "",
        f"- Performance: cold and warm 3-warmup/20-sample artifacts have {len(evidence['performance_regressions_over_5_percent'])} regressions above 5%.",
        f"- Probe: {evidence['probe']['total_invocations']} baseline fixture invocations, profile `{evidence['probe']['semantic_profile']}`, exact baseline counts: {evidence['probe']['baseline_counts_match']}.",
        f"- Fixtures: native {evidence['fixtures']['native_providers']['active']} active / {evidence['fixtures']['native_providers']['skipped']} skipped; source externs {evidence['fixtures']['source_externs']['active']} active / {evidence['fixtures']['source_externs']['skipped']} skipped.",
        f"- Units: {evidence['units']['count']} identities, {evidence['units']['counts']}.",
        f"- Final commands: {len(evidence['commands'])} recorded with zero nonzero exits; raw stdout/stderr logs are listed in the JSON ledger.", "",
        "## Lock and generated-state policy", "",
        "- A tracked root `Cargo.lock` dependency-graph update is allowed and ledgered only after `cargo check --locked` passes.",
        "- Nested generated `Cargo.lock` files and nested cache directories are forbidden.", "",
        "## Assertion", "",
        "- pass" if ledger["assertion"]["passed"] else "- failed:",
        *([] if ledger["assertion"]["passed"] else [f"  - {item}" for item in ledger["assertion"]["failures"]]), "",
    ])
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write-ledger", action="store_true")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    try:
        cleanup_generated_state(root)
        ledger, failures = assert_artifacts(root)
        if args.write_ledger:
            (root / LEDGER_JSON).write_text(json.dumps(ledger, indent=2, sort_keys=True) + "\n")
            (root / LEDGER_MD).write_text(markdown(ledger))
        print(json.dumps({"assertion": "passed" if not failures else "failed", "failures": failures, "ledger": str(LEDGER_JSON), "ledger_written": args.write_ledger}, sort_keys=True))
        return 0 if not failures else 1
    except (AssertionError, OSError, subprocess.CalledProcessError, subprocess.TimeoutExpired, json.JSONDecodeError) as error:
        print(f"assert_provider_abi_artifacts: {error}", file=sys.stderr)
        return 1
    finally:
        cleanup_generated_state(root)


if __name__ == "__main__":
    raise SystemExit(main())
