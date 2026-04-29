#!/usr/bin/env python3
"""Summarize new-frontend syntax corpus failures from todo/front.json."""

import argparse
import json
import sys
from collections import Counter
from pathlib import Path


MESSAGE_BUCKETS = (
    ("Expected error but got success", "missing rejection"),
    ("Expected success but got error:", "unexpected frontend failure"),
    ("* Expected output to contain:", "text assertion mismatch"),
    ("todo ->", "tracked todo"),
)


def classify_message(message: str) -> str:
    head = message.split("\n", 1)[0]
    if head.startswith("* Expected ") and "warning" in head and "contain:" in head:
        return "warning text mismatch"
    for prefix, label in MESSAGE_BUCKETS:
        if head.startswith(prefix):
            return label
    return head


def area_of(path: str) -> str:
    parts = Path(path).parts
    try:
        i = parts.index("syntax")
    except ValueError:
        return parts[0] if parts else ""
    if i + 1 < len(parts):
        return parts[i + 1]
    return ""


def load_issues(report_path: Path) -> tuple[dict, list[dict]]:
    report = json.loads(report_path.read_text())
    if not report.get("new_frontend"):
        raise SystemExit(f"{report_path} is not a --new-frontend report")
    return report, report["issues"]


def print_summary(report: dict, issues: list[dict], area: str | None) -> None:
    frontend = "new frontend" if report.get("new_frontend") else "old frontend"
    backend = report.get("backend", "?")
    if area:
        issues = [issue for issue in issues if area_of(issue["path"]) == area]
        print(f"report: {area} bucket ({frontend}, backend={backend})")
    else:
        print(f"report: full syntax corpus ({frontend}, backend={backend})")

    print(f"issues: {len(issues)}")
    if not area:
        print(f"passed: {report['passed']}")
        print(f"failed: {report['failed']}")
        print(f"skipped: {report['skipped']}")
        print(f"helpers: {report['helpers']}")

    print("\nby message bucket:")
    for label, count in Counter(classify_message(issue["message"]) for issue in issues).most_common():
        print(f"  {count:>4}  {label}")

    print("\nby area:")
    for name, count in Counter(area_of(issue["path"]) for issue in issues).most_common():
        print(f"  {count:>4}  {name}")


def print_examples(issues: list[dict], area: str | None, limit: int) -> None:
    if area:
        issues = [issue for issue in issues if area_of(issue["path"]) == area]

    if limit <= 0 or not issues:
        return

    print("\nexamples:")
    for issue in issues[:limit]:
        head = issue["message"].split("\n", 1)[0]
        print(f"  {issue['path']} :: {head}")


def main() -> None:
    parser = argparse.ArgumentParser(description="Summarize frontend syntax corpus buckets")
    parser.add_argument(
        "report",
        nargs="?",
        default="todo/front.json",
        help="Path to a test-runner JSON report (default: todo/front.json)",
    )
    parser.add_argument(
        "--area",
        help="Restrict to one syntax area, e.g. imports or externs",
    )
    parser.add_argument(
        "--examples",
        type=int,
        default=10,
        help="Show the first N examples after the summary (default: 10, use 0 to disable)",
    )
    args = parser.parse_args()

    report_path = Path(args.report)
    if not report_path.is_file():
        print(f"error: report not found: {report_path}", file=sys.stderr)
        raise SystemExit(1)

    report, issues = load_issues(report_path)
    print_summary(report, issues, args.area)
    print_examples(issues, args.area, args.examples)


if __name__ == "__main__":
    main()
