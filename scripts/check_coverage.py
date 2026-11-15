#!/usr/bin/env python3
"""Strict 100.0% function, line, and branch coverage verifier using lcov + gcov."""

import re
import shutil
import subprocess
import sys
from pathlib import Path


def find_gcov_tool() -> str:
    """Locate the GCC gcov executable matching gfortran."""
    for candidate in ("/opt/homebrew/bin/gcov-16", "gcov-16", "gcov-15", "gcov-14", "gcov"):
        resolved = shutil.which(candidate)
        if resolved:
            return resolved
    print("ERROR: Could not locate gcov executable.", file=sys.stderr)
    sys.exit(1)


def lcov_extra_args(lcov_bin: str) -> list[str]:
    """Return lcov 2.x compatibility flags so coverage works without requiring `~/.lcovrc`."""
    proc = subprocess.run([lcov_bin, "--version"], check=False, capture_output=True, text=True)
    ver_match = re.search(r"version\s+(\d+)", proc.stdout + proc.stderr, re.IGNORECASE)
    if ver_match and int(ver_match.group(1)) >= 2:
        return [
            "--ignore-errors",
            "inconsistent,inconsistent,mismatch,mismatch,negative,negative,unused,unused",
            "--rc",
            "geninfo_unexecuted_blocks=1",
        ]
    return []


def main() -> int:
    """Capture lcov tracefile, filter to src/ and app/, and enforce 100.0% across all metrics."""
    if len(sys.argv) < 2:
        print("Usage: check_coverage.py <build_dir>", file=sys.stderr)
        return 2

    build_dir = Path(sys.argv[1]).resolve()
    repo_root = Path(__file__).resolve().parent.parent
    lcov_bin = shutil.which("lcov")
    if not lcov_bin:
        print("ERROR: lcov not found on PATH.", file=sys.stderr)
        return 2

    gcov_tool = find_gcov_tool()
    extra_args = lcov_extra_args(lcov_bin)
    raw_info = build_dir / "coverage_raw.info"
    filtered_info = build_dir / "coverage.info"

    subprocess.run(
        [
            lcov_bin,
            "--capture",
            "--directory",
            str(build_dir),
            "--output-file",
            str(raw_info),
            "--gcov-tool",
            gcov_tool,
            "--rc",
            "branch_coverage=1",
            *extra_args,
            "--quiet",
        ],
        check=True,
    )

    extract_patterns = [str(repo_root / "src" / "*")]
    if (repo_root / "app" / "main.f90").exists():
        extract_patterns.append(str(repo_root / "app" / "*"))
    subprocess.run(
        [
            lcov_bin,
            "--extract",
            str(raw_info),
            *extract_patterns,
            "--output-file",
            str(filtered_info),
            "--rc",
            "branch_coverage=1",
            *extra_args,
            "--quiet",
        ],
        check=True,
    )

    summary = subprocess.run(
        [lcov_bin, "--summary", str(filtered_info), "--rc", "branch_coverage=1", *extra_args],
        check=True,
        capture_output=True,
        text=True,
    )
    output = summary.stdout + chr(10) + summary.stderr
    print(output.strip())

    metrics = {}
    for metric in ("lines", "functions", "branches"):
        match = re.search(rf"{metric}\.+:\s*([0-9.]+)%\s*\((\d+)\s+of\s+(\d+)", output)
        if match:
            metrics[metric] = (float(match.group(1)), int(match.group(2)), int(match.group(3)))

    failed = False
    for metric in ("functions", "lines", "branches"):
        if metric not in metrics:
            print(f"ERROR: Missing '{metric}' metric in lcov summary.", file=sys.stderr)
            failed = True
            continue
        pct, hit, total = metrics[metric]
        if hit != total or pct < 100.0:
            print(
                f"FAIL: {metric} coverage is {pct:.2f}% ({hit}/{total}); 100.0% required.",
                file=sys.stderr,
            )
            failed = True

    if failed:
        # Print uncovered details from tracefile to aid debugging
        current_sf = ""
        for raw_line in filtered_info.read_text(encoding="utf-8").splitlines():
            if raw_line.startswith("SF:"):
                current_sf = raw_line[3:]
            elif raw_line.startswith("DA:") and raw_line.endswith(",0"):
                lineno = raw_line.split(":")[1].split(",")[0]
                print(f"  Uncovered line: {current_sf}:{lineno}", file=sys.stderr)
            elif raw_line.startswith("FNA:") and ",0," in raw_line:
                fn_name = raw_line.split(",", 2)[2]
                print(f"  Uncovered function: {current_sf} -> {fn_name}", file=sys.stderr)
            elif raw_line.startswith("BRDA:") and (raw_line.endswith(",0") or raw_line.endswith(",-")):
                parts = raw_line[5:].split(",")
                print(f"  Uncovered branch: {current_sf}:{parts[0]} ({raw_line})", file=sys.stderr)
        return 1

    print("PASS: 100.0% function, 100.0% line, and 100.0% branch coverage verified.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
