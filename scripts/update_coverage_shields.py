#!/usr/bin/env python3
"""Compute documentation and test coverage percentages and add/update shields in README.md."""

from __future__ import annotations

import argparse
import pathlib
import re
import shutil
import subprocess
import sys

LF = chr(10)


def badge_color_for_percentage(pct: float) -> str:
    """Map coverage percentage `[0.0, 100.0]` to a standard Shields.io status color."""
    if pct >= 100.0 - 1e-9:
        return "brightgreen"
    if pct >= 90.0:
        return "green"
    if pct >= 80.0:
        return "yellowgreen"
    if pct >= 70.0:
        return "yellow"
    if pct >= 60.0:
        return "orange"
    return "red"


def format_percentage_text(pct: float) -> str:
    """Format `100.0` as `100%` and fractional percentages to one decimal place (e.g. `98.5%`)."""
    clamped = max(0.0, min(100.0, pct))
    rounded = f"{clamped:.1f}"
    if rounded.endswith(".0"):
        rounded = rounded[:-2]
    return f"{rounded}%"


def build_shield_markdown(kind: str, pct: float) -> str:
    """Construct Shields.io Markdown badge string for `kind` (`'doc'` or `'test'`)."""
    if kind not in {"doc", "test"}:
        msg = f"Unsupported shield kind: {kind}"
        raise ValueError(msg)
    label = f"{kind} coverage"
    encoded_label = f"{kind}%20coverage"
    pct_str = format_percentage_text(pct)
    encoded_pct = pct_str.replace("%", "%25")
    color = badge_color_for_percentage(pct)
    return f"![{label}](https://img.shields.io/badge/{encoded_label}-{encoded_pct}-{color})"


def upsert_shield_in_markdown(markdown: str, kind: str, pct: float) -> str:
    """Insert or update the `doc coverage` or `test coverage` shield in `markdown` text."""
    new_badge = build_shield_markdown(kind, pct)
    normalized = markdown.replace(chr(13) + LF, LF).replace(chr(13), LF)

    # Match either `[!['kind coverage'](...)](...)` or `!['kind coverage'](...)`
    pattern = re.compile(
        rf"(?:\[!\[[^\]]*{kind}[ _-]*coverage[^\]]*\]\([^)]+\)\]\([^)]+\)"
        rf"|!\[[^\]]*{kind}[ _-]*coverage[^\]]*\]\([^)]+\)"
        rf"|!\[[^\]]*\]\(https://img[.]shields[.]io/badge/{kind}(?:%20|_|-)+coverage-[^)]+\))",
        re.IGNORECASE,
    )
    if pattern.search(normalized):
        updated = pattern.sub(lambda _m: new_badge, normalized, count=1)
        return updated if updated.endswith(LF) else updated + LF

    lines = normalized.split(LF) if normalized else []
    if lines and lines[-1] == "":
        lines.pop()

    if not lines:
        return f"# cdd-fortran{LF}{LF}{new_badge}{LF}"

    # If the other coverage shield already exists, place directly adjacent to it.
    other_kind = "doc" if kind == "test" else "test"
    other_pat = re.compile(rf"{other_kind}(?:%20|[ _-])+coverage", re.IGNORECASE)
    for idx, line in enumerate(lines):
        if "img.shields.io/badge/" in line and other_pat.search(line):
            insert_at = idx if kind == "doc" else idx + 1
            lines.insert(insert_at, new_badge)
            return LF.join(lines) + LF

    # Otherwise insert immediately after the first Markdown H1 heading (`# ...`).
    for idx, line in enumerate(lines):
        if line.startswith("# "):
            insert_at = idx + 1
            if insert_at < len(lines) and lines[insert_at] != "":
                lines.insert(insert_at, "")
                lines.insert(insert_at + 1, new_badge)
            else:
                lines.insert(insert_at + 1 if insert_at < len(lines) else insert_at, new_badge)
            return LF.join(lines) + LF

    lines.insert(0, new_badge)
    lines.insert(1, "")
    return LF.join(lines) + LF


def update_readme_file(readme_path: pathlib.Path, kind: str, pct: float) -> bool:
    """Update `readme_path` on disk only when the rendered shield content changes."""
    existing = readme_path.read_text(encoding="utf-8") if readme_path.exists() else ""
    updated = upsert_shield_in_markdown(existing, kind, pct)
    if updated != existing:
        readme_path.write_text(updated, encoding="utf-8")
        return True
    return False


def parse_doc_audit_output(stdout_text: str, stderr_text: str, doxygen_failed: bool = False) -> tuple[float, int, int]:
    """Parse `doc_coverage_audit` output into `(coverage_pct, documented_items, total_items)`."""
    counts_match = re.search(
        r"files=([0-9]+)[ ]+containers=([0-9]+)[ ]+types=([0-9]+)[ ]+"
        r"fields=([0-9]+)[ ]+procedures=([0-9]+)[ ]+args=([0-9]+)",
        stdout_text,
    )
    if not counts_match:
        msg = "Unable to locate entity summary in doc_coverage_audit output."
        raise ValueError(msg)

    total_items = sum(int(counts_match.group(i)) for i in range(1, 7))
    violations = sum(1 for line in (stdout_text + LF + stderr_text).splitlines() if "DOC COVERAGE VIOLATION" in line)
    if doxygen_failed and violations == 0:
        violations = 1

    if total_items <= 0:
        return 0.0, 0, 0
    documented = max(0, total_items - violations)
    pct = (100.0 * documented) / float(total_items)
    return pct, documented, total_items


def parse_lcov_summary_output(summary_text: str) -> tuple[float, dict[str, tuple[float, int, int]]]:
    """Parse `lcov --summary` output and return `(strict_min_pct, metrics_map)`."""
    metrics: dict[str, tuple[float, int, int]] = {}
    for metric in ("lines", "functions", "branches"):
        match = re.search(rf"{metric}[.]+:[ ]*([0-9.]+)%[ ]*\(([0-9]+)[ ]+of[ ]+([0-9]+)", summary_text)
        if match:
            hit = int(match.group(2))
            total = int(match.group(3))
            exact_pct = (100.0 * hit / float(total)) if total > 0 else 0.0
            metrics[metric] = (exact_pct, hit, total)
    if len(metrics) < 3:
        missing = sorted({"lines", "functions", "branches"} - set(metrics))
        msg = f"Missing coverage metric(s) in lcov output: {', '.join(missing)}"
        raise ValueError(msg)
    strict_pct = min(val[0] for val in metrics.values())
    return strict_pct, metrics


def find_gcov_executable() -> str:
    """Locate the GCC `gcov` binary matching `gfortran`."""
    for candidate in ("/opt/homebrew/bin/gcov-16", "gcov-16", "gcov-15", "gcov-14", "gcov"):
        resolved = shutil.which(candidate)
        if resolved:
            return resolved
    msg = "Could not locate gcov executable on PATH."
    raise RuntimeError(msg)


def measure_doc_coverage(repo_root: pathlib.Path, build_dir: pathlib.Path) -> tuple[float, bool]:
    """Execute `doc_coverage_audit` and `doxygen` to compute documentation coverage `%`."""
    candidates = [
        build_dir / "test" / "doc_coverage_audit",
        build_dir / "test" / "doc_coverage_audit.exe",
        build_dir / "doc_coverage_audit",
    ]
    auditor = next((c for c in candidates if c.is_file()), None)
    if auditor is None:
        msg = f"doc_coverage_audit binary not found under {build_dir}; run build first."
        raise RuntimeError(msg)

    audit_proc = subprocess.run(
        [str(auditor)],
        cwd=repo_root,
        check=False,
        capture_output=True,
        text=True,
    )
    doxygen_failed = False
    doxygen_bin = shutil.which("doxygen")
    doxyfile = repo_root / "Doxyfile"
    if doxygen_bin and doxyfile.is_file():
        (repo_root / "build" / "docs").mkdir(parents=True, exist_ok=True)
        dox_proc = subprocess.run(
            [doxygen_bin, str(doxyfile)],
            cwd=repo_root,
            check=False,
            capture_output=True,
            text=True,
        )
        if dox_proc.returncode != 0:
            doxygen_failed = True
            err_txt = (dox_proc.stdout + LF + dox_proc.stderr).strip()
            if err_txt:
                print(err_txt, file=sys.stderr)

    pct, documented, total = parse_doc_audit_output(audit_proc.stdout, audit_proc.stderr, doxygen_failed)
    passed = audit_proc.returncode == 0 and not doxygen_failed and documented == total and total > 0
    print(f"Documentation coverage: {pct:.1f}% ({documented}/{total} entities documented)")
    if audit_proc.returncode != 0 and audit_proc.stderr.strip():
        print(audit_proc.stderr.strip(), file=sys.stderr)
    return pct, passed


def measure_test_coverage(repo_root: pathlib.Path, build_dir: pathlib.Path) -> tuple[float, bool]:
    """Capture `lcov` function, line, and branch metrics and compute strict test coverage `%`."""
    lcov_bin = shutil.which("lcov")
    if not lcov_bin:
        msg = "lcov executable not found on PATH."
        raise RuntimeError(msg)

    gcov_tool = find_gcov_executable()
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
            "--ignore-errors",
            "inconsistent,mismatch,negative,unused",
            "--quiet",
        ],
        check=True,
    )

    extract_patterns = [str(repo_root / "src" / "*")]
    if (repo_root / "app" / "main.f90").is_file():
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
            "--ignore-errors",
            "inconsistent,mismatch,negative,unused",
            "--quiet",
        ],
        check=True,
    )

    summary_proc = subprocess.run(
        [lcov_bin, "--summary", str(filtered_info), "--rc", "branch_coverage=1"],
        check=True,
        capture_output=True,
        text=True,
    )
    combined = summary_proc.stdout + LF + summary_proc.stderr
    pct, metrics = parse_lcov_summary_output(combined)
    lines_m = metrics["lines"]
    funcs_m = metrics["functions"]
    branch_m = metrics["branches"]
    print(
        f"Test coverage: {pct:.1f}% "
        f"(functions {funcs_m[1]}/{funcs_m[2]}, lines {lines_m[1]}/{lines_m[2]}, branches {branch_m[1]}/{branch_m[2]})"
    )
    passed = all(hit == tot and tot > 0 for (_p, hit, tot) in metrics.values())
    return pct, passed


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    """Parse CLI arguments for `update_coverage_shields.py`."""
    parser = argparse.ArgumentParser(description="Measure coverage and add/update shields in README.md.")
    parser.add_argument("kind", choices=("doc", "test"), help="Coverage shield type to compute and update.")
    parser.add_argument(
        "--readme",
        type=pathlib.Path,
        default=pathlib.Path("README.md"),
        help="Path to README.md file.",
    )
    parser.add_argument(
        "--build-dir",
        type=pathlib.Path,
        default=pathlib.Path("build"),
        help="Path to CMake build directory.",
    )
    parser.add_argument(
        "--root",
        type=pathlib.Path,
        default=pathlib.Path(__file__).resolve().parent.parent,
        help="Repository root directory.",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    """Entry point for `update_coverage_shields.py`."""
    args = parse_args(argv)
    repo_root = args.root.resolve()
    readme_path = args.readme if args.readme.is_absolute() else (repo_root / args.readme)
    build_dir = args.build_dir if args.build_dir.is_absolute() else (repo_root / args.build_dir)

    try:
        if args.kind == "doc":
            pct, passed = measure_doc_coverage(repo_root, build_dir)
        else:
            pct, passed = measure_test_coverage(repo_root, build_dir)
    except (RuntimeError, ValueError, subprocess.CalledProcessError) as exc:
        print(f"COVERAGE SHIELD ERROR: {exc}", file=sys.stderr)
        return 1

    changed = update_readme_file(readme_path, args.kind, pct)
    if changed:
        print(f"Updated '{args.kind} coverage' shield in {readme_path.name} -> {format_percentage_text(pct)}")
    else:
        print(f"'{args.kind} coverage' shield in {readme_path.name} is up to date ({format_percentage_text(pct)}).")

    if not passed:
        print(f"FAIL: {args.kind} coverage is {pct:.2f}% (100.0% required).", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
