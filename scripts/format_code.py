#!/usr/bin/env python3
"""Auto-formatter (`--fix`) and strict format verifier (`--check`) for cdd-fortran."""

from __future__ import annotations

import argparse
import pathlib
import shutil
import subprocess
import sys

NUL = chr(0)
TAB = chr(9)
LF = chr(10)
CR = chr(13)
CRLF = CR + LF
LF_BYTES = bytes([10])
CRLF_BYTES = bytes([13, 10])

FREE_FORTRAN_EXTS = {
    ".f90",
    ".f95",
    ".f03",
    ".f08",
    ".f18",
    ".f23",
    ".fpp",
    ".fypp",
    ".inc",
    ".fh",
}
FIXED_FORTRAN_EXTS = {".f", ".for", ".ftn", ".f66", ".f77"}
PYTHON_EXTS = {".py", ".pyi"}
WINDOWS_SCRIPT_EXTS = {".bat", ".cmd", ".ps1"}
MARKDOWN_EXTS = {".md", ".markdown"}
BINARY_EXTS = {
    ".mod",
    ".smod",
    ".mod0",
    ".rmod",
    ".o",
    ".obj",
    ".a",
    ".lib",
    ".so",
    ".dylib",
    ".dll",
    ".exe",
    ".out",
    ".pdb",
    ".gcno",
    ".gcda",
    ".profraw",
    ".profdata",
    ".tar",
    ".gz",
    ".tgz",
    ".xz",
    ".bz2",
    ".zst",
    ".zip",
    ".7z",
    ".png",
    ".jpg",
    ".jpeg",
    ".gif",
    ".webp",
    ".ico",
    ".pdf",
}


def discover_repo_files(repo_root: pathlib.Path) -> list[pathlib.Path]:
    """Return non-ignored repository text candidate paths relative to `repo_root`."""
    proc = subprocess.run(
        ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"],
        cwd=repo_root,
        check=True,
        capture_output=True,
    )
    rels = [entry for entry in proc.stdout.decode("utf-8").split(NUL) if entry]
    result: list[pathlib.Path] = []
    for rel in sorted(set(rels)):
        if rel == "TODO_PLAN.md":
            continue
        candidate = repo_root / rel
        if candidate.is_file() and candidate.suffix.lower() not in BINARY_EXTS:
            result.append(candidate)
    return result


def expand_tabs_outside_quotes(line: str, spaces_per_tab: int) -> str:
    """Replace horizontal tabs outside single- or double-quoted Fortran literals."""
    if TAB not in line:
        return line
    out: list[str] = []
    quote_char = ""
    for ch in line:
        if quote_char:
            out.append(ch)
            if ch == quote_char:
                quote_char = ""
        elif ch in {"'", '"'}:
            quote_char = ch
            out.append(ch)
        elif ch == TAB:
            out.append(" " * spaces_per_tab)
        else:
            out.append(ch)
    return "".join(out)


def normalize_doxygen_prefix(line: str) -> str:
    """Insert missing space between `!>`/`!<` Doxygen markers and `@` outside string literals."""
    if "!>@" not in line and "!<@" not in line:
        return line
    quote_char = ""
    for idx, ch in enumerate(line):
        if quote_char:
            if ch == quote_char:
                quote_char = ""
        elif ch in {"'", '"'}:
            quote_char = ch
        elif ch == "!" and line[idx : idx + 3] in {"!>@", "!<@"}:
            return f"{line[: idx + 2]} @{line[idx + 3 :]}"
    return line


def collapse_blank_runs(lines: list[str], max_consecutive_blank: int = 2) -> list[str]:
    """Collapse runs of empty lines exceeding `max_consecutive_blank` and strip EOF blanks."""
    collapsed: list[str] = []
    blank_run = 0
    for line in lines:
        if line == "":
            blank_run += 1
            if blank_run <= max_consecutive_blank:
                collapsed.append(line)
        else:
            blank_run = 0
            collapsed.append(line)
    while collapsed and collapsed[-1] == "":
        collapsed.pop()
    return collapsed


def format_text_for_path(path: pathlib.Path, raw: bytes) -> bytes:
    """Compute canonical formatted UTF-8 bytes for `path` according to project rules."""
    text = raw.decode("utf-8")
    if text == "":
        return b""

    normalized = text.replace(CRLF, LF).replace(CR, LF)
    lines = normalized.split(LF)
    if lines and lines[-1] == "":
        lines.pop()

    ext = path.suffix.lower()
    is_makefile = path.name in {"Makefile", "GNUmakefile", "makefile"} or ext == ".mk"

    if ext in WINDOWS_SCRIPT_EXTS:
        cleaned = [expand_tabs_outside_quotes(ln.rstrip(" " + TAB), 2) for ln in lines]
        cleaned = collapse_blank_runs(cleaned)
        return (CRLF.join(cleaned) + CRLF).encode("utf-8") if cleaned else b""

    if ext in FIXED_FORTRAN_EXTS:
        # Preserve columns 1-80 trailing spaces in fixed-form punched-card decks.
        while lines and lines[-1] == "":
            lines.pop()
        return (LF.join(lines) + LF).encode("utf-8") if lines else b""

    if ext in FREE_FORTRAN_EXTS:
        formatted_lines: list[str] = []
        for ln in lines:
            expanded = expand_tabs_outside_quotes(ln, 2).rstrip(" " + TAB)
            formatted_lines.append(normalize_doxygen_prefix(expanded))
        formatted_lines = collapse_blank_runs(formatted_lines)
        return (LF.join(formatted_lines) + LF).encode("utf-8") if formatted_lines else b""

    if ext in MARKDOWN_EXTS:
        md_lines: list[str] = []
        for ln in lines:
            no_tabs = ln.replace(TAB, "  ")
            if no_tabs.endswith("  "):
                # Preserve intentional two-space Markdown hard line breaks.
                trimmed = no_tabs.rstrip(" ") + "  "
            else:
                trimmed = no_tabs.rstrip(" ")
            md_lines.append(trimmed)
        md_lines = collapse_blank_runs(md_lines)
        return (LF.join(md_lines) + LF).encode("utf-8") if md_lines else b""

    if is_makefile:
        mk_lines = collapse_blank_runs([ln.rstrip(" " + TAB) for ln in lines])
        return (LF.join(mk_lines) + LF).encode("utf-8") if mk_lines else b""

    tab_width = 4 if ext in PYTHON_EXTS else 2
    gen_lines = collapse_blank_runs([ln.replace(TAB, " " * tab_width).rstrip(" " + TAB) for ln in lines])
    return (LF.join(gen_lines) + LF).encode("utf-8") if gen_lines else b""


def check_line_lengths(repo_root: pathlib.Path, path: pathlib.Path, formatted: bytes) -> list[str]:
    """Validate strict Fortran free-form and fixed-form line length constraints."""
    ext = path.suffix.lower()
    try:
        rel = path.resolve().relative_to(repo_root.resolve()).as_posix()
    except ValueError:
        rel = path.name

    violations: list[str] = []
    lines = formatted.decode("utf-8").splitlines()
    if ext in FREE_FORTRAN_EXTS:
        for idx, line in enumerate(lines, 1):
            if len(line) > 132:
                violations.append(f"{rel}:{idx}: Fortran line length {len(line)} exceeds 132")
    elif ext in FIXED_FORTRAN_EXTS:
        for idx, line in enumerate(lines, 1):
            if len(line) > 80:
                violations.append(f"{rel}:{idx}: Fixed-form card length {len(line)} exceeds 80")
    return violations


def run_ruff_formatter(py_files: list[pathlib.Path], check_only: bool) -> int:
    """Invoke `ruff format` (or `ruff format --check`) on Python files when available."""
    if not py_files:
        return 0
    ruff_bin = shutil.which("ruff")
    if not ruff_bin:
        return 0
    cmd = [ruff_bin, "format", "--no-cache", "--line-length", "120"]
    if check_only:
        cmd.append("--check")
    cmd.extend(str(p) for p in py_files)
    proc = subprocess.run(cmd, check=False, capture_output=True, text=True)
    if proc.returncode != 0:
        msg = (proc.stdout + LF + proc.stderr).strip()
        if msg:
            print(msg, file=sys.stderr)
    return proc.returncode


def process_files(repo_root: pathlib.Path, paths: list[pathlib.Path], check_only: bool) -> tuple[int, list[str]]:
    """Format (`check_only=False`) or verify (`check_only=True`) `paths` under `repo_root`."""
    diagnostics: list[str] = []
    modified_count = 0

    py_files = [p for p in paths if p.suffix.lower() in PYTHON_EXTS and p.is_file()]
    if not check_only:
        ruff_rc = run_ruff_formatter(py_files, check_only=False)
        if ruff_rc != 0:
            diagnostics.append("ruff format --fix failed on Python sources")

    for path in paths:
        if not path.is_file():
            continue
        try:
            rel = path.resolve().relative_to(repo_root.resolve()).as_posix()
        except ValueError:
            rel = str(path)

        raw = path.read_bytes()
        try:
            formatted = format_text_for_path(path, raw)
        except UnicodeDecodeError as exc:
            diagnostics.append(f"{rel}: invalid UTF-8 encoding ({exc})")
            continue

        if formatted != raw:
            if check_only:
                diagnostics.append(f"{rel}: file is not canonically formatted (run format_code.py --fix)")
            else:
                path.write_bytes(formatted)
                modified_count += 1
                print(f"Formatted {rel}")

        if check_only:
            diagnostics.extend(check_line_lengths(repo_root, path, formatted))

    if check_only:
        ruff_rc = run_ruff_formatter(py_files, check_only=True)
        if ruff_rc != 0:
            diagnostics.append("Python sources failed ruff format --check")

    exit_code = 1 if diagnostics else 0
    if not check_only and modified_count > 0:
        print(f"Auto-formatted {modified_count} file(s).")
    return exit_code, diagnostics


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    """Parse CLI options for `format_code.py`."""
    parser = argparse.ArgumentParser(description="Auto-format (--fix) or verify (--check) repository sources.")
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--fix", action="store_true", help="Format files in place.")
    mode.add_argument("--check", action="store_true", help="Verify files without modifying them.")
    parser.add_argument(
        "--root",
        type=pathlib.Path,
        default=pathlib.Path(__file__).resolve().parent.parent,
        help="Repository root directory.",
    )
    parser.add_argument("files", nargs="*", type=pathlib.Path, help="Optional explicit file list.")
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    """Entry point for `format_code.py`."""
    args = parse_args(argv)
    repo_root = args.root.resolve()
    targets = [p.resolve() for p in args.files] if args.files else discover_repo_files(repo_root)
    rc, errors = process_files(repo_root, targets, check_only=args.check)
    for err in errors:
        print(f"FORMAT ERROR: {err}", file=sys.stderr)
    return rc


if __name__ == "__main__":
    sys.exit(main())
