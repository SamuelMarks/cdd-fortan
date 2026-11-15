#!/usr/bin/env python3
"""Pre-commit task runner for CMake builds, static/doc linting, CTest execution, and push safety."""

from __future__ import annotations

import argparse
import json
import os
import pathlib
import py_compile
import re
import shutil
import subprocess
import sys
import tempfile

NUL = chr(0)
LF = chr(10)


def check_repo_hygiene(repo_root: pathlib.Path, max_kb: int = 500) -> list[str]:
    """Check merge conflicts, case collisions, symlinks, file sizes, JSON/TOML, and shebangs."""
    errors: list[str] = []
    proc = subprocess.run(
        ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"],
        cwd=repo_root,
        check=True,
        capture_output=True,
    )
    rels = sorted({entry for entry in proc.stdout.decode("utf-8").split(NUL) if entry})

    seen_lower: dict[str, str] = {}
    conflict_prefixes = ("<" * 7 + " ", "=" * 7, ">" * 7 + " ")

    for rel in rels:
        low = rel.lower()
        if low in seen_lower and seen_lower[low] != rel:
            errors.append(f"{rel}: case-insensitive filename collision with {seen_lower[low]}")
        else:
            seen_lower[low] = rel

        path = repo_root / rel
        if path.is_symlink() and not path.exists():
            errors.append(f"{rel}: broken symbolic link")
            continue
        if not path.is_file():
            continue

        size_bytes = path.stat().st_size
        if size_bytes > max_kb * 1024:
            errors.append(f"{rel}: file size {size_bytes // 1024} KiB exceeds {max_kb} KiB limit")

        raw = path.read_bytes()
        is_exec = os.name != "nt" and bool(path.stat().st_mode & 0o111)
        if is_exec and not raw.startswith(b"#!"):
            errors.append(f"{rel}: executable file is missing a #! shebang")

        try:
            text = raw.decode("utf-8")
        except UnicodeDecodeError:
            continue

        for lineno, line in enumerate(text.splitlines(), 1):
            if (
                line.startswith(conflict_prefixes[0])
                or line == conflict_prefixes[1]
                or line.startswith(conflict_prefixes[2])
            ):
                errors.append(f"{rel}:{lineno}: unresolved git merge-conflict marker")

        ext = path.suffix.lower()
        if ext == ".json":
            try:
                json.loads(text)
            except json.JSONDecodeError as exc:
                errors.append(f"{rel}: invalid JSON ({exc})")
        elif ext == ".toml":
            for lineno, line in enumerate(text.splitlines(), 1):
                stripped = line.split("#", 1)[0].strip()
                if not stripped or (stripped.startswith("[") and stripped.endswith("]")):
                    continue
                if "=" not in stripped:
                    errors.append(f"{rel}:{lineno}: invalid TOML entry '{stripped}'")
    return errors


def check_library_no_stop(repo_root: pathlib.Path) -> list[str]:
    """Verify no Fortran module under `src/` contains `STOP` or `ERROR STOP` statements."""
    errors: list[str] = []
    stop_pat = re.compile(r"^[ ]*(error[ ]+stop|stop)(?:[^a-z0-9_]|$)", re.IGNORECASE)
    src_dir = repo_root / "src"
    if not src_dir.is_dir():
        return errors
    for f90_path in sorted(src_dir.rglob("*.f90")):
        rel = f90_path.relative_to(repo_root).as_posix()
        for lineno, line in enumerate(f90_path.read_text(encoding="utf-8").splitlines(), 1):
            code_part = line.split("!", 1)[0]
            if stop_pat.search(code_part):
                errors.append(f"{rel}:{lineno}: prohibited STOP/ERROR STOP in library module")
    return errors


def find_fortran_comment_offset(line: str) -> int:
    """Return 0-based index of the first unquoted `!` comment marker in `line`, or -1."""
    in_single = False
    in_double = False
    for idx, ch in enumerate(line):
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif ch == "!" and not in_single and not in_double:
            return idx
    return -1


def check_fortran_cpp_comments(repo_root: pathlib.Path) -> list[str]:
    """Verify `.f90` comments contain no unclosed `/*` tokens that break Ninja `-cpp -E`."""
    errors: list[str] = []
    for subdir in ("src", "app", "test"):
        base = repo_root / subdir
        if not base.is_dir():
            continue
        for f90_path in sorted(base.rglob("*.f90")):
            rel = f90_path.relative_to(repo_root).as_posix()
            for lineno, line in enumerate(f90_path.read_text(encoding="utf-8").splitlines(), 1):
                comment_pos = find_fortran_comment_offset(line)
                if comment_pos < 0:
                    continue
                comment = line[comment_pos:]
                open_idx = comment.find("/*")
                if open_idx != -1 and "*/" not in comment[open_idx + 2 :]:
                    errors.append(f"{rel}:{lineno}: unclosed '/*' in Fortran comment breaks CMake Ninja '-cpp -E'")
    return errors


def check_fpm_manifest_schema(repo_root: pathlib.Path) -> list[str]:
    """Validate `fpm.toml` `[profiles]` feature arrays and `[features]` subtable keys."""
    fpm_path = repo_root / "fpm.toml"
    if not fpm_path.is_file():
        return []
    errors: list[str] = []
    section = ""
    for lineno, raw_line in enumerate(fpm_path.read_text(encoding="utf-8").splitlines(), 1):
        stripped = raw_line.split("#", 1)[0].strip()
        if not stripped:
            continue
        if stripped.startswith("[") and stripped.endswith("]"):
            section = stripped.strip("[]").strip()
            if section.startswith("profiles."):
                errors.append(
                    f"fpm.toml:{lineno}: [{section}] subtable is invalid in fpm >= 0.13; "
                    "map [profiles] keys to feature arrays"
                )
            continue
        if "=" not in stripped:
            continue
        key, val = (part.strip() for part in stripped.split("=", 1))
        if section == "profiles" and not (val.startswith("[") and val.endswith("]")):
            errors.append(f"fpm.toml:{lineno}: profile '{key}' must be an array of feature names")
        elif (section == "features" or section.startswith("features.")) and key == "compiler":
            errors.append(
                f"fpm.toml:{lineno}: 'compiler' key is invalid in [{section}]; "
                f"use a compiler subtable such as [{section}.gfortran]"
            )
    return errors


def check_manifest_versions(repo_root: pathlib.Path) -> list[str]:
    """Verify version strings agree across `CMakeLists.txt`, `fpm.toml`, `Doxyfile`, and SDK."""
    cmake_txt = (repo_root / "CMakeLists.txt").read_text(encoding="utf-8")
    fpm_txt = (repo_root / "fpm.toml").read_text(encoding="utf-8")
    dox_txt = (repo_root / "Doxyfile").read_text(encoding="utf-8")
    sdk_txt = (repo_root / "src" / "cdd_fortran.f90").read_text(encoding="utf-8")

    cmake_m = re.search(r"project[ ]*\([ ]*cdd_fortran[ ]+VERSION[ ]+([0-9]+[.][0-9]+[.][0-9]+)", cmake_txt)
    fpm_m = re.search(r'^version[ ]*=[ ]*"([^"]+)"', fpm_txt, re.MULTILINE)
    dox_m = re.search(r'^PROJECT_NUMBER[ ]*=[ ]*"([^"]+)"', dox_txt, re.MULTILINE)
    sdk_m = re.search(r'CDD_FORTRAN_VERSION[ ]*=[ ]*"([^"]+)"', sdk_txt)

    versions = {
        "CMakeLists.txt": cmake_m.group(1) if cmake_m else None,
        "fpm.toml": fpm_m.group(1) if fpm_m else None,
        "Doxyfile": dox_m.group(1) if dox_m else None,
        "src/cdd_fortran.f90": sdk_m.group(1) if sdk_m else None,
    }
    if None in versions.values() or len(set(versions.values())) != 1:
        return [f"Version mismatch across project manifests: {versions}"]
    return []


def ensure_cmake_configured(repo_root: pathlib.Path, build_dir: pathlib.Path) -> None:
    """Configure `build_dir` with CMake (preferring Ninja and coverage instrumentation)."""
    cmake_bin = shutil.which("cmake")
    if not cmake_bin:
        msg = "cmake executable not found on PATH."
        raise RuntimeError(msg)

    want_ninja = shutil.which("ninja") is not None
    want_cov = shutil.which("lcov") is not None
    cache_file = build_dir / "CMakeCache.txt"
    needs_configure = not cache_file.is_file()
    if cache_file.is_file():
        cache_txt = cache_file.read_text(encoding="utf-8", errors="replace")
        if want_ninja and "CMAKE_GENERATOR:INTERNAL=Ninja" not in cache_txt:
            cache_file.unlink()
            shutil.rmtree(build_dir / "CMakeFiles", ignore_errors=True)
            needs_configure = True
        elif want_cov and "ENABLE_COVERAGE:BOOL=ON" not in cache_txt:
            needs_configure = True

    if needs_configure:
        cmd = [cmake_bin, "-S", str(repo_root), "-B", str(build_dir)]
        if want_ninja:
            cmd.extend(["-G", "Ninja"])
        if want_cov:
            cmd.append("-DENABLE_COVERAGE=ON")
        subprocess.run(cmd, cwd=repo_root, check=True)


def run_build(repo_root: pathlib.Path, build_dir: pathlib.Path) -> int:
    """Configure and compile all SDK, CLI, and test targets with strict `-Werror` flags."""
    ensure_cmake_configured(repo_root, build_dir)
    cmake_bin = shutil.which("cmake")
    if not cmake_bin:
        print("BUILD ERROR: cmake not found.", file=sys.stderr)
        return 1
    subprocess.run([cmake_bin, "--build", str(build_dir), "--parallel"], cwd=repo_root, check=True)
    return 0


def run_lint(repo_root: pathlib.Path, build_dir: pathlib.Path) -> int:
    """Run Fortran library rules, manifest sync, Python/Shell linters, and Doxygen/AST audits."""
    errors = (
        check_library_no_stop(repo_root)
        + check_fortran_cpp_comments(repo_root)
        + check_manifest_versions(repo_root)
        + check_fpm_manifest_schema(repo_root)
    )
    if errors:
        for err in errors:
            print(f"LINT ERROR: {err}", file=sys.stderr)
        return 1

    py_files = sorted((repo_root / "scripts").glob("*.py")) + sorted((repo_root / "test").glob("*.py"))
    with tempfile.TemporaryDirectory() as pyc_tmp:
        for idx, py_file in enumerate(py_files):
            cfile = pathlib.Path(pyc_tmp) / f"chk_{idx}.pyc"
            py_compile.compile(str(py_file), cfile=str(cfile), doraise=True)

    ruff_bin = shutil.which("ruff")
    if ruff_bin and py_files:
        subprocess.run(
            [
                ruff_bin,
                "check",
                "--no-cache",
                "--select",
                "E,F,W,I,B,UP,D",
                "--ignore",
                "D203,D213",
                "--line-length",
                "120",
                *(str(p) for p in py_files),
            ],
            cwd=repo_root,
            check=True,
        )

    shellcheck_bin = shutil.which("shellcheck")
    sh_files = sorted((repo_root / "scripts").glob("*.sh"))
    if shellcheck_bin and sh_files:
        subprocess.run([shellcheck_bin, *(str(p) for p in sh_files)], cwd=repo_root, check=True)

    actionlint_bin = shutil.which("actionlint")
    if actionlint_bin and (repo_root / ".github" / "workflows").is_dir():
        subprocess.run([actionlint_bin], cwd=repo_root, check=True)

    ensure_cmake_configured(repo_root, build_dir)
    cmake_bin = shutil.which("cmake")
    if cmake_bin and shutil.which("doxygen"):
        subprocess.run([cmake_bin, "--build", str(build_dir), "--target", "docs"], cwd=repo_root, check=True)
    return 0


def run_hygiene(repo_root: pathlib.Path) -> int:
    """Run repository hygiene checks and report any errors."""
    errors = check_repo_hygiene(repo_root)
    if errors:
        for err in errors:
            print(f"HYGIENE ERROR: {err}", file=sys.stderr)
        return 1
    return 0


def run_test(repo_root: pathlib.Path, build_dir: pathlib.Path) -> int:
    """Zero coverage counters (when `lcov` is available) and run the full `CTest` suite."""
    ensure_cmake_configured(repo_root, build_dir)
    lcov_bin = shutil.which("lcov")
    if lcov_bin:
        subprocess.run([lcov_bin, "--zerocounters", "--directory", str(build_dir), "--quiet"], check=True)

    ctest_bin = shutil.which("ctest")
    if not ctest_bin:
        print("TEST ERROR: ctest not found on PATH.", file=sys.stderr)
        return 1
    subprocess.run([ctest_bin, "--test-dir", str(build_dir), "--output-on-failure"], cwd=repo_root, check=True)
    return 0


def run_block_push() -> int:
    """Reject `git push` unconditionally per cdd-fortran safety policy."""
    print("ERROR: 'git push' is strictly prohibited in cdd-fortran.", file=sys.stderr)
    return 1


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    """Parse CLI subcommand for `precommit_tasks.py`."""
    parser = argparse.ArgumentParser(
        description="Execute pre-commit hygiene, build, lint, test, or push-blocker hooks."
    )
    parser.add_argument(
        "task",
        choices=("hygiene", "build", "lint", "test", "block-push"),
        help="Task to execute.",
    )
    parser.add_argument(
        "--root",
        type=pathlib.Path,
        default=pathlib.Path(__file__).resolve().parent.parent,
        help="Repository root directory.",
    )
    parser.add_argument(
        "--build-dir",
        type=pathlib.Path,
        default=pathlib.Path("build"),
        help="CMake build directory.",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    """Entry point for `precommit_tasks.py`."""
    args = parse_args(argv)
    if args.task == "block-push":
        return run_block_push()

    repo_root = args.root.resolve()
    if args.task == "hygiene":
        return run_hygiene(repo_root)

    build_dir = args.build_dir if args.build_dir.is_absolute() else (repo_root / args.build_dir)
    try:
        if args.task == "build":
            return run_build(repo_root, build_dir)
        if args.task == "lint":
            return run_lint(repo_root, build_dir)
        return run_test(repo_root, build_dir)
    except (RuntimeError, subprocess.CalledProcessError, py_compile.PyCompileError) as exc:
        print(f"PRE-COMMIT {args.task.upper()} FAILED: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
