#!/usr/bin/env python3
"""Unit tests for pre-commit code formatting, task runner, and README coverage shield hooks."""

from __future__ import annotations

import pathlib
import sys
import tempfile
import unittest
from unittest import mock

REPO_ROOT = pathlib.Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO_ROOT / "scripts"))

import format_code  # noqa: E402
import precommit_tasks  # noqa: E402
import update_coverage_shields  # noqa: E402

TAB = chr(9)
LF = chr(10)
CR = chr(13)
CRLF = CR + LF


class TestFormatCode(unittest.TestCase):
    """Verify `--fix` auto-formatting and `--check` validation across project file types."""

    def test_expand_tabs_outside_quotes_preserves_string_literals(self) -> None:
        """Tabs outside quotes expand while tabs inside single or double quotes stay untouched."""
        src = f'{TAB}call foo("a{TAB}b", {chr(39)}c{TAB}d{chr(39)})'
        self.assertEqual(
            format_code.expand_tabs_outside_quotes(src, 2),
            f'  call foo("a{TAB}b", {chr(39)}c{TAB}d{chr(39)})',
        )

    def test_free_fortran_fix_then_recheck_roundtrip(self) -> None:
        """Unformatted `.f90` is rejected by `--check`, fixed by `--fix`, then passes `--check`."""
        with tempfile.TemporaryDirectory() as tmp:
            root = pathlib.Path(tmp)
            src_dir = root / "src"
            src_dir.mkdir()
            f90 = src_dir / "sample.f90"
            messy = (
                f"!>@file sample.f90   {CRLF}"
                f"!>@brief Demo.{CRLF}"
                f"{CRLF}{CRLF}{CRLF}{CRLF}"
                f"module sample{CRLF}"
                f"{TAB}implicit none   {CRLF}"
                f"  type :: box_t{CRLF}"
                f"    integer :: x !<@brief Field.{CRLF}"
                f"  end type box_t{CRLF}"
                f"end module sample{CRLF}{CRLF}"
            ).encode()
            f90.write_bytes(messy)

            rc_before, errs_before = format_code.process_files(root, [f90], check_only=True)
            self.assertEqual(rc_before, 1)
            self.assertTrue(any("not canonically formatted" in e for e in errs_before))

            rc_fix, errs_fix = format_code.process_files(root, [f90], check_only=False)
            self.assertEqual(rc_fix, 0)
            self.assertEqual(errs_fix, [])

            rc_after, errs_after = format_code.process_files(root, [f90], check_only=True)
            self.assertEqual(rc_after, 0)
            self.assertEqual(errs_after, [])
            cleaned = f90.read_text(encoding="utf-8")
            self.assertIn("!> @file sample.f90", cleaned)
            self.assertIn("!< @brief Field.", cleaned)
            self.assertNotIn(CR, cleaned)
            self.assertNotIn(TAB, cleaned)

    def test_fixed_fortran_markdown_makefile_and_windows_scripts(self) -> None:
        """Special trailing-space, tab, and CRLF rules follow `.editorconfig` per extension."""
        f77_raw = f"      INTEGER X     {CRLF}{CRLF}".encode()
        f77_out = format_code.format_text_for_path(pathlib.Path("deck.f"), f77_raw)
        self.assertEqual(f77_out, f"      INTEGER X     {LF}".encode())

        md_raw = f"# Title   {LF}Line break  {LF}Single space {LF}".encode()
        md_out = format_code.format_text_for_path(pathlib.Path("README.md"), md_raw)
        self.assertEqual(md_out, f"# Title  {LF}Line break  {LF}Single space{LF}".encode())

        mk_raw = f"all:{LF}{TAB}echo ok   {LF}".encode()
        mk_out = format_code.format_text_for_path(pathlib.Path("Makefile"), mk_raw)
        self.assertEqual(mk_out, f"all:{LF}{TAB}echo ok{LF}".encode())

        bat_raw = f"@echo off   {LF}rem done{LF}".encode()
        bat_out = format_code.format_text_for_path(pathlib.Path("run.bat"), bat_raw)
        self.assertEqual(bat_out, f"@echo off{CRLF}rem done{CRLF}".encode())
        self.assertEqual(format_code.format_text_for_path(pathlib.Path("empty.txt"), b""), b"")

    def test_line_length_checks_detect_overlong_fortran_lines(self) -> None:
        """Free-form >132 in `src/` and fixed-form >80 trigger diagnostic errors."""
        with tempfile.TemporaryDirectory() as tmp:
            root = pathlib.Path(tmp)
            (root / "src").mkdir()
            long_f90 = root / "src" / "wide.f90"
            body = ("! " + ("x" * 135) + LF).encode("utf-8")
            long_f90.write_bytes(body)
            errs = format_code.check_line_lengths(root, long_f90, body)
            self.assertEqual(len(errs), 1)
            self.assertIn("exceeds 132", errs[0])

            long_f77 = root / "wide.for"
            card = ((" " * 6) + ("A" * 78) + LF).encode("utf-8")
            errs77 = format_code.check_line_lengths(root, long_f77, card)
            self.assertEqual(len(errs77), 1)
            self.assertIn("exceeds 80", errs77[0])


class TestUpdateCoverageShields(unittest.TestCase):
    """Verify badge color tiers, metric parsers, and README.md shield insertion & updates."""

    def test_badge_colors_and_percentage_formatting(self) -> None:
        """Coverage percentages map to expected Shields.io colors and concise labels."""
        self.assertEqual(update_coverage_shields.badge_color_for_percentage(100.0), "brightgreen")
        self.assertEqual(update_coverage_shields.badge_color_for_percentage(94.2), "green")
        self.assertEqual(update_coverage_shields.badge_color_for_percentage(85.0), "yellowgreen")
        self.assertEqual(update_coverage_shields.badge_color_for_percentage(75.0), "yellow")
        self.assertEqual(update_coverage_shields.badge_color_for_percentage(62.0), "orange")
        self.assertEqual(update_coverage_shields.badge_color_for_percentage(40.0), "red")
        self.assertEqual(update_coverage_shields.format_percentage_text(100.0), "100%")
        self.assertEqual(update_coverage_shields.format_percentage_text(98.46), "98.5%")
        with self.assertRaises(ValueError):
            update_coverage_shields.build_shield_markdown("invalid", 100.0)

    def test_upsert_shields_into_missing_fresh_and_existing_readme(self) -> None:
        """Shields are added when absent, updated when stale, and idempotent when current."""
        with tempfile.TemporaryDirectory() as tmp:
            readme = pathlib.Path(tmp) / "README.md"

            # 1. Missing README.md -> created with doc coverage shield
            self.assertTrue(update_coverage_shields.update_readme_file(readme, "doc", 100.0))
            txt1 = readme.read_text(encoding="utf-8")
            self.assertIn("![doc coverage](https://img.shields.io/badge/doc%20coverage-100%25-brightgreen)", txt1)

            # 2. Idempotent second run -> returns False (no file write)
            self.assertFalse(update_coverage_shields.update_readme_file(readme, "doc", 100.0))

            # 3. Add test coverage shield adjacent to existing doc coverage shield
            self.assertTrue(update_coverage_shields.update_readme_file(readme, "test", 92.5))
            txt2 = readme.read_text(encoding="utf-8")
            self.assertIn("![test coverage](https://img.shields.io/badge/test%20coverage-92.5%25-green)", txt2)

            # 4. Update existing test coverage shield from 92.5% -> 100%
            self.assertTrue(update_coverage_shields.update_readme_file(readme, "test", 100.0))
            txt3 = readme.read_text(encoding="utf-8")
            self.assertIn("![test coverage](https://img.shields.io/badge/test%20coverage-100%25-brightgreen)", txt3)
            self.assertNotIn("92.5%25", txt3)

            # 5. Insert after H1 when no shields exist yet
            plain = f"# Demo Project{LF}Overview paragraph.{LF}"
            inserted = update_coverage_shields.upsert_shield_in_markdown(plain, "doc", 100.0)
            self.assertTrue(inserted.startswith(f"# Demo Project{LF}{LF}![doc coverage]"))

            # 6. Insert at top when Markdown has no H1 heading
            no_h1 = f"Plain paragraph without heading.{LF}"
            prepended = update_coverage_shields.upsert_shield_in_markdown(no_h1, "test", 100.0)
            self.assertTrue(prepended.startswith("![test coverage]"))

    def test_parse_doc_audit_and_lcov_summary_outputs(self) -> None:
        """Parsers extract exact entity & branch/line/function counts and detect violations."""
        audit_ok = (
            "[doc_coverage_audit] 100% check: files=39 containers=39 types=39 fields=224 procedures=353 args=1018"
        )
        pct, doc_cnt, tot_cnt = update_coverage_shields.parse_doc_audit_output(audit_ok, "")
        self.assertAlmostEqual(pct, 100.0)
        self.assertEqual(doc_cnt, 1712)
        self.assertEqual(tot_cnt, 1712)

        pct_bad, doc_bad, _ = update_coverage_shields.parse_doc_audit_output(
            audit_ok,
            f"DOC COVERAGE VIOLATION [m.f90 :: f]: missing @return{LF}",
        )
        self.assertEqual(doc_bad, 1711)
        self.assertLess(pct_bad, 100.0)

        with self.assertRaises(ValueError):
            update_coverage_shields.parse_doc_audit_output("unexpected output", "")

        lcov_out = (
            f"Summary coverage rate:{LF}"
            f"  lines.......: 100.0% (8268 of 8268 lines){LF}"
            f"  functions...: 100.0% (399 of 399 functions){LF}"
            f"  branches....: 100.0% (3648 of 3648 branches){LF}"
        )
        strict_pct, metrics = update_coverage_shields.parse_lcov_summary_output(lcov_out)
        self.assertAlmostEqual(strict_pct, 100.0)
        self.assertEqual(metrics["branches"][1], 3648)

        with self.assertRaises(ValueError):
            update_coverage_shields.parse_lcov_summary_output("lines...: 100.0% (10 of 10 lines)")


class TestPrecommitTasks(unittest.TestCase):
    """Verify library `STOP` detection, manifest version checks, and `block-push` safety hook."""

    def test_library_stop_and_manifest_version_checks(self) -> None:
        """`STOP` in `src/` and mismatched manifest versions are flagged accurately."""
        self.assertEqual(precommit_tasks.check_library_no_stop(REPO_ROOT), [])
        self.assertEqual(precommit_tasks.check_manifest_versions(REPO_ROOT), [])

        with tempfile.TemporaryDirectory() as tmp:
            root = pathlib.Path(tmp)
            (root / "src").mkdir()
            (root / "src" / "bad.f90").write_text(
                f"module bad{LF}contains{LF}  subroutine s(){LF}"
                f"    error stop 1{LF}  end subroutine s{LF}end module bad{LF}",
                encoding="utf-8",
            )
            errs = precommit_tasks.check_library_no_stop(root)
            self.assertEqual(len(errs), 1)
            self.assertIn("prohibited STOP/ERROR STOP", errs[0])

            (root / "CMakeLists.txt").write_text(
                "project(cdd_fortran VERSION 0.1.0 LANGUAGES Fortran)" + LF, encoding="utf-8"
            )
            (root / "fpm.toml").write_text('version = "0.2.0"' + LF, encoding="utf-8")
            (root / "Doxyfile").write_text('PROJECT_NUMBER = "0.1.0"' + LF, encoding="utf-8")
            (root / "src" / "cdd_fortran.f90").write_text('CDD_FORTRAN_VERSION = "0.1.0"' + LF, encoding="utf-8")
            ver_errs = precommit_tasks.check_manifest_versions(root)
            self.assertEqual(len(ver_errs), 1)
            self.assertIn("Version mismatch", ver_errs[0])

    def test_block_push_always_returns_nonzero(self) -> None:
        """`block-push` unconditionally exits 1 to prevent accidental `git push`."""
        with mock.patch("sys.stderr"):
            self.assertEqual(precommit_tasks.main(["block-push"]), 1)


if __name__ == "__main__":
    unittest.main()
