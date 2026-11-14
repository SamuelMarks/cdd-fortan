!> @file test_errors_and_strings.f90
!> @brief Comprehensive 100% branch-coverage unit tests for cdd_kinds, cdd_errors, cdd_string, and cdd_fs.

!> @brief Execute all Milestone 1 core unit tests.
!> @return Program exit status (0 on pass).
program test_errors_and_strings
  use cdd_kinds, only: i8, i16, i32, i64, r32, r64, r128, lk, c_k
  use cdd_errors, only: CDD_ERROR_MSG_MAX, CDD_OK, CDD_ERR_INVALID_OPTION, CDD_ERR_IO_READ, &
    CDD_ERR_IO_WRITE, CDD_ERR_JSON_LEX, CDD_ERR_JSON_PARSE, CDD_ERR_JSON_SCHEMA, &
    CDD_ERR_JSON_INFER, CDD_ERR_FORTRAN_LEX, CDD_ERR_FORTRAN_PARSE, CDD_ERR_UDT_NOT_FOUND, &
    CDD_ERR_DIALECT_CONSTRAINT, CDD_ERR_IDENTIFIER_COLLISION, CDD_ERR_CLI_USAGE, &
    CDD_ERR_BUFFER_OVERFLOW, cdd_error_t, cdd_error_clear, cdd_error_set, &
    cdd_error_set_simple, cdd_has_error, cdd_error_code_name, cdd_error_format
  use cdd_string, only: CDD_STR_MAX, CDD_BUF_MAX, TERM_NONE, TERM_LF, TERM_CRLF, &
    cdd_string_t, cdd_buffer_t, cdd_line_slice_t, str_clear, str_set, str_eq, str_eq_nocase, &
    char_to_upper, char_to_lower, is_ascii_alpha, is_ascii_digit, is_ascii_alnum, &
    is_ascii_space, is_hex_digit, hex_digit_val, to_upper_buf, to_lower_buf, trim_bounds, &
    starts_with, ends_with, buf_init, buf_init_bounded, buf_clear, buf_append_char, &
    buf_append_str, buf_append_i64, buf_equals, split_lines_lossless, escape_json_to_buf, &
    unescape_json_to_buf, parse_i64_strict, format_i64_to_str, parse_r64_strict, format_r64_to_str
  use cdd_fs, only: CDD_MOCK_MAX_FILES, cdd_fs_t, cdd_os_fs_t, cdd_mock_fs_t, cdd_mock_entry_t, &
    fs_init_os, fs_init_mock, fs_read_text_file, fs_write_text_file, fs_file_exists, &
    fs_read_stdin, fs_write_stdout, mock_fs_init, mock_fs_put, mock_fs_get
  use test_harness, only: test_state_t, th_init, th_check, th_eq_i32, th_eq_i64, &
    th_near_r64, th_eq_str, th_touch_vtable, th_finish
  implicit none (type, external)

  type(test_state_t) :: st

  call th_init(st)
  call run_kinds_and_errors_tests(st)
  call run_string_and_buffer_tests(st)
  call run_json_escape_and_numeric_tests(st)
  call run_filesystem_tests(st)
  call th_finish(st, "test_errors_and_strings")

contains

  !> @brief Exercise all error codes, truncation boundaries, and formatting branches.
  !> @param[inout] st Test state accumulator.
  subroutine run_kinds_and_errors_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_error_t) :: err
    character(len=512, kind=c_k) :: long_msg
    character(len=256, kind=c_k) :: fmt_buf
    character(len=8, kind=c_k) :: tiny_buf
    character(len=48, kind=c_k) :: cname
    integer(i32) :: flen, clen, code_idx
    integer(i8) :: v8
    integer(i16) :: v16
    real(r32) :: f32
    real(r128) :: f128

    v8 = 1_i8
    v16 = 2_i16
    f32 = 1.0_r32
    f128 = 2.0_r128
    call th_check(st, int(v8, i32) + int(v16, i32) == 3_i32 .and. f32 < real(f128, r32), "kinds")

    call th_touch_vtable(err)
    call cdd_error_clear(err)
    call th_check(st, .not. cdd_has_error(err), "clean error is false")
    call cdd_error_format(err, fmt_buf, flen)
    call th_eq_str(st, fmt_buf(1:flen), "OK", "format OK")

    do code_idx = 0_i32, 15_i32
      call cdd_error_code_name(code_idx, cname, clen)
      call th_check(st, clen > 0_i32, "error code name non-empty")
    end do
    call th_eq_i32(st, CDD_ERR_INVALID_OPTION + CDD_ERR_IO_READ + CDD_ERR_IO_WRITE + &
      CDD_ERR_JSON_LEX + CDD_ERR_JSON_PARSE + CDD_ERR_JSON_SCHEMA + CDD_ERR_JSON_INFER + &
      CDD_ERR_FORTRAN_LEX + CDD_ERR_FORTRAN_PARSE + CDD_ERR_UDT_NOT_FOUND + &
      CDD_ERR_DIALECT_CONSTRAINT + CDD_ERR_IDENTIFIER_COLLISION + CDD_ERR_CLI_USAGE + &
      CDD_ERR_BUFFER_OVERFLOW, 105_i32, "sum of error codes")

    ! Format with line + col + message + hint
    call cdd_error_set(err, CDD_ERR_JSON_PARSE, "bad token", 12_i32, 34_i32, 100_i32, 105_i32, "fix syntax")
    call th_check(st, cdd_has_error(err), "has_error true")
    call cdd_error_format(err, fmt_buf, flen)
    call th_eq_str(st, fmt_buf(1:flen), &
      "CDD_ERR_JSON_PARSE (line 12, col 34): bad token [hint: fix syntax]", "full error format")

    ! Format into tiny buffer to cover append_raw cap truncation branch
    call cdd_error_format(err, tiny_buf, flen)
    call th_eq_i32(st, flen, 8_i32, "tiny error format clamped")

    ! Format with line > 0 and col == 0, empty message, empty hint
    call cdd_error_set(err, CDD_ERR_IO_READ, "", 5_i32, 0_i32, 0_i32, 0_i32, "")
    call cdd_error_format(err, fmt_buf, flen)
    call th_eq_str(st, fmt_buf(1:flen), "CDD_ERR_IO_READ (line 5)", "line-only no-msg format")

    ! Overlong message and hint (> CDD_ERROR_MSG_MAX)
    long_msg = repeat("X", 512)
    call cdd_error_set(err, CDD_ERR_CLI_USAGE, long_msg, 0_i32, 0_i32, 0_i32, 0_i32, long_msg)
    call th_eq_i32(st, err%message_len, CDD_ERROR_MSG_MAX, "msg clamped")
    call th_eq_i32(st, err%hint_len, CDD_ERROR_MSG_MAX, "hint clamped")

    call cdd_error_set_simple(err, CDD_ERR_UDT_NOT_FOUND, "missing type")
    call cdd_error_format(err, fmt_buf, flen)
    call th_eq_str(st, fmt_buf(1:flen), "CDD_ERR_UDT_NOT_FOUND: missing type", "simple format")
  end subroutine run_kinds_and_errors_tests

  !> @brief Exercise string comparisons, case helpers, trimming, buffers, and lossless line splitting.
  !> @param[inout] st Test state accumulator.
  subroutine run_string_and_buffer_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_string_t) :: s
    type(cdd_buffer_t) :: b
    type(cdd_line_slice_t) :: lines(4), small_lines(1)
    type(cdd_error_t) :: err
    character(len=512, kind=c_k) :: over_str
    character(len=4, kind=c_k) :: short_dst
    character(len=16, kind=c_k) :: case_dst
    integer(i32) :: out_n, lo, hi, lcount
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=1, kind=c_k), parameter :: CR = achar(13, kind=c_k)

    call th_touch_vtable(s)
    call th_touch_vtable(b)
    call th_touch_vtable(lines(1))

    call str_clear(s)
    call th_eq_i32(st, s%len, 0_i32, "str_clear")
    call str_set(s, "Hello_World", err)
    call th_eq_i32(st, err%code, CDD_OK, "str_set ok")
    call th_eq_str(st, s%raw(1:s%len), "Hello_World", "str_set value")

    over_str = repeat("A", CDD_STR_MAX + 5_i32)
    call str_set(s, over_str, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "str_set overflow")

    call th_check(st, str_eq("abc", "abc"), "str_eq true")
    call th_check(st, .not. str_eq("abc", "ab"), "str_eq len diff")
    call th_check(st, .not. str_eq("abc", "abd"), "str_eq byte diff")
    call th_check(st, str_eq_nocase("AbC_1", "aBc_1"), "str_eq_nocase true")
    call th_check(st, .not. str_eq_nocase("AbC", "Ab"), "str_eq_nocase len diff")
    call th_check(st, .not. str_eq_nocase("AbC", "AbD"), "str_eq_nocase char diff")

    call th_check(st, is_ascii_alpha("A") .and. is_ascii_alpha("z") .and. .not. is_ascii_alpha("5"), "alpha")
    call th_check(st, .not. is_ascii_alpha("@") .and. .not. is_ascii_alpha("["), "alpha boundaries")
    call th_check(st, is_ascii_digit("0") .and. is_ascii_digit("9") .and. .not. is_ascii_digit("/"), "digit")
    call th_check(st, is_ascii_alnum("A") .and. is_ascii_alnum("7") .and. .not. is_ascii_alnum("-"), "alnum")
    call th_check(st, is_ascii_space(" ") .and. is_ascii_space(achar(9, c_k)) .and. &
                  is_ascii_space(LF) .and. is_ascii_space(CR) .and. .not. is_ascii_space("x"), "space")
    call th_check(st, is_hex_digit("0") .and. is_hex_digit("F") .and. is_hex_digit("e") .and. &
                  .not. is_hex_digit("g") .and. .not. is_hex_digit("/") .and. .not. is_hex_digit("@"), "hex")
    call th_eq_i32(st, hex_digit_val("9"), 9_i32, "hex 9")
    call th_eq_i32(st, hex_digit_val("B"), 11_i32, "hex B")
    call th_eq_i32(st, hex_digit_val("f"), 15_i32, "hex f")
    call th_eq_i32(st, hex_digit_val("z"), -1_i32, "hex invalid z")
    call th_eq_i32(st, hex_digit_val("!"), -1_i32, "hex invalid !")
    call th_eq_i32(st, hex_digit_val("["), -1_i32, "hex invalid [")

    call to_upper_buf("aBc!9", case_dst, out_n)
    call th_eq_str(st, case_dst(1:out_n), "ABC!9", "to_upper_buf")
    call to_upper_buf("abcdef", short_dst, out_n)
    call th_eq_i32(st, out_n, 4_i32, "to_upper_buf clamped")

    call to_lower_buf("AbC!9", case_dst, out_n)
    call th_eq_str(st, case_dst(1:out_n), "abc!9", "to_lower_buf")
    call to_lower_buf("ABCDEF", short_dst, out_n)
    call th_eq_i32(st, out_n, 4_i32, "to_lower_buf clamped")

    call trim_bounds("  hello  ", lo, hi)
    call th_eq_i32(st, lo, 3_i32, "trim lo")
    call th_eq_i32(st, hi, 7_i32, "trim hi")
    call trim_bounds("   ", lo, hi)
    call th_check(st, lo > hi, "trim blank")

    call th_check(st, starts_with("fortran", "for") .and. .not. starts_with("for", "fortran"), "starts_with")
    call th_check(st, ends_with("fortran", "ran") .and. .not. ends_with("ran", "fortran"), "ends_with")

    call buf_init(b)
    call th_eq_i32(st, b%cap, CDD_BUF_MAX, "buf_init cap")
    call buf_init_bounded(b, 0_i32, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "buf_init_bounded < 1")
    call buf_init_bounded(b, CDD_BUF_MAX + 1_i32, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "buf_init_bounded > max")

    call buf_init_bounded(b, 6_i32, err)
    call th_eq_i32(st, err%code, CDD_OK, "buf_init_bounded valid")
    call buf_append_char(b, "x", err)
    call buf_append_i64(b, 42_i64, err)
    call th_check(st, buf_equals(b, "x42"), "buf_equals x42")
    call buf_append_str(b, "abcd", err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "buf_append_str overflow")
    ! Subsequent appends while err is set should short-circuit
    call buf_append_char(b, "z", err)
    call buf_append_str(b, "zz", err)

    call buf_clear(b)
    call cdd_error_clear(err)
    call buf_init_bounded(b, 1_i32, err)
    call buf_append_char(b, "a", err)
    call buf_append_char(b, "b", err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "buf_append_char overflow")

    ! Lossless line splitting: empty, LF, CRLF, bare LF at start, trailing no-LF, overflow
    call split_lines_lossless("", 4_i32, lines, lcount, err)
    call th_eq_i32(st, lcount, 0_i32, "split empty")

    call split_lines_lossless(LF // "b" // CR // LF // "d" // LF // "c", 4_i32, lines, lcount, err)
    call th_eq_i32(st, lcount, 4_i32, "split 4 lines")
    call th_eq_i32(st, lines(1)%term_kind, TERM_LF, "line 1 LF")
    call th_eq_i32(st, lines(2)%term_kind, TERM_CRLF, "line 2 CRLF")
    call th_eq_i32(st, lines(3)%term_kind, TERM_LF, "line 3 non-empty LF")
    call th_eq_i32(st, lines(4)%term_kind, TERM_NONE, "line 4 NONE")

    call split_lines_lossless("a" // LF // "b", 1_i32, small_lines, lcount, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "split overflow")
  end subroutine run_string_and_buffer_tests

  !> @brief Exercise JSON escaping/unescaping (including UTF-16 surrogates) and strict i64/r64 codecs.
  !> @param[inout] st Test state accumulator.
  subroutine run_json_escape_and_numeric_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_buffer_t) :: b, u
    type(cdd_error_t) :: err
    character(len=1, kind=c_k), parameter :: BS = achar(92, kind=c_k)
    character(len=1, kind=c_k), parameter :: QT = achar(34, kind=c_k)
    integer(i64) :: iv
    real(r64) :: rv
    character(len=32, kind=c_k) :: nbuf
    integer(i32) :: nlen

    call buf_init(b)
    call cdd_error_clear(err)
    call escape_json_to_buf("", b, err)
    call th_eq_i32(st, b%len, 0_i32, "escape_json empty")
    call escape_json_to_buf(QT // BS // "/" // achar(8, c_k) // achar(12, c_k) // &
      achar(10, c_k) // achar(13, c_k) // achar(9, c_k) // achar(1, c_k) // "A", b, err)
    call th_eq_i32(st, err%code, CDD_OK, "escape_json ok")

    call buf_init(u)
    call unescape_json_to_buf(b%raw(1:b%len) // BS // "/" // &
      BS // "u0041" // BS // "u00E9" // BS // "u20AC" // BS // "uD83D" // BS // "uDE00", u, err)
    call th_eq_i32(st, err%code, CDD_OK, "unescape_json valid")

    ! Unescape error branches
    call buf_init(u); call cdd_error_clear(err)
    call unescape_json_to_buf(achar(10, c_k), u, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_LEX, "unescape control char")

    call buf_init(u); call cdd_error_clear(err)
    call unescape_json_to_buf(BS, u, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_LEX, "unescape trailing backslash")

    call buf_init(u); call cdd_error_clear(err)
    call unescape_json_to_buf(BS // "x", u, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_LEX, "unescape bad escape char")

    call buf_init(u); call cdd_error_clear(err)
    call unescape_json_to_buf(BS // "u12", u, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_LEX, "unescape short hex4")

    call buf_init(u); call cdd_error_clear(err)
    call unescape_json_to_buf(BS // "u12GZ", u, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_LEX, "unescape non-hex4")

    call buf_init(u); call cdd_error_clear(err)
    call unescape_json_to_buf(BS // "uDC00", u, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_LEX, "unescape lone low surrogate")

    call buf_init(u); call cdd_error_clear(err)
    call unescape_json_to_buf(BS // "uD83Dshort", u, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_LEX, "unescape truncated surrogate pair")

    call buf_init(u); call cdd_error_clear(err)
    call unescape_json_to_buf(BS // "uD83D_no_bs", u, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_LEX, "unescape surrogate missing backslash-u")

    call buf_init(u); call cdd_error_clear(err)
    call unescape_json_to_buf(BS // "uD83D" // BS // "n1234", u, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_LEX, "unescape surrogate backslash not u")

    call buf_init(u); call cdd_error_clear(err)
    call unescape_json_to_buf(BS // "uD83D" // BS // "u0041", u, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_LEX, "unescape surrogate bad low range")

    call buf_init(u); call cdd_error_clear(err)
    call unescape_json_to_buf(BS // "uD83D" // BS // "uZZZZ", u, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_LEX, "unescape surrogate bad low hex")

    call buf_init(u); call cdd_error_clear(err)
    call unescape_json_to_buf(BS // "uD83D" // BS // "uE000", u, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_LEX, "unescape surrogate above low range")

    ! Buffer overflow mid-unescape triggers loop err%code /= 0 early return
    call buf_init_bounded(u, 1_i32, err)
    call unescape_json_to_buf("abc", u, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "unescape overflow mid-loop")

    ! Strict i64 tests
    call parse_i64_strict("0", iv, err)
    call th_eq_i64(st, iv, 0_i64, "i64 0")
    call parse_i64_strict("+12345", iv, err)
    call th_eq_i64(st, iv, 12345_i64, "i64 +12345")
    call parse_i64_strict("-9223372036854775807", iv, err)
    call th_eq_i32(st, err%code, CDD_OK, "i64 min ok")
    call format_i64_to_str(0_i64, nbuf, nlen)
    call th_eq_str(st, nbuf(1:nlen), "0", "fmt i64 0")
    call format_i64_to_str(-9223372036854775807_i64, nbuf, nlen)
    call th_eq_str(st, nbuf(1:nlen), "-9223372036854775807", "fmt i64 min")

    call parse_i64_strict("", iv, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "i64 empty")
    call parse_i64_strict("-", iv, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "i64 bare sign")
    call parse_i64_strict("12a", iv, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "i64 non-digit")
    call parse_i64_strict("99999999999999999999", iv, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "i64 overflow")

    ! Strict r64 tests
    call parse_r64_strict("0", rv, err)
    call th_near_r64(st, rv, 0.0_r64, 1.0e-12_r64, "r64 0")
    call parse_r64_strict("+3.25e+2", rv, err)
    call th_near_r64(st, rv, 325.0_r64, 1.0e-9_r64, "r64 +3.25e+2")
    call parse_r64_strict("-1.5D-2", rv, err)
    call th_near_r64(st, rv, -0.015_r64, 1.0e-9_r64, "r64 -1.5D-2")
    call parse_r64_strict("1e10000", rv, err)
    call th_eq_i32(st, err%code, CDD_OK, "r64 huge exp clamped parse")

    call parse_r64_strict("", rv, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "r64 empty")
    call parse_r64_strict(".", rv, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "r64 bare dot")
    call parse_r64_strict("1.0e", rv, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "r64 empty exp")
    call parse_r64_strict("1.0e+", rv, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "r64 sign-only exp")
    call parse_r64_strict("1.0xyz", rv, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "r64 trailing")
    call parse_r64_strict("1.0e2xyz", rv, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "r64 exp trailing")

    call format_r64_to_str(0.0_r64, nbuf, nlen)
    call th_eq_str(st, nbuf(1:nlen), "0.0", "fmt r64 0.0")
    call format_r64_to_str(-12.5_r64, nbuf, nlen)
    call th_eq_str(st, nbuf(1:nlen), "-12.5", "fmt r64 -12.5")
    call format_r64_to_str(4.0_r64, nbuf, nlen)
    call th_eq_str(st, nbuf(1:nlen), "4.0", "fmt r64 4.0")
    call format_r64_to_str(2.5e12_r64, nbuf, nlen)
    call th_eq_str(st, nbuf(1:nlen), "2.5e12", "fmt r64 sci large")
    call format_r64_to_str(5.0e-6_r64, nbuf, nlen)
    call th_eq_str(st, nbuf(1:nlen), "5.0e-6", "fmt r64 sci small")
  end subroutine run_json_escape_and_numeric_tests

  !> @brief Exercise OS stream filesystem and mock filesystem including all failure paths.
  !> @param[inout] st Test state accumulator.
  subroutine run_filesystem_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_fs_t) :: fs
    type(cdd_os_fs_t) :: dummy_os
    type(cdd_mock_fs_t) :: dummy_mock
    type(cdd_mock_entry_t) :: dummy_entry
    type(cdd_buffer_t) :: buf
    type(cdd_error_t) :: err
    character(len=512, kind=c_k) :: over_path
    character(len=*), parameter :: TMP_FILE = "build/test_tmp_io.txt"
    character(len=*), parameter :: TMP_EMPTY = "build/test_tmp_empty.txt"
    integer(i32) :: scratch_u, ro_u, ios, idx

    call th_touch_vtable(fs)
    call th_touch_vtable(dummy_os)
    call th_touch_vtable(dummy_mock)
    call th_touch_vtable(dummy_entry)

    ! Invalid backend selector tests
    call fs_init_os(fs)
    fs%backend = 99_i32
    call fs_read_text_file(fs, "x", buf, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "invalid backend read")
    call fs_write_text_file(fs, "x", "y", err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "invalid backend write")
    call th_check(st, .not. fs_file_exists(fs, "x"), "invalid backend exists")
    call fs_read_stdin(fs, buf, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "invalid backend stdin")
    call fs_write_stdout(fs, "y", err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "invalid backend stdout")

    ! OS FS read/write/exists/overflow/error tests
    call fs_init_os(fs)
    fs%os%max_read_bytes = -1_i32
    call fs_write_text_file(fs, TMP_FILE, "abc", err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "os negative max_read_bytes")
    fs%os%max_read_bytes = 32768_i32

    call fs_write_text_file(fs, "/nonexistent_dir_cdd_xyz/out.txt", "abc", err)
    call th_eq_i32(st, err%code, CDD_ERR_IO_WRITE, "os write invalid dir")
    call fs_read_text_file(fs, "/nonexistent_dir_cdd_xyz/in.txt", buf, err)
    call th_eq_i32(st, err%code, CDD_ERR_IO_READ, "os read missing")
    call th_check(st, .not. fs_file_exists(fs, "/nonexistent_dir_cdd_xyz/in.txt"), "os exists false")

    call fs_write_text_file(fs, TMP_FILE, "hello" // achar(13, c_k) // achar(10, c_k), err)
    call th_eq_i32(st, err%code, CDD_OK, "os write ok")
    call th_check(st, fs_file_exists(fs, TMP_FILE), "os exists true")
    call fs_read_text_file(fs, TMP_FILE, buf, err)
    call th_eq_i32(st, err%code, CDD_OK, "os read ok")
    call th_check(st, buf_equals(buf, "hello" // achar(13, c_k) // achar(10, c_k)), "os CRLF preserved")

    fs%os%max_read_bytes = 3_i32
    call fs_read_text_file(fs, TMP_FILE, buf, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "os read max_read_bytes")
    fs%os%max_read_bytes = 32768_i32

    call fs_write_text_file(fs, TMP_EMPTY, "", err)
    call fs_read_text_file(fs, TMP_EMPTY, buf, err)
    call th_eq_i32(st, buf%len, 0_i32, "os empty file")

    ! OS stdin / stdout tests via scratch units (including >512-char line for multi-chunk read)
    open(newunit=scratch_u, status="scratch", form="formatted", action="readwrite", iostat=ios)
    fs%os%stdout_unit = scratch_u
    call fs_write_stdout(fs, repeat("z", 600) // achar(10, c_k) // "line2", err)
    call th_eq_i32(st, err%code, CDD_OK, "os write_stdout ok")
    rewind(scratch_u)
    fs%os%stdin_unit = scratch_u
    call fs_read_stdin(fs, buf, err)
    call th_eq_i32(st, err%code, CDD_OK, "os read_stdin ok")
    call th_check(st, buf%len >= 605_i32, "os read_stdin multi-chunk content")
    call fs_read_stdin(fs, buf, err)
    call th_eq_i32(st, buf%len, 0_i32, "os read_stdin empty EOF")
    close(scratch_u)

    ! Trigger ios > 0 on read_stdin & write_stdout using incompatible unformatted scratch unit
    open(newunit=ro_u, status="scratch", form="unformatted", action="readwrite", iostat=ios)
    fs%os%stdin_unit = ro_u
    call fs_read_stdin(fs, buf, err)
    call th_eq_i32(st, err%code, CDD_ERR_IO_READ, "os read_stdin formatted-on-unformatted error")
    fs%os%stdout_unit = ro_u
    call fs_write_stdout(fs, "bad", err)
    call th_eq_i32(st, err%code, CDD_ERR_IO_WRITE, "os write_stdout formatted-on-unformatted error")
    close(ro_u)

    ! Clean up temp files
    open(newunit=scratch_u, file=TMP_FILE, status="old", iostat=ios)
    if (ios == 0_i32) close(scratch_u, status="delete")
    open(newunit=scratch_u, file=TMP_EMPTY, status="old", iostat=ios)
    if (ios == 0_i32) close(scratch_u, status="delete")

    ! Mock FS tests
    call fs_init_mock(fs)
    call th_check(st, .not. fs_file_exists(fs, "a.f90"), "mock exists false")
    call fs_read_text_file(fs, "a.f90", buf, err)
    call th_eq_i32(st, err%code, CDD_ERR_IO_READ, "mock read missing")

    call fs_write_text_file(fs, "a.f90", "v1", err)
    call th_eq_i32(st, err%code, CDD_OK, "mock write v1")
    call fs_write_text_file(fs, "a.f90", "v2", err)
    call th_check(st, fs_file_exists(fs, "a.f90"), "mock exists true")
    call fs_read_text_file(fs, "a.f90", buf, err)
    call th_check(st, buf_equals(buf, "v2"), "mock overwrite v2")

    fs%mock%fail_next_read = .true._lk
    call fs_read_text_file(fs, "a.f90", buf, err)
    call th_eq_i32(st, err%code, CDD_ERR_IO_READ, "mock injected read err")

    fs%mock%fail_next_write = .true._lk
    call fs_write_text_file(fs, "a.f90", "v3", err)
    call th_eq_i32(st, err%code, CDD_ERR_IO_WRITE, "mock injected write err")

    call cdd_error_clear(err)
    call buf_append_str(fs%mock%stdin_buf, '{"x":1}', err)
    call fs_read_stdin(fs, buf, err)
    call th_check(st, buf_equals(buf, '{"x":1}'), "mock stdin ok")
    fs%mock%fail_next_read = .true._lk
    call fs_read_stdin(fs, buf, err)
    call th_eq_i32(st, err%code, CDD_ERR_IO_READ, "mock stdin injected err")

    call fs_write_stdout(fs, "out!", err)
    call th_check(st, buf_equals(fs%mock%stdout_buf, "out!"), "mock stdout ok")
    fs%mock%fail_next_write = .true._lk
    call fs_write_stdout(fs, "fail", err)
    call th_eq_i32(st, err%code, CDD_ERR_IO_WRITE, "mock stdout injected err")

    ! Mock FS overlong path, overlong content, & full table
    over_path = repeat("p", CDD_STR_MAX + 4_i32)
    call mock_fs_put(fs%mock, over_path, "x", err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "mock_fs_put path overflow")
    call mock_fs_put(fs%mock, "big.txt", repeat("x", CDD_BUF_MAX + 1_i32), err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "mock_fs_put content overflow")

    call mock_fs_init(fs%mock)
    do idx = 1_i32, CDD_MOCK_MAX_FILES
      call mock_fs_put(fs%mock, achar(64_i32 + idx, c_k), "ok", err)
    end do
    call mock_fs_put(fs%mock, "extra.txt", "no_room", err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "mock_fs_put table full")
    call mock_fs_get(fs%mock, "A", buf, err)
    call th_check(st, buf_equals(buf, "ok"), "mock_fs_get A")
  end subroutine run_filesystem_tests

end program test_errors_and_strings
