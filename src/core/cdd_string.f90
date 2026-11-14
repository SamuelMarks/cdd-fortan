!> @file cdd_string.f90
!> @brief Pure strongly-typed string, lossless line-splitting, JSON-escape, numeric, and buffer utilities.

!> @brief Core character manipulation, dynamic-length bounded buffers, and lossless line splitting.
module cdd_string
  use cdd_kinds, only: i32, i64, r64, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set_simple, &
    CDD_ERR_BUFFER_OVERFLOW, CDD_ERR_INVALID_OPTION, CDD_ERR_JSON_LEX
  implicit none (type, external)
  private

  public :: CDD_STR_MAX, CDD_BUF_MAX
  public :: TERM_NONE, TERM_LF, TERM_CRLF
  public :: cdd_string_t, cdd_buffer_t, cdd_line_slice_t
  public :: str_clear, str_set, str_eq, str_eq_nocase
  public :: char_to_upper, char_to_lower
  public :: is_ascii_alpha, is_ascii_digit, is_ascii_alnum, is_ascii_space, is_hex_digit, hex_digit_val
  public :: to_upper_buf, to_lower_buf, trim_bounds, starts_with, ends_with
  public :: buf_init, buf_init_bounded, buf_clear
  public :: buf_append_char, buf_append_str, buf_append_i64, buf_equals
  public :: split_lines_lossless
  public :: escape_json_to_buf, unescape_json_to_buf
  public :: parse_i64_strict, format_i64_to_str
  public :: parse_r64_strict, format_r64_to_str

  integer(i32), parameter :: CDD_STR_MAX = 256_i32     !< Max length of short identifier/token strings.
  integer(i32), parameter :: CDD_BUF_MAX = 229376_i32  !< Max capacity (224 KiB) of a text/code buffer.
  integer(i32), parameter :: TERM_NONE = 0_i32         !< Line has no trailing line terminator (EOF).
  integer(i32), parameter :: TERM_LF = 1_i32           !< Line ends with LF (0x0A).
  integer(i32), parameter :: TERM_CRLF = 2_i32         !< Line ends with CRLF (0x0D 0x0A).

  !> @brief Bounded value string wrapper suitable for arrays without heap fragmentation.
  type :: cdd_string_t
    integer(i32) :: len = 0_i32                     !< Active byte count in raw(1:len).
    character(len=CDD_STR_MAX, kind=c_k) :: raw     !< Fixed backing character array.
  end type cdd_string_t

  !> @brief Amortized O(1) bounded character accumulator with configurable test cap.
  type :: cdd_buffer_t
    integer(i32) :: len = 0_i32                     !< Active byte count in raw(1:len).
    integer(i32) :: cap = CDD_BUF_MAX               !< Effective capacity limit (<= CDD_BUF_MAX).
    character(len=CDD_BUF_MAX, kind=c_k) :: raw     !< Fixed 128 KiB backing storage.
  end type cdd_buffer_t

  !> @brief Lossless line span recording body indices and original line-ending kind.
  type :: cdd_line_slice_t
    integer(i32) :: first_idx = 1_i32               !< 1-based start index in source string.
    integer(i32) :: last_idx = 0_i32                !< 1-based end index of content (excluding LF/CRLF).
    integer(i32) :: term_kind = TERM_NONE           !< Line ending kind (TERM_NONE, TERM_LF, TERM_CRLF).
  end type cdd_line_slice_t

contains

  !> @brief Reset a cdd_string_t to zero length.
  !> @param[out] s String wrapper to clear.
  pure subroutine str_clear(s)
    type(cdd_string_t), intent(out) :: s

    s%len = 0_i32
  end subroutine str_clear

  !> @brief Copy text into a cdd_string_t, reporting CDD_ERR_BUFFER_OVERFLOW if too long.
  !> @param[out] s   Destination string wrapper.
  !> @param[in]  txt Source character slice.
  !> @param[out] err Error status record.
  pure subroutine str_set(s, txt, err)
    type(cdd_string_t), intent(out) :: s
    character(len=*, kind=c_k), intent(in) :: txt
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: n, i

    call cdd_error_clear(err)
    n = len(txt, kind=i32)
    if (n > CDD_STR_MAX) then
      s%len = 0_i32
      call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "String exceeds CDD_STR_MAX")
      return
    end if
    s%len = n
    do i = 1_i32, n
      s%raw(i:i) = txt(i:i)
    end do
  end subroutine str_set

  !> @brief Perform exact byte-for-byte equality comparison (including trailing spaces).
  !> @param[in] a First string slice.
  !> @param[in] b Second string slice.
  !> @return True if lengths and all bytes match exactly.
  pure function str_eq(a, b) result(same)
    character(len=*, kind=c_k), intent(in) :: a
    character(len=*, kind=c_k), intent(in) :: b
    logical(lk) :: same
    integer(i32) :: na, nb, i

    na = len(a, kind=i32)
    nb = len(b, kind=i32)
    if (na /= nb) then
      same = .false._lk
      return
    end if
    do i = 1_i32, na
      if (iachar(a(i:i), kind=i32) /= iachar(b(i:i), kind=i32)) then
        same = .false._lk
        return
      end if
    end do
    same = .true._lk
  end function str_eq

  !> @brief Perform ASCII case-insensitive exact-length equality comparison.
  !> @param[in] a First string slice.
  !> @param[in] b Second string slice.
  !> @return True if lengths match and all ASCII characters match ignoring case.
  pure function str_eq_nocase(a, b) result(same)
    character(len=*, kind=c_k), intent(in) :: a
    character(len=*, kind=c_k), intent(in) :: b
    logical(lk) :: same
    integer(i32) :: na, nb, i

    na = len(a, kind=i32)
    nb = len(b, kind=i32)
    if (na /= nb) then
      same = .false._lk
      return
    end if
    do i = 1_i32, na
      if (char_to_lower(a(i:i)) /= char_to_lower(b(i:i))) then
        same = .false._lk
        return
      end if
    end do
    same = .true._lk
  end function str_eq_nocase

  !> @brief Convert a single ASCII character to uppercase.
  !> @param[in] ch Input character.
  !> @return Uppercase ASCII character (or unchanged if non-lowercase).
  pure function char_to_upper(ch) result(up)
    character(len=1, kind=c_k), intent(in) :: ch
    character(len=1, kind=c_k) :: up
    integer(i32) :: code

    code = iachar(ch, kind=i32)
    if (code >= 97_i32 .and. code <= 122_i32) then
      up = achar(code - 32_i32, kind=c_k)
    else
      up = ch
    end if
  end function char_to_upper

  !> @brief Convert a single ASCII character to lowercase.
  !> @param[in] ch Input character.
  !> @return Lowercase ASCII character (or unchanged if non-uppercase).
  pure function char_to_lower(ch) result(lo)
    character(len=1, kind=c_k), intent(in) :: ch
    character(len=1, kind=c_k) :: lo
    integer(i32) :: code

    code = iachar(ch, kind=i32)
    if (code >= 65_i32 .and. code <= 90_i32) then
      lo = achar(code + 32_i32, kind=c_k)
    else
      lo = ch
    end if
  end function char_to_lower

  !> @brief Test whether a character is an ASCII letter (A-Z or a-z).
  !> @param[in] ch Input character.
  !> @return True if ASCII alphabetic.
  pure function is_ascii_alpha(ch) result(ok)
    character(len=1, kind=c_k), intent(in) :: ch
    logical(lk) :: ok
    integer(i32) :: code

    code = iachar(ch, kind=i32)
    ok = (code >= 65_i32 .and. code <= 90_i32) .or. (code >= 97_i32 .and. code <= 122_i32)
  end function is_ascii_alpha

  !> @brief Test whether a character is an ASCII decimal digit (0-9).
  !> @param[in] ch Input character.
  !> @return True if ASCII digit.
  pure function is_ascii_digit(ch) result(ok)
    character(len=1, kind=c_k), intent(in) :: ch
    logical(lk) :: ok
    integer(i32) :: code

    code = iachar(ch, kind=i32)
    ok = (code >= 48_i32 .and. code <= 57_i32)
  end function is_ascii_digit

  !> @brief Test whether a character is ASCII alphanumeric (A-Z, a-z, 0-9).
  !> @param[in] ch Input character.
  !> @return True if ASCII letter or digit.
  pure function is_ascii_alnum(ch) result(ok)
    character(len=1, kind=c_k), intent(in) :: ch
    logical(lk) :: ok

    ok = is_ascii_alpha(ch) .or. is_ascii_digit(ch)
  end function is_ascii_alnum

  !> @brief Test whether a character is horizontal or vertical ASCII whitespace.
  !> @param[in] ch Input character.
  !> @return True if space (32), tab (9), LF (10), or CR (13).
  pure function is_ascii_space(ch) result(ok)
    character(len=1, kind=c_k), intent(in) :: ch
    logical(lk) :: ok
    integer(i32) :: code

    code = iachar(ch, kind=i32)
    ok = (code == 32_i32 .or. code == 9_i32 .or. code == 10_i32 .or. code == 13_i32)
  end function is_ascii_space

  !> @brief Test whether a character is a hexadecimal digit (0-9, A-F, a-f).
  !> @param[in] ch Input character.
  !> @return True if hexadecimal digit.
  pure function is_hex_digit(ch) result(ok)
    character(len=1, kind=c_k), intent(in) :: ch
    logical(lk) :: ok
    integer(i32) :: code

    code = iachar(ch, kind=i32)
    ok = (code >= 48_i32 .and. code <= 57_i32) .or. &
         (code >= 65_i32 .and. code <= 70_i32) .or. &
         (code >= 97_i32 .and. code <= 102_i32)
  end function is_hex_digit

  !> @brief Convert a validated hexadecimal digit character to its integer value (0..15).
  !> @param[in] ch Hexadecimal character.
  !> @return Numeric value 0..15 (or -1 if not a hex digit).
  pure function hex_digit_val(ch) result(v)
    character(len=1, kind=c_k), intent(in) :: ch
    integer(i32) :: v
    integer(i32) :: code

    code = iachar(ch, kind=i32)
    if (code >= 48_i32 .and. code <= 57_i32) then
      v = code - 48_i32
    else if (code >= 65_i32 .and. code <= 70_i32) then
      v = code - 55_i32
    else if (code >= 97_i32 .and. code <= 102_i32) then
      v = code - 87_i32
    else
      v = -1_i32
    end if
  end function hex_digit_val

  !> @brief Write uppercase conversion of src into dst(1:out_len).
  !> @param[in]  src     Input string slice.
  !> @param[out] dst     Output buffer (must have len >= len(src) for full copy).
  !> @param[out] out_len Number of bytes written.
  pure subroutine to_upper_buf(src, dst, out_len)
    character(len=*, kind=c_k), intent(in) :: src
    character(len=*, kind=c_k), intent(out) :: dst
    integer(i32), intent(out) :: out_len
    integer(i32) :: n, cap, i

    n = len(src, kind=i32)
    cap = len(dst, kind=i32)
    if (n > cap) then
      n = cap
    end if
    out_len = n
    do i = 1_i32, n
      dst(i:i) = char_to_upper(src(i:i))
    end do
  end subroutine to_upper_buf

  !> @brief Write lowercase conversion of src into dst(1:out_len).
  !> @param[in]  src     Input string slice.
  !> @param[out] dst     Output buffer.
  !> @param[out] out_len Number of bytes written.
  pure subroutine to_lower_buf(src, dst, out_len)
    character(len=*, kind=c_k), intent(in) :: src
    character(len=*, kind=c_k), intent(out) :: dst
    integer(i32), intent(out) :: out_len
    integer(i32) :: n, cap, i

    n = len(src, kind=i32)
    cap = len(dst, kind=i32)
    if (n > cap) then
      n = cap
    end if
    out_len = n
    do i = 1_i32, n
      dst(i:i) = char_to_lower(src(i:i))
    end do
  end subroutine to_lower_buf

  !> @brief Compute 1-based non-whitespace start and end indices of src (lo > hi if blank).
  !> @param[in]  src Input string slice.
  !> @param[out] lo  First non-whitespace index.
  !> @param[out] hi  Last non-whitespace index.
  pure subroutine trim_bounds(src, lo, hi)
    character(len=*, kind=c_k), intent(in) :: src
    integer(i32), intent(out) :: lo
    integer(i32), intent(out) :: hi
    integer(i32) :: n

    n = len(src, kind=i32)
    lo = 1_i32
    do while (lo <= n)
      if (.not. is_ascii_space(src(lo:lo))) exit
      lo = lo + 1_i32
    end do
    hi = n
    do while (hi >= lo)
      if (.not. is_ascii_space(src(hi:hi))) exit
      hi = hi - 1_i32
    end do
  end subroutine trim_bounds

  !> @brief Check whether text starts with prefix.
  !> @param[in] text   Target string slice.
  !> @param[in] prefix Prefix string slice.
  !> @return True if text begins with prefix.
  pure function starts_with(text, prefix) result(ok)
    character(len=*, kind=c_k), intent(in) :: text
    character(len=*, kind=c_k), intent(in) :: prefix
    logical(lk) :: ok
    integer(i32) :: nt, np

    nt = len(text, kind=i32)
    np = len(prefix, kind=i32)
    if (np > nt) then
      ok = .false._lk
    else
      ok = str_eq(text(1:np), prefix)
    end if
  end function starts_with

  !> @brief Check whether text ends with suffix.
  !> @param[in] text   Target string slice.
  !> @param[in] suffix Suffix string slice.
  !> @return True if text ends with suffix.
  pure function ends_with(text, suffix) result(ok)
    character(len=*, kind=c_k), intent(in) :: text
    character(len=*, kind=c_k), intent(in) :: suffix
    logical(lk) :: ok
    integer(i32) :: nt, ns

    nt = len(text, kind=i32)
    ns = len(suffix, kind=i32)
    if (ns > nt) then
      ok = .false._lk
    else
      ok = str_eq(text(nt - ns + 1_i32:nt), suffix)
    end if
  end function ends_with

  !> @brief Initialize a cdd_buffer_t with full CDD_BUF_MAX capacity.
  !> @param[out] buf Buffer to initialize.
  pure subroutine buf_init(buf)
    type(cdd_buffer_t), intent(out) :: buf

    buf%len = 0_i32
    buf%cap = CDD_BUF_MAX
  end subroutine buf_init

  !> @brief Initialize a cdd_buffer_t with a custom bounded capacity (for testing/quotas).
  !> @param[out] buf     Buffer to initialize.
  !> @param[in]  max_cap Requested byte limit (1..CDD_BUF_MAX).
  !> @param[out] err     Error status if max_cap is out of valid bounds.
  pure subroutine buf_init_bounded(buf, max_cap, err)
    type(cdd_buffer_t), intent(out) :: buf
    integer(i32), intent(in) :: max_cap
    type(cdd_error_t), intent(out) :: err

    call cdd_error_clear(err)
    buf%len = 0_i32
    if (max_cap < 1_i32 .or. max_cap > CDD_BUF_MAX) then
      buf%cap = CDD_BUF_MAX
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Invalid buffer capacity")
      return
    end if
    buf%cap = max_cap
  end subroutine buf_init_bounded

  !> @brief Reset active length of a buffer without altering its configured capacity.
  !> @param[inout] buf Buffer to clear.
  pure subroutine buf_clear(buf)
    type(cdd_buffer_t), intent(inout) :: buf

    buf%len = 0_i32
  end subroutine buf_clear

  !> @brief Append a single character to a cdd_buffer_t with overflow protection.
  !> @param[inout] buf Target buffer.
  !> @param[in]    ch  Character to append.
  !> @param[inout] err Error status (no-op if already in error state).
  pure subroutine buf_append_char(buf, ch, err)
    type(cdd_buffer_t), intent(inout) :: buf
    character(len=1, kind=c_k), intent(in) :: ch
    type(cdd_error_t), intent(inout) :: err

    if (err%code /= 0_i32) return
    if (buf%len >= buf%cap) then
      call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "Buffer capacity exceeded")
      return
    end if
    buf%len = buf%len + 1_i32
    buf%raw(buf%len:buf%len) = ch
  end subroutine buf_append_char

  !> @brief Append a character slice to a cdd_buffer_t with overflow protection.
  !> @param[inout] buf Target buffer.
  !> @param[in]    s   Character slice to append.
  !> @param[inout] err Error status (no-op if already in error state).
  pure subroutine buf_append_str(buf, s, err)
    type(cdd_buffer_t), intent(inout) :: buf
    character(len=*, kind=c_k), intent(in) :: s
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: n, i

    if (err%code /= 0_i32) return
    n = len(s, kind=i32)
    if (buf%len + n > buf%cap) then
      call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "Buffer capacity exceeded")
      return
    end if
    do i = 1_i32, n
      buf%len = buf%len + 1_i32
      buf%raw(buf%len:buf%len) = s(i:i)
    end do
  end subroutine buf_append_str

  !> @brief Append a signed 64-bit integer formatted in base 10 to a cdd_buffer_t.
  !> @param[inout] buf Target buffer.
  !> @param[in]    val Integer value to append.
  !> @param[inout] err Error status.
  pure subroutine buf_append_i64(buf, val, err)
    type(cdd_buffer_t), intent(inout) :: buf
    integer(i64), intent(in) :: val
    type(cdd_error_t), intent(inout) :: err
    character(len=24, kind=c_k) :: tmp
    integer(i32) :: n

    call format_i64_to_str(val, tmp, n)
    call buf_append_str(buf, tmp(1:n), err)
  end subroutine buf_append_i64

  !> @brief Compare the active content of a cdd_buffer_t with an expected string slice.
  !> @param[in] buf      Buffer to inspect.
  !> @param[in] expected Expected character slice.
  !> @return True if buf%raw(1:buf%len) equals expected byte-for-byte.
  pure function buf_equals(buf, expected) result(ok)
    type(cdd_buffer_t), intent(in) :: buf
    character(len=*, kind=c_k), intent(in) :: expected
    logical(lk) :: ok

    ok = str_eq(buf%raw(1:buf%len), expected)
  end function buf_equals

  !> @brief Split source text into line spans while preserving exact LF vs CRLF terminators.
  !> @param[in]    src        Input source text.
  !> @param[in]    max_lines  Capacity of lines array (>= 1).
  !> @param[inout] lines      Output array of line slices (1:line_count populated).
  !> @param[out]   line_count Number of lines populated in lines(1:line_count).
  !> @param[out]   err        Error status if line array capacity is exceeded.
  pure subroutine split_lines_lossless(src, max_lines, lines, line_count, err)
    character(len=*, kind=c_k), intent(in) :: src
    integer(i32), intent(in) :: max_lines
    type(cdd_line_slice_t), intent(inout) :: lines(max_lines)
    integer(i32), intent(out) :: line_count
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: n, pos, start_pos
    character(len=1, kind=c_k), parameter :: CH_LF = achar(10, kind=c_k)
    character(len=1, kind=c_k), parameter :: CH_CR = achar(13, kind=c_k)

    call cdd_error_clear(err)
    line_count = 0_i32
    n = len(src, kind=i32)
    if (n == 0_i32) return

    pos = 1_i32
    do while (pos <= n)
      if (line_count >= max_lines) then
        call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "Too many lines for slice array")
        return
      end if
      start_pos = pos
      do while (pos <= n)
        if (src(pos:pos) == CH_LF) exit
        pos = pos + 1_i32
      end do
      line_count = line_count + 1_i32
      lines(line_count)%first_idx = start_pos
      if (pos <= n) then
        if (pos > start_pos .and. &
            src(max(1_i32, pos - 1_i32):max(1_i32, pos - 1_i32)) == CH_CR) then
          lines(line_count)%last_idx = pos - 2_i32
          lines(line_count)%term_kind = TERM_CRLF
        else
          lines(line_count)%last_idx = pos - 1_i32
          lines(line_count)%term_kind = TERM_LF
        end if
        pos = pos + 1_i32
      else
        lines(line_count)%last_idx = n
        lines(line_count)%term_kind = TERM_NONE
      end if
    end do
  end subroutine split_lines_lossless

  !> @brief Escape raw string content as RFC 8259 JSON string body (without outer quotes).
  !> @param[in]    raw Input unescaped string slice.
  !> @param[inout] buf Destination buffer receiving escaped characters.
  !> @param[inout] err Error status.
  pure subroutine escape_json_to_buf(raw, buf, err)
    character(len=*, kind=c_k), intent(in) :: raw
    type(cdd_buffer_t), intent(inout) :: buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: i, n, code, hi, lo
    character(len=1, kind=c_k), parameter :: BS = achar(92, kind=c_k)
    character(len=1, kind=c_k), parameter :: QT = achar(34, kind=c_k)
    character(len=16, kind=c_k), parameter :: HEX = "0123456789abcdef"

    n = len(raw, kind=i32)
    do i = 1_i32, n
      code = iachar(raw(i:i), kind=i32)
      select case (code)
      case (34_i32)
        call buf_append_char(buf, BS, err)
        call buf_append_char(buf, QT, err)
      case (92_i32)
        call buf_append_char(buf, BS, err)
        call buf_append_char(buf, BS, err)
      case (8_i32)
        call buf_append_char(buf, BS, err)
        call buf_append_char(buf, "b", err)
      case (12_i32)
        call buf_append_char(buf, BS, err)
        call buf_append_char(buf, "f", err)
      case (10_i32)
        call buf_append_char(buf, BS, err)
        call buf_append_char(buf, "n", err)
      case (13_i32)
        call buf_append_char(buf, BS, err)
        call buf_append_char(buf, "r", err)
      case (9_i32)
        call buf_append_char(buf, BS, err)
        call buf_append_char(buf, "t", err)
      case default
        if (code < 32_i32) then
          hi = code / 16_i32 + 1_i32
          lo = mod(code, 16_i32) + 1_i32
          call buf_append_char(buf, BS, err)
          call buf_append_str(buf, "u00", err)
          call buf_append_char(buf, HEX(hi:hi), err)
          call buf_append_char(buf, HEX(lo:lo), err)
        else
          call buf_append_char(buf, raw(i:i), err)
        end if
      end select
    end do
  end subroutine escape_json_to_buf

  !> @brief Unescape an RFC 8259 JSON string interior (without outer quotes) into buf.
  !> @param[in]    esc Input escaped JSON string body.
  !> @param[inout] buf Destination buffer receiving decoded UTF-8/ASCII bytes.
  !> @param[inout] err Error status (CDD_ERR_JSON_LEX on invalid escape or control char).
  pure subroutine unescape_json_to_buf(esc, buf, err)
    character(len=*, kind=c_k), intent(in) :: esc
    type(cdd_buffer_t), intent(inout) :: buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: i, n, code, cp, low_surr
    logical(lk) :: ok
    character(len=1, kind=c_k), parameter :: BS = achar(92, kind=c_k)

    n = len(esc, kind=i32)
    i = 1_i32
    do while (i <= n)
      if (err%code /= 0_i32) return
      code = iachar(esc(i:i), kind=i32)
      if (code < 32_i32) then
        call cdd_error_set_simple(err, CDD_ERR_JSON_LEX, "Unescaped control char in JSON string")
        return
      end if
      if (esc(i:i) /= BS) then
        call buf_append_char(buf, esc(i:i), err)
        i = i + 1_i32
      else
        if (i == n) then
          call cdd_error_set_simple(err, CDD_ERR_JSON_LEX, "Trailing backslash in JSON string")
          return
        end if
        select case (esc(i + 1_i32:i + 1_i32))
        case ('"')
          call buf_append_char(buf, achar(34, kind=c_k), err)
          i = i + 2_i32
        case (BS)
          call buf_append_char(buf, BS, err)
          i = i + 2_i32
        case ("/")
          call buf_append_char(buf, "/", err)
          i = i + 2_i32
        case ("b")
          call buf_append_char(buf, achar(8, kind=c_k), err)
          i = i + 2_i32
        case ("f")
          call buf_append_char(buf, achar(12, kind=c_k), err)
          i = i + 2_i32
        case ("n")
          call buf_append_char(buf, achar(10, kind=c_k), err)
          i = i + 2_i32
        case ("r")
          call buf_append_char(buf, achar(13, kind=c_k), err)
          i = i + 2_i32
        case ("t")
          call buf_append_char(buf, achar(9, kind=c_k), err)
          i = i + 2_i32
        case ("u")
          call parse_hex4(esc, i + 2_i32, n, cp, ok)
          if (.not. ok) then
            call cdd_error_set_simple(err, CDD_ERR_JSON_LEX, "Invalid \uXXXX escape in JSON")
            return
          end if
          if (cp >= 56320_i32 .and. cp <= 57343_i32) then
            call cdd_error_set_simple(err, CDD_ERR_JSON_LEX, "Lone low UTF-16 surrogate in JSON")
            return
          end if
          if (cp >= 55296_i32 .and. cp <= 56319_i32) then
            if (i + 11_i32 > n) then
              call cdd_error_set_simple(err, CDD_ERR_JSON_LEX, "Truncated UTF-16 surrogate pair")
              return
            end if
            if (esc(i + 6_i32:i + 6_i32) /= BS .or. esc(i + 7_i32:i + 7_i32) /= "u") then
              call cdd_error_set_simple(err, CDD_ERR_JSON_LEX, "Missing low surrogate escape")
              return
            end if
            call parse_hex4(esc, i + 8_i32, n, low_surr, ok)
            if (.not. ok .or. low_surr < 56320_i32 .or. low_surr > 57343_i32) then
              call cdd_error_set_simple(err, CDD_ERR_JSON_LEX, "Invalid low UTF-16 surrogate")
              return
            end if
            cp = 65536_i32 + (cp - 55296_i32) * 1024_i32 + (low_surr - 56320_i32)
            i = i + 12_i32
          else
            i = i + 6_i32
          end if
          call append_utf8_codepoint(cp, buf, err)
        case default
          call cdd_error_set_simple(err, CDD_ERR_JSON_LEX, "Invalid escape sequence in JSON string")
          return
        end select
      end if
    end do
  end subroutine unescape_json_to_buf

  !> @brief Parse 4 hexadecimal digits starting at pos in src(1:n).
  !> @param[in]  src Source character slice.
  !> @param[in]  pos 1-based start index of 4 hex digits.
  !> @param[in]  n   Total length of src.
  !> @param[out] val Parsed 16-bit integer value (0..65535).
  !> @param[out] ok  True if 4 valid hex digits were parsed.
  pure subroutine parse_hex4(src, pos, n, val, ok)
    character(len=*, kind=c_k), intent(in) :: src
    integer(i32), intent(in) :: pos
    integer(i32), intent(in) :: n
    integer(i32), intent(out) :: val
    logical(lk), intent(out) :: ok
    integer(i32) :: k, d

    val = 0_i32
    if (pos + 3_i32 > n) then
      ok = .false._lk
      return
    end if
    do k = 0_i32, 3_i32
      d = hex_digit_val(src(pos + k:pos + k))
      if (d < 0_i32) then
        ok = .false._lk
        return
      end if
      val = val * 16_i32 + d
    end do
    ok = .true._lk
  end subroutine parse_hex4

  !> @brief Encode a Unicode scalar value (0..0x10FFFF) as UTF-8 bytes into buf.
  !> @param[in]    cp  Unicode codepoint.
  !> @param[inout] buf Destination character buffer.
  !> @param[inout] err Error status.
  pure subroutine append_utf8_codepoint(cp, buf, err)
    integer(i32), intent(in) :: cp
    type(cdd_buffer_t), intent(inout) :: buf
    type(cdd_error_t), intent(inout) :: err

    if (cp <= 127_i32) then
      call buf_append_char(buf, achar(cp, kind=c_k), err)
    else if (cp <= 2047_i32) then
      call buf_append_char(buf, achar(192_i32 + cp / 64_i32, kind=c_k), err)
      call buf_append_char(buf, achar(128_i32 + mod(cp, 64_i32), kind=c_k), err)
    else if (cp <= 65535_i32) then
      call buf_append_char(buf, achar(224_i32 + cp / 4096_i32, kind=c_k), err)
      call buf_append_char(buf, achar(128_i32 + mod(cp / 64_i32, 64_i32), kind=c_k), err)
      call buf_append_char(buf, achar(128_i32 + mod(cp, 64_i32), kind=c_k), err)
    else
      call buf_append_char(buf, achar(240_i32 + cp / 262144_i32, kind=c_k), err)
      call buf_append_char(buf, achar(128_i32 + mod(cp / 4096_i32, 64_i32), kind=c_k), err)
      call buf_append_char(buf, achar(128_i32 + mod(cp / 64_i32, 64_i32), kind=c_k), err)
      call buf_append_char(buf, achar(128_i32 + mod(cp, 64_i32), kind=c_k), err)
    end if
  end subroutine append_utf8_codepoint

  !> @brief Strictly parse a signed 64-bit decimal integer with overflow detection.
  !> @param[in]  text Input character slice (no leading/trailing whitespace allowed).
  !> @param[out] val  Parsed 64-bit integer value.
  !> @param[out] err  Error status (CDD_ERR_INVALID_OPTION on invalid/overflowing input).
  pure subroutine parse_i64_strict(text, val, err)
    character(len=*, kind=c_k), intent(in) :: text
    integer(i64), intent(out) :: val
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: n, pos, d
    logical(lk) :: neg
    integer(i64) :: acc, limit

    call cdd_error_clear(err)
    val = 0_i64
    n = len(text, kind=i32)
    if (n == 0_i32) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Empty integer literal")
      return
    end if

    pos = 1_i32
    neg = .false._lk
    if (text(1:1) == "-") then
      neg = .true._lk
      pos = 2_i32
    else if (text(1:1) == "+") then
      pos = 2_i32
    end if

    if (pos > n) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Sign without digits in integer")
      return
    end if

    acc = 0_i64
    limit = -huge(0_i64)

    do while (pos <= n)
      if (.not. is_ascii_digit(text(pos:pos))) then
        call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Non-digit in integer literal")
        return
      end if
      d = iachar(text(pos:pos), kind=i32) - 48_i32
      if (acc < (limit + int(d, kind=i64)) / 10_i64) then
        call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "64-bit integer overflow")
        return
      end if
      acc = acc * 10_i64 - int(d, kind=i64)
      pos = pos + 1_i32
    end do

    if (neg) then
      val = acc
    else
      val = -acc
    end if
  end subroutine parse_i64_strict

  !> @brief Format a signed 64-bit integer into a fixed buffer (min 24 chars recommended).
  !> @param[in]  val     64-bit signed integer to format.
  !> @param[out] out_str Destination string buffer.
  !> @param[out] out_len Number of characters written.
  pure subroutine format_i64_to_str(val, out_str, out_len)
    integer(i64), intent(in) :: val
    character(len=*, kind=c_k), intent(out) :: out_str
    integer(i32), intent(out) :: out_len
    character(len=24, kind=c_k) :: rev
    integer(i64) :: neg_acc, rem
    integer(i32) :: digits, k

    out_len = 0_i32
    if (val == 0_i64) then
      out_len = 1_i32
      out_str(1:1) = "0"
      return
    end if

    if (val < 0_i64) then
      out_len = 1_i32
      out_str(1:1) = "-"
      neg_acc = val
    else
      neg_acc = -val
    end if

    digits = 0_i32
    do while (neg_acc < 0_i64)
      rem = -(mod(neg_acc, 10_i64))
      digits = digits + 1_i32
      rev(digits:digits) = achar(48_i32 + int(rem, kind=i32), kind=c_k)
      neg_acc = neg_acc / 10_i64
    end do

    do k = digits, 1_i32, -1_i32
      out_len = out_len + 1_i32
      out_str(out_len:out_len) = rev(k:k)
    end do
  end subroutine format_i64_to_str

  !> @brief Parse an IEEE-754 double-precision number from decimal/scientific string syntax.
  !> @param[in]  text Input numeric text (supports sign, fraction, e/E/d/D exponent).
  !> @param[out] val  Parsed real(r64) value.
  !> @param[out] err  Error status if syntax is invalid.
  pure subroutine parse_r64_strict(text, val, err)
    character(len=*, kind=c_k), intent(in) :: text
    real(r64), intent(out) :: val
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: n, pos, digit_count, exp_digits, d, exp_val
    logical(lk) :: neg, exp_neg
    real(r64) :: mant, frac_scale

    call cdd_error_clear(err)
    val = 0.0_r64
    n = len(text, kind=i32)
    if (n == 0_i32) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Empty floating-point literal")
      return
    end if

    pos = 1_i32
    neg = .false._lk
    if (text(1:1) == "-") then
      neg = .true._lk
      pos = 2_i32
    else if (text(1:1) == "+") then
      pos = 2_i32
    end if

    mant = 0.0_r64
    digit_count = 0_i32
    do while (pos <= n)
      if (.not. is_ascii_digit(text(pos:pos))) exit
      d = iachar(text(pos:pos), kind=i32) - 48_i32
      mant = mant * 10.0_r64 + real(d, kind=r64)
      digit_count = digit_count + 1_i32
      pos = pos + 1_i32
    end do

    if (pos <= n) then
      if (text(pos:pos) == ".") then
        pos = pos + 1_i32
        frac_scale = 0.1_r64
        do while (pos <= n)
          if (.not. is_ascii_digit(text(pos:pos))) exit
          d = iachar(text(pos:pos), kind=i32) - 48_i32
          mant = mant + real(d, kind=r64) * frac_scale
          frac_scale = frac_scale * 0.1_r64
          digit_count = digit_count + 1_i32
          pos = pos + 1_i32
        end do
      end if
    end if

    if (digit_count == 0_i32) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "No digits in floating-point literal")
      return
    end if

    exp_val = 0_i32
    exp_neg = .false._lk
    if (pos <= n) then
      if (char_to_lower(text(pos:pos)) == "e" .or. char_to_lower(text(pos:pos)) == "d") then
        pos = pos + 1_i32
        if (pos <= n) then
          if (text(pos:pos) == "-") then
            exp_neg = .true._lk
            pos = pos + 1_i32
          else if (text(pos:pos) == "+") then
            pos = pos + 1_i32
          end if
        end if
        exp_digits = 0_i32
        do while (pos <= n)
          if (.not. is_ascii_digit(text(pos:pos))) exit
          d = iachar(text(pos:pos), kind=i32) - 48_i32
          if (exp_val < 1000_i32) then
            exp_val = exp_val * 10_i32 + d
          end if
          exp_digits = exp_digits + 1_i32
          pos = pos + 1_i32
        end do
        if (exp_digits == 0_i32) then
          call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Empty exponent in float literal")
          return
        end if
      end if
    end if

    if (pos <= n) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Trailing garbage in float literal")
      return
    end if

    if (exp_neg) then
      mant = mant * (10.0_r64 ** (-exp_val))
    else if (exp_val > 0_i32) then
      mant = mant * (10.0_r64 ** exp_val)
    end if

    if (neg) then
      val = -mant
    else
      val = mant
    end if
  end subroutine parse_r64_strict

  !> @brief Format a finite real(r64) into deterministic JSON-valid decimal syntax.
  !> @param[in]  val     Floating-point number to format.
  !> @param[out] out_str Destination string buffer (at least 32 chars recommended).
  !> @param[out] out_len Number of characters written.
  pure subroutine format_r64_to_str(val, out_str, out_len)
    real(r64), intent(in) :: val
    character(len=*, kind=c_k), intent(out) :: out_str
    integer(i32), intent(out) :: out_len
    real(r64) :: abs_v, scaled
    integer(i64) :: int_scaled, int_part, frac_part
    character(len=24, kind=c_k) :: ibuf
    integer(i32) :: ilen, exp10, k, frac_digits

    out_len = 0_i32
    if (abs(val) < tiny(0.0_r64)) then
      out_str(1:3) = "0.0"
      out_len = 3_i32
      return
    end if

    abs_v = abs(val)
    if (val < 0.0_r64) then
      out_len = 1_i32
      out_str(1:1) = "-"
    end if

    exp10 = 0_i32
    scaled = abs_v
    if (scaled >= 1.0e9_r64 .or. scaled < 1.0e-4_r64) then
      do while (scaled >= 10.0_r64)
        scaled = scaled * 0.1_r64
        exp10 = exp10 + 1_i32
      end do
      do while (scaled < 1.0_r64)
        scaled = scaled * 10.0_r64
        exp10 = exp10 - 1_i32
      end do
    end if

    int_scaled = nint(scaled * 1000000.0_r64, kind=i64)
    int_part = int_scaled / 1000000_i64
    frac_part = mod(int_scaled, 1000000_i64)

    call format_i64_to_str(int_part, ibuf, ilen)
    do k = 1_i32, ilen
      out_len = out_len + 1_i32
      out_str(out_len:out_len) = ibuf(k:k)
    end do

    out_len = out_len + 1_i32
    out_str(out_len:out_len) = "."

    if (frac_part == 0_i64) then
      out_len = out_len + 1_i32
      out_str(out_len:out_len) = "0"
    else
      do k = 6_i32, 1_i32, -1_i32
        ibuf(k:k) = achar(48_i32 + int(mod(frac_part, 10_i64), kind=i32), kind=c_k)
        frac_part = frac_part / 10_i64
      end do
      frac_digits = 6_i32
      do while (frac_digits > 1_i32 .and. ibuf(frac_digits:frac_digits) == "0")
        frac_digits = frac_digits - 1_i32
      end do
      do k = 1_i32, frac_digits
        out_len = out_len + 1_i32
        out_str(out_len:out_len) = ibuf(k:k)
      end do
    end if

    if (exp10 /= 0_i32) then
      out_len = out_len + 1_i32
      out_str(out_len:out_len) = "e"
      call format_i64_to_str(int(exp10, kind=i64), ibuf, ilen)
      do k = 1_i32, ilen
        out_len = out_len + 1_i32
        out_str(out_len:out_len) = ibuf(k:k)
      end do
    end if
  end subroutine format_r64_to_str

end module cdd_string
