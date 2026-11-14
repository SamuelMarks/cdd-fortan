!> @file cdd_errors.f90
!> @brief Single unified error enumeration and structured diagnostic container for cdd-fortran.

!> @brief Repository-wide error codes, diagnostic type, and pure formatting helpers.
module cdd_errors
  use cdd_kinds, only: i32, lk, c_k
  implicit none (type, external)
  private

  public :: CDD_ERROR_MSG_MAX
  public :: CDD_OK
  public :: CDD_ERR_INVALID_OPTION
  public :: CDD_ERR_IO_READ
  public :: CDD_ERR_IO_WRITE
  public :: CDD_ERR_JSON_LEX
  public :: CDD_ERR_JSON_PARSE
  public :: CDD_ERR_JSON_SCHEMA
  public :: CDD_ERR_JSON_INFER
  public :: CDD_ERR_FORTRAN_LEX
  public :: CDD_ERR_FORTRAN_PARSE
  public :: CDD_ERR_UDT_NOT_FOUND
  public :: CDD_ERR_DIALECT_CONSTRAINT
  public :: CDD_ERR_IDENTIFIER_COLLISION
  public :: CDD_ERR_CLI_USAGE
  public :: CDD_ERR_BUFFER_OVERFLOW
  public :: cdd_error_t
  public :: cdd_error_clear
  public :: cdd_error_set
  public :: cdd_error_set_simple
  public :: cdd_has_error
  public :: cdd_error_code_name
  public :: cdd_error_format

  integer(i32), parameter :: CDD_ERROR_MSG_MAX = 256_i32         !< Max diagnostic string byte length.
  integer(i32), parameter :: CDD_OK = 0_i32                      !< Operation succeeded with no error.
  integer(i32), parameter :: CDD_ERR_INVALID_OPTION = 1_i32      !< Invalid SDK option or standard flag.
  integer(i32), parameter :: CDD_ERR_IO_READ = 2_i32             !< Failed to read file or stdin stream.
  integer(i32), parameter :: CDD_ERR_IO_WRITE = 3_i32            !< Failed to write file or stdout stream.
  integer(i32), parameter :: CDD_ERR_JSON_LEX = 4_i32            !< Malformed JSON token or escape sequence.
  integer(i32), parameter :: CDD_ERR_JSON_PARSE = 5_i32          !< Unexpected JSON token or nesting error.
  integer(i32), parameter :: CDD_ERR_JSON_SCHEMA = 6_i32         !< Invalid or unsupported JSON Schema node.
  integer(i32), parameter :: CDD_ERR_JSON_INFER = 7_i32          !< Conflicting sample JSON value types.
  integer(i32), parameter :: CDD_ERR_FORTRAN_LEX = 8_i32         !< Invalid fixed or free-form Fortran token.
  integer(i32), parameter :: CDD_ERR_FORTRAN_PARSE = 9_i32       !< Malformed Fortran UDT or module syntax.
  integer(i32), parameter :: CDD_ERR_UDT_NOT_FOUND = 10_i32      !< Target UDT name not found in source.
  integer(i32), parameter :: CDD_ERR_DIALECT_CONSTRAINT = 11_i32 !< Construct unsupported by target standard.
  integer(i32), parameter :: CDD_ERR_IDENTIFIER_COLLISION = 12_i32 !< Identifier shortening collision exhausted.
  integer(i32), parameter :: CDD_ERR_CLI_USAGE = 13_i32          !< Invalid CLI flag, argument, or subcommand.
  integer(i32), parameter :: CDD_ERR_BUFFER_OVERFLOW = 14_i32    !< Fixed arena or text buffer limit exceeded.

  !> @brief Structured diagnostic record replacing exceptions, unwrap, or library stop calls.
  type :: cdd_error_t
    integer(i32) :: code = CDD_OK                        !< Error enumeration code (CDD_OK when valid).
    integer(i32) :: line = 0_i32                         !< 1-based source line number (0 if N/A).
    integer(i32) :: column = 0_i32                       !< 1-based source column number (0 if N/A).
    integer(i32) :: span_start = 0_i32                   !< 1-based starting byte offset (0 if N/A).
    integer(i32) :: span_end = 0_i32                     !< 1-based ending byte offset (0 if N/A).
    integer(i32) :: message_len = 0_i32                  !< Active byte length in message.
    character(len=CDD_ERROR_MSG_MAX, kind=c_k) :: message !< Primary human-readable diagnostic text.
    integer(i32) :: hint_len = 0_i32                     !< Active byte length in hint.
    character(len=CDD_ERROR_MSG_MAX, kind=c_k) :: hint   !< Optional actionable remediation hint.
  end type cdd_error_t

contains

  !> @brief Reset an error container back to the clean CDD_OK state.
  !> @param[out] err Error record to clear.
  pure subroutine cdd_error_clear(err)
    type(cdd_error_t), intent(out) :: err

    err%code = CDD_OK
    err%line = 0_i32
    err%column = 0_i32
    err%span_start = 0_i32
    err%span_end = 0_i32
    err%message_len = 0_i32
    err%hint_len = 0_i32
  end subroutine cdd_error_clear

  !> @brief Populate a structured error record with full source location and hint metadata.
  !> @param[out] err        Destination error record.
  !> @param[in]  code       Error enumeration constant.
  !> @param[in]  message    Primary diagnostic message string.
  !> @param[in]  line       1-based line number (or 0).
  !> @param[in]  column     1-based column number (or 0).
  !> @param[in]  span_start 1-based byte start offset (or 0).
  !> @param[in]  span_end   1-based byte end offset (or 0).
  !> @param[in]  hint       Optional remediation hint string (empty if none).
  pure subroutine cdd_error_set(err, code, message, line, column, span_start, span_end, hint)
    type(cdd_error_t), intent(out) :: err
    integer(i32), intent(in) :: code
    character(len=*, kind=c_k), intent(in) :: message
    integer(i32), intent(in) :: line
    integer(i32), intent(in) :: column
    integer(i32), intent(in) :: span_start
    integer(i32), intent(in) :: span_end
    character(len=*, kind=c_k), intent(in) :: hint
    integer(i32) :: idx, msg_n, hint_n

    err%code = code
    err%line = line
    err%column = column
    err%span_start = span_start
    err%span_end = span_end

    msg_n = len(message, kind=i32)
    if (msg_n > CDD_ERROR_MSG_MAX) then
      msg_n = CDD_ERROR_MSG_MAX
    end if
    err%message_len = msg_n
    do idx = 1_i32, msg_n
      err%message(idx:idx) = message(idx:idx)
    end do

    hint_n = len(hint, kind=i32)
    if (hint_n > CDD_ERROR_MSG_MAX) then
      hint_n = CDD_ERROR_MSG_MAX
    end if
    err%hint_len = hint_n
    do idx = 1_i32, hint_n
      err%hint(idx:idx) = hint(idx:idx)
    end do
  end subroutine cdd_error_set

  !> @brief Populate an error record with code and message only (zeroed spans and hint).
  !> @param[out] err     Destination error record.
  !> @param[in]  code    Error enumeration constant.
  !> @param[in]  message Primary diagnostic message string.
  pure subroutine cdd_error_set_simple(err, code, message)
    type(cdd_error_t), intent(out) :: err
    integer(i32), intent(in) :: code
    character(len=*, kind=c_k), intent(in) :: message

    call cdd_error_set(err, code, message, 0_i32, 0_i32, 0_i32, 0_i32, "")
  end subroutine cdd_error_set_simple

  !> @brief Check whether an error record holds a non-OK failure status.
  !> @param[in] err Error record to inspect.
  !> @return True if err%code /= CDD_OK, false otherwise.
  pure function cdd_has_error(err) result(has_err)
    type(cdd_error_t), intent(in) :: err
    logical(lk) :: has_err

    has_err = (err%code /= CDD_OK)
  end function cdd_has_error

  !> @brief Look up the canonical symbolic constant name for an error code.
  !> @param[in]  code     Error enumeration integer.
  !> @param[out] name_buf Output character buffer (at least 32 chars recommended).
  !> @param[out] name_len Active length written into name_buf.
  pure subroutine cdd_error_code_name(code, name_buf, name_len)
    integer(i32), intent(in) :: code
    character(len=*, kind=c_k), intent(out) :: name_buf
    integer(i32), intent(out) :: name_len

    select case (code)
    case (CDD_OK)
      call put_lit("CDD_OK", name_buf, name_len)
    case (CDD_ERR_INVALID_OPTION)
      call put_lit("CDD_ERR_INVALID_OPTION", name_buf, name_len)
    case (CDD_ERR_IO_READ)
      call put_lit("CDD_ERR_IO_READ", name_buf, name_len)
    case (CDD_ERR_IO_WRITE)
      call put_lit("CDD_ERR_IO_WRITE", name_buf, name_len)
    case (CDD_ERR_JSON_LEX)
      call put_lit("CDD_ERR_JSON_LEX", name_buf, name_len)
    case (CDD_ERR_JSON_PARSE)
      call put_lit("CDD_ERR_JSON_PARSE", name_buf, name_len)
    case (CDD_ERR_JSON_SCHEMA)
      call put_lit("CDD_ERR_JSON_SCHEMA", name_buf, name_len)
    case (CDD_ERR_JSON_INFER)
      call put_lit("CDD_ERR_JSON_INFER", name_buf, name_len)
    case (CDD_ERR_FORTRAN_LEX)
      call put_lit("CDD_ERR_FORTRAN_LEX", name_buf, name_len)
    case (CDD_ERR_FORTRAN_PARSE)
      call put_lit("CDD_ERR_FORTRAN_PARSE", name_buf, name_len)
    case (CDD_ERR_UDT_NOT_FOUND)
      call put_lit("CDD_ERR_UDT_NOT_FOUND", name_buf, name_len)
    case (CDD_ERR_DIALECT_CONSTRAINT)
      call put_lit("CDD_ERR_DIALECT_CONSTRAINT", name_buf, name_len)
    case (CDD_ERR_IDENTIFIER_COLLISION)
      call put_lit("CDD_ERR_IDENTIFIER_COLLISION", name_buf, name_len)
    case (CDD_ERR_CLI_USAGE)
      call put_lit("CDD_ERR_CLI_USAGE", name_buf, name_len)
    case (CDD_ERR_BUFFER_OVERFLOW)
      call put_lit("CDD_ERR_BUFFER_OVERFLOW", name_buf, name_len)
    case default
      call put_lit("CDD_ERR_UNKNOWN", name_buf, name_len)
    end select
  end subroutine cdd_error_code_name

  !> @brief Format a structured error record into a human-readable diagnostic string.
  !> @param[in]  err     Error record to format.
  !> @param[out] out_buf Destination character buffer.
  !> @param[out] out_len Active byte count written to out_buf.
  pure subroutine cdd_error_format(err, out_buf, out_len)
    type(cdd_error_t), intent(in) :: err
    character(len=*, kind=c_k), intent(out) :: out_buf
    integer(i32), intent(out) :: out_len
    character(len=32, kind=c_k) :: cname
    integer(i32) :: cname_len

    out_len = 0_i32
    if (err%code == CDD_OK) then
      call append_raw("OK", out_buf, out_len)
      return
    end if

    call cdd_error_code_name(err%code, cname, cname_len)
    call append_raw(cname(1:cname_len), out_buf, out_len)

    if (err%line > 0_i32) then
      call append_raw(" (line ", out_buf, out_len)
      call append_pos_i32(err%line, out_buf, out_len)
      if (err%column > 0_i32) then
        call append_raw(", col ", out_buf, out_len)
        call append_pos_i32(err%column, out_buf, out_len)
      end if
      call append_raw(")", out_buf, out_len)
    end if

    if (err%message_len > 0_i32) then
      call append_raw(": ", out_buf, out_len)
      call append_raw(err%message(1:err%message_len), out_buf, out_len)
    end if

    if (err%hint_len > 0_i32) then
      call append_raw(" [hint: ", out_buf, out_len)
      call append_raw(err%hint(1:err%hint_len), out_buf, out_len)
      call append_raw("]", out_buf, out_len)
    end if
  end subroutine cdd_error_format

  !> @brief Copy a literal string into dst up to dst capacity without padding branches.
  !> @param[in]  lit     Source literal string.
  !> @param[out] dst     Destination buffer.
  !> @param[out] dst_len Number of characters written.
  pure subroutine put_lit(lit, dst, dst_len)
    character(len=*, kind=c_k), intent(in) :: lit
    character(len=*, kind=c_k), intent(out) :: dst
    integer(i32), intent(out) :: dst_len

    dst_len = 0_i32
    call append_raw(lit, dst, dst_len)
  end subroutine put_lit

  !> @brief Append characters of src into dst starting at dst_len + 1, clamped to len(dst).
  !> @param[in]    src     Source character slice.
  !> @param[inout] dst     Destination character buffer.
  !> @param[inout] dst_len Active length in dst.
  pure subroutine append_raw(src, dst, dst_len)
    character(len=*, kind=c_k), intent(in) :: src
    character(len=*, kind=c_k), intent(inout) :: dst
    integer(i32), intent(inout) :: dst_len
    integer(i32) :: i, cap, n

    cap = len(dst, kind=i32)
    n = len(src, kind=i32)
    do i = 1_i32, n
      if (dst_len < cap) then
        dst_len = dst_len + 1_i32
        dst(dst_len:dst_len) = src(i:i)
      end if
    end do
  end subroutine append_raw

  !> @brief Append a positive 32-bit decimal integer into dst without internal WRITE.
  !> @param[in]    val     Positive integer value (> 0).
  !> @param[inout] dst     Destination character buffer.
  !> @param[inout] dst_len Active length in dst.
  pure subroutine append_pos_i32(val, dst, dst_len)
    integer(i32), intent(in) :: val
    character(len=*, kind=c_k), intent(inout) :: dst
    integer(i32), intent(inout) :: dst_len
    character(len=12, kind=c_k) :: rev
    integer(i32) :: tmp, digits, d, k

    tmp = val
    digits = 0_i32
    do while (tmp > 0_i32)
      d = mod(tmp, 10_i32)
      digits = digits + 1_i32
      rev(digits:digits) = achar(48_i32 + d, kind=c_k)
      tmp = tmp / 10_i32
    end do
    do k = digits, 1_i32, -1_i32
      call append_raw(rev(k:k), dst, dst_len)
    end do
  end subroutine append_pos_i32

end module cdd_errors
