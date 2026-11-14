!> @file cdd_json_lexer.f90
!> @brief Zero-dependency RFC 8259 JSON tokenizer with exact line/column diagnostics.

!> @brief Pure JSON lexical scanner validating string escapes, UTF-16 surrogates, and numbers.
module cdd_json_lexer
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set, &
    CDD_ERR_JSON_LEX, CDD_ERR_BUFFER_OVERFLOW, CDD_ERR_INVALID_OPTION
  use cdd_string, only: cdd_buffer_t, buf_init, unescape_json_to_buf, is_ascii_digit, str_eq
  use cdd_json_tokens, only: CDD_MAX_JTOKENS, JTOK_EOF, JTOK_LBRACE, JTOK_RBRACE, &
    JTOK_LBRACKET, JTOK_RBRACKET, JTOK_COLON, JTOK_COMMA, JTOK_STRING, JTOK_INTEGER, &
    JTOK_FLOAT, JTOK_TRUE, JTOK_FALSE, JTOK_NULL, cdd_json_token_stream_t, jstream_init
  implicit none (type, external)
  private

  public :: json_lex, json_lex_bounded

contains

  !> @brief Tokenize an RFC 8259 JSON document using full token stream capacity.
  !> @param[in]  source Input JSON text.
  !> @param[out] stream Populated token stream ending with JTOK_EOF.
  !> @param[out] err    Structured error status on lexical failure.
  pure subroutine json_lex(source, stream, err)
    character(len=*, kind=c_k), intent(in) :: source
    type(cdd_json_token_stream_t), intent(out) :: stream
    type(cdd_error_t), intent(out) :: err

    call json_lex_bounded(source, CDD_MAX_JTOKENS, stream, err)
  end subroutine json_lex

  !> @brief Tokenize an RFC 8259 JSON document with an explicit maximum token quota.
  !> @param[in]  source   Input JSON text.
  !> @param[in]  max_toks Maximum allowed tokens (1..CDD_MAX_JTOKENS).
  !> @param[out] stream   Populated token stream ending with JTOK_EOF.
  !> @param[out] err      Structured error status.
  pure subroutine json_lex_bounded(source, max_toks, stream, err)
    character(len=*, kind=c_k), intent(in) :: source
    integer(i32), intent(in) :: max_toks
    type(cdd_json_token_stream_t), intent(out) :: stream
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: n, pos, line, col, start_pos, start_col, end_pos, tok_kind
    character(len=1, kind=c_k) :: ch
    character(len=1, kind=c_k), parameter :: CH_LF = achar(10, kind=c_k)
    character(len=1, kind=c_k), parameter :: CH_CR = achar(13, kind=c_k)
    character(len=1, kind=c_k), parameter :: CH_TAB = achar(9, kind=c_k)
    character(len=1, kind=c_k), parameter :: CH_QT = achar(34, kind=c_k)

    call jstream_init(stream)
    call cdd_error_clear(err)
    if (max_toks < 1_i32 .or. max_toks > CDD_MAX_JTOKENS) then
      call cdd_error_set(err, CDD_ERR_INVALID_OPTION, "Invalid JSON max_toks limit", &
        1_i32, 1_i32, 1_i32, 1_i32, "")
      return
    end if
    stream%max_tokens = max_toks

    n = len(source, kind=i32)
    pos = 1_i32
    line = 1_i32
    col = 1_i32

    do while (pos <= n)
      ch = source(pos:pos)
      if (ch == " " .or. ch == CH_TAB) then
        pos = pos + 1_i32
        col = col + 1_i32
        cycle
      else if (ch == CH_CR) then
        pos = pos + 1_i32
        if (pos <= n) then
          if (source(pos:pos) == CH_LF) then
            pos = pos + 1_i32
          end if
        end if
        line = line + 1_i32
        col = 1_i32
        cycle
      else if (ch == CH_LF) then
        pos = pos + 1_i32
        line = line + 1_i32
        col = 1_i32
        cycle
      end if

      start_pos = pos
      start_col = col
      end_pos = pos

      if (ch == "{") then
        tok_kind = JTOK_LBRACE
        pos = pos + 1_i32
        col = col + 1_i32
      else if (ch == "}") then
        tok_kind = JTOK_RBRACE
        pos = pos + 1_i32
        col = col + 1_i32
      else if (ch == "[") then
        tok_kind = JTOK_LBRACKET
        pos = pos + 1_i32
        col = col + 1_i32
      else if (ch == "]") then
        tok_kind = JTOK_RBRACKET
        pos = pos + 1_i32
        col = col + 1_i32
      else if (ch == ":") then
        tok_kind = JTOK_COLON
        pos = pos + 1_i32
        col = col + 1_i32
      else if (ch == ",") then
        tok_kind = JTOK_COMMA
        pos = pos + 1_i32
        col = col + 1_i32
      else if (ch == CH_QT) then
        tok_kind = JTOK_STRING
        call scan_json_string(source, n, pos, line, col, start_pos, end_pos, err)
        if (err%code /= 0_i32) return
      else if (ch == "-" .or. is_ascii_digit(ch)) then
        call scan_json_number(source, n, pos, col, tok_kind, line, start_pos, start_col, err)
        if (err%code /= 0_i32) return
        end_pos = pos - 1_i32
      else if (ch == "t") then
        if (.not. match_kw(source, n, pos, "true")) then
          call cdd_error_set(err, CDD_ERR_JSON_LEX, "Invalid literal starting with 't'", &
            line, col, pos, pos, "Expected 'true'")
          return
        end if
        tok_kind = JTOK_TRUE
        end_pos = pos + 3_i32
        pos = pos + 4_i32
        col = col + 4_i32
      else if (ch == "f") then
        if (.not. match_kw(source, n, pos, "false")) then
          call cdd_error_set(err, CDD_ERR_JSON_LEX, "Invalid literal starting with 'f'", &
            line, col, pos, pos, "Expected 'false'")
          return
        end if
        tok_kind = JTOK_FALSE
        end_pos = pos + 4_i32
        pos = pos + 5_i32
        col = col + 5_i32
      else if (ch == "n") then
        if (.not. match_kw(source, n, pos, "null")) then
          call cdd_error_set(err, CDD_ERR_JSON_LEX, "Invalid literal starting with 'n'", &
            line, col, pos, pos, "Expected 'null'")
          return
        end if
        tok_kind = JTOK_NULL
        end_pos = pos + 3_i32
        pos = pos + 4_i32
        col = col + 4_i32
      else
        call cdd_error_set(err, CDD_ERR_JSON_LEX, "Unexpected character in JSON", &
          line, col, pos, pos, "Ensure keys and strings use double quotes")
        return
      end if

      call emit_tok(stream, tok_kind, line, start_col, start_pos, end_pos, err)
      if (err%code /= 0_i32) return
    end do

    call emit_tok(stream, JTOK_EOF, line, col, pos, pos - 1_i32, err)
  end subroutine json_lex_bounded

  !> @brief Check whether source(pos:) begins with exact keyword kw followed by delimiter/EOF.
  !> @param[in] source Input JSON text.
  !> @param[in] n      Length of source.
  !> @param[in] pos    1-based start offset.
  !> @param[in] kw     Expected lowercase keyword literal.
  !> @return True if matched and delimited.
  pure function match_kw(source, n, pos, kw) result(ok)
    character(len=*, kind=c_k), intent(in) :: source
    integer(i32), intent(in) :: n
    integer(i32), intent(in) :: pos
    character(len=*, kind=c_k), intent(in) :: kw
    logical(lk) :: ok
    integer(i32) :: kw_len

    kw_len = len(kw, kind=i32)
    if (pos + kw_len - 1_i32 > n) then
      ok = .false._lk
      return
    end if
    ok = str_eq(source(pos:pos + kw_len - 1_i32), kw) .and. &
         is_delim_or_eof(source, n, pos + kw_len)
  end function match_kw

  !> @brief Append a single token to stream, checking stream%max_tokens capacity.
  !> @param[inout] stream Target token stream.
  !> @param[in]    kind   Token category constant.
  !> @param[in]    line   1-based line number.
  !> @param[in]    col    1-based column number.
  !> @param[in]    s_beg  1-based start offset.
  !> @param[in]    s_end  1-based inclusive end offset.
  !> @param[out]   err    Error status if token stream overflows.
  pure subroutine emit_tok(stream, kind, line, col, s_beg, s_end, err)
    type(cdd_json_token_stream_t), intent(inout) :: stream
    integer(i32), intent(in) :: kind
    integer(i32), intent(in) :: line
    integer(i32), intent(in) :: col
    integer(i32), intent(in) :: s_beg
    integer(i32), intent(in) :: s_end
    type(cdd_error_t), intent(out) :: err

    call cdd_error_clear(err)
    if (stream%count >= stream%max_tokens) then
      call cdd_error_set(err, CDD_ERR_BUFFER_OVERFLOW, "JSON token stream limit exceeded", &
        line, col, s_beg, s_end, "")
      return
    end if
    stream%count = stream%count + 1_i32
    stream%tokens(stream%count)%kind = kind
    stream%tokens(stream%count)%line = line
    stream%tokens(stream%count)%column = col
    stream%tokens(stream%count)%span_start = s_beg
    stream%tokens(stream%count)%span_end = s_end
  end subroutine emit_tok

  !> @brief Scan a double-quoted JSON string, returning interior [inner_beg..inner_end] offsets.
  !> @param[in]    source    Input JSON text.
  !> @param[in]    n         Total length of source.
  !> @param[inout] pos       Current 1-based byte offset (points to opening quote on entry).
  !> @param[in]    line      Current 1-based line number.
  !> @param[inout] col       Current 1-based column number.
  !> @param[out]   inner_beg 1-based first char after opening quote.
  !> @param[out]   inner_end 1-based last char before closing quote.
  !> @param[out]   err       Error status if unterminated or invalid escape.
  pure subroutine scan_json_string(source, n, pos, line, col, inner_beg, inner_end, err)
    character(len=*, kind=c_k), intent(in) :: source
    integer(i32), intent(in) :: n
    integer(i32), intent(inout) :: pos
    integer(i32), intent(in) :: line
    integer(i32), intent(inout) :: col
    integer(i32), intent(out) :: inner_beg
    integer(i32), intent(out) :: inner_end
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: quote_pos, quote_col
    type(cdd_buffer_t) :: scratch
    character(len=1, kind=c_k), parameter :: BS = achar(92, kind=c_k)
    character(len=1, kind=c_k), parameter :: QT = achar(34, kind=c_k)

    call cdd_error_clear(err)
    quote_pos = pos
    quote_col = col
    pos = pos + 1_i32
    col = col + 1_i32
    inner_beg = pos
    inner_end = pos - 1_i32

    do while (pos <= n)
      if (iachar(source(pos:pos), kind=i32) < 32_i32) then
        call cdd_error_set(err, CDD_ERR_JSON_LEX, "Unescaped control character in JSON string", &
          line, col, quote_pos, pos, "")
        return
      end if
      if (source(pos:pos) == QT) then
        inner_end = pos - 1_i32
        pos = pos + 1_i32
        col = col + 1_i32
        call buf_init(scratch)
        if (inner_end >= inner_beg) then
          call unescape_json_to_buf(source(inner_beg:inner_end), scratch, err)
          if (err%code /= 0_i32) then
            err%line = line
            err%column = quote_col
            err%span_start = quote_pos
            err%span_end = pos - 1_i32
          end if
        end if
        return
      else if (source(pos:pos) == BS) then
        if (pos == n) then
          call cdd_error_set(err, CDD_ERR_JSON_LEX, "Unterminated escape at end of JSON string", &
            line, quote_col, quote_pos, pos, "")
          return
        end if
        pos = pos + 2_i32
        col = col + 2_i32
      else
        pos = pos + 1_i32
        col = col + 1_i32
      end if
    end do

    call cdd_error_set(err, CDD_ERR_JSON_LEX, "Unterminated JSON string literal", &
      line, quote_col, quote_pos, n, "")
  end subroutine scan_json_string

  !> @brief Scan an RFC 8259 number literal and classify as JTOK_INTEGER or JTOK_FLOAT.
  !> @param[in]    source    Input JSON text.
  !> @param[in]    n         Total length of source.
  !> @param[inout] pos       Current 1-based offset.
  !> @param[inout] col       Current 1-based column.
  !> @param[out]   tok_kind  Classified token kind (JTOK_INTEGER or JTOK_FLOAT).
  !> @param[in]    line      Current 1-based line number.
  !> @param[in]    start_pos Starting byte offset of number.
  !> @param[in]    start_col Starting column of number.
  !> @param[out]   err       Error status on malformed number syntax.
  pure subroutine scan_json_number(source, n, pos, col, tok_kind, line, start_pos, start_col, err)
    character(len=*, kind=c_k), intent(in) :: source
    integer(i32), intent(in) :: n
    integer(i32), intent(inout) :: pos
    integer(i32), intent(inout) :: col
    integer(i32), intent(out) :: tok_kind
    integer(i32), intent(in) :: line
    integer(i32), intent(in) :: start_pos
    integer(i32), intent(in) :: start_col
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: frac_digits, exp_digits

    call cdd_error_clear(err)
    tok_kind = JTOK_INTEGER

    if (source(pos:pos) == "-") then
      pos = pos + 1_i32
      col = col + 1_i32
      if (pos > n) then
        call cdd_error_set(err, CDD_ERR_JSON_LEX, "Bare minus sign in JSON", &
          line, start_col, start_pos, pos - 1_i32, "")
        return
      end if
      if (.not. is_ascii_digit(source(pos:pos))) then
        call cdd_error_set(err, CDD_ERR_JSON_LEX, "Expected digit after '-' in JSON", &
          line, start_col, start_pos, pos, "")
        return
      end if
    end if

    if (source(pos:pos) == "0") then
      pos = pos + 1_i32
      col = col + 1_i32
      if (pos <= n) then
        if (is_ascii_digit(source(pos:pos))) then
          call cdd_error_set(err, CDD_ERR_JSON_LEX, "Leading zeros are forbidden in JSON numbers", &
            line, start_col, start_pos, pos, "")
          return
        end if
      end if
    else
      do while (pos <= n)
        if (.not. is_ascii_digit(source(pos:pos))) exit
        pos = pos + 1_i32
        col = col + 1_i32
      end do
    end if

    if (pos <= n) then
      if (source(pos:pos) == ".") then
        tok_kind = JTOK_FLOAT
        pos = pos + 1_i32
        col = col + 1_i32
        frac_digits = 0_i32
        do while (pos <= n)
          if (.not. is_ascii_digit(source(pos:pos))) exit
          frac_digits = frac_digits + 1_i32
          pos = pos + 1_i32
          col = col + 1_i32
        end do
        if (frac_digits == 0_i32) then
          call cdd_error_set(err, CDD_ERR_JSON_LEX, "Missing fractional digits after '.' in JSON", &
            line, start_col, start_pos, pos - 1_i32, "")
          return
        end if
      end if
    end if

    if (pos <= n) then
      if (source(pos:pos) == "e" .or. source(pos:pos) == "E") then
        tok_kind = JTOK_FLOAT
        pos = pos + 1_i32
        col = col + 1_i32
        if (pos <= n) then
          if (source(pos:pos) == "+" .or. source(pos:pos) == "-") then
            pos = pos + 1_i32
            col = col + 1_i32
          end if
        end if
        exp_digits = 0_i32
        do while (pos <= n)
          if (.not. is_ascii_digit(source(pos:pos))) exit
          exp_digits = exp_digits + 1_i32
          pos = pos + 1_i32
          col = col + 1_i32
        end do
        if (exp_digits == 0_i32) then
          call cdd_error_set(err, CDD_ERR_JSON_LEX, "Missing exponent digits in JSON number", &
            line, start_col, start_pos, pos - 1_i32, "")
          return
        end if
      end if
    end if

    if (.not. is_delim_or_eof(source, n, pos)) then
      call cdd_error_set(err, CDD_ERR_JSON_LEX, "Invalid trailing character on JSON number", &
        line, start_col, start_pos, pos, "")
    end if
  end subroutine scan_json_number

  !> @brief Check if offset idx in source(1:n) is past EOF or at a valid JSON delimiter.
  !> @param[in] source Input JSON text.
  !> @param[in] n      Length of source.
  !> @param[in] idx    1-based index to test.
  !> @return True if idx > n or source(idx:idx) is whitespace or structural punctuation.
  pure function is_delim_or_eof(source, n, idx) result(ok)
    character(len=*, kind=c_k), intent(in) :: source
    integer(i32), intent(in) :: n
    integer(i32), intent(in) :: idx
    logical(lk) :: ok
    character(len=1, kind=c_k) :: c
    integer(i32) :: code

    if (idx > n) then
      ok = .true._lk
      return
    end if
    c = source(idx:idx)
    code = iachar(c, kind=i32)
    ok = (code == 32_i32 .or. code == 9_i32 .or. code == 10_i32 .or. code == 13_i32 .or. &
          c == "," .or. c == "}" .or. c == "]" .or. c == ":")
  end function is_delim_or_eof

end module cdd_json_lexer
