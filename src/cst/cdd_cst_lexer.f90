!> @file cdd_cst_lexer.f90
!> @brief Lossless dual-mode (fixed-form cols 1-80 and free-form) Fortran CST lexer.

!> @brief Tokenizes FORTRAN I..2023 preserving 100% of comments, continuations, and card columns.
module cdd_cst_lexer
  use cdd_kinds, only: i32, i64, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set, &
    CDD_ERR_FORTRAN_LEX
  use cdd_string, only: TERM_LF, TERM_CRLF, cdd_buffer_t, cdd_line_slice_t, &
    buf_init, buf_append_char, buf_append_str, split_lines_lossless, &
    char_to_upper, is_ascii_alpha, is_ascii_digit, is_ascii_alnum, starts_with, parse_i64_strict
  use cdd_standard, only: FORM_FIXED, cdd_options_t, cdd_dialect_caps_t, &
    get_dialect_capabilities, resolve_effective_form
  use cdd_cst_trivia, only: TRIVIA_WHITESPACE, TRIVIA_NEWLINE, TRIVIA_LINE_COMMENT, &
    TRIVIA_DOXYGEN_COMMENT, TRIVIA_CONTINUATION_FREE, TRIVIA_FIXED_LABEL_GAP, &
    TRIVIA_FIXED_MARGIN_73_80, TRIVIA_DIRECTIVE
  use cdd_cst_tokens, only: FTOK_EOF, FTOK_IDENT, FTOK_INT_LIT, FTOK_REAL_LIT, FTOK_STR_LIT, &
    FTOK_HOLLERITH, FTOK_LABEL, FTOK_LPAREN, FTOK_RPAREN, FTOK_LBRACKET, FTOK_RBRACKET, &
    FTOK_COMMA, FTOK_COLON, FTOK_DCOLON, FTOK_SEMI, FTOK_EQ, FTOK_ARROW, FTOK_SLASH, &
    FTOK_CONC, FTOK_STAR, FTOK_DOT_OP, FTOK_QUESTION, FTOK_OP, &
    cdd_cst_token_stream_t, fstream_init, fstream_add_trivia, fstream_add_token
  implicit none (type, external)
  private

  public :: CDD_MAX_SRC_LINES
  public :: cst_lex, cst_lex_into

  integer(i32), parameter :: CDD_MAX_SRC_LINES = 8192_i32 !< Max physical lines per source unit.

contains

  !> @brief Tokenize Fortran source in fixed or free form with 100% byte-exact trivia retention.
  !> @param[in]  source  Input Fortran source code.
  !> @param[in]  options Dialect and source-form configuration.
  !> @param[out] stream  Populated lossless CST token & trivia stream.
  !> @param[out] err     Structured error status on lexical violation.
  pure subroutine cst_lex(source, options, stream, err)
    character(len=*, kind=c_k), intent(in) :: source
    type(cdd_options_t), intent(in) :: options
    type(cdd_cst_token_stream_t), intent(out) :: stream
    type(cdd_error_t), intent(out) :: err

    call fstream_init(stream)
    call cst_lex_into(source, options, stream, err)
  end subroutine cst_lex

  !> @brief Tokenize Fortran source into a pre-configured cdd_cst_token_stream_t arena.
  !> @param[in]    source  Input Fortran source code.
  !> @param[in]    options Dialect and source-form configuration.
  !> @param[inout] stream  Pre-initialized CST token & trivia stream.
  !> @param[out]   err     Structured error status on lexical violation.
  pure subroutine cst_lex_into(source, options, stream, err)
    character(len=*, kind=c_k), intent(in) :: source
    type(cdd_options_t), intent(in) :: options
    type(cdd_cst_token_stream_t), intent(inout) :: stream
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: form
    type(cdd_dialect_caps_t) :: caps

    call resolve_effective_form(options, form, err)
    if (err%code /= 0_i32) return

    call buf_append_str(stream%source_buf, source, err)
    if (err%code /= 0_i32) return

    caps = get_dialect_capabilities(options%standard, options%allow_dec_structure)
    if (form == FORM_FIXED) then
      call lex_fixed_form(source, stream, err)
    else
      call lex_free_form(source, caps%max_line_width, stream, err)
    end if
  end subroutine cst_lex_into

  !> @brief Losslessly lex punch-card Fixed-Form Fortran (cols 1-5 label, 6 cont, 7-72 code, 73-80 ID).
  !> @param[in]    src    Input source text.
  !> @param[inout] stream Target CST token stream.
  !> @param[out]   err    Error status.
  pure subroutine lex_fixed_form(src, stream, err)
    character(len=*, kind=c_k), intent(in) :: src
    type(cdd_cst_token_stream_t), intent(inout) :: stream
    type(cdd_error_t), intent(out) :: err
    type(cdd_line_slice_t) :: lines(CDD_MAX_SRC_LINES)
    integer(i32) :: lcount, ln, l_beg, l_end, l_len, body_end, pos, pend_first, pend_cnt
    integer(i32) :: last_stmt_tok, line_last_tok, tidx, tok_idx, ckind
    logical(lk) :: is_cont, is_com

    call cdd_error_clear(err)
    call split_lines_lossless(src, CDD_MAX_SRC_LINES, lines, lcount, err)

    pend_first = 1_i32
    pend_cnt = 0_i32
    last_stmt_tok = 0_i32
    ln = 1_i32

    do while (ln <= lcount)
      l_beg = lines(ln)%first_idx
      l_end = lines(ln)%last_idx
      l_len = l_end - l_beg + 1_i32
      body_end = min(l_end, l_beg + 71_i32)

      call check_fixed_comment_or_blank(src, l_beg, l_end, body_end, is_com, ckind)
      if (is_com) then
        if (body_end >= l_beg) then
          call push_pending_trivia(stream, ckind, l_beg, body_end - l_beg + 1_i32, &
            0_i32, pend_first, pend_cnt, err)
        end if
        if (l_len > 72_i32) then
          call push_pending_trivia(stream, TRIVIA_FIXED_MARGIN_73_80, l_beg + 72_i32, &
            l_end - (l_beg + 72_i32) + 1_i32, 0_i32, pend_first, pend_cnt, err)
        end if
        call push_line_term_pending(stream, lines(ln), pend_first, pend_cnt, err)
        ln = ln + 1_i32
        cycle
      end if

      is_cont = .false._lk
      if (l_len >= 6_i32) then
        if (src(l_beg + 5_i32:l_beg + 5_i32) /= " " .and. &
            src(l_beg + 5_i32:l_beg + 5_i32) /= "0") then
          is_cont = .true._lk
        end if
      end if

      if (.not. is_cont .and. last_stmt_tok > 0_i32) then
        stream%tokens(last_stmt_tok)%stmt_end = .true._lk
      end if

      line_last_tok = 0_i32
      if (is_cont) then
        call push_pending_trivia(stream, TRIVIA_FIXED_LABEL_GAP, l_beg, 6_i32, &
          0_i32, pend_first, pend_cnt, err)
        pos = l_beg + 6_i32
      else
        call scan_fixed_label_cols(src, ln, l_beg, min(l_end, l_beg + 5_i32), &
          stream, pend_first, pend_cnt, line_last_tok, err)
        if (line_last_tok > 0_i32) last_stmt_tok = line_last_tok
        pos = l_beg + 6_i32
      end if
      if (err%code /= 0_i32) return

      do while (pos <= body_end)
        if (src(pos:pos) == " " .or. src(pos:pos) == achar(9, c_k)) then
          call consume_horiz_ws(src, body_end, pos, stream, pend_first, pend_cnt, err)
        else if (src(pos:pos) == "!") then
          ckind = classify_bang_comment(src, pos, body_end)
          if (line_last_tok > 0_i32) then
            if (pend_cnt > 0_i32) then
              stream%tokens(line_last_tok)%trail_count = &
                stream%tokens(line_last_tok)%trail_count + pend_cnt
              pend_cnt = 0_i32
            end if
            call append_trail_trivia(stream, line_last_tok, ckind, pos, &
              body_end - pos + 1_i32, 0_i32, err)
            pend_first = stream%trivia_count + 1_i32
          else
            call push_pending_trivia(stream, ckind, pos, body_end - pos + 1_i32, &
              0_i32, pend_first, pend_cnt, err)
          end if
          pos = body_end + 1_i32
        else
          call scan_fixed_body_token(src, lines, lcount, ln, l_beg, l_end, body_end, &
            pos, stream, pend_first, pend_cnt, tok_idx, err)
          if (err%code /= 0_i32) return
          line_last_tok = tok_idx
          last_stmt_tok = tok_idx
        end if
      end do

      l_len = l_end - l_beg + 1_i32
      if (line_last_tok > 0_i32) then
        if (pend_cnt > 0_i32) then
          stream%tokens(line_last_tok)%trail_count = &
            stream%tokens(line_last_tok)%trail_count + pend_cnt
          pend_cnt = 0_i32
          pend_first = stream%trivia_count + 1_i32
        end if
        if (l_len > 72_i32) then
          call append_trail_trivia(stream, line_last_tok, TRIVIA_FIXED_MARGIN_73_80, &
            l_beg + 72_i32, l_end - (l_beg + 72_i32) + 1_i32, 0_i32, err)
        end if
        call append_line_term_trail(stream, line_last_tok, lines(ln), err)
        pend_first = stream%trivia_count + 1_i32
      else
        if (l_len > 72_i32) then
          call push_pending_trivia(stream, TRIVIA_FIXED_MARGIN_73_80, l_beg + 72_i32, &
            l_end - (l_beg + 72_i32) + 1_i32, 0_i32, pend_first, pend_cnt, err)
        end if
        call push_line_term_pending(stream, lines(ln), pend_first, pend_cnt, err)
      end if
      ln = ln + 1_i32
    end do

    if (last_stmt_tok > 0_i32) then
      stream%tokens(last_stmt_tok)%stmt_end = .true._lk
    end if
    call fstream_add_token(stream, FTOK_EOF, lcount + 1_i32, 1_i32, &
      len(src, kind=i32) + 1_i32, 0_i32, "", pend_first, pend_cnt, tidx, err)
  end subroutine lex_fixed_form

  !> @brief Losslessly lex Free-Form Fortran (F90..F2023) with '&' continuations and '!' trivia.
  !> @param[in]    src            Input source text.
  !> @param[in]    max_line_width Maximum allowed characters per line (132 or 10000).
  !> @param[inout] stream         Target CST token stream.
  !> @param[out]   err            Error status.
  pure subroutine lex_free_form(src, max_line_width, stream, err)
    character(len=*, kind=c_k), intent(in) :: src
    integer(i32), intent(in) :: max_line_width
    type(cdd_cst_token_stream_t), intent(inout) :: stream
    type(cdd_error_t), intent(out) :: err
    type(cdd_line_slice_t) :: lines(CDD_MAX_SRC_LINES)
    integer(i32) :: lcount, ln, l_beg, l_end, l_len, pos, pend_first, pend_cnt
    integer(i32) :: line_last_tok, last_stmt_tok, tok_idx, ckind, eof_idx, amp_end
    logical(lk) :: continued_line, prev_continued

    call cdd_error_clear(err)
    call split_lines_lossless(src, CDD_MAX_SRC_LINES, lines, lcount, err)

    pend_first = 1_i32
    pend_cnt = 0_i32
    last_stmt_tok = 0_i32
    prev_continued = .false._lk

    do ln = 1_i32, lcount
      l_beg = lines(ln)%first_idx
      l_end = lines(ln)%last_idx
      l_len = l_end - l_beg + 1_i32

      if (l_len > max_line_width) then
        call cdd_error_set(err, CDD_ERR_FORTRAN_LEX, &
          "Free-form line exceeds maximum standard line width", ln, max_line_width + 1_i32, &
          l_beg, l_end, "")
        return
      end if

      if (l_len > 0_i32 .and. src(l_beg:l_beg) == "#") then
        call push_pending_trivia(stream, TRIVIA_DIRECTIVE, l_beg, l_len, &
          0_i32, pend_first, pend_cnt, err)
        call push_line_term_pending(stream, lines(ln), pend_first, pend_cnt, err)
        cycle
      end if

      pos = l_beg
      line_last_tok = 0_i32
      continued_line = .false._lk

      if (prev_continued) then
        call consume_horiz_ws(src, l_end, pos, stream, pend_first, pend_cnt, err)
        if (pos <= l_end) then
          if (src(pos:pos) == "&") then
            call push_pending_trivia(stream, TRIVIA_CONTINUATION_FREE, pos, 1_i32, &
              0_i32, pend_first, pend_cnt, err)
            pos = pos + 1_i32
          end if
        end if
      end if

      do while (pos <= l_end)
        if (src(pos:pos) == " " .or. src(pos:pos) == achar(9, c_k)) then
          call consume_horiz_ws(src, l_end, pos, stream, pend_first, pend_cnt, err)
        else if (src(pos:pos) == "!") then
          ckind = classify_bang_comment(src, pos, l_end)
          if (line_last_tok > 0_i32 .and. .not. continued_line) then
            if (pend_cnt > 0_i32) then
              stream%tokens(line_last_tok)%trail_count = &
                stream%tokens(line_last_tok)%trail_count + pend_cnt
              pend_cnt = 0_i32
            end if
            call append_trail_trivia(stream, line_last_tok, ckind, pos, &
              l_end - pos + 1_i32, 0_i32, err)
            pend_first = stream%trivia_count + 1_i32
          else
            call push_pending_trivia(stream, ckind, pos, l_end - pos + 1_i32, &
              0_i32, pend_first, pend_cnt, err)
          end if
          pos = l_end + 1_i32
        else if (src(pos:pos) == "&") then
          amp_end = pos
          if (pos < l_end) then
            if (.not. is_only_ws_or_comment(src, pos + 1_i32, l_end)) then
              call cdd_error_set(err, CDD_ERR_FORTRAN_LEX, &
                "Unexpected token after '&' continuation on same line", &
                ln, pos - l_beg + 1_i32, pos, l_end, "")
              return
            end if
          end if
          continued_line = .true._lk
          call push_pending_trivia(stream, TRIVIA_CONTINUATION_FREE, pos, 1_i32, &
            0_i32, pend_first, pend_cnt, err)
          pos = amp_end + 1_i32
        else
          if (.not. prev_continued .and. line_last_tok == 0_i32 .and. last_stmt_tok > 0_i32) then
            stream%tokens(last_stmt_tok)%stmt_end = .true._lk
          end if
          call scan_free_token(src, ln, l_beg, l_end, pos, stream, pend_first, pend_cnt, &
            tok_idx, err)
          if (err%code /= 0_i32) return
          line_last_tok = tok_idx
          last_stmt_tok = tok_idx
          if (stream%tokens(tok_idx)%kind == FTOK_SEMI) then
            stream%tokens(tok_idx)%stmt_end = .true._lk
          end if
        end if
      end do

      if (line_last_tok > 0_i32 .and. .not. continued_line) then
        if (pend_cnt > 0_i32) then
          stream%tokens(line_last_tok)%trail_count = &
            stream%tokens(line_last_tok)%trail_count + pend_cnt
          pend_cnt = 0_i32
        end if
        call append_line_term_trail(stream, line_last_tok, lines(ln), err)
        pend_first = stream%trivia_count + 1_i32
        prev_continued = .false._lk
      else
        call push_line_term_pending(stream, lines(ln), pend_first, pend_cnt, err)
        if (continued_line) then
          prev_continued = .true._lk
        end if
      end if
    end do

    if (prev_continued) then
      call cdd_error_set(err, CDD_ERR_FORTRAN_LEX, &
        "Dangling '&' continuation at end of file", lcount, 1_i32, len(src, kind=i32), &
        len(src, kind=i32), "")
      return
    end if

    if (last_stmt_tok > 0_i32) then
      stream%tokens(last_stmt_tok)%stmt_end = .true._lk
    end if
    call fstream_add_token(stream, FTOK_EOF, lcount + 1_i32, 1_i32, &
      len(src, kind=i32) + 1_i32, 0_i32, "", pend_first, pend_cnt, eof_idx, err)
  end subroutine lex_free_form

  !> @brief Determine whether a fixed-form line is a full-line comment, directive, or blank card.
  !> @param[in]  src      Source string.
  !> @param[in]  l_beg    1-based line start.
  !> @param[in]  l_end    1-based line content end.
  !> @param[in]  body_end 1-based end of column 72.
  !> @param[out] is_com   True if line has no Fortran statement tokens.
  !> @param[out] ckind    Trivia kind when is_com is true.
  pure subroutine check_fixed_comment_or_blank(src, l_beg, l_end, body_end, is_com, ckind)
    character(len=*, kind=c_k), intent(in) :: src
    integer(i32), intent(in) :: l_beg
    integer(i32), intent(in) :: l_end
    integer(i32), intent(in) :: body_end
    logical(lk), intent(out) :: is_com
    integer(i32), intent(out) :: ckind
    character(len=1, kind=c_k) :: c1, c2
    integer(i32) :: k

    ckind = TRIVIA_WHITESPACE
    if (l_end < l_beg) then
      is_com = .true._lk
      return
    end if

    c1 = src(l_beg:l_beg)
    if (c1 == "#") then
      ckind = TRIVIA_DIRECTIVE
      is_com = .true._lk
      return
    end if

    if (c1 == "C" .or. c1 == "c" .or. c1 == "*" .or. c1 == "!") then
      ckind = TRIVIA_LINE_COMMENT
      if (l_end >= l_beg + 1_i32) then
        c2 = src(l_beg + 1_i32:l_beg + 1_i32)
        if (c2 == "$") then
          ckind = TRIVIA_DIRECTIVE
        else if (c2 == ">" .or. c2 == "<" .or. c2 == "!") then
          ckind = TRIVIA_DOXYGEN_COMMENT
        end if
      end if
      is_com = .true._lk
      return
    end if

    do k = l_beg, body_end
      if (src(k:k) /= " " .and. src(k:k) /= achar(9, c_k)) then
        is_com = .false._lk
        return
      end if
    end do
    ckind = TRIVIA_WHITESPACE
    is_com = .true._lk
  end subroutine check_fixed_comment_or_blank

  !> @brief Scan fixed-form columns 1..6 on an initial (non-continuation) card.
  !> @param[in]    src           Source text.
  !> @param[in]    ln            1-based line number.
  !> @param[in]    l_beg         1-based start of line.
  !> @param[in]    col6_end      min(l_end, l_beg + 5).
  !> @param[inout] stream        Token stream.
  !> @param[inout] pend_first    Pending trivia start.
  !> @param[inout] pend_cnt      Pending trivia count.
  !> @param[inout] line_last_tok Updated if a statement label token is emitted.
  !> @param[out]   err           Error status.
  pure subroutine scan_fixed_label_cols(src, ln, l_beg, col6_end, stream, &
      pend_first, pend_cnt, line_last_tok, err)
    character(len=*, kind=c_k), intent(in) :: src
    integer(i32), intent(in) :: ln
    integer(i32), intent(in) :: l_beg
    integer(i32), intent(in) :: col6_end
    type(cdd_cst_token_stream_t), intent(inout) :: stream
    integer(i32), intent(inout) :: pend_first
    integer(i32), intent(inout) :: pend_cnt
    integer(i32), intent(inout) :: line_last_tok
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: label_end, p, d_beg, d_end, k, tok_idx
    character(len=6, kind=c_k) :: lbl_norm
    integer(i32) :: nlen

    call cdd_error_clear(err)
    label_end = min(col6_end, l_beg + 4_i32)

    d_beg = 0_i32
    d_end = 0_i32
    do p = l_beg, label_end
      if (is_ascii_digit(src(p:p))) then
        if (d_beg == 0_i32) d_beg = p
        d_end = p
      else if (src(p:p) /= " ") then
        call cdd_error_set(err, CDD_ERR_FORTRAN_LEX, &
          "Invalid non-digit character in fixed-form label field (cols 1-5)", &
          ln, p - l_beg + 1_i32, p, p, "")
        return
      end if
    end do

    if (d_beg == 0_i32) then
      call push_pending_trivia(stream, TRIVIA_FIXED_LABEL_GAP, l_beg, &
        col6_end - l_beg + 1_i32, 0_i32, pend_first, pend_cnt, err)
    else
      if (d_beg > l_beg) then
        call push_pending_trivia(stream, TRIVIA_FIXED_LABEL_GAP, l_beg, &
          d_beg - l_beg, 0_i32, pend_first, pend_cnt, err)
      end if
      nlen = 0_i32
      do k = d_beg, d_end
        if (src(k:k) /= " ") then
          nlen = nlen + 1_i32
          lbl_norm(nlen:nlen) = src(k:k)
        end if
      end do
      call fstream_add_token(stream, FTOK_LABEL, ln, d_beg - l_beg + 1_i32, &
        d_beg, d_end - d_beg + 1_i32, lbl_norm(1:nlen), pend_first, pend_cnt, tok_idx, err)
      line_last_tok = tok_idx
      pend_first = stream%trivia_count + 1_i32
      pend_cnt = 0_i32
      if (col6_end > d_end) then
        call push_pending_trivia(stream, TRIVIA_FIXED_LABEL_GAP, d_end + 1_i32, &
          col6_end - d_end, 0_i32, pend_first, pend_cnt, err)
      end if
    end if
  end subroutine scan_fixed_label_cols

  !> @brief Scan one token in fixed-form cols 7..72 (including multi-card Hollerith nH and spaced kw).
  !> @param[in]    src        Full source text.
  !> @param[in]    lines      Line slice array.
  !> @param[in]    lcount     Total line count.
  !> @param[inout] ln         Current 1-based line index (advanced on multi-card Hollerith).
  !> @param[inout] l_beg      Current line start index.
  !> @param[inout] l_end      Current line end index.
  !> @param[inout] body_end   Current line col-72 end index.
  !> @param[inout] pos        Current 1-based byte offset.
  !> @param[inout] stream     Token stream.
  !> @param[inout] pend_first Pending trivia start.
  !> @param[inout] pend_cnt   Pending trivia count.
  !> @param[out]   tok_idx    Emitted token index.
  !> @param[out]   err        Error status.
  pure subroutine scan_fixed_body_token(src, lines, lcount, ln, l_beg, l_end, body_end, &
      pos, stream, pend_first, pend_cnt, tok_idx, err)
    character(len=*, kind=c_k), intent(in) :: src
    type(cdd_line_slice_t), intent(in) :: lines(CDD_MAX_SRC_LINES)
    integer(i32), intent(in) :: lcount
    integer(i32), intent(inout) :: ln
    integer(i32), intent(inout) :: l_beg
    integer(i32), intent(inout) :: l_end
    integer(i32), intent(inout) :: body_end
    integer(i32), intent(inout) :: pos
    type(cdd_cst_token_stream_t), intent(inout) :: stream
    integer(i32), intent(inout) :: pend_first
    integer(i32), intent(inout) :: pend_cnt
    integer(i32), intent(out) :: tok_idx
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: d_end, h_pos, start_ln, start_col, start_pos, raw_end, rem, avail
    integer(i64) :: h_count
    type(cdd_buffer_t) :: h_norm
    type(cdd_error_t) :: tmp_err
    integer(i32) :: sp_len
    character(len=32, kind=c_k) :: sp_kw
    integer(i32) :: sp_kw_len

    call cdd_error_clear(err)
    tok_idx = 0_i32
    start_ln = ln
    start_col = pos - l_beg + 1_i32
    start_pos = pos
    raw_end = start_pos

    ! Check for Hollerith nH... constant when starting with digits followed by H/h
    if (is_ascii_digit(src(pos:pos))) then
      d_end = pos
      do while (d_end <= body_end)
        if (.not. is_ascii_digit(src(d_end:d_end))) exit
        d_end = d_end + 1_i32
      end do
      if (d_end <= body_end) then
        if (char_to_upper(src(d_end:d_end)) == "H") then
          call parse_i64_strict(src(pos:d_end - 1_i32), h_count, tmp_err)
          if (tmp_err%code == 0_i32 .and. h_count >= 1_i64) then
            call buf_init(h_norm)
            call buf_append_str(h_norm, src(pos:d_end - 1_i32), err)
            call buf_append_char(h_norm, "H", err)
            rem = int(h_count, kind=i32)
            h_pos = d_end + 1_i32
            do while (rem > 0_i32)
              avail = body_end - h_pos + 1_i32
              if (avail >= rem) then
                call buf_append_str(h_norm, src(h_pos:h_pos + rem - 1_i32), err)
                h_pos = h_pos + rem
                raw_end = h_pos - 1_i32
                rem = 0_i32
              else
                if (avail > 0_i32) then
                  call buf_append_str(h_norm, src(h_pos:body_end), err)
                  rem = rem - avail
                end if
                if (ln >= lcount) then
                  call cdd_error_set(err, CDD_ERR_FORTRAN_LEX, &
                    "Truncated Hollerith constant at end of file", start_ln, start_col, &
                    start_pos, l_end, "")
                  return
                end if
                ln = ln + 1_i32
                l_beg = lines(ln)%first_idx
                l_end = lines(ln)%last_idx
                body_end = min(l_end, l_beg + 71_i32)
                if (l_end - l_beg + 1_i32 < 6_i32 .or. &
                    src(l_beg + 5_i32:l_beg + 5_i32) == " " .or. &
                    src(l_beg + 5_i32:l_beg + 5_i32) == "0") then
                  call cdd_error_set(err, CDD_ERR_FORTRAN_LEX, &
                    "Truncated Hollerith constant missing continuation card", &
                    start_ln, start_col, start_pos, l_end, "")
                  return
                end if
                h_pos = l_beg + 6_i32
              end if
            end do
            pos = h_pos
            call fstream_add_token(stream, FTOK_HOLLERITH, start_ln, start_col, &
              start_pos, raw_end - start_pos + 1_i32, h_norm%raw(1:h_norm%len), &
              pend_first, pend_cnt, tok_idx, err)
            pend_first = stream%trivia_count + 1_i32
            pend_cnt = 0_i32
            return
          end if
        end if
      end if
    end if

    ! Check for classical spaced keywords (e.g., "DOUB LE PRE CISION", "END STRUCTURE", "BLOCK DATA")
    if (is_ascii_alpha(src(pos:pos))) then
      call match_spaced_fixed_keyword(src, pos, body_end, sp_len, sp_kw, sp_kw_len)
      if (sp_len > 0_i32) then
        call fstream_add_token(stream, FTOK_IDENT, start_ln, start_col, &
          pos, sp_len, sp_kw(1:sp_kw_len), pend_first, pend_cnt, tok_idx, err)
        pos = pos + sp_len
        pend_first = stream%trivia_count + 1_i32
        pend_cnt = 0_i32
        return
      end if
    end if

    call scan_free_token(src, ln, l_beg, body_end, pos, stream, pend_first, pend_cnt, tok_idx, err)
  end subroutine scan_fixed_body_token

  !> @brief Recognize spaced fixed-form multi-word keywords while preserving interior raw spaces.
  !> @param[in]  src      Source string.
  !> @param[in]  pos      1-based start offset.
  !> @param[in]  body_end 1-based end of column 72.
  !> @param[out] raw_span Raw byte length consumed (> 0 if matched, 0 otherwise).
  !> @param[out] norm_kw  Collapsed uppercase keyword buffer.
  !> @param[out] norm_len Length of norm_kw.
  pure subroutine match_spaced_fixed_keyword(src, pos, body_end, raw_span, norm_kw, norm_len)
    character(len=*, kind=c_k), intent(in) :: src
    integer(i32), intent(in) :: pos
    integer(i32), intent(in) :: body_end
    integer(i32), intent(out) :: raw_span
    character(len=32, kind=c_k), intent(out) :: norm_kw
    integer(i32), intent(out) :: norm_len

    raw_span = 0_i32
    norm_len = 0_i32

    call try_spaced_kw(src, pos, body_end, "DOUBLEPRECISION", raw_span, norm_kw, norm_len)
    if (raw_span > 0_i32) return
    call try_spaced_kw(src, pos, body_end, "ENDSTRUCTURE", raw_span, norm_kw, norm_len)
    if (raw_span > 0_i32) return
    call try_spaced_kw(src, pos, body_end, "BLOCKDATA", raw_span, norm_kw, norm_len)
    if (raw_span > 0_i32) return
    call try_spaced_kw(src, pos, body_end, "GOTO", raw_span, norm_kw, norm_len)
  end subroutine match_spaced_fixed_keyword

  !> @brief Attempt to match target_kw ignoring interior spaces at src(pos:body_end).
  !> @param[in]  src       Source string.
  !> @param[in]  pos       1-based start offset.
  !> @param[in]  body_end  1-based limit.
  !> @param[in]  target_kw Collapsed uppercase target keyword.
  !> @param[out] raw_span  Consumed raw bytes (only > 0 if interior space was actually present).
  !> @param[out] norm_kw   Output collapsed keyword.
  !> @param[out] norm_len  Output keyword length.
  pure subroutine try_spaced_kw(src, pos, body_end, target_kw, raw_span, norm_kw, norm_len)
    character(len=*, kind=c_k), intent(in) :: src
    integer(i32), intent(in) :: pos
    integer(i32), intent(in) :: body_end
    character(len=*, kind=c_k), intent(in) :: target_kw
    integer(i32), intent(out) :: raw_span
    character(len=32, kind=c_k), intent(out) :: norm_kw
    integer(i32), intent(out) :: norm_len
    integer(i32) :: tlen, ti, p, k
    logical(lk) :: saw_space

    raw_span = 0_i32
    norm_len = 0_i32
    tlen = len(target_kw, kind=i32)
    ti = 1_i32
    p = pos
    saw_space = .false._lk

    do while (ti <= tlen .and. p <= body_end)
      if (src(p:p) == " ") then
        saw_space = .true._lk
        p = p + 1_i32
      else if (char_to_upper(src(p:p)) == target_kw(ti:ti)) then
        ti = ti + 1_i32
        p = p + 1_i32
      else
        return
      end if
    end do

    if (ti <= tlen .or. .not. saw_space) return
    if (p <= body_end) then
      if (is_ascii_alnum(src(p:p)) .or. src(p:p) == "_") return
    end if

    raw_span = p - pos
    norm_len = tlen
    do k = 1_i32, tlen
      norm_kw(k:k) = target_kw(k:k)
    end do
  end subroutine try_spaced_kw

  !> @brief Scan a single lexical token within src(pos:limit_end) and append to stream.
  !> @param[in]    src        Source string.
  !> @param[in]    ln         1-based line number.
  !> @param[in]    l_beg      1-based line start offset.
  !> @param[in]    limit_end  1-based active end of current line.
  !> @param[inout] pos        1-based cursor (advanced past scanned token).
  !> @param[inout] stream     Token stream.
  !> @param[inout] pend_first Pending trivia start.
  !> @param[inout] pend_cnt   Pending trivia count.
  !> @param[out]   tok_idx    Allocated token index.
  !> @param[out]   err        Error status.
  pure subroutine scan_free_token(src, ln, l_beg, limit_end, pos, stream, &
      pend_first, pend_cnt, tok_idx, err)
    character(len=*, kind=c_k), intent(in) :: src
    integer(i32), intent(in) :: ln
    integer(i32), intent(in) :: l_beg
    integer(i32), intent(in) :: limit_end
    integer(i32), intent(inout) :: pos
    type(cdd_cst_token_stream_t), intent(inout) :: stream
    integer(i32), intent(inout) :: pend_first
    integer(i32), intent(inout) :: pend_cnt
    integer(i32), intent(out) :: tok_idx
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: start_pos, col, kind, k, nlen
    character(len=1, kind=c_k) :: ch, qch
    character(len=256, kind=c_k) :: norm

    call cdd_error_clear(err)
    tok_idx = 0_i32
    start_pos = pos
    col = pos - l_beg + 1_i32
    ch = src(pos:pos)

    if (is_ascii_alpha(ch) .or. ch == "_") then
      do while (pos <= limit_end)
        if (.not. (is_ascii_alnum(src(pos:pos)) .or. src(pos:pos) == "_")) exit
        pos = pos + 1_i32
      end do
      nlen = min(256_i32, pos - start_pos)
      do k = 1_i32, nlen
        norm(k:k) = char_to_upper(src(start_pos + k - 1_i32:start_pos + k - 1_i32))
      end do
      call fstream_add_token(stream, FTOK_IDENT, ln, col, start_pos, pos - start_pos, &
        norm(1:nlen), pend_first, pend_cnt, tok_idx, err)

    else if (is_ascii_digit(ch)) then
      kind = FTOK_INT_LIT
      do while (pos <= limit_end)
        if (.not. is_ascii_digit(src(pos:pos))) exit
        pos = pos + 1_i32
      end do
      if (pos <= limit_end) then
        if (src(pos:pos) == "." .and. .not. is_dot_keyword_ahead(src, pos, limit_end)) then
          kind = FTOK_REAL_LIT
          pos = pos + 1_i32
          do while (pos <= limit_end)
            if (.not. is_ascii_digit(src(pos:pos))) exit
            pos = pos + 1_i32
          end do
        end if
      end if
      if (pos <= limit_end) then
        if (char_to_upper(src(pos:pos)) == "E" .or. char_to_upper(src(pos:pos)) == "D") then
          kind = FTOK_REAL_LIT
          pos = pos + 1_i32
          if (pos <= limit_end) then
            if (src(pos:pos) == "+" .or. src(pos:pos) == "-") pos = pos + 1_i32
          end if
          do while (pos <= limit_end)
            if (.not. is_ascii_digit(src(pos:pos))) exit
            pos = pos + 1_i32
          end do
        end if
      end if
      if (pos <= limit_end) then
        if (src(pos:pos) == "_") then
          pos = pos + 1_i32
          do while (pos <= limit_end)
            if (.not. (is_ascii_alnum(src(pos:pos)) .or. src(pos:pos) == "_")) exit
            pos = pos + 1_i32
          end do
        end if
      end if
      nlen = min(256_i32, pos - start_pos)
      do k = 1_i32, nlen
        norm(k:k) = char_to_upper(src(start_pos + k - 1_i32:start_pos + k - 1_i32))
      end do
      call fstream_add_token(stream, kind, ln, col, start_pos, pos - start_pos, &
        norm(1:nlen), pend_first, pend_cnt, tok_idx, err)

    else if (ch == "'" .or. ch == '"') then
      qch = ch
      pos = pos + 1_i32
      do
        if (pos > limit_end) then
          call cdd_error_set(err, CDD_ERR_FORTRAN_LEX, &
            "Unterminated character literal on line", ln, col, start_pos, limit_end, "")
          return
        end if
        if (src(pos:pos) == qch) then
          if (pos < limit_end) then
            if (src(pos + 1_i32:pos + 1_i32) == qch) then
              pos = pos + 2_i32
              cycle
            end if
          end if
          pos = pos + 1_i32
          exit
        end if
        pos = pos + 1_i32
      end do
      if (pos <= limit_end) then
        if (src(pos:pos) == "_") then
          pos = pos + 1_i32
          do while (pos <= limit_end)
            if (.not. (is_ascii_alnum(src(pos:pos)) .or. src(pos:pos) == "_")) exit
            pos = pos + 1_i32
          end do
        end if
      end if
      call fstream_add_token(stream, FTOK_STR_LIT, ln, col, start_pos, pos - start_pos, &
        "", pend_first, pend_cnt, tok_idx, err)

    else if (ch == ".") then
      if (pos < limit_end .and. &
          src(min(limit_end, pos + 1_i32):min(limit_end, pos + 1_i32)) == ".") then
        pos = pos + 2_i32
        call fstream_add_token(stream, FTOK_DOT_OP, ln, col, start_pos, 2_i32, "..", &
          pend_first, pend_cnt, tok_idx, err)
      else if (pos < limit_end .and. &
               is_ascii_alpha(src(min(limit_end, pos + 1_i32):min(limit_end, pos + 1_i32)))) then
        pos = pos + 1_i32
        do while (pos <= limit_end)
          if (.not. (is_ascii_alnum(src(pos:pos)) .or. src(pos:pos) == "_")) exit
          pos = pos + 1_i32
        end do
        if (pos > limit_end .or. src(min(limit_end, pos):min(limit_end, pos)) /= ".") then
          if (start_pos > l_beg .and. &
              (is_ascii_alnum(src(max(1_i32, start_pos - 1_i32):max(1_i32, start_pos - 1_i32))) .or. &
               src(max(1_i32, start_pos - 1_i32):max(1_i32, start_pos - 1_i32)) == "_")) then
            pos = start_pos + 1_i32
            call fstream_add_token(stream, FTOK_OP, ln, col, start_pos, 1_i32, ".", &
              pend_first, pend_cnt, tok_idx, err)
            pend_first = stream%trivia_count + 1_i32
            pend_cnt = 0_i32
            return
          end if
          call cdd_error_set(err, CDD_ERR_FORTRAN_LEX, &
            "Unterminated dot-delimited Fortran operator/literal", ln, col, start_pos, pos - 1_i32, "")
          return
        end if
        pos = pos + 1_i32
        if (pos <= limit_end) then
          if (src(pos:pos) == "_") then
            pos = pos + 1_i32
            do while (pos <= limit_end)
              if (.not. (is_ascii_alnum(src(pos:pos)) .or. src(pos:pos) == "_")) exit
              pos = pos + 1_i32
            end do
          end if
        end if
        nlen = min(256_i32, pos - start_pos)
        do k = 1_i32, nlen
          norm(k:k) = char_to_upper(src(start_pos + k - 1_i32:start_pos + k - 1_i32))
        end do
        call fstream_add_token(stream, FTOK_DOT_OP, ln, col, start_pos, pos - start_pos, &
          norm(1:nlen), pend_first, pend_cnt, tok_idx, err)
      else
        pos = pos + 1_i32
        call fstream_add_token(stream, FTOK_OP, ln, col, start_pos, 1_i32, ".", &
          pend_first, pend_cnt, tok_idx, err)
      end if

    else if (ch == "(") then
      pos = pos + 1_i32
      call fstream_add_token(stream, FTOK_LPAREN, ln, col, start_pos, 1_i32, "(", &
        pend_first, pend_cnt, tok_idx, err)
    else if (ch == ")") then
      pos = pos + 1_i32
      call fstream_add_token(stream, FTOK_RPAREN, ln, col, start_pos, 1_i32, ")", &
        pend_first, pend_cnt, tok_idx, err)
    else if (ch == "[") then
      pos = pos + 1_i32
      call fstream_add_token(stream, FTOK_LBRACKET, ln, col, start_pos, 1_i32, "[", &
        pend_first, pend_cnt, tok_idx, err)
    else if (ch == "]") then
      pos = pos + 1_i32
      call fstream_add_token(stream, FTOK_RBRACKET, ln, col, start_pos, 1_i32, "]", &
        pend_first, pend_cnt, tok_idx, err)
    else if (ch == ",") then
      pos = pos + 1_i32
      call fstream_add_token(stream, FTOK_COMMA, ln, col, start_pos, 1_i32, ",", &
        pend_first, pend_cnt, tok_idx, err)
    else if (ch == ";") then
      pos = pos + 1_i32
      call fstream_add_token(stream, FTOK_SEMI, ln, col, start_pos, 1_i32, ";", &
        pend_first, pend_cnt, tok_idx, err)
    else if (ch == "?") then
      pos = pos + 1_i32
      call fstream_add_token(stream, FTOK_QUESTION, ln, col, start_pos, 1_i32, "?", &
        pend_first, pend_cnt, tok_idx, err)
    else if (ch == ":") then
      if (pos < limit_end .and. src(pos + 1_i32:pos + 1_i32) == ":") then
        pos = pos + 2_i32
        call fstream_add_token(stream, FTOK_DCOLON, ln, col, start_pos, 2_i32, "::", &
          pend_first, pend_cnt, tok_idx, err)
      else
        pos = pos + 1_i32
        call fstream_add_token(stream, FTOK_COLON, ln, col, start_pos, 1_i32, ":", &
          pend_first, pend_cnt, tok_idx, err)
      end if
    else if (ch == "=") then
      if (pos < limit_end .and. src(pos + 1_i32:pos + 1_i32) == ">") then
        pos = pos + 2_i32
        call fstream_add_token(stream, FTOK_ARROW, ln, col, start_pos, 2_i32, "=>", &
          pend_first, pend_cnt, tok_idx, err)
      else if (pos < limit_end .and. src(pos + 1_i32:pos + 1_i32) == "=") then
        pos = pos + 2_i32
        call fstream_add_token(stream, FTOK_OP, ln, col, start_pos, 2_i32, "==", &
          pend_first, pend_cnt, tok_idx, err)
      else
        pos = pos + 1_i32
        call fstream_add_token(stream, FTOK_EQ, ln, col, start_pos, 1_i32, "=", &
          pend_first, pend_cnt, tok_idx, err)
      end if
    else if (ch == "/") then
      if (pos < limit_end .and. src(pos + 1_i32:pos + 1_i32) == "/") then
        pos = pos + 2_i32
        call fstream_add_token(stream, FTOK_CONC, ln, col, start_pos, 2_i32, "//", &
          pend_first, pend_cnt, tok_idx, err)
      else if (pos < limit_end .and. src(pos + 1_i32:pos + 1_i32) == "=") then
        pos = pos + 2_i32
        call fstream_add_token(stream, FTOK_OP, ln, col, start_pos, 2_i32, "/=", &
          pend_first, pend_cnt, tok_idx, err)
      else
        pos = pos + 1_i32
        call fstream_add_token(stream, FTOK_SLASH, ln, col, start_pos, 1_i32, "/", &
          pend_first, pend_cnt, tok_idx, err)
      end if
    else if (ch == "*") then
      if (pos < limit_end .and. src(pos + 1_i32:pos + 1_i32) == "*") then
        pos = pos + 2_i32
        call fstream_add_token(stream, FTOK_OP, ln, col, start_pos, 2_i32, "**", &
          pend_first, pend_cnt, tok_idx, err)
      else
        pos = pos + 1_i32
        call fstream_add_token(stream, FTOK_STAR, ln, col, start_pos, 1_i32, "*", &
          pend_first, pend_cnt, tok_idx, err)
      end if
    else if (ch == "<" .or. ch == ">") then
      if (pos < limit_end .and. src(pos + 1_i32:pos + 1_i32) == "=") then
        pos = pos + 2_i32
        call fstream_add_token(stream, FTOK_OP, ln, col, start_pos, 2_i32, &
          src(start_pos:start_pos + 1_i32), pend_first, pend_cnt, tok_idx, err)
      else
        pos = pos + 1_i32
        call fstream_add_token(stream, FTOK_OP, ln, col, start_pos, 1_i32, ch, &
          pend_first, pend_cnt, tok_idx, err)
      end if
    else if (ch == "+" .or. ch == "-" .or. ch == "%") then
      pos = pos + 1_i32
      call fstream_add_token(stream, FTOK_OP, ln, col, start_pos, 1_i32, ch, &
        pend_first, pend_cnt, tok_idx, err)
    else
      call cdd_error_set(err, CDD_ERR_FORTRAN_LEX, &
        "Invalid character in Fortran statement", ln, col, start_pos, start_pos, "")
      return
    end if

    pend_first = stream%trivia_count + 1_i32
    pend_cnt = 0_i32
  end subroutine scan_free_token

  !> @brief Look ahead after '123.' to avoid eating '.EQ.' / '.AND.' as a decimal point.
  !> @param[in] src       Source string.
  !> @param[in] dot_pos   1-based index of '.'.
  !> @param[in] limit_end 1-based end of line.
  !> @return True if '.' begins a '.OP.' operator such as '1.EQ.2'.
  pure function is_dot_keyword_ahead(src, dot_pos, limit_end) result(is_op)
    character(len=*, kind=c_k), intent(in) :: src
    integer(i32), intent(in) :: dot_pos
    integer(i32), intent(in) :: limit_end
    logical(lk) :: is_op
    integer(i32) :: p
    character(len=1, kind=c_k) :: u1

    is_op = .false._lk
    if (dot_pos + 2_i32 > limit_end) return
    if (.not. is_ascii_alpha(src(dot_pos + 1_i32:dot_pos + 1_i32))) return
    u1 = char_to_upper(src(dot_pos + 1_i32:dot_pos + 1_i32))
    if ((u1 == "E" .or. u1 == "D") .and. &
        (is_ascii_digit(src(dot_pos + 2_i32:dot_pos + 2_i32)) .or. &
         src(dot_pos + 2_i32:dot_pos + 2_i32) == "+" .or. &
         src(dot_pos + 2_i32:dot_pos + 2_i32) == "-")) then
      return
    end if
    p = dot_pos + 2_i32
    do while (p <= limit_end)
      if (src(p:p) == ".") then
        is_op = .true._lk
        return
      else if (.not. is_ascii_alpha(src(p:p))) then
        return
      end if
      p = p + 1_i32
    end do
  end function is_dot_keyword_ahead

  !> @brief Classify a '!' comment starting at pos as directive, Doxygen, or plain comment.
  !> @param[in] src      Source text.
  !> @param[in] pos      1-based index of '!'.
  !> @param[in] line_end 1-based end of line.
  !> @return TRIVIA_DIRECTIVE, TRIVIA_DOXYGEN_COMMENT, or TRIVIA_LINE_COMMENT.
  pure function classify_bang_comment(src, pos, line_end) result(ckind)
    character(len=*, kind=c_k), intent(in) :: src
    integer(i32), intent(in) :: pos
    integer(i32), intent(in) :: line_end
    integer(i32) :: ckind
    character(len=1, kind=c_k) :: next_ch

    ckind = TRIVIA_LINE_COMMENT
    if (pos < line_end) then
      next_ch = src(pos + 1_i32:pos + 1_i32)
      if (next_ch == ">" .or. next_ch == "<" .or. next_ch == "!" .or. next_ch == "*") then
        ckind = TRIVIA_DOXYGEN_COMMENT
      else if (next_ch == "$" .or. starts_with(src(pos:line_end), "!DEC$") .or. &
               starts_with(src(pos:line_end), "!GCC$")) then
        ckind = TRIVIA_DIRECTIVE
      end if
    end if
  end function classify_bang_comment

  !> @brief Test whether src(beg_idx:end_idx) consists solely of whitespace or a '!' comment.
  !> @param[in] src     Source string.
  !> @param[in] beg_idx 1-based start.
  !> @param[in] end_idx 1-based end.
  !> @return True if only spaces/tabs or a trailing '!' comment follow.
  pure function is_only_ws_or_comment(src, beg_idx, end_idx) result(ok)
    character(len=*, kind=c_k), intent(in) :: src
    integer(i32), intent(in) :: beg_idx
    integer(i32), intent(in) :: end_idx
    logical(lk) :: ok
    integer(i32) :: p

    ok = .true._lk
    do p = beg_idx, end_idx
      if (src(p:p) == "!") return
      if (src(p:p) /= " " .and. src(p:p) /= achar(9, c_k)) then
        ok = .false._lk
        return
      end if
    end do
  end function is_only_ws_or_comment

  !> @brief Consume contiguous spaces/tabs starting at pos into pending trivia.
  !> @param[in]    src        Source string.
  !> @param[in]    limit_end  1-based bound.
  !> @param[inout] pos        1-based cursor.
  !> @param[inout] stream     Token stream.
  !> @param[inout] pend_first Pending trivia start.
  !> @param[inout] pend_cnt   Pending trivia count.
  !> @param[inout] err        Error status.
  pure subroutine consume_horiz_ws(src, limit_end, pos, stream, pend_first, pend_cnt, err)
    character(len=*, kind=c_k), intent(in) :: src
    integer(i32), intent(in) :: limit_end
    integer(i32), intent(inout) :: pos
    type(cdd_cst_token_stream_t), intent(inout) :: stream
    integer(i32), intent(inout) :: pend_first
    integer(i32), intent(inout) :: pend_cnt
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: ws_start

    ws_start = pos
    do while (pos <= limit_end)
      if (src(pos:pos) /= " " .and. src(pos:pos) /= achar(9, c_k)) exit
      pos = pos + 1_i32
    end do
    call push_pending_trivia(stream, TRIVIA_WHITESPACE, ws_start, pos - ws_start, &
      0_i32, pend_first, pend_cnt, err)
  end subroutine consume_horiz_ws

  !> @brief Append a trivia slice into the pending leading-trivia window.
  !> @param[inout] stream     Token stream.
  !> @param[in]    kind       Trivia kind.
  !> @param[in]    off        1-based byte offset.
  !> @param[in]    slen       Byte length.
  !> @param[in]    term_kind  Line ending kind.
  !> @param[inout] pend_first First index of pending window.
  !> @param[inout] pend_cnt   Count in pending window.
  !> @param[inout] err        Error status.
  pure subroutine push_pending_trivia(stream, kind, off, slen, term_kind, pend_first, pend_cnt, err)
    type(cdd_cst_token_stream_t), intent(inout) :: stream
    integer(i32), intent(in) :: kind
    integer(i32), intent(in) :: off
    integer(i32), intent(in) :: slen
    integer(i32), intent(in) :: term_kind
    integer(i32), intent(inout) :: pend_first
    integer(i32), intent(inout) :: pend_cnt
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: tidx

    call fstream_add_trivia(stream, kind, off, slen, term_kind, tidx, err)
    if (pend_cnt == 0_i32) pend_first = tidx
    pend_cnt = pend_cnt + 1_i32
  end subroutine push_pending_trivia

  !> @brief Append a trivia slice onto token tok_idx's trailing_trivia range.
  !> @param[inout] stream    Token stream.
  !> @param[in]    tok_idx   1-based token index.
  !> @param[in]    kind      Trivia kind.
  !> @param[in]    off       1-based byte offset.
  !> @param[in]    slen      Byte length.
  !> @param[in]    term_kind Line ending kind.
  !> @param[inout] err       Error status.
  pure subroutine append_trail_trivia(stream, tok_idx, kind, off, slen, term_kind, err)
    type(cdd_cst_token_stream_t), intent(inout) :: stream
    integer(i32), intent(in) :: tok_idx
    integer(i32), intent(in) :: kind
    integer(i32), intent(in) :: off
    integer(i32), intent(in) :: slen
    integer(i32), intent(in) :: term_kind
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: tidx

    call fstream_add_trivia(stream, kind, off, slen, term_kind, tidx, err)
    if (stream%tokens(tok_idx)%trail_count == 0_i32) then
      stream%tokens(tok_idx)%trail_first = tidx
    end if
    stream%tokens(tok_idx)%trail_count = stream%tokens(tok_idx)%trail_count + 1_i32
  end subroutine append_trail_trivia

  !> @brief Push a line's LF or CRLF terminator (if any) into pending trivia.
  !> @param[inout] stream     Token stream.
  !> @param[in]    ls         Line slice descriptor.
  !> @param[inout] pend_first Pending trivia start.
  !> @param[inout] pend_cnt   Pending trivia count.
  !> @param[inout] err        Error status.
  pure subroutine push_line_term_pending(stream, ls, pend_first, pend_cnt, err)
    type(cdd_cst_token_stream_t), intent(inout) :: stream
    type(cdd_line_slice_t), intent(in) :: ls
    integer(i32), intent(inout) :: pend_first
    integer(i32), intent(inout) :: pend_cnt
    type(cdd_error_t), intent(inout) :: err

    if (ls%term_kind == TERM_LF) then
      call push_pending_trivia(stream, TRIVIA_NEWLINE, ls%last_idx + 1_i32, 1_i32, &
        TERM_LF, pend_first, pend_cnt, err)
    else if (ls%term_kind == TERM_CRLF) then
      call push_pending_trivia(stream, TRIVIA_NEWLINE, ls%last_idx + 1_i32, 2_i32, &
        TERM_CRLF, pend_first, pend_cnt, err)
    end if
  end subroutine push_line_term_pending

  !> @brief Append a line's LF or CRLF terminator (if any) onto tok_idx's trailing trivia.
  !> @param[inout] stream  Token stream.
  !> @param[in]    tok_idx Target token index.
  !> @param[in]    ls      Line slice descriptor.
  !> @param[inout] err     Error status.
  pure subroutine append_line_term_trail(stream, tok_idx, ls, err)
    type(cdd_cst_token_stream_t), intent(inout) :: stream
    integer(i32), intent(in) :: tok_idx
    type(cdd_line_slice_t), intent(in) :: ls
    type(cdd_error_t), intent(inout) :: err

    if (ls%term_kind == TERM_LF) then
      call append_trail_trivia(stream, tok_idx, TRIVIA_NEWLINE, ls%last_idx + 1_i32, &
        1_i32, TERM_LF, err)
    else if (ls%term_kind == TERM_CRLF) then
      call append_trail_trivia(stream, tok_idx, TRIVIA_NEWLINE, ls%last_idx + 1_i32, &
        2_i32, TERM_CRLF, err)
    end if
  end subroutine append_line_term_trail

end module cdd_cst_lexer
