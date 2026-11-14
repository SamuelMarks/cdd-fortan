!> @file cdd_cst_tokens.f90
!> @brief Fixed-form and free-form Fortran CST token types and lossless token-stream arena.

!> @brief Strongly-typed Fortran lexical tokens carrying leading/trailing trivia slices.
module cdd_cst_tokens
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set_simple, CDD_ERR_BUFFER_OVERFLOW
  use cdd_string, only: cdd_buffer_t, buf_init, buf_append_str, str_eq, str_eq_nocase
  use cdd_cst_trivia, only: CDD_MAX_TRIVIA, cdd_trivia_piece_t
  implicit none (type, external)
  private

  public :: CDD_MAX_FTOKENS
  public :: FTOK_EOF, FTOK_IDENT, FTOK_INT_LIT, FTOK_REAL_LIT, FTOK_STR_LIT, FTOK_HOLLERITH
  public :: FTOK_LABEL, FTOK_LPAREN, FTOK_RPAREN, FTOK_LBRACKET, FTOK_RBRACKET
  public :: FTOK_COMMA, FTOK_COLON, FTOK_DCOLON, FTOK_SEMI, FTOK_EQ, FTOK_ARROW
  public :: FTOK_SLASH, FTOK_CONC, FTOK_STAR, FTOK_DOT_OP, FTOK_QUESTION, FTOK_OP
  public :: cdd_cst_token_t, cdd_cst_token_stream_t
  public :: fstream_init, fstream_add_trivia, fstream_add_token
  public :: ftok_raw_eq, ftok_norm_eq, fstream_concat_lossless

  integer(i32), parameter :: CDD_MAX_FTOKENS = 36864_i32 !< Max Fortran tokens per source file.

  integer(i32), parameter :: FTOK_EOF = 0_i32            !< End-of-file sentinel token.
  integer(i32), parameter :: FTOK_IDENT = 1_i32          !< Identifier or Fortran keyword token.
  integer(i32), parameter :: FTOK_INT_LIT = 2_i32        !< Integer literal (with optional _kind).
  integer(i32), parameter :: FTOK_REAL_LIT = 3_i32       !< Floating-point literal.
  integer(i32), parameter :: FTOK_STR_LIT = 4_i32        !< Single- or double-quoted character literal.
  integer(i32), parameter :: FTOK_HOLLERITH = 5_i32      !< Legacy Hollerith nH... constant.
  integer(i32), parameter :: FTOK_LABEL = 6_i32          !< Fixed-form statement label (cols 1-5).
  integer(i32), parameter :: FTOK_LPAREN = 7_i32         !< Left parenthesis '('.
  integer(i32), parameter :: FTOK_RPAREN = 8_i32         !< Right parenthesis ')'.
  integer(i32), parameter :: FTOK_LBRACKET = 9_i32       !< Left bracket '[' (F2003+ array/coarray).
  integer(i32), parameter :: FTOK_RBRACKET = 10_i32      !< Right bracket ']'.
  integer(i32), parameter :: FTOK_COMMA = 11_i32         !< Comma ','.
  integer(i32), parameter :: FTOK_COLON = 12_i32         !< Single colon ':'.
  integer(i32), parameter :: FTOK_DCOLON = 13_i32        !< Double colon '::'.
  integer(i32), parameter :: FTOK_SEMI = 14_i32          !< Statement separator ';'.
  integer(i32), parameter :: FTOK_EQ = 15_i32            !< Assignment/initializer '='.
  integer(i32), parameter :: FTOK_ARROW = 16_i32         !< Pointer/TBP binding '=>'.
  integer(i32), parameter :: FTOK_SLASH = 17_i32         !< Slash '/' (COMMON/STRUCTURE/DATA).
  integer(i32), parameter :: FTOK_CONC = 18_i32          !< String concatenation '//'.
  integer(i32), parameter :: FTOK_STAR = 19_i32          !< Asterisk '*' (CHARACTER*N, assumed *).
  integer(i32), parameter :: FTOK_DOT_OP = 20_i32        !< Dot-delimited literal/op (.TRUE., .EQ., ..).
  integer(i32), parameter :: FTOK_QUESTION = 21_i32      !< Fortran 2023 conditional '?'.
  integer(i32), parameter :: FTOK_OP = 22_i32            !< Other operator (+, -, ==, /=, <=, >=, %).

  !> @brief Lossless Fortran token referencing original source bytes, normalized text, and trivia.
  type :: cdd_cst_token_t
    integer(i32) :: kind = FTOK_EOF       !< Token category (FTOK_*).
    integer(i32) :: line = 1_i32          !< 1-based starting line number.
    integer(i32) :: column = 1_i32        !< 1-based starting column number.
    integer(i32) :: raw_off = 1_i32       !< 1-based start offset in stream%source_buf.
    integer(i32) :: raw_len = 0_i32       !< Exact raw byte length in stream%source_buf.
    integer(i32) :: norm_off = 1_i32      !< 1-based start offset in stream%norm_pool (uppercase).
    integer(i32) :: norm_len = 0_i32      !< Byte length in stream%norm_pool.
    integer(i32) :: lead_first = 1_i32    !< First leading trivia index in stream%trivia.
    integer(i32) :: lead_count = 0_i32    !< Number of leading trivia pieces.
    integer(i32) :: trail_first = 1_i32   !< First trailing trivia index in stream%trivia.
    integer(i32) :: trail_count = 0_i32   !< Number of trailing trivia pieces.
    logical(lk) :: stmt_end = .false._lk  !< True when token terminates a logical statement.
  end type cdd_cst_token_t

  !> @brief Self-contained lossless Fortran token and trivia arena.
  type :: cdd_cst_token_stream_t
    type(cdd_buffer_t) :: source_buf                      !< Verbatim copy of input Fortran source.
    type(cdd_buffer_t) :: norm_pool                       !< Uppercase/de-spaced normalized lexemes.
    integer(i32) :: token_count = 0_i32                   !< Active tokens (including FTOK_EOF).
    integer(i32) :: max_tokens = CDD_MAX_FTOKENS          !< Configurable token limit for testing.
    type(cdd_cst_token_t) :: tokens(CDD_MAX_FTOKENS)      !< Token storage array.
    integer(i32) :: trivia_count = 0_i32                  !< Active trivia pieces.
    integer(i32) :: max_trivia = CDD_MAX_TRIVIA           !< Configurable trivia limit for testing.
    type(cdd_trivia_piece_t) :: trivia(CDD_MAX_TRIVIA)    !< Contiguous trivia storage array.
  end type cdd_cst_token_stream_t

contains

  !> @brief Initialize an empty Fortran CST token stream arena.
  !> @param[out] stream Token stream arena to reset.
  pure subroutine fstream_init(stream)
    type(cdd_cst_token_stream_t), intent(out) :: stream

    call buf_init(stream%source_buf)
    call buf_init(stream%norm_pool)
    stream%token_count = 0_i32
    stream%max_tokens = CDD_MAX_FTOKENS
    stream%trivia_count = 0_i32
    stream%max_trivia = CDD_MAX_TRIVIA
  end subroutine fstream_init

  !> @brief Append a trivia slice to stream%trivia, returning its 1-based index.
  !> @param[inout] stream    Target token stream.
  !> @param[in]    kind      Trivia category (TRIVIA_*).
  !> @param[in]    src_off   1-based start offset in stream%source_buf.
  !> @param[in]    src_len   Byte length in stream%source_buf.
  !> @param[in]    term_kind Line terminator kind (0 if non-newline).
  !> @param[out]   triv_idx  Allocated 1-based trivia index.
  !> @param[inout] err       Error status record.
  pure subroutine fstream_add_trivia(stream, kind, src_off, src_len, term_kind, triv_idx, err)
    type(cdd_cst_token_stream_t), intent(inout) :: stream
    integer(i32), intent(in) :: kind
    integer(i32), intent(in) :: src_off
    integer(i32), intent(in) :: src_len
    integer(i32), intent(in) :: term_kind
    integer(i32), intent(out) :: triv_idx
    type(cdd_error_t), intent(inout) :: err

    triv_idx = 0_i32
    if (err%code /= 0_i32) return
    if (stream%trivia_count >= stream%max_trivia) then
      call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "Fortran CST trivia arena full")
      return
    end if
    stream%trivia_count = stream%trivia_count + 1_i32
    triv_idx = stream%trivia_count
    stream%trivia(triv_idx)%kind = kind
    stream%trivia(triv_idx)%src_off = src_off
    stream%trivia(triv_idx)%src_len = src_len
    stream%trivia(triv_idx)%term_kind = term_kind
  end subroutine fstream_add_trivia

  !> @brief Append a token to stream%tokens and store its normalized text in norm_pool.
  !> @param[inout] stream     Target token stream.
  !> @param[in]    kind       Token kind (FTOK_*).
  !> @param[in]    line       1-based line number.
  !> @param[in]    col        1-based column number.
  !> @param[in]    raw_off    1-based start offset in source_buf.
  !> @param[in]    raw_len    Raw byte length in source_buf.
  !> @param[in]    norm_txt   Normalized uppercase/collapsed string.
  !> @param[in]    lead_first First leading trivia index.
  !> @param[in]    lead_cnt   Leading trivia count.
  !> @param[out]   tok_idx    Allocated 1-based token index.
  !> @param[inout] err        Error status record.
  pure subroutine fstream_add_token(stream, kind, line, col, raw_off, raw_len, norm_txt, &
      lead_first, lead_cnt, tok_idx, err)
    type(cdd_cst_token_stream_t), intent(inout) :: stream
    integer(i32), intent(in) :: kind
    integer(i32), intent(in) :: line
    integer(i32), intent(in) :: col
    integer(i32), intent(in) :: raw_off
    integer(i32), intent(in) :: raw_len
    character(len=*, kind=c_k), intent(in) :: norm_txt
    integer(i32), intent(in) :: lead_first
    integer(i32), intent(in) :: lead_cnt
    integer(i32), intent(out) :: tok_idx
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: noff, nlen

    tok_idx = 0_i32
    if (err%code /= 0_i32) return
    if (stream%token_count >= stream%max_tokens) then
      call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "Fortran CST token arena full")
      return
    end if

    noff = stream%norm_pool%len + 1_i32
    nlen = len(norm_txt, kind=i32)
    if (nlen > 0_i32) then
      call buf_append_str(stream%norm_pool, norm_txt, err)
      if (err%code /= 0_i32) return
    end if

    stream%token_count = stream%token_count + 1_i32
    tok_idx = stream%token_count
    stream%tokens(tok_idx)%kind = kind
    stream%tokens(tok_idx)%line = line
    stream%tokens(tok_idx)%column = col
    stream%tokens(tok_idx)%raw_off = raw_off
    stream%tokens(tok_idx)%raw_len = raw_len
    stream%tokens(tok_idx)%norm_off = noff
    stream%tokens(tok_idx)%norm_len = nlen
    stream%tokens(tok_idx)%lead_first = lead_first
    stream%tokens(tok_idx)%lead_count = lead_cnt
    stream%tokens(tok_idx)%trail_first = stream%trivia_count + 1_i32
    stream%tokens(tok_idx)%trail_count = 0_i32
    stream%tokens(tok_idx)%stmt_end = .false._lk
  end subroutine fstream_add_token

  !> @brief Check exact byte-for-byte equality of token tok_idx's raw lexeme against target.
  !> @param[in] stream  Token stream arena.
  !> @param[in] tok_idx 1-based token index.
  !> @param[in] target  Expected raw string.
  !> @return True if raw bytes match target.
  pure function ftok_raw_eq(stream, tok_idx, target) result(ok)
    type(cdd_cst_token_stream_t), intent(in) :: stream
    integer(i32), intent(in) :: tok_idx
    character(len=*, kind=c_k), intent(in) :: target
    logical(lk) :: ok
    integer(i32) :: roff, rlen

    roff = stream%tokens(tok_idx)%raw_off
    rlen = stream%tokens(tok_idx)%raw_len
    ok = str_eq(stream%source_buf%raw(roff:roff + rlen - 1_i32), target)
  end function ftok_raw_eq

  !> @brief Case-insensitive comparison of token tok_idx's normalized lexeme against target.
  !> @param[in] stream  Token stream arena.
  !> @param[in] tok_idx 1-based token index.
  !> @param[in] target  Expected keyword/identifier string.
  !> @return True if normalized lexeme matches target ignoring case.
  pure function ftok_norm_eq(stream, tok_idx, target) result(ok)
    type(cdd_cst_token_stream_t), intent(in) :: stream
    integer(i32), intent(in) :: tok_idx
    character(len=*, kind=c_k), intent(in) :: target
    logical(lk) :: ok
    integer(i32) :: noff, nlen

    noff = stream%tokens(tok_idx)%norm_off
    nlen = stream%tokens(tok_idx)%norm_len
    ok = str_eq_nocase(stream%norm_pool%raw(noff:noff + nlen - 1_i32), target)
  end function ftok_norm_eq

  !> @brief Reconstruct full source text by concatenating leading_trivia + raw + trailing_trivia.
  !> @param[in]  stream  Token stream arena.
  !> @param[out] out_buf Reconstructed character buffer.
  !> @param[out] err     Error status record.
  pure subroutine fstream_concat_lossless(stream, out_buf, err)
    type(cdd_cst_token_stream_t), intent(in) :: stream
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: t, k, idx, off, slen

    call buf_init(out_buf)
    call cdd_error_clear(err)
    do t = 1_i32, stream%token_count
      do k = 0_i32, stream%tokens(t)%lead_count - 1_i32
        idx = stream%tokens(t)%lead_first + k
        off = stream%trivia(idx)%src_off
        slen = stream%trivia(idx)%src_len
        call buf_append_str(out_buf, stream%source_buf%raw(off:off + slen - 1_i32), err)
      end do
      off = stream%tokens(t)%raw_off
      slen = stream%tokens(t)%raw_len
      if (slen > 0_i32) then
        call buf_append_str(out_buf, stream%source_buf%raw(off:off + slen - 1_i32), err)
      end if
      do k = 0_i32, stream%tokens(t)%trail_count - 1_i32
        idx = stream%tokens(t)%trail_first + k
        off = stream%trivia(idx)%src_off
        slen = stream%trivia(idx)%src_len
        call buf_append_str(out_buf, stream%source_buf%raw(off:off + slen - 1_i32), err)
      end do
    end do
  end subroutine fstream_concat_lossless

end module cdd_cst_tokens
