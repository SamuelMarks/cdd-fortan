!> @file cdd_json_tokens.f90
!> @brief Strongly-typed RFC 8259 JSON token definitions and bounded token-stream arena.

!> @brief JSON token enumeration constants, span records, and stream container.
module cdd_json_tokens
  use cdd_kinds, only: i32
  implicit none (type, external)
  private

  public :: CDD_MAX_JTOKENS
  public :: JTOK_EOF, JTOK_LBRACE, JTOK_RBRACE, JTOK_LBRACKET, JTOK_RBRACKET
  public :: JTOK_COLON, JTOK_COMMA, JTOK_STRING, JTOK_INTEGER, JTOK_FLOAT
  public :: JTOK_TRUE, JTOK_FALSE, JTOK_NULL
  public :: cdd_json_token_t, cdd_json_token_stream_t
  public :: jstream_init

  integer(i32), parameter :: CDD_MAX_JTOKENS = 2048_i32 !< Max JSON tokens in one document stream.

  integer(i32), parameter :: JTOK_EOF = 0_i32           !< End-of-input sentinel token.
  integer(i32), parameter :: JTOK_LBRACE = 1_i32        !< Left curly brace '{'.
  integer(i32), parameter :: JTOK_RBRACE = 2_i32        !< Right curly brace '}'.
  integer(i32), parameter :: JTOK_LBRACKET = 3_i32      !< Left square bracket '['.
  integer(i32), parameter :: JTOK_RBRACKET = 4_i32      !< Right square bracket ']'.
  integer(i32), parameter :: JTOK_COLON = 5_i32         !< Key-value separator ':'.
  integer(i32), parameter :: JTOK_COMMA = 6_i32         !< Element separator ','.
  integer(i32), parameter :: JTOK_STRING = 7_i32        !< Quoted string token (interior span).
  integer(i32), parameter :: JTOK_INTEGER = 8_i32       !< Exact integer literal token.
  integer(i32), parameter :: JTOK_FLOAT = 9_i32         !< Fractional/scientific number token.
  integer(i32), parameter :: JTOK_TRUE = 10_i32         !< Boolean literal 'true'.
  integer(i32), parameter :: JTOK_FALSE = 11_i32        !< Boolean literal 'false'.
  integer(i32), parameter :: JTOK_NULL = 12_i32         !< Null literal 'null'.

  !> @brief Single JSON lexical token with 1-based line, column, and byte span offsets.
  type :: cdd_json_token_t
    integer(i32) :: kind = JTOK_EOF     !< Token category (JTOK_*).
    integer(i32) :: line = 1_i32        !< 1-based starting line number.
    integer(i32) :: column = 1_i32      !< 1-based starting column number.
    integer(i32) :: span_start = 1_i32  !< 1-based start offset in source string.
    integer(i32) :: span_end = 0_i32    !< 1-based inclusive end offset in source string.
  end type cdd_json_token_t

  !> @brief Bounded token stream arena produced by cdd_json_lexer.
  type :: cdd_json_token_stream_t
    integer(i32) :: count = 0_i32                        !< Number of emitted tokens (including EOF).
    integer(i32) :: max_tokens = CDD_MAX_JTOKENS         !< Configurable token limit for testing.
    type(cdd_json_token_t) :: tokens(CDD_MAX_JTOKENS)    !< Pre-allocated token storage array.
  end type cdd_json_token_stream_t

contains

  !> @brief Initialize an empty JSON token stream with full CDD_MAX_JTOKENS capacity.
  !> @param[out] stream Token stream arena to reset.
  pure subroutine jstream_init(stream)
    type(cdd_json_token_stream_t), intent(out) :: stream

    stream%count = 0_i32
    stream%max_tokens = CDD_MAX_JTOKENS
  end subroutine jstream_init

end module cdd_json_tokens
