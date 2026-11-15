!> @file cdd_cst_trivia.f90
!> @brief Lossless Fortran trivia piece definitions (whitespace, comments, continuations, cols 1-80).

!> @brief Trivia categories and arena structures preserving every non-semantic byte in Fortran source.
module cdd_cst_trivia
  use cdd_kinds, only: i32
  implicit none (type, external)
  private

  public :: CDD_MAX_TRIVIA
  public :: TRIVIA_WHITESPACE, TRIVIA_NEWLINE, TRIVIA_LINE_COMMENT, TRIVIA_DOXYGEN_COMMENT
  public :: TRIVIA_CONTINUATION_FREE, TRIVIA_FIXED_LABEL_GAP, TRIVIA_FIXED_MARGIN_73_80
  public :: TRIVIA_DIRECTIVE
  public :: cdd_trivia_piece_t
  public :: trivia_piece_init

  integer(i32), parameter :: CDD_MAX_TRIVIA = 57344_i32           !< Max trivia slices per file CST.

  integer(i32), parameter :: TRIVIA_WHITESPACE = 1_i32            !< Horizontal spaces/tabs.
  integer(i32), parameter :: TRIVIA_NEWLINE = 2_i32               !< Exact LF or CRLF line ending.
  integer(i32), parameter :: TRIVIA_LINE_COMMENT = 3_i32          !< Standard comment (! or col-1 C or *).
  integer(i32), parameter :: TRIVIA_DOXYGEN_COMMENT = 4_i32       !< Doxygen comment (!>, !<, !!, C>).
  integer(i32), parameter :: TRIVIA_CONTINUATION_FREE = 5_i32     !< Free-form '&' continuation span.
  integer(i32), parameter :: TRIVIA_FIXED_LABEL_GAP = 6_i32       !< Fixed-form columns 1-6 gap/cont.
  integer(i32), parameter :: TRIVIA_FIXED_MARGIN_73_80 = 7_i32    !< Fixed-form card ID cols 73-80.
  integer(i32), parameter :: TRIVIA_DIRECTIVE = 8_i32             !< Preprocessor (#) or !$OMP/!DEC$.

  !> @brief Single contiguous byte slice of non-token trivia in source_buf.
  type :: cdd_trivia_piece_t
    integer(i32) :: kind = TRIVIA_WHITESPACE !< Trivia category constant (TRIVIA_*).
    integer(i32) :: src_off = 1_i32          !< 1-based start offset in source_buf.
    integer(i32) :: src_len = 0_i32          !< Byte length in source_buf.
    integer(i32) :: term_kind = 0_i32        !< TERM_LF / TERM_CRLF when kind == TRIVIA_NEWLINE.
  end type cdd_trivia_piece_t

contains

  !> @brief Initialize a trivia piece record to empty whitespace defaults.
  !> @param[out] piece Trivia piece record to reset.
  pure subroutine trivia_piece_init(piece)
    type(cdd_trivia_piece_t), intent(out) :: piece

    piece%kind = TRIVIA_WHITESPACE
    piece%src_off = 1_i32
    piece%src_len = 0_i32
    piece%term_kind = 0_i32
  end subroutine trivia_piece_init

end module cdd_cst_trivia
