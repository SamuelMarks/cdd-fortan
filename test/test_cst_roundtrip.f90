!> @file test_cst_roundtrip.f90
!> @brief Comprehensive 100% branch-coverage unit tests for Fortran CST lexer, parser, and printer.

!> @brief Execute all Milestone 5 and Milestone 6 lossless CST round-trip and error tests.
!> @return Program exit status (0 on pass).
program test_cst_roundtrip
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: CDD_OK, CDD_ERR_FORTRAN_LEX, CDD_ERR_FORTRAN_PARSE, &
    CDD_ERR_BUFFER_OVERFLOW, CDD_ERR_INVALID_OPTION, cdd_error_t, cdd_error_clear
  use cdd_string, only: CDD_BUF_MAX, cdd_buffer_t, buf_init_bounded, buf_equals
  use cdd_standard, only: STD_FORTRAN_I, STD_FORTRAN_66, STD_FORTRAN_77, STD_FORTRAN_90, &
    STD_FORTRAN_2023, FORM_FREE, cdd_options_t, cdd_default_options
  use cdd_cst_trivia, only: TRIVIA_WHITESPACE, cdd_trivia_piece_t, trivia_piece_init
  use cdd_cst_tokens, only: FTOK_IDENT, cdd_cst_token_t, cdd_cst_token_stream_t, fstream_init, &
    fstream_add_trivia, fstream_add_token, ftok_raw_eq, ftok_norm_eq, fstream_concat_lossless
  use cdd_cst_lexer, only: cst_lex
  use cdd_cst_nodes, only: CST_FIELD_DECL, cdd_cst_node_t, cdd_cst_tree_t, cst_node_init, &
    cst_tree_init, cst_add_node, cst_mark_deleted, cst_replace_node, cst_insert_before, &
    cst_insert_after
  use cdd_cst_parser, only: cst_parse, cst_parse_bounded
  use cdd_cst_printer, only: cst_print, cst_print_to_buf
  use test_harness, only: test_state_t, th_init, th_check, th_eq_i32, th_touch_vtable, th_finish
  implicit none (type, external)

  type(test_state_t) :: st

  call th_init(st)
  call run_trivia_and_tokens_tests(st)
  call run_fixed_form_roundtrip_tests(st)
  call run_free_form_roundtrip_tests(st)
  call run_surgical_printer_and_error_tests(st)
  call th_finish(st, "test_cst_roundtrip")

contains

  !> @brief Exercise trivia, token stream arenas, raw/norm equality, and bounded overflows.
  !> @param[inout] st Test state accumulator.
  subroutine run_trivia_and_tokens_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_trivia_piece_t) :: tp
    type(cdd_cst_token_t) :: tk
    type(cdd_cst_token_stream_t) :: stream
    type(cdd_cst_node_t) :: nd
    type(cdd_cst_tree_t) :: tree
    type(cdd_error_t) :: err
    integer(i32) :: tidx, tok_idx

    call trivia_piece_init(tp)
    call cst_node_init(nd)
    call cst_tree_init(tree)
    call th_touch_vtable(tp)
    call th_touch_vtable(tk)
    call th_touch_vtable(stream)
    call th_touch_vtable(nd)
    call th_touch_vtable(tree)

    call fstream_init(stream)
    stream%max_trivia = 1_i32
    stream%max_tokens = 1_i32
    call cdd_error_clear(err)
    call fstream_add_trivia(stream, TRIVIA_WHITESPACE, 1_i32, 0_i32, 0_i32, tidx, err)
    call fstream_add_trivia(stream, TRIVIA_WHITESPACE, 1_i32, 1_i32, 0_i32, tidx, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "trivia arena full")
    call fstream_add_trivia(stream, TRIVIA_WHITESPACE, 1_i32, 1_i32, 0_i32, tidx, err)

    call cdd_error_clear(err)
    call fstream_add_token(stream, FTOK_IDENT, 1_i32, 1_i32, 1_i32, 3_i32, "ABC", &
      1_i32, 1_i32, tok_idx, err)
    stream%source_buf%raw(1:3) = "Abc"
    stream%source_buf%len = 3_i32
    call th_check(st, ftok_raw_eq(stream, tok_idx, "Abc") .and. &
                      ftok_norm_eq(stream, tok_idx, "abc"), "ftok raw & norm eq")
    call fstream_add_token(stream, FTOK_IDENT, 1_i32, 1_i32, 1_i32, 1_i32, "X", &
      1_i32, 0_i32, tok_idx, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "token arena full")
    call fstream_add_token(stream, FTOK_IDENT, 1_i32, 1_i32, 1_i32, 1_i32, "X", &
      1_i32, 0_i32, tok_idx, err)

    ! Norm pool overflow inside fstream_add_token
    call fstream_init(stream)
    call buf_init_bounded(stream%norm_pool, 1_i32, err)
    call fstream_add_token(stream, FTOK_IDENT, 1_i32, 1_i32, 1_i32, 3_i32, "ABC", &
      1_i32, 0_i32, tok_idx, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "norm_pool overflow")
  end subroutine run_trivia_and_tokens_tests

  !> @brief Verify byte-for-byte CST round-trip on fixed-form FORTRAN I, IV, 66, and 77 decks.
  !> @param[inout] st Test state accumulator.
  subroutine run_fixed_form_roundtrip_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_options_t) :: opts
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=1, kind=c_k), parameter :: CR = achar(13, kind=c_k)
    character(len=*), parameter :: DECK_F57 = &
      "C> IBM 704 FORTRAN (I) PUNCH CARD DECK                                  SEQ00010" // CR // LF // &
      "C$OMPLEGACY                                                             SEQ00020" // CR // LF // &
      "#ifdef IBM704" // CR // LF // &
      "                                                                        SEQ00030" // CR // LF // &
      "      DIMENSION IBUF(10), XARR(4)                                       SEQ00040" // CR // LF // &
      "      EQUIVALENCE (IBUF(1), ICOUNT)                                     SEQ00050" // CR // LF // &
      "C @cdd-begin-routine: U2JPKT" // CR // LF // &
      "  100 ICOUNT = 1   ! inline comment before col 73                       SEQ00060" // CR // LF // &
      "      ! bang comment in col 7                                           SEQ00065" // CR // LF // &
      "      GO TO 200                                                         SEQ00070" // CR // LF // &
      "  200 CONTINUE                                                          SEQ00080" // CR // LF // &
      "C @cdd-end-routine: U2JPKT" // CR // LF // &
      "#endif"

    character(len=*), parameter :: DECK_F66_HOLL = &
      "C" // LF // &
      LF // &
      "      BLOCK DATA                                                        CARD0001" // LF // &
      "      END" // LF // &
      "      BLOCK DATA INITBD" // LF // &
      "      INTEGER*2 ISHORT, IARR(1:4)" // LF // &
      "      REAL*8 XVAL" // LF // &
      "      REAL*16 QVAL" // LF // &
      "      DOUB LE PRE CISION DVAL" // LF // &
      "      COMMON A, B" // LF // &
      "      COMMON /PKTBLK/ ISHORT, XVAL, DVAL" // LF // &
      "      DATA ISHORT /1/, DVAL /1.0D+00/" // LF // &
      "      DATA IARR /4HABCD,                                                CARD0007" // LF // &
      "     1 10H0123456789/                                                   CARD0008" // LF // &
      "C Multi-card Hollerith spanning column 72 boundary (and exact col-72 2H):" // LF // &
      "      DATA KSTR /                                                       CARD0010" // LF // &
      "     *                                                           8HHELLO" // LF // &
      "     *!YZ,                                                            2H" // LF // &
      "     *AB/                                                               CARD0012" // LF // &
      "      GO TO_VAR = 0H!tight" // LF // &
      "      ! short bang" // LF // &
      "1 0   CONTINUE" // LF // &
      "  10" // LF // &
      "  999                                                                   CARD0014" // LF // &
      "      END" // LF

    character(len=*), parameter :: DECK_F77_DEC = &
      "      SUBROUTINE PKTSUB(CBUF, N)" // LF // &
      "      CHARACTER*(*) CBUF" // LF // &
      "      CHARACTER*16 CNAME*24" // LF // &
      "      STRUCTURE /SENSOR/" // LF // &
      "        INTEGER*4 ID" // LF // &
      "      END STRUCTURE" // LF // &
      "      RECORD /SENSOR/ SREC" // LF // &
      "      SAVE /PKTBLK/" // LF // &
      "      IF (N .EQ. 1 .AND. 1.EQ.1) THEN" // LF // &
      "        SREC.ID = 2" // LF // &
      "      END IF" // LF // &
      "      END" // LF

    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_I
    call assert_roundtrip(st, DECK_F57, opts, "FORTRAN I IBM 704 CRLF card deck")

    opts%standard%id = STD_FORTRAN_66
    call assert_roundtrip(st, DECK_F66_HOLL, opts, "FORTRAN 66 multi-card Hollerith & spaced kw")

    opts%standard%id = STD_FORTRAN_77
    opts%allow_dec_structure = .true._lk
    call assert_roundtrip(st, DECK_F77_DEC, opts, "FORTRAN 77 DEC STRUCTURE & RECORD")
  end subroutine run_fixed_form_roundtrip_tests

  !> @brief Verify byte-for-byte CST round-trip across free-form F90..F2023 constructs.
  !> @param[inout] st Test state accumulator.
  subroutine run_free_form_roundtrip_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_options_t) :: opts
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=1, kind=c_k), parameter :: TAB = achar(9, kind=c_k)
    character(len=*), parameter :: SRC_F2023 = &
      "!> @file modern.f90" // LF // &
      "#include ""defs.inc""" // LF // &
      "!DEC$ ATTRIBUTES INLINE :: fast_fn" // LF // &
      "!GCC$ ATTRIBUTES COLD :: slow_fn" // LF // &
      "module modern_mod" // LF // &
      "  implicit none (type, external)" // LF // &
      "  integer, parameter :: CONST_K = 4_4" // LF // &
      "  enum, bind(c)" // LF // &
      "    enumerator :: C_A = 0, C_B = 1" // LF // &
      "  end enum" // LF // &
      "  enumeration type :: color_e" // LF // &
      "    enumerator :: RED, GREEN" // LF // &
      "  end enumeration type color_e" // LF // &
      "  type, public :: base_t" // LF // &
      "    integer(int8) :: b8 = 1" // LF // &
      "    integer(int16) :: b16 = 2" // LF // &
      "    integer(int64) :: b64 = 3" // LF // &
      "  end type base_t" // LF // &
      "  type, extends(base_t), bind(c) :: child_t" // LF // &
      TAB // "real(real64), allocatable, dimension(:,:) :: mat !< Matrix" // LF // &
      "    real(real128), pointer :: ptr(:) => null()" // LF // &
      "    complex(real64) :: z = (0.0d0, 1.0e-2)" // LF // &
      "    logical(1) :: flag = .true._1" // LF // &
      "    character(len=:), allocatable :: label" // LF // &
      "    character(len=12) :: tag = 'it''s_ok'_1" // LF // &
      "    character(*) :: star_c" // LF // &
      "    type(base_t) :: nested(-2:5, *)" // LF // &
      "  contains" // LF // &
      "    procedure :: to_json => child_to_json" // LF // &
      "    procedure :: plain_tbp" // LF // &
      "  end type child_t" // LF // &
      "  type legacy_no_dc" // LF // &
      "    double precision :: dp_f = 1.E+2 + 1.E2 + 1." // LF // &
      "    integer :: fixed_arr(10_4)   " // LF // &
      "  end type legacy_no_dc" // LF // &
      "  interface gen_op" // LF // &
      "    module procedure fast_fn" // LF // &
      "  end interface gen_op" // LF // &
      "  abstract interface" // LF // &
      "    subroutine iface_sub(x)" // LF // &
      "      integer, intent(in out) :: x(N)" // LF // &
      "    end subroutine iface_sub" // LF // &
      "  end interface" // LF // &
      "contains" // LF // &
      "  pure function fast_fn(a, b) result(res)" // LF // &
      "    integer, intent(in) :: a ! trailing" // LF // &
      "    integer, intent(out) :: & ! inline on amp" // LF // &
      "      & b" // LF // &
      "    integer :: v(2), end; v = [1, 2]; end = 0" // LF // &
      "    integer :: res; res = (a == b ? 1 : 0)" // LF // &
      "    if (a < b .or. a > 0 .or. a <= b .or. a >= 0 .and. a /= 2) then" // LF // &
      "      b = a ** 2 + 1_4" // LF // &
      "    end if" // LF // &
      "    blk_lbl: block" // LF // &
      "      integer :: tmp" // LF // &
      "      tmp = 1" // LF // &
      "    end block blk_lbl" // LF // &
      "  end function fast_fn" // LF // &
      "  subroutine child_to_json(self, arr)" // LF // &
      "    class(child_t), intent(inout) :: self" // LF // &
      "    real :: arr(..)" // LF // &
      "    select type (self)" // LF // &
      "    type is (child_t)" // LF // &
      "      self%label = ""a""""b"" // 'c'_1" // LF // &
      "    end select" // LF // &
      "  end subroutine child_to_json" // LF // &
      "end module modern_mod" // LF // &
      "submodule (modern_mod) modern_sub" // LF // &
      "end submodule modern_sub" // LF // &
      "program demo_prog" // LF // &
      "end program demo_prog"

    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_2023
    call assert_roundtrip(st, SRC_F2023, opts, "Fortran 2023 comprehensive round-trip")
  end subroutine run_free_form_roundtrip_tests

  !> @brief Parse src, print CST + concat stream, and assert both equal src byte-for-byte.
  !> @param[inout] st    Test state accumulator.
  !> @param[in]    src   Input Fortran source.
  !> @param[in]    opts  Dialect options.
  !> @param[in]    label Assertion description.
  subroutine assert_roundtrip(st, src, opts, label)
    type(test_state_t), intent(inout) :: st
    character(len=*, kind=c_k), intent(in) :: src
    type(cdd_options_t), intent(in) :: opts
    character(len=*, kind=c_k), intent(in) :: label
    type(cdd_cst_tree_t) :: tree
    type(cdd_buffer_t) :: printed, concatted
    type(cdd_error_t) :: err

    call cst_parse(src, opts, tree, err)
    call th_eq_i32(st, err%code, CDD_OK, label)
    call fstream_concat_lossless(tree%stream, concatted, err)
    call th_check(st, buf_equals(concatted, src), label)
    call cst_print(tree, printed, err)
    call th_check(st, buf_equals(printed, src), label)
  end subroutine assert_roundtrip

  !> @brief Exercise surgical CST edits (delete, replace, insert_before/after) and all error paths.
  !> @param[inout] st Test state accumulator.
  subroutine run_surgical_printer_and_error_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_options_t) :: opts
    type(cdd_cst_tree_t) :: tree
    type(cdd_cst_token_stream_t) :: stream
    type(cdd_buffer_t) :: out_buf
    type(cdd_error_t) :: err
    integer(i32) :: i, f1, f2, f3, dummy_n
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=*), parameter :: MINI_TYPE = &
      "type :: p_t" // LF // &
      "  integer :: a ! keep_a" // LF // &
      "  integer :: b ! del_b" // LF // &
      "  integer :: c ! rep_c" // LF // &
      "end type p_t" // LF

    call cdd_default_options(opts)
    call cst_parse(MINI_TYPE, opts, tree, err)
    call th_eq_i32(st, err%code, CDD_OK, "parse mini_type")

    f1 = 0_i32; f2 = 0_i32; f3 = 0_i32
    do i = 1_i32, tree%node_count
      if (tree%nodes(i)%kind == CST_FIELD_DECL) then
        if (f1 == 0_i32) then
          f1 = i
        else if (f2 == 0_i32) then
          f2 = i
        else
          f3 = i
        end if
      end if
    end do

    call cst_insert_before(tree, f1, "  ! pre1" // LF, err)
    call cst_insert_before(tree, f1, "  ! pre2" // LF, err)
    call cst_insert_after(tree, f1, "  integer :: a2" // LF, err)
    call cst_insert_after(tree, f1, "  integer :: a3" // LF, err)
    call cst_mark_deleted(tree, f2)
    call cst_insert_after(tree, f2, "  ! where_b_was" // LF, err)
    call cst_replace_node(tree, f3, "real :: c_new", .true._lk, .true._lk, err)
    call cst_print(tree, out_buf, err)
    call th_eq_i32(st, err%code, CDD_OK, "surgical cst_print")
    call th_check(st, buf_equals(out_buf, &
      "type :: p_t" // LF // &
      "  ! pre1" // LF // &
      "  ! pre2" // LF // &
      "  integer :: a ! keep_a" // LF // &
      "  integer :: a2" // LF // &
      "  integer :: a3" // LF // &
      "  ! where_b_was" // LF // &
      "  real :: c_new ! rep_c" // LF // &
      "end type p_t" // LF), "surgical output exact")

    ! Also test cst_replace_node with empty replacement, keep_lead=.false., and cst_print_to_buf overflow
    call cst_replace_node(tree, f3, "", .false._lk, .false._lk, err)
    call cst_print(tree, out_buf, err)
    call th_eq_i32(st, err%code, CDD_OK, "replace with empty string")
    call buf_init_bounded(out_buf, 3_i32, err)
    call cst_print_to_buf(tree, out_buf, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "cst_print_to_buf overflow")
    call buf_init_bounded(out_buf, 1_i32, err)
    call cst_insert_before(tree, 1_i32, "!!", err)
    call cst_print_to_buf(tree, out_buf, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "cst_print_to_buf before_ins overflow")

    ! Empty source round-trip in free and fixed form
    call assert_roundtrip(st, "", opts, "empty free-form round-trip")
    opts%standard%id = STD_FORTRAN_66
    call assert_roundtrip(st, "", opts, "empty fixed-form round-trip")
    call cdd_default_options(opts)

    ! Malformed / degenerate opaque statement shapes that still round-trip losslessly
    call assert_roundtrip(st, &
      "10 = 1!tight" // LF // &
      "x = 1 !" // LF // &
      "s = 'abc'" // LF // &
      "s2 = 'abc'_1 + 1" // LF // &
      "b = .TRUE." // LF // &
      "b2 = .TRUE._1 .and. .FALSE." // LF // &
      "x = 1 &   " // LF // &
      "" // LF // &
      "  + 2" // LF // &
      "r = 1.E" // LF // &
      "r2 = 1.E + 1" // LF // &
      "enumeration = 1; structure = 1; record = 1 + 2 + 3" // LF // &
      "type" // LF // &
      "integer ::" // LF // &
      "integer :: 123, ok_after_bad + 1" // LF // &
      "real(real32), dimension, allocatable :: rf32" // LF // &
      "type :: 123" // LF // "  procedure :: 123" // LF // "end type" // LF // &
      "subroutine s0" // LF // "end subroutine" // LF // &
      "block" // LF // "end block" // LF // &
      "end block_not_data = 1" // LF // &
      "common" // LF // "common /123/" // LF // &
      "complex :: cz; logical :: lb; character :: ch1; character*x :: ch2; character() :: ch3" // LF // &
      "type() :: et; type :: tbad" // LF // "end type" // LF // &
      "integer((4)), intent(bad), dimension :: kd" // LF // &
      "integer :: r5(1,2,3,4,5), neg(-x:1), a(:10), b(), u(1" // LF // &
      "! @cdd-begin-routine:   " // LF // &
      "! @cdd-begin-routine:R1   " // LF // &
      "x = ." // LF // &
      "! @cdd-end-routine:R1" // LF, &
      opts, "degenerate statements round-trip")

    ! Lex error through cst_parse & bounded CST node arena overflows
    call cst_parse("@", opts, tree, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_LEX, "cst_parse lex error")
    call cst_parse_bounded("x = 1", opts, 0_i32, tree, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "cst_parse max_nodes=0")
    call cst_parse_bounded("! @cdd-begin-routine: R" // LF // "x = 1", opts, 1_i32, tree, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "cst_parse routine max_nodes=1")
    call cst_parse_bounded("integer :: a, b", opts, 2_i32, tree, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "cst_parse field max_nodes=2")

    ! Source buffer > CDD_BUF_MAX overflow in cst_lex
    call cst_lex(repeat(" ", CDD_BUF_MAX + 1_i32), opts, stream, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "cst_lex >CDD_BUF_MAX overflow")

    ! Max block depth (32 nested interfaces)
    call cst_parse(repeat("interface" // LF, 33), opts, tree, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_PARSE, "max block depth exceeded")

    call buf_init_bounded(tree%patch_pool, 1_i32, err)
    call cst_replace_node(tree, f3, "abcd", .false._lk, .false._lk, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "cst_replace_node pool overflow")
    call cdd_error_clear(err)
    call cst_insert_before(tree, f3, "abcd", err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "cst_insert_before pool overflow")
    call cdd_error_clear(err)
    call cst_insert_after(tree, f3, "abcd", err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "cst_insert_after pool overflow")

    tree%max_nodes = tree%node_count
    call cdd_error_clear(err)
    call cst_add_node(tree, CST_FIELD_DECL, 1_i32, 1_i32, 1_i32, dummy_n, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "cst_add_node overflow")
    call cst_add_node(tree, CST_FIELD_DECL, 1_i32, 1_i32, 1_i32, dummy_n, err)

    ! Invalid form option on cst_lex
    opts%standard%id = STD_FORTRAN_66
    opts%source_form = FORM_FREE
    call cst_lex("      END", opts, stream, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "cst_lex F66 free-form rejected")

    ! Fixed-form lexical errors
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_66
    call cst_lex("  X   CONTINUE", opts, stream, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_LEX, "fixed bad label col")
    call cst_lex("      DATA K /8HSHORT", opts, stream, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_LEX, "truncated Hollerith EOF")
    call cst_lex("      DATA K /80HSHORT" // LF // "      END", opts, stream, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_LEX, "truncated Hollerith no cont")

    ! Free-form lexical errors
    opts%standard%id = STD_FORTRAN_90
    call cst_lex(repeat("x", 140), opts, stream, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_LEX, "F90 >132 line width")
    call cst_lex("integer :: x & y" // LF // "  z", opts, stream, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_LEX, "token after ampersand")
    call cst_lex("integer :: x &", opts, stream, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_LEX, "dangling ampersand at EOF")
    call cst_lex("character :: c = 'unterminated", opts, stream, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_LEX, "unterminated char literal")
    call cst_lex("logical :: b = .TRUE", opts, stream, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_LEX, "unterminated dot op")
    call cst_lex("integer :: x = @", opts, stream, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_LEX, "invalid character @")

    ! CST parser errors
    call cst_parse("end module bad_m", opts, tree, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_PARSE, "unmatched top-level END")
    call cst_parse("module unclosed_m" // LF // "  integer :: x", opts, tree, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_PARSE, "unclosed module at EOF")
  end subroutine run_surgical_printer_and_error_tests

end program test_cst_roundtrip
