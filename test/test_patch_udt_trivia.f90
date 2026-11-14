!> @file test_patch_udt_trivia.f90
!> @brief Comprehensive trivia-preservation, idempotency, and 100% branch-coverage tests for Op 3.

!> @brief Execute all Milestone 10 surgical CST patching and trivia-preservation tests.
!> @return Program exit status (0 on pass).
program test_patch_udt_trivia
  use cdd_kinds, only: lk, c_k
  use cdd_errors, only: CDD_OK, CDD_ERR_INVALID_OPTION, CDD_ERR_JSON_LEX, CDD_ERR_JSON_SCHEMA, &
    CDD_ERR_FORTRAN_LEX, cdd_error_t
  use cdd_string, only: cdd_buffer_t, buf_equals, str_set
  use cdd_standard, only: STD_FORTRAN_I, STD_FORTRAN_66, STD_FORTRAN_77, &
    STD_FORTRAN_90, STD_FORTRAN_2003, STD_FORTRAN_2023, FORM_FREE, JSON_MODE_SCHEMA, &
    JSON_MODE_SAMPLE, cdd_options_t, cdd_default_options
  use cdd_patch_udt, only: patch_fortran_with_json
  use test_harness, only: test_state_t, th_init, th_check, th_eq_i32, th_finish
  implicit none (type, external)

  type(test_state_t) :: st

  call th_init(st)
  call test_free_form_add_remove_modify_and_banners(st)
  call test_companion_codecs_trivia_and_crlf(st)
  call test_fixed_form_73_80_and_dec_structure(st)
  call test_missing_udt_insertion_and_errors(st)
  call th_finish(st, "test_patch_udt_trivia")

contains

  !> @brief Test add/remove/modify across empty/1/multi-field UDTs, banners, tabs, and idempotency.
  !> @param[inout] st Test harness state.
  subroutine test_free_form_add_remove_modify_and_banners(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_options_t) :: opts
    type(cdd_buffer_t) :: p1, p2
    type(cdd_error_t) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=1, kind=c_k), parameter :: TAB = achar(9, kind=c_k)
    character(len=*), parameter :: SRC0 = &
      "! === Top File Banner ===" // LF // &
      "module phys_mod" // LF // &
      "  use, intrinsic :: iso_fortran_env, only: int32, real64" // LF // &
      "  implicit none" // LF // &
      "" // LF // &
      "  type, public :: state_t" // LF // &
      "    ! --- Physics Constants ---" // LF // &
      "" // LF // &
      "    ! @cdd-field: drop_first json=""drop_first"" type=int32 len=0" // LF // &
      "    !> Member doc to excise with drop_first" // LF // &
      "    integer(int32) :: drop_first !< First field" // LF // &
      "    ! @cdd-field: keep_x json=""keep_x"" type=int32 len=0" // LF // &
      "    integer(int32) :: keep_x      !< Aligned comment preserved!" // LF // &
      "    ! --- Mid Section Banner ---" // LF // &
      "    ! @cdd-field: drop_mid json=""drop_mid"" type=int32 len=0" // LF // &
      "    integer(int32) :: drop_mid" // LF // &
      "    ! @cdd-field: mod_v json=""mod_v"" type=int32 len=0" // LF // &
      "    integer(int32) :: mod_v       !< Modified type keeps inline doc!" // LF // &
      "    integer(int32) :: drop_last" // LF // &
      "  end type state_t" // LF // &
      "end module phys_mod" // LF
    character(len=*), parameter :: J_SYNC = &
      '{"title":"State","type":"object","properties":{' // &
      '"keep_x":{"type":"integer"},' // &
      '"mod_v":{"type":"array","maxItems":3,"items":{"type":"number"}},' // &
      '"added_z":{"type":"string"}' // &
      '}}'

    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_2023
    opts%include_codecs_in_sync = .false._lk

    call patch_fortran_with_json(SRC0, J_SYNC, opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "simultaneous add+remove+modify")
    call th_check(st, index(p1%raw(1:p1%len), "! --- Physics Constants ---") > 0, &
      "top interior banner comment preserved")
    call th_check(st, index(p1%raw(1:p1%len), "! --- Mid Section Banner ---") > 0, &
      "mid interior banner comment preserved")
    call th_check(st, index(p1%raw(1:p1%len), "Member doc to excise") == 0, &
      "removed field Doxygen docstring excised")
    call th_check(st, index(p1%raw(1:p1%len), "drop_first") == 0 .and. &
                      index(p1%raw(1:p1%len), "drop_mid") == 0 .and. &
                      index(p1%raw(1:p1%len), "drop_last") == 0, &
      "first, middle, and last obsolete fields excised")
    call th_check(st, index(p1%raw(1:p1%len), &
      "integer(int32) :: keep_x      !< Aligned comment preserved!") > 0, &
      "unchanged field horizontal alignment & inline comment 100% preserved")
    call th_check(st, index(p1%raw(1:p1%len), &
      "real(real64) :: mod_v(3)       !< Modified type keeps inline doc!") > 0, &
      "modified field type/bounds updated while keeping inline alignment & doc")
    call th_check(st, index(p1%raw(1:p1%len), "added_z") > 0, "added_z spliced")

    ! Idempotency verification: second sync produces 0 byte diffs!
    call patch_fortran_with_json(p1%raw(1:p1%len), J_SYNC, opts, p2, err)
    call th_eq_i32(st, err%code, CDD_OK, "idempotent second pass")
    call th_check(st, buf_equals(p1, p2%raw(1:p2%len)), "idempotency: patched_2 == patched_1 byte-for-byte")

    ! Removing ALL fields from UDT, then adding fields to empty UDT (with TAB + UPPERCASE + no ::)
    call patch_fortran_with_json( &
      "MODULE T_MOD" // LF // &
      TAB // "TYPE EMPTY_T" // LF // &
      TAB // TAB // "INTEGER OLD_F" // LF // &
      TAB // "END TYPE EMPTY_T" // LF // &
      "END MODULE T_MOD" // LF, &
      '{"title":"Empty","type":"object","properties":{"a":{"type":"integer"},"b":{"type":"boolean"}}}', &
      opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "replace all fields in TAB+UPPER+no-dcolon UDT")
    call th_check(st, index(p1%raw(1:p1%len), TAB // TAB // "INTEGER(INT32) a") > 0, &
      "detected TAB indent, UPPERCASE keywords, and omitted ::")

    ! Adding fields to a truly 0-field derived type
    call patch_fortran_with_json( &
      "module z_mod" // LF // "  type :: z_t" // LF // "  end type z_t" // LF // &
      "end module z_mod" // LF, &
      '{"x": 42}', opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "add field to 0-field UDT via sample JSON")
    call th_check(st, index(p1%raw(1:p1%len), "integer(int32) :: x") > 0, "x added to 0-field UDT")

    ! String length, deferred string, rank-only, upper_bound-only, dim kind, & col-1 blank-line UDT
    opts%standard%id = STD_FORTRAN_90
    call patch_fortran_with_json( &
      "module s90_mod" // LF // &
      "  type :: first_t" // LF // &
      "    integer :: u" // LF // &
      "  end type first_t" // LF // &
      "type :: s90_t" // LF // &
      "" // LF // &
      "character(len=8) :: tag; integer :: drop_inline" // LF // &
      "    ! @cdd-field: drop_doc_only json=""drop_doc_only"" type=int32 len=0" // LF // &
      "    integer :: drop_doc_only" // LF // &
      "    character(len=64) :: s_def" // LF // &
      "    character(len=4) :: str_to_int" // LF // &
      "    integer :: r_same" // LF // &
      "    integer :: r_ub(2)" // LF // &
      "    integer :: arr(2)" // LF // &
      "    integer :: num_f" // LF // &
      "end type s90_t" // LF // &
      "end module s90_mod" // LF, &
      '{"title":"S90","type":"object","properties":{' // &
      '"tag":{"type":"string","maxLength":16},' // &
      '"s_def":{"type":"string"},' // &
      '"str_to_int":{"type":"integer"},' // &
      '"r_same":{"type":"array","maxItems":3,"items":{"type":"integer"}},' // &
      '"r_ub":{"type":"array","maxItems":4,"items":{"type":"integer"}},' // &
      '"arr":{"type":"array","items":{"type":"integer"}},' // &
      '"num_f":{"type":"number"}}' // &
      '}', &
      opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "F90 char_len, dim kind, scalar type & col-1 inline")
    call th_check(st, index(p1%raw(1:p1%len), "character(len=16)") > 0 .and. &
                      index(p1%raw(1:p1%len), "drop_inline") == 0, &
      "F90 len=16 and inline ; field excised")

    ! Col-1 standalone statement (first_tok lead_count == 0, plus string->int in F90)
    call patch_fortran_with_json( &
      "type::c1_t;integer::drop0;character(len=4)::keep0;end type c1_t" // LF, &
      '{"keep0": 1}', opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "col-1 type with 0-lead-trivia token & string->int")
  end subroutine test_free_form_add_remove_modify_and_banners

  !> @brief Test trivia preservation above, between, and below `<udt>_to_json` & `<udt>_from_json` + CRLF.
  !> @param[inout] st Test harness state.
  subroutine test_companion_codecs_trivia_and_crlf(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_options_t) :: opts
    type(cdd_buffer_t) :: p1, p2
    type(cdd_error_t) :: err
    character(len=2, kind=c_k), parameter :: CRLF = achar(13, kind=c_k) // achar(10, kind=c_k)
    character(len=*), parameter :: CRLF_MOD = &
      "module orb_mod" // CRLF // &
      "  use, intrinsic :: iso_fortran_env, only: int32, int64" // CRLF // &
      "  implicit none" // CRLF // &
      "  type, public :: orb_t" // CRLF // &
      "    ! @cdd-field: id json=""id"" type=int32 len=0" // CRLF // &
      "    integer(int32) :: id !< ID" // CRLF // &
      "    integer(int32) :: legacy_drop !< Drop me" // CRLF // &
      "  contains" // CRLF // &
      "    procedure :: to_json => orb_t_to_json" // CRLF // &
      "    procedure :: from_json => orb_t_from_json" // CRLF // &
      "  end type orb_t" // CRLF // &
      "contains" // CRLF // &
      "" // CRLF // &
      "  ! === Custom User Helper Above Codecs ===" // CRLF // &
      "  pure integer(int32) function user_above(x)" // CRLF // &
      "    integer(int32), intent(in) :: x" // CRLF // &
      "    user_above = x + 1_int32" // CRLF // &
      "  end function user_above" // CRLF // &
      "" // CRLF // &
      "#ifdef ENABLE_JSON" // CRLF // &
      "  !> Custom user remark above orb_t_to_json" // CRLF // &
      "  pure recursive subroutine orb_t_to_json(self, json_str, ierr, errmsg)" // CRLF // &
      "    class(orb_t), intent(in) :: self" // CRLF // &
      "    character(len=:), allocatable, intent(out) :: json_str" // CRLF // &
      "    integer(int32), intent(out) :: ierr" // CRLF // &
      "    character(len=:), allocatable, intent(out), optional :: errmsg" // CRLF // &
      "    ierr = 0_int32; json_str = '{}'" // CRLF // &
      "  end subroutine orb_t_to_json" // CRLF // &
      "" // CRLF // &
      "  ! === Banner & User Subroutine BETWEEN to_json and from_json ===" // CRLF // &
      "" // CRLF // &
      "  subroutine user_between()" // CRLF // &
      "  end subroutine user_between" // CRLF // &
      "" // CRLF // &
      "  !> Custom user remark above orb_t_from_json" // CRLF // &
      "  pure recursive subroutine orb_t_from_json(self, json_str, ierr, errmsg)" // CRLF // &
      "    class(orb_t), intent(inout) :: self" // CRLF // &
      "    character(len=*), intent(in) :: json_str" // CRLF // &
      "    integer(int32), intent(out) :: ierr" // CRLF // &
      "    character(len=:), allocatable, intent(out), optional :: errmsg" // CRLF // &
      "    ierr = 0_int32" // CRLF // &
      "  end subroutine orb_t_from_json" // CRLF // &
      "#endif" // CRLF // &
      "" // CRLF // &
      "  ! === Custom User Helper BELOW Codecs ===" // CRLF // &
      "  subroutine user_below()" // CRLF // &
      "  end subroutine user_below" // CRLF // &
      "end module orb_mod" // CRLF
    character(len=*), parameter :: J_NEW = &
      '{"title":"Orb","type":"object","properties":{"id":{"type":"integer"},"vel":{"type":"integer"}}}'

    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_2003
    opts%json_mode = JSON_MODE_SCHEMA
    opts%include_codecs_in_sync = .true._lk

    call patch_fortran_with_json(CRLF_MOD, J_NEW, opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "CRLF module + companion codecs sync")
    call th_check(st, index(p1%raw(1:p1%len), "user_above") > 0 .and. &
                      index(p1%raw(1:p1%len), "! === Banner & User Subroutine BETWEEN") > 0 .and. &
                      index(p1%raw(1:p1%len), "user_between") > 0 .and. &
                      index(p1%raw(1:p1%len), "#ifdef ENABLE_JSON") > 0 .and. &
                      index(p1%raw(1:p1%len), "#endif") > 0 .and. &
                      index(p1%raw(1:p1%len), "user_below") > 0, &
      "100% of trivia, #ifdef, blank lines, and user procs above/between/below preserved")
    call th_check(st, index(p1%raw(1:p1%len), "self%vel") > 0 .and. &
                      index(p1%raw(1:p1%len), "legacy_drop") == 0, &
      "codecs updated to serialize/deserialize vel and drop legacy_drop")
    call th_check(st, index(p1%raw(1:p1%len), CRLF) > 0, "CRLF line terminators preserved")

    call patch_fortran_with_json(p1%raw(1:p1%len), J_NEW, opts, p2, err)
    call th_check(st, buf_equals(p1, p2%raw(1:p2%len)), "CRLF + codec sync idempotency: p2 == p1")

    ! Existing module with UDT but NO codecs yet (both without CONTAINS and WITH CONTAINS)
    call patch_fortran_with_json( &
      "module noc_mod" // CRLF // "  type :: noc_t" // CRLF // &
      "    integer(int32) :: id" // CRLF // "  end type noc_t" // CRLF // &
      "end module noc_mod" // CRLF, &
      '{"id": 1}', opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "splice contains + codecs into UDT-only module")
    call th_check(st, index(p1%raw(1:p1%len), "noc_t_to_json") > 0, "codecs spliced")

    call patch_fortran_with_json( &
      "module wc_mod" // CRLF // "  type :: wc_t" // CRLF // &
      "    integer(int32) :: id" // CRLF // "  end type wc_t" // CRLF // &
      "contains" // CRLF // "  subroutine h()" // CRLF // "  end subroutine h" // CRLF // &
      "end module wc_mod" // CRLF, &
      '{"id": 1}', opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "splice codecs into module that already has CONTAINS")
  end subroutine test_companion_codecs_trivia_and_crlf

  !> @brief Test fixed-form cols 73-80 card IDs across FORTRAN I, II, 66, 77 COMMON & DEC STRUCTURE.
  !> @param[inout] st Test harness state.
  subroutine test_fixed_form_73_80_and_dec_structure(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_options_t) :: opts
    type(cdd_buffer_t) :: p1, p2
    type(cdd_error_t) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=80, kind=c_k) :: c1, c2, c3, c4, c5

    ! Build 80-col punch cards with sequence IDs in cols 73-80
    c1 = "C === IBM 704 / ANSI F77 DECK HEADER ==="
    c1(73:80) = "CARD0001"
    c2 = "      INTEGER KEEP1"
    c2(73:80) = "CARD0002"
    c3 = "      INTEGER MOD1"
    c3(73:80) = "CARD0003"
    c4 = "      INTEGER DROP1"
    c4(73:80) = "CARD0004"
    c5 = "      COMMON /DECK/ KEEP1, MOD1, DROP1"
    c5(73:80) = "CARD0005"

    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_77
    opts%json_mode = JSON_MODE_SAMPLE
    opts%include_codecs_in_sync = .true._lk

    call patch_fortran_with_json( &
      c1 // LF // c2 // LF // c3 // LF // c4 // LF // c5 // LF // &
      "      END" // LF, &
      '{"keep1": 10, "mod1": 2.5, "name": "SAT"}', opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "F77 punch-card sync + split COMMON + codec splice")
    call th_check(st, index(p1%raw(1:p1%len), "CARD0001") > 0 .and. &
                      index(p1%raw(1:p1%len), "CARD0002") > 0 .and. &
                      index(p1%raw(1:p1%len), "CARD0003") > 0 .and. &
                      index(p1%raw(1:p1%len), "CARD0005") > 0, &
      "cols 73-80 card IDs preserved on banner, unchanged, modified, and COMMON cards!")
    call th_check(st, index(p1%raw(1:p1%len), "DROP1") == 0, "DROP1 excised from decls & COMMON")
    call th_check(st, index(p1%raw(1:p1%len), "DECK_TO_JSON") > 0, "F77 codecs appended")

    ! Second pass on F77 (tests replacing existing legacy subroutines + unchanged UDT shortcut)
    call patch_fortran_with_json(p1%raw(1:p1%len), &
      '{"keep1": 10, "mod1": 2.5, "name": "SAT"}', opts, p2, err)
    call th_check(st, buf_equals(p1, p2%raw(1:p2%len)), "F77 sync idempotency: p2 == p1")

    ! F66 sync covering REAL32, COMPLEX, BOOL, packed STRING, and 0-field COMMON deletion
    opts%standard%id = STD_FORTRAN_66
    opts%json_mode = JSON_MODE_SCHEMA
    opts%include_codecs_in_sync = .false._lk
    call patch_fortran_with_json( &
      "      INTEGER OLD1" // LF // "      COMMON /BLK/ OLD1" // LF, &
      '{"title":"Blk","type":"object","properties":{' // &
      '"r":{"type":"number","format":"float"},' // &
      '"ok":{"type":"boolean"},' // &
      '"s":{"type":"string","maxLength":8},' // &
      '"a":{"type":"array","maxItems":2,"items":{"type":"integer"}}}}', opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "F66 add before COMMON + rebuild single COMMON")

    ! F66 removing all fields deletes obsolete COMMON cleanly
    call patch_fortran_with_json( &
      "      INTEGER OLD1" // LF // "      COMMON /BLK/ OLD1" // LF, &
      '{"title":"Blk","type":"object","properties":{}}', opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "F66 0-field COMMON deleted")

    ! FORTRAN I (assigned GOTO routine blocks 8000/8500) & FORTRAN II sync
    opts%standard%id = STD_FORTRAN_I
    opts%include_codecs_in_sync = .true._lk
    call patch_fortran_with_json( &
      "C === TOP DECK ===" // LF // &
      "C @cdd-udt: Card" // LF // &
      "C @cdd-field: IOLD json=""old"" type=int32 len=0" // LF // &
      "      DIMENSION IUDTBK(16)" // LF // &
      "      EQUIVALENCE (IUDTBK(1), IOLD)" // LF // &
      "      DIMENSION JBUF(256)" // LF // &
      "      STOP" // LF // &
      "C === ROUTINES ===" // LF // &
      "C @cdd-begin-routine: CARD_TO_JSON" // LF // &
      " 8000 IERR = 0" // LF // &
      "      GO TO IJSRET" // LF // &
      "C @cdd-end-routine: CARD_TO_JSON" // LF // &
      "C === BETWEEN 8000 AND 8500 ===" // LF // &
      "C @cdd-begin-routine: CARD_FROM_JSON" // LF // &
      " 8500 IERR = 0" // LF // &
      "      GO TO IJSRET" // LF // &
      "C @cdd-end-routine: CARD_FROM_JSON" // LF // &
      "      END" // LF, &
      '{"title":"Card","type":"object","properties":{"cnt":{"type":"integer"}}}', &
      opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "FORTRAN I deck & assigned-GOTO routine sync")
    call th_check(st, index(p1%raw(1:p1%len), "C === TOP DECK ===") > 0 .and. &
                      index(p1%raw(1:p1%len), "C === BETWEEN 8000 AND 8500 ===") > 0, &
      "FORTRAN I banners above and between 8000/8500 preserved")
    call patch_fortran_with_json(p1%raw(1:p1%len), &
      '{"title":"Card","type":"object","properties":{"cnt":{"type":"integer"}}}', &
      opts, p2, err)
    call th_check(st, buf_equals(p1, p2%raw(1:p2%len)), "FORTRAN I sync idempotent")

    ! F77 numeric-only single COMMON + SAVE + banner above deleted + @cdd-field on modified + 2D + local var
    opts%standard%id = STD_FORTRAN_77
    opts%json_mode = JSON_MODE_SCHEMA
    opts%include_codecs_in_sync = .false._lk
    call patch_fortran_with_json( &
      "      INTEGER LOCAL_UNRELATED" // LF // &
      "C --- BANNER ABOVE DROP ---" // LF // &
      "      INTEGER DROP_B" // LF // &
      "C @cdd-field: MOD_F json=""mod_f"" type=int32 len=0" // LF // &
      "      INTEGER MOD_F" // LF // &
      "      COMMON /NUM1N/ DROP_B" // LF // &
      "      COMMON /NUM1C/ MOD_F" // LF // &
      "      SAVE /NUM1N/, /NUM1C/" // LF, &
      '{"title":"Num1","type":"object","properties":{' // &
      '"mod_f":{"type":"number"},' // &
      '"mat":{"type":"array","maxItems":2,"items":{"type":"array","maxItems":2,"items":{"type":"integer"}}}}}' , &
      opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "F77 numeric-only COMMON + SAVE + @cdd-field + 2D")
    call th_check(st, index(p1%raw(1:p1%len), "C --- BANNER ABOVE DROP ---") > 0, &
      "F77 banner above deleted field preserved")

    ! F77 udt_all_fields_unchanged branches (count 2->1, count 1->2, diff key, diff type, enum)
    call patch_fortran_with_json( &
      "      INTEGER A, B" // LF // "      COMMON /B1/ A, B" // LF, &
      '{"title":"B1","type":"object","properties":{"A":{"type":"integer"}}}', opts, p1, err)
    call patch_fortran_with_json( &
      "      INTEGER A" // LF // "      COMMON /B1/ A" // LF, &
      '{"title":"B1","type":"object","properties":{"A":{"type":"integer"},' // &
      '"en":{"type":"string","e' // 'num":["X","Y"]},"sd":{"type":"string"}}}', &
      opts, p1, err)
    call patch_fortran_with_json( &
      "      INTEGER A" // LF // "      COMMON /B1/ A" // LF, &
      '{"title":"B1","type":"object","properties":{"B":{"type":"integer"}}}', opts, p1, err)
    call patch_fortran_with_json( &
      "      INTEGER A" // LF // "      COMMON /B1/ A" // LF, &
      '{"title":"B1","type":"object","properties":{"A":{"type":"number"}}}', opts, p1, err)

    ! FORTRAN I minimal @cdd-field-only deck (no DIMENSION/EQUIVALENCE & no 8000/8500)
    opts%standard%id = STD_FORTRAN_I
    opts%include_codecs_in_sync = .true._lk
    call patch_fortran_with_json( &
      "C @cdd-field: IOLD json=""old"" type=int32 len=0" // LF // "      STOP" // LF, &
      '{"title":"Card","type":"object","properties":{"cnt":{"type":"integer"}}}', opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "FORTRAN I @cdd-field-only deck sync")

    ! DEC STRUCTURE sync covering INT16, INT64, REAL32, REAL64, BOOL, STRING, and nested RECORD
    opts%standard%id = STD_FORTRAN_77
    opts%allow_dec_structure = .true._lk
    opts%include_codecs_in_sync = .false._lk
    call patch_fortran_with_json( &
      "      STRUCTURE /pose_t/" // LF // &
      "        INTEGER*4 old_x" // LF // &
      "        INTEGER*4 mod_x" // LF // &
      "      END STRUCTURE" // LF, &
      '{"$defs":{"Sub":{"type":"object","properties":{"v":{"type":"integer"}}}},' // &
      '"title":"Pose","type":"object","properties":{' // &
      '"mod_x":{"type":"integer","format":"int64"},' // &
      '"i16":{"type":"integer","format":"int16"},' // &
      '"f32":{"type":"number","format":"float"},' // &
      '"f64":{"type":"number","format":"double"},' // &
      '"ok":{"type":"boolean"},' // &
      '"nm":{"type":"string","maxLength":8},' // &
      '"sub":{"$ref":"#/$defs/Sub"},' // &
      '"id":{"type":"integer"}}' // &
      '}', opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "F77 DEC STRUCTURE add/remove/modify all DEC types")
    call th_check(st, index(p1%raw(1:p1%len), "INTEGER*8 mod_x") > 0 .and. &
                      index(p1%raw(1:p1%len), "RECORD /sub_t/ sub") > 0, &
      "DEC INTEGER*8 and RECORD /sub_t/ emitted")
  end subroutine test_fixed_form_73_80_and_dec_structure

  !> @brief Test inserting UDT into module (with/without CONTAINS), empty files, and error branches.
  !> @param[inout] st Test harness state.
  subroutine test_missing_udt_insertion_and_errors(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_options_t) :: opts
    type(cdd_buffer_t) :: p1
    type(cdd_error_t) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_2023
    opts%include_codecs_in_sync = .true._lk
    call str_set(opts%target_udt_name, "Orbit", err)

    ! 1. Module WITH existing CONTAINS & helper (plus unrelated derived type with TBP CONTAINS!)
    call patch_fortran_with_json( &
      "module app_mod" // LF // &
      "  use, intrinsic :: iso_fortran_env, only: int32" // LF // &
      "  implicit none" // LF // &
      "  type :: other_t" // LF // &
      "    integer(int32) :: a" // LF // &
      "  contains" // LF // &
      "    procedure :: p => helper_sub" // LF // &
      "  end type other_t" // LF // &
      "contains" // LF // &
      "  subroutine helper_sub(self)" // LF // &
      "    class(other_t), intent(in) :: self" // LF // &
      "  end subroutine helper_sub" // LF // &
      "end module app_mod" // LF, &
      '{"title":"Orbit","type":"object","properties":{"id":{"type":"integer"}}}', &
      opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "insert missing UDT into module with CONTAINS")
    call th_check(st, index(p1%raw(1:p1%len), "type, public :: orbit_t") > 0 .and. &
                      index(p1%raw(1:p1%len), "helper_sub") > 0 .and. &
                      index(p1%raw(1:p1%len), "orbit_t_to_json") > 0, &
      "orbit_t spliced before module CONTAINS (ignoring TBP contains) and codecs before END MODULE")

    ! 2. Module WITHOUT CONTAINS (with codecs and without codecs) + WITH CONTAINS without codecs
    call patch_fortran_with_json( &
      "module bare_mod" // LF // &
      "  use, intrinsic :: iso_fortran_env, only: int32" // LF // &
      "  implicit none" // LF // &
      "  private" // LF // &
      "end module bare_mod" // LF, &
      '{"title":"Orbit","type":"object","properties":{"id":{"type":"integer"}}}', &
      opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "insert missing UDT + CONTAINS into bare module")
    call th_check(st, index(p1%raw(1:p1%len), "contains") > 0, "contains added")

    opts%include_codecs_in_sync = .false._lk
    call patch_fortran_with_json( &
      "module bare2_mod" // LF // "  implicit none" // LF // "end module bare2_mod" // LF, &
      '{"title":"Orbit","type":"object","properties":{"id":{"type":"integer"}}}', &
      opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "insert missing UDT without codecs into bare module")

    call patch_fortran_with_json( &
      "module wc2_mod" // LF // "contains" // LF // "  subroutine s()" // LF // &
      "  end subroutine s" // LF // "end module wc2_mod" // LF, &
      '{"title":"Orbit","type":"object","properties":{"id":{"type":"integer"}}}', &
      opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "insert missing UDT without codecs before CONTAINS")

    ! 3. Empty/comment-only file in modern mode (with and without codecs) & legacy F77 mode
    call cdd_default_options(opts)
    call patch_fortran_with_json("! File header comment" // LF, '{"id": 1}', opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "insert into comment-only modern file with codecs")
    call th_check(st, index(p1%raw(1:p1%len), "! File header comment") > 0, "header comment kept")

    opts%include_codecs_in_sync = .false._lk
    call patch_fortran_with_json("! Header" // LF, '{"id": 1}', opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "insert into comment-only modern file without codecs")

    opts%standard%id = STD_FORTRAN_77
    call patch_fortran_with_json("      END" // LF, '{"id": 1}', opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "insert missing UDT without codecs into F77 deck")
    opts%include_codecs_in_sync = .true._lk
    call patch_fortran_with_json("      END" // LF, '{"id": 1}', opts, p1, err)
    call th_eq_i32(st, err%code, CDD_OK, "insert missing UDT + codecs into F77 deck")

    ! 4. Error branches: invalid form, invalid JSON lex, invalid schema, invalid Fortran lex
    opts%standard%id = STD_FORTRAN_66
    opts%source_form = FORM_FREE
    call patch_fortran_with_json("", '{"id":1}', opts, p1, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "invalid form rejected")

    call cdd_default_options(opts)
    call patch_fortran_with_json("", "{bad", opts, p1, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_LEX, "bad JSON rejected")

    opts%json_mode = JSON_MODE_SCHEMA
    call patch_fortran_with_json("", "[1,2]", opts, p1, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_SCHEMA, "non-object schema rejected")

    call cdd_default_options(opts)
    call patch_fortran_with_json("@bad_fortran", '{"id":1}', opts, p1, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_LEX, "bad Fortran rejected")
  end subroutine test_missing_udt_insertion_and_errors

end program test_patch_udt_trivia
