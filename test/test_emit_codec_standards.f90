!> @file test_emit_codec_standards.f90
!> @brief Comprehensive 100% branch-coverage unit tests for Op 2 (`cdd_emit_codec`) across 12 standards.

!> @brief Execute all Milestone 9 unit and branch-coverage tests for `cdd_emit_codec`.
!> @return Program exit status (0 on pass).
program test_emit_codec_standards
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: CDD_OK, CDD_ERR_INVALID_OPTION, cdd_error_t
  use cdd_string, only: cdd_buffer_t
  use cdd_standard, only: STD_FORTRAN_I, STD_FORTRAN_II, &
    STD_FORTRAN_66, STD_FORTRAN_77, STD_FORTRAN_90, STD_FORTRAN_95, STD_FORTRAN_2003, &
    STD_FORTRAN_2008, STD_FORTRAN_2018, STD_FORTRAN_2023, FORM_FREE, &
    cdd_options_t, cdd_default_options
  use cdd_json_dom, only: cdd_json_doc_t
  use cdd_json_parser, only: json_parse
  use cdd_json_schema, only: json_schema_to_ir
  use cdd_ir_types, only: IR_INT8, IR_INT16, IR_INT32, IR_INT64, IR_REAL32, &
    IR_REAL128, IR_COMPLEX64, IR_COMPLEX128, IR_BOOL, IR_STRING_FIXED, IR_STRING_DEFERRED, &
    IR_ENUM, IR_NESTED_UDT, DIM_DEFERRED, cdd_ir_schema_bundle_t, ir_bundle_init, &
    ident_set
  use cdd_emit_codec, only: emit_codec_fortran, codec_routine_names
  use test_harness, only: test_state_t, th_init, th_check, th_eq_i32, th_finish
  implicit none (type, external)

  type(test_state_t) :: st

  call th_init(st)
  call run_all_12_standards_matrix(st)
  call run_special_types_and_modes(st)
  call run_edge_and_error_branches(st)
  call th_finish(st, "test_emit_codec_standards")

contains

  !> @brief Verify Op 2 codec emission across all 12 Fortran standards (FORTRAN I .. Fortran 2023).
  !> @param[inout] st Test harness state.
  subroutine run_all_12_standards_matrix(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_json_doc_t) :: doc
    type(cdd_options_t) :: opts
    type(cdd_ir_schema_bundle_t) :: bundle
    type(cdd_buffer_t) :: code
    type(cdd_error_t) :: err
    integer(i32) :: s
    character(len=*), parameter :: SCHEMA_12 = &
      '{"$schema":"https://json-schema.org/draft/2020-12/schema",' // &
      '"title":"Orbiter","description":"Orbital telemetry",' // &
      '"type":"object","required":["seq","mass"],' // &
      '"properties":{' // &
      '"seq":{"type":"integer","format":"int32"},' // &
      '"mass":{"type":"number","format":"double"},' // &
      '"active":{"type":"boolean"},' // &
      '"callsign":{"type":"string","maxLength":12},' // &
      '"mode":{"type":"string","enum":["IDLE","BURN"]},' // &
      '"rates":{"type":"array","maxItems":3,"items":{"type":"number","format":"float"}}' // &
      '}}'

    call json_parse(SCHEMA_12, doc, err)
    call th_eq_i32(st, err%code, CDD_OK, "parse SCHEMA_12")

    do s = STD_FORTRAN_I, STD_FORTRAN_2023
      call cdd_default_options(opts)
      opts%standard%id = s
      call json_schema_to_ir(doc, opts, bundle, err)
      call th_eq_i32(st, err%code, CDD_OK, "schema_to_ir std")
      call emit_codec_fortran(bundle, opts, code, err)
      call th_eq_i32(st, err%code, CDD_OK, "emit_codec std")
      call th_check(st, code%len > 80_i32, "non-empty codec emitted")
    end do

    ! Uppercase keywords across modern standards & submodule modes (F2008 vs F2018+)
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_2008
    opts%emit_submodules = .true._lk
    opts%uppercase_keywords = .true._lk
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_codec_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F2008 uppercase submodule codec")
    call th_check(st, index(code%raw(1:code%len), "SUBMODULE (") > 0, "F2008 SUBMODULE header")

    opts%standard%id = STD_FORTRAN_2018
    opts%uppercase_keywords = .false._lk
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_codec_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F2018 lowercase submodule codec")
    call th_check(st, index(code%raw(1:code%len), "implicit none (type, external)") > 0, &
      "F2018 submodule implicit none (type, external)")
  end subroutine run_all_12_standards_matrix

  !> @brief Exercise complex/quad/int8/int16/2D-arrays/DEC-structure/inheritance/pointer branches.
  !> @param[inout] st Test harness state.
  subroutine run_special_types_and_modes(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_json_doc_t) :: doc
    type(cdd_options_t) :: opts
    type(cdd_ir_schema_bundle_t) :: bundle
    type(cdd_buffer_t) :: code
    type(cdd_error_t) :: err
    character(len=*), parameter :: OOP_SCHEMA = &
      '{"$defs":{' // &
      '"Base":{"type":"object","properties":{"id":{"type":"integer"}}},' // &
      '"Sibling":{"allOf":[{"$ref":"#/$defs/Base"}],"type":"object","properties":{"s":{"type":"integer"}}},' // &
      '"Sub":{"type":"object","properties":{"v":{"type":"number"}}}' // &
      '},' // &
      '"title":"Child","allOf":[{"$ref":"#/$defs/Base"}],' // &
      '"type":"object","properties":{' // &
      '"sib":{"$ref":"#/$defs/Sibling"},' // &
      '"label":{"type":"string"},' // &
      '"tag":{"type":["string","null"]},' // &
      '"sub":{"$ref":"#/$defs/Sub"},' // &
      '"dyn1":{"type":"array","items":{"type":"integer"}},' // &
      '"dyn2":{"type":"array","items":{"type":"array","items":{"type":"number"}}},' // &
      '"fix2":{"type":"array","maxItems":2,"items":{"type":"array","maxItems":3,"items":{"type":"integer"}}}' // &
      '}}'

    call json_parse(OOP_SCHEMA, doc, err)
    call th_eq_i32(st, err%code, CDD_OK, "parse OOP_SCHEMA")

    ! F90 & F95 (POINTER dynamic arrays, nullable strings, nested UDT)
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_90
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_codec_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F90 pointer & nested codec")

    opts%standard%id = STD_FORTRAN_95
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_codec_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F95 PURE to_json + pointer from_json")

    ! F2003 OOP EXTENDS + TBPs + dynamic 1D & 2D allocatable arrays
    opts%standard%id = STD_FORTRAN_2003
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_codec_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F2003 OOP EXTENDS codec")
    call th_check(st, index(code%raw(1:code%len), "%base_t%to_json") > 0, "parent TBP delegation")

    ! F2003 without TBPs & with bind(c)
    opts%emit_type_bound_procs = .false._lk
    call emit_codec_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F2003 non-TBP codec")

    opts%emit_type_bound_procs = .true._lk
    bundle%udts(1)%is_bind_c = .true._lk
    call emit_codec_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F2003 bind(c) UDT codec")

    ! DEC STRUCTURE mode in FORTRAN 77 (with nested RECORD, 2D array, unresolved ref fallback)
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_77
    opts%allow_dec_structure = .true._lk
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_codec_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F77 DEC STRUCTURE codec")
    call th_check(st, index(code%raw(1:code%len), "RECORD /") > 0, "F77 RECORD dummy")
  end subroutine run_special_types_and_modes

  !> @brief Exercise synthetic IR types (`COMPLEX`, `INT8/16/64`, `REAL128`, 0-field UDT, errors).
  !> @param[inout] st Test harness state.
  subroutine run_edge_and_error_branches(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_ir_schema_bundle_t) :: b
    type(cdd_options_t) :: opts
    type(cdd_buffer_t) :: code
    type(cdd_error_t) :: err
    character(len=64, kind=c_k) :: to_nm, fr_nm
    integer(i32) :: to_len, fr_len, i

    ! Empty bundle error
    call ir_bundle_init(b)
    call cdd_default_options(opts)
    call emit_codec_fortran(b, opts, code, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "empty bundle rejected")

    ! Free-form on F66 rejected
    b%udt_count = 1_i32
    b%main_udt_idx = 1_i32
    call ident_set(b%module_name, "m_mod", err)
    call ident_set(b%udts(1)%fortran_name, "A_", err)
    call ident_set(b%udts(1)%json_name, "A", err)
    opts%standard%id = STD_FORTRAN_66
    opts%source_form = FORM_FREE
    call emit_codec_fortran(b, opts, code, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "F66 FORM_FREE rejected")

    ! 1-char UDT name padding in codec_routine_names ("A_" -> "U2JAXX" / "J2UAXX")
    call codec_routine_names("A_", STD_FORTRAN_II, .false._lk, to_nm, to_len, fr_nm, fr_len)
    call th_check(st, to_nm(1:to_len) == "U2JAXX" .and. fr_nm(1:fr_len) == "J2UAXX", &
      "6-char routine padding with X")

    ! Zero-field UDT across F58, F66, F77 COMMON, and F2008 TBP
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_II
    call emit_codec_fortran(b, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F58 0-field UDT")

    opts%standard%id = STD_FORTRAN_66
    call emit_codec_fortran(b, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F66 0-field UDT")

    opts%standard%id = STD_FORTRAN_77
    call emit_codec_fortran(b, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F77 0-field UDT")

    opts%standard%id = STD_FORTRAN_2008
    call ident_set(b%udts(1)%fortran_name, "empty_t", err)
    call emit_codec_fortran(b, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F2008 0-field extensible UDT")

    ! Rich synthetic IR covering COMPLEX64/128, INT8/16/64, REAL128, unresolved ENUM & UDT,
    ! scalar allocatable, scalar pointer, 2D F77 COMMON array, and >12 fields in F58 (>70 cols)
    b%udts(1)%field_count = 14_i32
    do i = 1_i32, 14_i32
      call ident_set(b%udts(1)%fields(i)%fortran_name, "IFLD" // &
        achar(48_i32 + (i / 10_i32), kind=c_k) // achar(48_i32 + mod(i, 10_i32), kind=c_k), err)
      call ident_set(b%udts(1)%fields(i)%json_name, "f" // &
        achar(48_i32 + (i / 10_i32), kind=c_k) // achar(48_i32 + mod(i, 10_i32), kind=c_k), err)
      b%udts(1)%fields(i)%type_kind = IR_INT32
    end do
    b%udts(1)%fields(1)%type_kind = IR_COMPLEX64
    b%udts(1)%fields(2)%type_kind = IR_COMPLEX128
    b%udts(1)%fields(3)%type_kind = IR_REAL128
    b%udts(1)%fields(4)%type_kind = IR_INT8
    b%udts(1)%fields(5)%type_kind = IR_INT16
    b%udts(1)%fields(6)%type_kind = IR_INT64
    b%udts(1)%fields(7)%type_kind = IR_ENUM
    call ident_set(b%udts(1)%fields(7)%enum_name, "missing_enum_e", err)
    b%udts(1)%fields(8)%type_kind = IR_NESTED_UDT
    call ident_set(b%udts(1)%fields(8)%ref_udt_name, "ext_t", err)
    b%udts(1)%fields(8)%is_allocatable = .true._lk
    b%udts(1)%fields(9)%type_kind = IR_INT32
    b%udts(1)%fields(9)%is_pointer = .true._lk
    b%udts(1)%fields(10)%type_kind = IR_STRING_FIXED
    b%udts(1)%fields(10)%char_len = 8_i32
    b%udts(1)%fields(10)%rank = 1_i32
    b%udts(1)%fields(10)%dims(1)%upper_bound = 2_i32
    b%udts(1)%fields(11)%type_kind = IR_REAL32
    b%udts(1)%fields(11)%rank = 2_i32
    b%udts(1)%fields(11)%dims(1)%upper_bound = 2_i32
    b%udts(1)%fields(11)%dims(2)%upper_bound = 2_i32
    b%udts(1)%fields(12)%type_kind = IR_STRING_DEFERRED
    b%udts(1)%fields(12)%char_len = 8_i32
    b%udts(1)%fields(13)%type_kind = IR_BOOL
    b%udts(1)%fields(14)%type_kind = IR_INT32
    b%udts(1)%fields(14)%rank = 1_i32
    b%udts(1)%fields(14)%dims(1)%kind = DIM_DEFERRED
    b%udts(1)%fields(14)%dims(1)%upper_bound = 2_i32

    ! Emit across F58 (tests >70-col COMMON wrap), F66 (COMPLEX/REAL/DOUBLE), F77 COMMON,
    ! F77 DEC (unresolved nested UDT), and F2008 (COMPLEX, unresolved enum/udt, alloc/ptr)
    opts%standard%id = STD_FORTRAN_II
    call emit_codec_fortran(b, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F58 >70-col COMMON wrap")

    opts%standard%id = STD_FORTRAN_66
    call emit_codec_fortran(b, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F66 COMPLEX/DOUBLE/STR/ARR")

    opts%standard%id = STD_FORTRAN_77
    b%udts(1)%fields(8)%type_kind = IR_INT32
    call emit_codec_fortran(b, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F77 COMMON COMPLEX/2D/split")

    opts%allow_dec_structure = .true._lk
    b%udts(1)%fields(8)%type_kind = IR_NESTED_UDT
    call emit_codec_fortran(b, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F77 DEC unresolved nested UDT")

    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_2008
    call emit_codec_fortran(b, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F2008 synthetic COMPLEX/ptr/alloc/unresolved")

    opts%standard%id = STD_FORTRAN_95
    call emit_codec_fortran(b, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F95 synthetic COMPLEX/unresolved")

    ! Numeric-only F77 COMMON with apostrophe key + external extends_name in F2008
    b%udts(1)%field_count = 1_i32
    b%udts(1)%fields(1)%type_kind = IR_INT32
    call ident_set(b%udts(1)%fields(1)%json_name, "a'b", err)
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_77
    call emit_codec_fortran(b, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F77 numeric-only single COMMON + apostrophe key")

    b%udts(1)%has_extends = .true._lk
    call ident_set(b%udts(1)%extends_name, "ext_parent_t", err)
    opts%standard%id = STD_FORTRAN_2008
    call emit_codec_fortran(b, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F2008 external extends_name")

    ! UDT 1 extends UDT 2 (where UDT 2 appears after UDT 1 in bundle%udts)
    b%udt_count = 2_i32
    call ident_set(b%udts(2)%fortran_name, "ext_parent_t", err)
    call ident_set(b%udts(2)%json_name, "ExtParent", err)
    b%udts(2)%field_count = 0_i32
    call emit_codec_fortran(b, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F2008 forward-referenced parent UDT")
  end subroutine run_edge_and_error_branches

end program test_emit_codec_standards
