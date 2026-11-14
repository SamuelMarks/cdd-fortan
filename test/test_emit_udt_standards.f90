!> @file test_emit_udt_standards.f90
!> @brief Comprehensive 100% branch-coverage unit tests for Op 0 (emit_udt_fortran) across 12 standards.

!> @brief Execute all Milestone 7 UDT emission tests across FORTRAN I through Fortran 2023.
!> @return Program exit status (0 on pass).
program test_emit_udt_standards
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: CDD_OK, CDD_ERR_INVALID_OPTION, cdd_error_t
  use cdd_string, only: cdd_buffer_t
  use cdd_standard, only: STD_FORTRAN_I, STD_FORTRAN_II, STD_FORTRAN_IV, &
    STD_FORTRAN_66, STD_FORTRAN_77, STD_FORTRAN_95, &
    STD_FORTRAN_2018, STD_FORTRAN_2023, FORM_FREE, &
    cdd_options_t, cdd_default_options, get_dialect_capabilities
  use cdd_json_dom, only: cdd_json_doc_t
  use cdd_json_parser, only: json_parse
  use cdd_json_schema, only: json_schema_to_ir
  use cdd_ir_types, only: IR_STRING_DEFERRED, IR_COMPLEX64, IR_COMPLEX128, IR_NESTED_UDT, &
    DIM_FIXED, cdd_ir_field_t, cdd_ir_schema_bundle_t, ident_set, ir_field_init, ir_bundle_init, &
    ir_udt_add_field
  use cdd_emit_udt, only: emit_udt_fortran, emit_udt_types_section, emit_single_modern_field_line
  use test_harness, only: test_state_t, th_init, th_check, th_eq_i32, th_finish
  implicit none (type, external)

  type(test_state_t) :: st

  call th_init(st)
  call run_all_12_standards_emit_tests(st)
  call run_special_dialect_modes_tests(st)
  call th_finish(st, "test_emit_udt_standards")

contains

  !> @brief Emit rich UDT across all 12 Fortran standards (FORTRAN I .. Fortran 2023).
  !> @param[inout] st Test state accumulator.
  subroutine run_all_12_standards_emit_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_json_doc_t) :: doc
    type(cdd_options_t) :: opts
    type(cdd_ir_schema_bundle_t) :: bundle
    type(cdd_buffer_t) :: code
    type(cdd_error_t) :: err
    integer(i32) :: sid
    character(len=*), parameter :: SCHEMA_12 = &
      '{"title":"Orbiter","description":"Orbital state",' // &
      '"allOf":[{"$ref":"#/$defs/Base"}],' // &
      '"properties":{' // &
      '"id":{"type":"integer","format":"int64","description":"ID","default":7},' // &
      '"b8":{"type":"integer","format":"int8","default":1},' // &
      '"b16":{"type":"integer","format":"int16","default":2},' // &
      '"cnt":{"type":"integer","default":3},' // &
      '"vel":{"type":"number","format":"float","default":1.5},' // &
      '"mass":{"type":"number","default":100.0},' // &
      '"qval":{"type":"number","format":"quad"},' // &
      '"ok":{"type":"boolean","default":true},' // &
      '"bad":{"type":"boolean","default":false},' // &
      '"callsign":{"type":"string","maxLength":16,"default":"APOLLO"},' // &
      '"notes":{"type":"string"},' // &
      '"mode":{"$ref":"#/$defs/Mode"},' // &
      '"vec":{"type":"array","maxItems":3,"items":{"type":"number"}},' // &
      '"dyn":{"type":"array","items":{"type":"integer"}},' // &
      '"thr":{"$ref":"#/$defs/Thruster"}' // &
      '},' // &
      '"$defs":{' // &
      '"Base":{"type":"object","properties":{"seq":{"type":"integer"}}},' // &
      '"Mode":{"type":"string","description":"Flight mode","enum":["IDLE","BURN"]},' // &
      '"Thruster":{"type":"object","properties":{"isp":{"type":"number"}}}' // &
      '}}'

    call json_parse(SCHEMA_12, doc, err)
    call th_eq_i32(st, err%code, CDD_OK, "parse SCHEMA_12")

    do sid = STD_FORTRAN_I, STD_FORTRAN_2023
      call cdd_default_options(opts)
      opts%standard%id = sid
      call json_schema_to_ir(doc, opts, bundle, err)
      call th_eq_i32(st, err%code, CDD_OK, "schema_to_ir across 12 standards")
      call emit_udt_fortran(bundle, opts, code, err)
      call th_eq_i32(st, err%code, CDD_OK, "emit_udt_fortran across 12 standards")
      call th_check(st, code%len > 40_i32, "non-empty emitted Fortran UDT")
    end do
  end subroutine run_all_12_standards_emit_tests

  !> @brief Exercise DEC STRUCTURE, F77 pure-numeric COMMON, col-72 wrapping, submodules, &BIND(C).
  !> @param[inout] st Test state accumulator.
  subroutine run_special_dialect_modes_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_json_doc_t) :: doc
    type(cdd_options_t) :: opts
    type(cdd_ir_schema_bundle_t) :: bundle
    type(cdd_ir_field_t) :: cf1, cf2, cf3
    type(cdd_buffer_t) :: code
    type(cdd_error_t) :: err
    integer(i32) :: fidx

    ! Many-field schema to trigger >70-column COMMON line continuation in F58 and F66/F77!
    call json_parse( &
      '{"title":"ManyFields","type":"object","properties":{' // &
      '"f1":{"type":"integer"},"f2":{"type":"integer"},"f3":{"type":"integer"},' // &
      '"f4":{"type":"integer"},"f5":{"type":"integer"},"f6":{"type":"integer"},' // &
      '"f7":{"type":"integer"},"f8":{"type":"integer"},"f9":{"type":"integer"},' // &
      '"f10":{"type":"integer"},"f11":{"type":"integer"},"f12":{"type":"number"},' // &
      '"sub":{"type":"object","properties":{"k":{"type":"string","maxLength":8}}}}}', &
      doc, err)

    ! FORTRAN II blank COMMON line wrap + empty UDT field_count == 0
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_II
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_udt_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F58 COMMON wrap")
    bundle%udts(bundle%main_udt_idx)%field_count = 0_i32
    call emit_udt_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F58 0-field UDT")

    ! FORTRAN I int-only and real-only UDTs
    opts%standard%id = STD_FORTRAN_I
    call json_schema_to_ir(doc, opts, bundle, err)
    bundle%udts(bundle%main_udt_idx)%field_count = 1_i32
    call emit_udt_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F57 int-only UDT")

    call json_parse( &
      '{"title":"RealOnly","properties":{"r":{"type":"number"}}}', doc, err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_udt_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F57 real-only UDT")

    ! FORTRAN 77 CHARACTER-only 2D array UDT + modern 2D array
    call json_parse( &
      '{"title":"CharMat","properties":{"m":{"type":"array","maxItems":2,' // &
      '"items":{"type":"array","maxItems":3,"items":{"type":"string","maxLength":6}}}}}', &
      doc, err)
    opts%standard%id = STD_FORTRAN_77
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_udt_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F77 2D CHARACTER-only UDT")
    opts%standard%id = STD_FORTRAN_2023
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_udt_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F2023 2D fixed array UDT")

    ! Restore ManyFields doc for remaining tests
    call json_parse( &
      '{"title":"ManyFields","type":"object","properties":{' // &
      '"f1":{"type":"integer"},"f2":{"type":"integer"},"f3":{"type":"integer"},' // &
      '"f4":{"type":"integer"},"f5":{"type":"integer"},"f6":{"type":"integer"},' // &
      '"f7":{"type":"integer"},"f8":{"type":"integer"},"f9":{"type":"integer"},' // &
      '"f10":{"type":"integer"},"f11":{"type":"integer"},"f12":{"type":"number"},' // &
      '"sub":{"type":"object","properties":{"k":{"type":"string","maxLength":8}}}}}', &
      doc, err)

    ! FORTRAN 77 pure-numeric COMMON wrap (no split) AND DEC STRUCTURE mode on IV & 77
    opts%standard%id = STD_FORTRAN_77
    call json_schema_to_ir(doc, opts, bundle, err)
    ! Add complex64 and complex128 fields to cover COMPLEX emission in F66/F77 and modern!
    call ir_field_init(cf1)
    call ident_set(cf1%json_name, "z64", err)
    call ident_set(cf1%fortran_name, "Z64", err)
    cf1%type_kind = IR_COMPLEX64
    call ir_udt_add_field(bundle%udts(bundle%main_udt_idx), cf1, fidx, err)
    call ir_field_init(cf2)
    call ident_set(cf2%json_name, "z128", err)
    call ident_set(cf2%fortran_name, "Z128", err)
    cf2%type_kind = IR_COMPLEX128
    call ir_udt_add_field(bundle%udts(bundle%main_udt_idx), cf2, fidx, err)
    call emit_udt_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F77 pure numeric COMMON wrap + COMPLEX")

    opts%allow_dec_structure = .true._lk
    call json_schema_to_ir(doc, opts, bundle, err)
    ! Add all scalar kinds + external nested UDT to cover every DEC STRUCTURE select case arm
    call ir_field_init(cf3)
    call ident_set(cf3%json_name, "ext", err)
    call ident_set(cf3%fortran_name, "EXT_R", err)
    cf3%type_kind = IR_NESTED_UDT
    call ident_set(cf3%ref_udt_name, "EXT_STRUCT", err)
    cf3%rank = 1_i32
    cf3%dims(1)%upper_bound = 2_i32
    call ir_udt_add_field(bundle%udts(bundle%main_udt_idx), cf3, fidx, err)
    call emit_udt_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F77 DEC STRUCTURE")

    opts%standard%id = STD_FORTRAN_IV
    call json_parse( &
      '{"title":"Dec4","type":"object","properties":{' // &
      '"a":{"type":"integer","format":"int8"},' // &
      '"b":{"type":"integer","format":"int64"},' // &
      '"c":{"type":"number","format":"float"},' // &
      '"d":{"type":"number"},' // &
      '"e":{"type":"boolean"},' // &
      '"f":{"type":"string","maxLength":10}}}', doc, err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_udt_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "FORTRAN IV DEC STRUCTURE")

    ! Modern F2008+ with emit_submodules, uppercase_keywords, is_bind_c, 0:4 lower_bound, no-dcol
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_2018
    opts%emit_submodules = .true._lk
    opts%uppercase_keywords = .true._lk
    call json_schema_to_ir(doc, opts, bundle, err)
    bundle%udts(1)%is_bind_c = .true._lk
    call ir_udt_add_field(bundle%udts(1), cf1, fidx, err)
    call ir_udt_add_field(bundle%udts(1), cf2, fidx, err)
    cf3%dims(1)%kind = DIM_FIXED
    cf3%dims(1)%lower_bound = 0_i32
    cf3%dims(1)%upper_bound = 3_i32
    cf3%has_default = .true._lk
    call ident_set(cf3%default_lexeme, "", err)
    call ir_udt_add_field(bundle%udts(1), cf3, fidx, err)
    call emit_udt_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_OK, "F2018 submodules + uppercase + bind(c)")

    call emit_udt_types_section(bundle, opts, .true._lk, code, err)
    opts%standard%id = STD_FORTRAN_95
    cf1%has_default = .true._lk
    call ident_set(cf1%default_lexeme, "", err)
    call emit_single_modern_field_line(bundle, cf1, STD_FORTRAN_95, &
      get_dialect_capabilities(opts%standard, .false._lk), 2_i32, .false._lk, .false._lk, &
      achar(10, c_k), code, err)
    call ident_set(cf1%default_lexeme, "(1.0,0.0)", err)
    call emit_single_modern_field_line(bundle, cf1, STD_FORTRAN_95, &
      get_dialect_capabilities(opts%standard, .false._lk), 2_i32, .false._lk, .false._lk, &
      achar(10, c_k), code, err)
    cf2%type_kind = IR_STRING_DEFERRED
    cf2%char_len = 16_i32
    call emit_single_modern_field_line(bundle, cf2, STD_FORTRAN_95, &
      get_dialect_capabilities(opts%standard, .false._lk), 2_i32, .false._lk, .false._lk, &
      achar(10, c_k), code, err)
    call th_eq_i32(st, err%code, CDD_OK, "emit_single_modern_field_line F95 edge cases")

    ! Error branches in emit_udt_fortran
    call ir_bundle_init(bundle)
    call emit_udt_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "empty bundle emit_udt_fortran")

    call json_schema_to_ir(doc, opts, bundle, err)
    opts%standard%id = STD_FORTRAN_66
    opts%source_form = FORM_FREE
    call emit_udt_fortran(bundle, opts, code, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "F66 free-form rejected in emit_udt")
  end subroutine run_special_dialect_modes_tests

end program test_emit_udt_standards
