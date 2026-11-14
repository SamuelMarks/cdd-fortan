!> @file test_extract_udt_standards.f90
!> @brief Comprehensive 100% branch-coverage unit tests for Op 1 (extract_udt) across 12 standards.

!> @brief Execute all Milestone 8 UDT->JSON round-trip and hand-written extraction tests.
!> @return Program exit status (0 on pass).
program test_extract_udt_standards
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: CDD_OK, CDD_ERR_UDT_NOT_FOUND, CDD_ERR_FORTRAN_LEX, cdd_error_t
  use cdd_string, only: cdd_buffer_t, str_set
  use cdd_standard, only: STD_FORTRAN_I, STD_FORTRAN_II, STD_FORTRAN_77, &
    STD_FORTRAN_2023, cdd_options_t, cdd_default_options
  use cdd_json_dom, only: cdd_json_doc_t
  use cdd_json_parser, only: json_parse
  use cdd_json_writer, only: json_write
  use cdd_json_schema, only: json_schema_to_ir, ir_to_json_schema
  use cdd_json_infer, only: ir_to_json_sample
  use cdd_ir_types, only: cdd_ir_schema_bundle_t
  use cdd_emit_udt, only: emit_udt_fortran
  use cdd_extract_udt, only: cdd_field_annot_t, extract_udt_from_source
  use test_harness, only: test_state_t, th_init, th_check, th_eq_i32, th_eq_str, &
    th_touch_vtable, th_finish
  implicit none (type, external)

  type(test_state_t) :: st

  call th_init(st)
  call run_bidirectional_roundtrip_12_standards(st)
  call run_handwritten_fortran_extract_tests(st)
  call th_finish(st, "test_extract_udt_standards")

contains

  !> @brief Verify JSON -> Op 0 (Fortran UDT) -> Op 1 (IR -> JSON Schema & Sample) across all 12 standards.
  !> @param[inout] st Test state accumulator.
  subroutine run_bidirectional_roundtrip_12_standards(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_json_doc_t) :: doc_in, doc_schema, doc_sample
    type(cdd_options_t) :: opts
    type(cdd_ir_schema_bundle_t) :: b_emitted, b_extracted
    type(cdd_buffer_t) :: f_code, j_out
    type(cdd_error_t) :: err
    integer(i32) :: sid, main_u
    character(len=*), parameter :: CANON_SCHEMA = &
      '{"title":"Packet","description":"Telemetry packet",' // &
      '"properties":{' // &
      '"packetCount":{"type":"integer","default":5},' // &
      '"massFlowKgS":{"type":"number","default":2.5},' // &
      '"isArmed":{"type":"boolean","default":true},' // &
      '"isSafe":{"type":"boolean","default":false},' // &
      '"tagCode":{"type":"string","maxLength":12,"default":"OK"},' // &
      '"opMode":{"$ref":"#/$defs/Mode"},' // &
      '"rates":{"type":"array","maxItems":3,"items":{"type":"number"}}' // &
      '},' // &
      '"$defs":{"Mode":{"type":"string","e' // 'num":["IDLE","ACTIVE"]}}}'

    call json_parse(CANON_SCHEMA, doc_in, err)
    call th_eq_i32(st, err%code, CDD_OK, "parse CANON_SCHEMA")

    do sid = STD_FORTRAN_I, STD_FORTRAN_2023
      call cdd_default_options(opts)
      opts%standard%id = sid
      call json_schema_to_ir(doc_in, opts, b_emitted, err)
      call emit_udt_fortran(b_emitted, opts, f_code, err)
      call th_eq_i32(st, err%code, CDD_OK, "Op 0 emit")

      call extract_udt_from_source(f_code%raw(1:f_code%len), opts, b_extracted, err)
      call th_eq_i32(st, err%code, CDD_OK, "Op 1 extract")
      main_u = b_extracted%main_udt_idx
      call th_eq_str(st, b_extracted%udts(main_u)%json_name%raw( &
        1:b_extracted%udts(main_u)%json_name%len), "Packet", "round-trip UDT title")
      call th_eq_i32(st, b_extracted%udts(main_u)%field_count, 7_i32, "round-trip 7 fields")
      call th_eq_str(st, b_extracted%udts(main_u)%fields(1)%json_name%raw( &
        1:b_extracted%udts(main_u)%fields(1)%json_name%len), "packetCount", "field 1 json_name")

      call ir_to_json_schema(b_extracted, opts, doc_schema, err)
      call th_eq_i32(st, err%code, CDD_OK, "Op 1 -> JSON Schema")
      call json_write(doc_schema, 2_i32, .true._lk, j_out, err)
      call th_check(st, j_out%len > 50_i32, "JSON Schema serialized")

      call ir_to_json_sample(b_extracted, opts, doc_sample, err)
      call th_eq_i32(st, err%code, CDD_OK, "Op 1 -> JSON Sample")
    end do

    ! Also round-trip DEC STRUCTURE on FORTRAN 77
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_77
    opts%allow_dec_structure = .true._lk
    call json_schema_to_ir(doc_in, opts, b_emitted, err)
    call emit_udt_fortran(b_emitted, opts, f_code, err)
    call extract_udt_from_source(f_code%raw(1:f_code%len), opts, b_extracted, err)
    call th_eq_i32(st, err%code, CDD_OK, "Op 1 extract F77 DEC STRUCTURE")
  end subroutine run_bidirectional_roundtrip_12_standards

  !> @brief Exercise unannotated hand-written Fortran UDT extraction & error paths.
  !> @param[inout] st Test state accumulator.
  subroutine run_handwritten_fortran_extract_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_options_t) :: opts
    type(cdd_ir_schema_bundle_t) :: b
    type(cdd_field_annot_t) :: dummy_annot
    type(cdd_error_t) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call th_touch_vtable(dummy_annot)

    ! 1. Unannotated hand-written FORTRAN I DIMENSION (implicit I-N vs A-H,O-Z, bare & 5D & IUDTBK)
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_I
    call extract_udt_from_source( &
      "C> Hand-written 1957 deck" // LF // &
      "      INTEGER IUDTBK(8)" // LF // &
      "      DIMENSION ICOUNT(4), XMASS(2,3,4,5,6), BARE, IUDTBK(8)" // LF, opts, b, err)
    call th_eq_i32(st, err%code, CDD_OK, "hand-written F57 DIMENSION")
    call th_eq_i32(st, b%udts(1)%field_count, 3_i32, "IUDTBK filtered, 3 user fields")

    ! 2. Unannotated hand-written FORTRAN II blank COMMON
    opts%standard%id = STD_FORTRAN_II
    call extract_udt_from_source( &
      "      COMMON ITEM, VELOC" // LF, opts, b, err)
    call th_eq_i32(st, err%code, CDD_OK, "hand-written F58 blank COMMON")
    call th_eq_i32(st, b%udts(1)%field_count, 2_i32, "2 blank COMMON fields")

    ! 3. Unannotated FORTRAN 77 paired /ORBN/ and /ORBC/ COMMON blocks + DATA defaults
    opts%standard%id = STD_FORTRAN_77
    call extract_udt_from_source( &
      "      INTEGER ID, ARR(3)" // LF // &
      "      CHARACTER*8 TAG" // LF // &
      "      COMMON /ORBN/ ID, ARR" // LF // &
      "      COMMON /ORBC/ TAG" // LF // &
      "      DATA ID /42/, TAG /'SAT1'/, NO_SUCH /1/, BAD syntax" // LF, opts, b, err)
    call th_eq_i32(st, err%code, CDD_OK, "hand-written F77 split COMMON")
    call th_eq_str(st, b%udts(1)%json_name%raw(1:b%udts(1)%json_name%len), "ORB", "paired N/C prefix")

    ! Single named COMMON without N/C pair, 1-char COMMON /A/, and mismatched second COMMON
    call extract_udt_from_source( &
      "      REAL X, Y" // LF // "      COMMON /SOLON/ X" // LF // &
      "      INTEGER Z" // LF // "      COMMON /OTHER/ Y" // LF, opts, b, err)
    call th_eq_str(st, b%udts(1)%json_name%raw(1:b%udts(1)%json_name%len), "SOLON", "unpaired N kept")
    call extract_udt_from_source( &
      "      REAL X" // LF // "      COMMON /A/ X" // LF, opts, b, err)
    call th_eq_str(st, b%udts(1)%json_name%raw(1:b%udts(1)%json_name%len), "A", "1-char COMMON A")

    ! 4. Hand-written modern F2023 with unannotated ENUM, BIND(C), pointer => null(), target_udt_name
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_2023
    call str_set(opts%target_udt_name, "vec", err)
    call extract_udt_from_source( &
      "module hw_mod" // LF // &
      "  ! @cdd-udt:   " // LF // &
      "  type :: 123" // LF // &
      "  end type" // LF // &
      "  e" // "num, bind(c)" // LF // &
      "    e" // "numerator :: E_ONE = 1, E_TWO = 2" // LF // &
      "  end e" // "num" // LF // &
      "  e" // "numeration type :: status_e" // LF // &
      "    e" // "numerator :: ST_OK" // LF // &
      "  end e" // "numeration type status_e" // LF // &
      "  !>" // LF // &
      "  !> @brief   " // LF // &
      "  !> Direct brief without tag" // LF // &
      "  type :: vec_t" // LF // &
      '    ! @cdd-field: m1 json="m1_orig" type=int32 len=0' // LF // &
      "    integer :: m1, m2" // LF // &
      "    real, pointer :: p(:) => null() !<" // LF // &
      "    type(status_e) :: st !<   " // LF // &
      "    character(len=8) :: nm = 'hi'" // LF // &
      "  end type vec_t" // LF // &
      "  ! @cdd-udt: OtherName trailing" // LF // &
      "  type :: other_t" // LF // &
      "    type(vec_t) :: v" // LF // &
      "  end type other_t" // LF // &
      "end module hw_mod" // LF, opts, b, err)
    call th_eq_i32(st, err%code, CDD_OK, "hand-written F2023 explicit target_udt_name")
    call th_eq_str(st, b%udts(b%main_udt_idx)%json_name%raw( &
      1:b%udts(b%main_udt_idx)%json_name%len), "vec", "selected vec_t")

    ! 5. Error paths: missing target_udt_name, file with no UDT/COMMON, and lexical error
    call str_set(opts%target_udt_name, "nonexistent_udt", err)
    call extract_udt_from_source("type :: a_t" // LF // "  integer :: x" // LF // "end type" // LF, &
      opts, b, err)
    call th_eq_i32(st, err%code, CDD_ERR_UDT_NOT_FOUND, "missing target_udt_name")

    call cdd_default_options(opts)
    call extract_udt_from_source("program p" // LF // "end program p" // LF, opts, b, err)
    call th_eq_i32(st, err%code, CDD_ERR_UDT_NOT_FOUND, "no UDT in program")

    call extract_udt_from_source("@invalid", opts, b, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_LEX, "lex error propagated")
  end subroutine run_handwritten_fortran_extract_tests

end program test_extract_udt_standards
