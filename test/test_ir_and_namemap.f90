!> @file test_ir_and_namemap.f90
!> @brief Comprehensive 100% branch-coverage unit tests for cdd_ir_types and cdd_ir_namemap.

!> @brief Execute all Milestone 4.1 and 4.2 IR and namemap unit tests.
!> @return Program exit status (0 on pass).
program test_ir_and_namemap
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: CDD_OK, CDD_ERR_BUFFER_OVERFLOW, CDD_ERR_INVALID_OPTION, &
    CDD_ERR_IDENTIFIER_COLLISION, cdd_error_t
  use cdd_string, only: cdd_buffer_t
  use cdd_ir_types, only: CDD_IDENT_MAX, CDD_MAX_FIELDS, IR_INT8, IR_INT16, IR_INT32, IR_INT64, &
    IR_REAL32, IR_REAL64, IR_REAL128, IR_COMPLEX64, IR_COMPLEX128, IR_BOOL, &
    IR_STRING_FIXED, IR_STRING_DEFERRED, IR_ENUM, IR_NESTED_UDT, DIM_FIXED, DIM_DEFERRED, &
    DIM_POINTER, DIM_ASSUMED, cdd_ident_t, cdd_ir_dim_t, cdd_ir_field_t, cdd_ir_enum_t, &
    cdd_ir_udt_t, cdd_ir_schema_bundle_t, ident_clear, ident_set, ident_eq, ir_dim_init, &
    ir_field_init, ir_enum_init, ir_udt_init, ir_bundle_init, ir_bundle_add_udt, &
    ir_bundle_find_udt, ir_udt_add_field, ir_udt_find_field, ir_bundle_add_enum, &
    ir_bundle_find_enum, ir_type_to_tag, ir_tag_to_type
  use cdd_ir_namemap, only: sanitize_modern_ident, generate_legacy_ident, &
    assign_udt_field_names, is_reserved_fortran_word, is_implicit_int_letter, &
    format_cdd_field_comment, parse_cdd_field_comment
  use test_harness, only: test_state_t, th_init, th_check, th_eq_i32, th_eq_str, &
    th_touch_vtable, th_finish
  implicit none (type, external)

  type(test_state_t) :: st

  call th_init(st)
  call run_ir_types_tests(st)
  call run_namemap_tests(st)
  call run_cdd_comment_tests(st)
  call th_finish(st, "test_ir_and_namemap")

contains

  !> @brief Exercise all IR structs, arena bounds, lookup paths, and type tag conversions.
  !> @param[inout] st Test state accumulator.
  subroutine run_ir_types_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_ident_t) :: id
    type(cdd_ir_dim_t) :: d
    type(cdd_ir_field_t) :: f
    type(cdd_ir_enum_t) :: e
    type(cdd_ir_udt_t) :: u
    type(cdd_ir_schema_bundle_t) :: b
    type(cdd_error_t) :: err
    character(len=32, kind=c_k) :: tag
    character(len=3, kind=c_k) :: tiny_tag
    character(len=128, kind=c_k) :: over_id
    integer(i32) :: tlen, idx, parsed_t

    call th_touch_vtable(id)
    call th_touch_vtable(d)
    call th_touch_vtable(f)
    call th_touch_vtable(e)
    call th_touch_vtable(u)
    call th_touch_vtable(b)

    call th_eq_i32(st, DIM_FIXED + DIM_DEFERRED + DIM_POINTER + DIM_ASSUMED, 10_i32, "dim kinds")

    call ident_clear(id)
    call ident_set(id, "alpha", err)
    call th_check(st, ident_eq(id, "alpha"), "ident_eq alpha")
    over_id = repeat("x", CDD_IDENT_MAX + 5_i32)
    call ident_set(id, over_id, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "ident_set overflow")

    call ir_dim_init(d)
    call ir_field_init(f)
    call ir_enum_init(e)
    call ir_udt_init(u)
    call ir_bundle_init(b)

    ! UDT add/find/overflow/invalid-name tests
    b%max_udts = 2_i32
    call ir_bundle_add_udt(b, over_id, idx, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "udt overlong name")
    call ir_bundle_add_udt(b, "Orbiter", idx, err)
    call ident_set(b%udts(idx)%fortran_name, "orbiter_t", err)
    call ir_bundle_add_udt(b, "Thruster", idx, err)
    call ir_bundle_add_udt(b, "Third", idx, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "udt arena full")

    call th_eq_i32(st, ir_bundle_find_udt(b, "Orbiter"), 1_i32, "find udt by json")
    call th_eq_i32(st, ir_bundle_find_udt(b, "ORBITER_T"), 1_i32, "find udt by fortran")
    call th_eq_i32(st, ir_bundle_find_udt(b, "Missing"), 0_i32, "find udt missing")

    ! Field add/find/overflow
    u%max_fields = 1_i32
    call ir_field_init(f)
    call ident_set(f%json_name, "massKg", err)
    call ir_udt_add_field(u, f, idx, err)
    call th_eq_i32(st, ir_udt_find_field(u, "other"), 0_i32, "find field empty f_name miss")
    call ident_set(u%fields(1)%fortran_name, "mass_kg", err)
    call th_eq_i32(st, ir_udt_find_field(u, "massKg"), 1_i32, "find field json")
    call th_eq_i32(st, ir_udt_find_field(u, "MASS_KG"), 1_i32, "find field fortran")
    call th_eq_i32(st, ir_udt_find_field(u, "absent"), 0_i32, "find field absent")
    call ir_udt_add_field(u, f, idx, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "field arena full")

    ! Enum add/find/overflow
    b%max_enums = 2_i32
    call ir_bundle_add_enum(b, over_id, idx, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "enum overlong name")
    call ir_bundle_add_enum(b, "Mode", idx, err)
    call th_eq_i32(st, ir_bundle_find_enum(b, "Other"), 0_i32, "find enum empty f_name miss")
    call ident_set(b%enums(idx)%fortran_name, "mode_e", err)
    call ir_bundle_add_enum(b, "State", idx, err)
    call ir_bundle_add_enum(b, "Extra", idx, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "enum arena full")
    call th_eq_i32(st, ir_bundle_find_enum(b, "Mode"), 1_i32, "find enum json")
    call th_eq_i32(st, ir_bundle_find_enum(b, "MODE_E"), 1_i32, "find enum fortran")
    call th_eq_i32(st, ir_bundle_find_enum(b, "None"), 0_i32, "find enum missing")

    ! Type tag round-trip across all 14 IR types + tiny buffer clamp + invalid tag
    do idx = IR_INT8, IR_NESTED_UDT
      call ir_type_to_tag(idx, tag, tlen)
      call ir_tag_to_type(tag(1:tlen), parsed_t, err)
      call th_eq_i32(st, err%code, CDD_OK, "tag roundtrip ok")
      call th_eq_i32(st, parsed_t, idx, "tag roundtrip match")
    end do
    call ir_type_to_tag(IR_STRING_DEFERRED, tiny_tag, tlen)
    call th_eq_i32(st, tlen, 3_i32, "tag clamped")
    call ir_tag_to_type("bogus_type", parsed_t, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "unknown tag error")
  end subroutine run_ir_types_tests

  !> @brief Exercise modern sanitizer, 6-char legacy I-N namer, collisions, and UDT field naming.
  !> @param[inout] st Test state accumulator.
  subroutine run_namemap_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_ident_t) :: id, used(64), tiny_used(1)
    type(cdd_ir_field_t) :: fields(CDD_MAX_FIELDS)
    type(cdd_error_t) :: err
    integer(i32) :: ucnt, k

    call sanitize_modern_ident("x", 2_i32, .false._lk, id, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "max_len < 4")
    call sanitize_modern_ident("x", CDD_IDENT_MAX + 1_i32, .false._lk, id, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "max_len > max")

    call sanitize_modern_ident("  ---$$$---  ", 31_i32, .false._lk, id, err)
    call th_eq_str(st, id%raw(1:id%len), "field", "empty symbols lower")
    call sanitize_modern_ident("  ---$$$---  ", 31_i32, .true._lk, id, err)
    call th_eq_str(st, id%raw(1:id%len), "FIELD", "empty symbols upper")

    call sanitize_modern_ident("99bottles-of--beer-", 10_i32, .false._lk, id, err)
    call th_eq_str(st, id%raw(1:id%len), "f_99bottle", "leading digit lower clamped")
    call sanitize_modern_ident("9ab", 10_i32, .true._lk, id, err)
    call th_eq_str(st, id%raw(1:id%len), "F_9AB", "leading digit upper")

    call sanitize_modern_ident("type", 31_i32, .false._lk, id, err)
    call th_eq_str(st, id%raw(1:id%len), "type_v", "reserved lower")
    call sanitize_modern_ident("INTEGER", 7_i32, .true._lk, id, err)
    call th_eq_str(st, id%raw(1:id%len), "INTEG_V", "reserved upper clamped")

    call th_check(st, is_implicit_int_letter("i") .and. is_implicit_int_letter("N") .and. &
                  .not. is_implicit_int_letter("A") .and. .not. is_implicit_int_letter("O"), &
                  "implicit I-N check")
    call th_check(st, is_reserved_fortran_word("call") .and. &
                  .not. is_reserved_fortran_word("velocity"), "reserved check")

    ! Legacy 6-char implicit rules & multi-tier base-36 collision tests
    ucnt = 0_i32
    call generate_legacy_ident("count_value", IR_INT32, .true._lk, 100_i32, 64_i32, &
      used, ucnt, id, err)
    call th_eq_str(st, id%raw(1:id%len), "ICOUNT", "F57 int prefix I")

    call generate_legacy_ident("mass_kg", IR_REAL64, .true._lk, 100_i32, 64_i32, &
      used, ucnt, id, err)
    call th_eq_str(st, id%raw(1:id%len), "XMASSK", "F57 real M->XMASSK")

    call generate_legacy_ident("mass_cplx", IR_COMPLEX64, .true._lk, 100_i32, 64_i32, &
      used, ucnt, id, err)
    call th_eq_str(st, id%raw(1:id%len), "XMASSC", "F57 complex64 M->XMASSC")
    call generate_legacy_ident("mass_z", IR_COMPLEX128, .true._lk, 100_i32, 64_i32, &
      used, ucnt, id, err)
    call generate_legacy_ident("mass_q", IR_REAL128, .true._lk, 100_i32, 64_i32, &
      used, ucnt, id, err)
    call generate_legacy_ident("mass_f", IR_REAL32, .true._lk, 100_i32, 64_i32, &
      used, ucnt, id, err)
    call generate_legacy_ident("byte_a", IR_INT8, .true._lk, 100_i32, 64_i32, &
      used, ucnt, id, err)
    call generate_legacy_ident("short_a", IR_INT16, .true._lk, 100_i32, 64_i32, &
      used, ucnt, id, err)
    call generate_legacy_ident("long_a", IR_INT64, .true._lk, 100_i32, 64_i32, &
      used, ucnt, id, err)
    call generate_legacy_ident("flag_a", IR_BOOL, .true._lk, 100_i32, 64_i32, &
      used, ucnt, id, err)
    call generate_legacy_ident("str_a", IR_STRING_FIXED, .true._lk, 100_i32, 64_i32, &
      used, ucnt, id, err)
    call generate_legacy_ident("enum_a", IR_ENUM, .true._lk, 100_i32, 64_i32, &
      used, ucnt, id, err)
    call generate_legacy_ident("item_u", IR_NESTED_UDT, .true._lk, 100_i32, 64_i32, &
      used, ucnt, id, err)

    call generate_legacy_ident("$$$", IR_INT32, .true._lk, 100_i32, 64_i32, used, ucnt, id, err)
    call th_eq_str(st, id%raw(1:id%len), "IFLD", "F57 empty int -> IFLD")
    call generate_legacy_ident("999", IR_REAL32, .true._lk, 100_i32, 64_i32, used, ucnt, id, err)
    call th_eq_str(st, id%raw(1:id%len), "XFLD", "F57 digits real -> XFLD")

    ! Exercise 40 collisions on 6-char AND 2-char names to hit clen>5/4 true and false!
    ucnt = 0_i32
    do k = 1_i32, 40_i32
      call generate_legacy_ident("common", IR_REAL64, .false._lk, 100_i32, 64_i32, &
        used, ucnt, id, err)
      call th_eq_i32(st, err%code, CDD_OK, "legacy 40-way 6-char collision")
    end do
    ucnt = 0_i32
    do k = 1_i32, 40_i32
      call generate_legacy_ident("do", IR_REAL64, .false._lk, 100_i32, 64_i32, &
        used, ucnt, id, err)
      call th_eq_i32(st, err%code, CDD_OK, "legacy 40-way 2-char collision")
    end do

    ! Exhaustion & full table errors
    call generate_legacy_ident("common", IR_REAL64, .false._lk, 1_i32, 64_i32, &
      used, ucnt, id, err)
    call th_eq_i32(st, err%code, CDD_ERR_IDENTIFIER_COLLISION, "exhausted attempts")

    ucnt = 1_i32
    call ident_set(tiny_used(1), "OTHER", err)
    call generate_legacy_ident("alpha", IR_REAL64, .false._lk, 5_i32, 1_i32, &
      tiny_used, ucnt, id, err)
    call th_eq_i32(st, err%code, CDD_ERR_IDENTIFIER_COLLISION, "used table full")

    ! assign_udt_field_names in both modern (with collision & max_len clamp) and legacy modes
    do k = 1_i32, 3_i32
      call ir_field_init(fields(k))
    end do
    call ident_set(fields(1)%json_name, "alpha-val", err)
    call ident_set(fields(2)%json_name, "alpha_val", err)
    call ident_set(fields(3)%json_name, "alpha__val", err)
    call assign_udt_field_names(9_i32, .false._lk, .false._lk, CDD_MAX_FIELDS, 3_i32, fields, err)
    call th_eq_i32(st, err%code, CDD_OK, "assign modern collisions")
    call th_eq_str(st, fields(2)%fortran_name%raw(1:fields(2)%fortran_name%len), &
      "alpha_v_2", "modern collision _2 clamped")
    call th_eq_str(st, fields(3)%fortran_name%raw(1:fields(3)%fortran_name%len), &
      "alpha_v_3", "modern collision _3 clamped")

    call assign_udt_field_names(31_i32, .false._lk, .false._lk, CDD_MAX_FIELDS, 3_i32, fields, err)
    call th_eq_str(st, fields(2)%fortran_name%raw(1:fields(2)%fortran_name%len), &
      "alpha_val_2", "modern collision _2 unclamped")

    call assign_udt_field_names(6_i32, .true._lk, .true._lk, CDD_MAX_FIELDS, 3_i32, fields, err)
    call th_eq_i32(st, err%code, CDD_OK, "assign legacy 6-char")

    ! Trigger error paths inside assign_udt_field_names
    call assign_udt_field_names(65_i32, .false._lk, .false._lk, CDD_MAX_FIELDS, 1_i32, fields, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "assign invalid modern max")
  end subroutine run_namemap_tests

  !> @brief Exercise format_cdd_field_comment and all valid/invalid parse_cdd_field_comment branches.
  !> @param[inout] st Test state accumulator.
  subroutine run_cdd_comment_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_ir_field_t) :: fld
    type(cdd_buffer_t) :: cbuf
    type(cdd_ident_t) :: fn, jn
    type(cdd_error_t) :: err
    integer(i32) :: tk, clen
    logical(lk) :: matched
    character(len=128, kind=c_k) :: over_tok

    call ir_field_init(fld)
    call ident_set(fld%fortran_name, "ISPEED", err)
    call ident_set(fld%json_name, "maxSpeedMps", err)
    fld%type_kind = IR_STRING_FIXED
    fld%char_len = 24_i32
    call format_cdd_field_comment("C", fld, cbuf, err)
    call th_eq_i32(st, err%code, CDD_OK, "format_cdd_field_comment")

    call parse_cdd_field_comment(cbuf%raw(1:cbuf%len), fn, jn, tk, clen, matched)
    call th_check(st, matched, "parsed @cdd-field comment")
    call th_eq_str(st, fn%raw(1:fn%len), "ISPEED", "parsed fortran name")
    call th_eq_str(st, jn%raw(1:jn%len), "maxSpeedMps", "parsed json name")
    call th_eq_i32(st, tk, IR_STRING_FIXED, "parsed type_kind")
    call th_eq_i32(st, clen, 24_i32, "parsed char_len")

    ! Parse without len= attribute, with empty len=, and with non-numeric len=
    call parse_cdd_field_comment('! @cdd-field: X json="x" type=bool', fn, jn, tk, clen, matched)
    call th_check(st, matched .and. tk == IR_BOOL, "parsed without len=")
    call parse_cdd_field_comment('! @cdd-field: X json="x" type=bool len=   ', &
      fn, jn, tk, clen, matched)
    call th_check(st, matched, "parsed blank len=")
    call parse_cdd_field_comment('! @cdd-field: X json="x" type=bool len=bad', &
      fn, jn, tk, clen, matched)
    call th_check(st, matched .and. clen == 0_i32, "parsed bad len=")

    ! Negative/malformed comment lines
    call parse_cdd_field_comment("C ordinary comment", fn, jn, tk, clen, matched)
    call th_check(st, .not. matched, "no marker")
    call parse_cdd_field_comment("", fn, jn, tk, clen, matched)
    call th_check(st, .not. matched, "empty line")
    call parse_cdd_field_comment("C @cdd-field:   ", fn, jn, tk, clen, matched)
    call th_check(st, .not. matched, "marker only")
    over_tok = repeat("Z", 70)
    call parse_cdd_field_comment("C @cdd-field: " // over_tok // ' json="a" type=bool', &
      fn, jn, tk, clen, matched)
    call th_check(st, .not. matched, "overlong f_name")
    call parse_cdd_field_comment("C @cdd-field: A", fn, jn, tk, clen, matched)
    call th_check(st, .not. matched, "identifier at EOF without json=")
    call parse_cdd_field_comment("C @cdd-field: A missing_json", fn, jn, tk, clen, matched)
    call th_check(st, .not. matched, "missing json=")
    call parse_cdd_field_comment('C @cdd-field: A json="unterminated', fn, jn, tk, clen, matched)
    call th_check(st, .not. matched, "unterminated json quote")
    call parse_cdd_field_comment('C @cdd-field: A json="' // over_tok // '" type=bool', &
      fn, jn, tk, clen, matched)
    call th_check(st, .not. matched, "overlong j_name")
    call parse_cdd_field_comment('C @cdd-field: A json="x" no_type', fn, jn, tk, clen, matched)
    call th_check(st, .not. matched, "missing type=")
    call parse_cdd_field_comment('C @cdd-field: A json="x" type=not_a_type', &
      fn, jn, tk, clen, matched)
    call th_check(st, .not. matched, "bad type=")
  end subroutine run_cdd_comment_tests

end program test_ir_and_namemap
