!> @file cdd_emit_codec.f90
!> @brief Operation 2: Emit zero-dependency JSON <-> Fortran UDT codecs across FORTRAN I..2023.

!> @brief Self-contained JSON serializer and deserializer code generator for all 12 standards.
module cdd_emit_codec
  use cdd_kinds, only: i32, i64, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set_simple, CDD_ERR_INVALID_OPTION
  use cdd_string, only: cdd_buffer_t, buf_init, buf_append_char, buf_append_str, buf_append_i64, &
    to_upper_buf, is_ascii_alnum
  use cdd_standard, only: STD_FORTRAN_I, STD_FORTRAN_II, STD_FORTRAN_III, STD_FORTRAN_IV, &
    STD_FORTRAN_66, STD_FORTRAN_77, STD_FORTRAN_95, STD_FORTRAN_2003, STD_FORTRAN_2018, &
    STD_FORTRAN_2023, cdd_options_t, cdd_dialect_caps_t, get_dialect_capabilities, &
    resolve_effective_form
  use cdd_ir_types, only: CDD_MAX_UDTS, IR_INT8, IR_INT16, IR_INT32, IR_INT64, IR_REAL32, &
    IR_REAL64, IR_REAL128, IR_COMPLEX64, IR_COMPLEX128, IR_BOOL, IR_STRING_FIXED, &
    IR_STRING_DEFERRED, IR_ENUM, IR_NESTED_UDT, cdd_ir_field_t, cdd_ir_udt_t, &
    cdd_ir_schema_bundle_t, ir_bundle_find_udt, ir_bundle_find_enum
  use cdd_emit_udt, only: emit_modern_module_header, emit_udt_types_section, &
    emit_dec_structure_defs_only, make_common_names, emit_common_line, compute_udt_topo_order
  implicit none (type, external)
  private

  public :: emit_codec_fortran
  public :: emit_modern_to_json_proc, emit_modern_from_json_proc
  public :: emit_legacy_to_json_proc, emit_legacy_from_json_proc
  public :: codec_routine_names

contains

  !> @brief Emit complete dialect-pure JSON <-> UDT codec source code for bundle (Operation 2).
  !> @param[in]  bundle  Canonical IR schema bundle.
  !> @param[in]  options Target Fortran standard, source form, and codegen flags.
  !> @param[out] out_buf Generated Fortran codec source buffer.
  !> @param[out] err     Structured error status.
  pure subroutine emit_codec_fortran(bundle, options, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_options_t), intent(in) :: options
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: form, sid, k, u, order(CDD_MAX_UDTS)
    type(cdd_dialect_caps_t) :: caps
    logical(lk) :: is_submod, up
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call buf_init(out_buf)
    call cdd_error_clear(err)
    if (bundle%main_udt_idx < 1_i32 .or. bundle%main_udt_idx > bundle%udt_count) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Empty IR bundle in emit_codec_fortran")
      return
    end if

    call resolve_effective_form(options, form, err)
    if (err%code /= 0_i32) return

    caps = get_dialect_capabilities(options%standard, options%allow_dec_structure)
    sid = options%standard%id
    up = options%uppercase_keywords

    if (sid <= STD_FORTRAN_77) then
      if (caps%has_dec_structure) then
        call compute_udt_topo_order(bundle, order)
        do k = 1_i32, bundle%udt_count
          u = order(k)
          call emit_legacy_to_json_proc(bundle, u, options, out_buf, err)
          call emit_legacy_from_json_proc(bundle, u, options, out_buf, err)
        end do
      else
        call emit_legacy_to_json_proc(bundle, bundle%main_udt_idx, options, out_buf, err)
        call emit_legacy_from_json_proc(bundle, bundle%main_udt_idx, options, out_buf, err)
      end if
      return
    end if

    is_submod = options%emit_submodules .and. caps%has_submodules .and. &
                options%emit_type_bound_procs
    if (is_submod) then
      call buf_append_str(out_buf, "!> @brief Generated JSON codec submodule for " // &
        bundle%module_name%raw(1:bundle%module_name%len) // LF, err)
      call emit_kw(out_buf, "submodule (", up, err)
      call buf_append_str(out_buf, bundle%module_name%raw(1:bundle%module_name%len) // ") " // &
        bundle%module_name%raw(1:bundle%module_name%len) // "_codec_smod" // LF, err)
      call emit_sp(out_buf, max(2_i32, options%indent_width), err)
      if (sid >= STD_FORTRAN_2018) then
        call emit_kw(out_buf, "implicit none (type, external)", up, err)
      else
        call emit_kw(out_buf, "implicit none", up, err)
      end if
      call buf_append_char(out_buf, LF, err)
    else
      call emit_modern_module_header(bundle, options, form, out_buf, err)
      call emit_udt_types_section(bundle, options, options%emit_type_bound_procs, out_buf, err)
    end if

    call emit_kw(out_buf, "contains", up, err)
    call buf_append_char(out_buf, LF, err)

    call compute_udt_topo_order(bundle, order)
    do k = 1_i32, bundle%udt_count
      u = order(k)
      call emit_modern_to_json_proc(bundle, u, options, is_submod, out_buf, err)
      if (sid >= STD_FORTRAN_2003 .and. .not. is_submod .and. &
          options%emit_type_bound_procs .and. .not. bundle%udts(u)%is_bind_c) then
        call emit_modern_to_json_func(bundle%udts(u), options, out_buf, err)
      end if
      call emit_modern_from_json_proc(bundle, u, options, is_submod, out_buf, err)
    end do

    if (is_submod) then
      call emit_kw(out_buf, "end submodule ", up, err)
      call buf_append_str(out_buf, &
        bundle%module_name%raw(1:bundle%module_name%len) // "_codec_smod" // LF, err)
    else
      call emit_kw(out_buf, "end module ", up, err)
      call buf_append_str(out_buf, bundle%module_name%raw(1:bundle%module_name%len) // LF, err)
    end if
  end subroutine emit_codec_fortran

  !> @brief Compute dialect-idiomatic serializer and deserializer routine names for a UDT.
  !> @param[in]  udt_name  Dialect-sanitized Fortran UDT name.
  !> @param[in]  sid       Standard ID constant (STD_FORTRAN_I..2023).
  !> @param[in]  has_dec   True when DEC STRUCTURE mode is active.
  !> @param[out] to_name   Output serializer routine name.
  !> @param[out] to_len    Length of to_name.
  !> @param[out] from_name Output deserializer routine name.
  !> @param[out] from_len  Length of from_name.
  pure subroutine codec_routine_names(udt_name, sid, has_dec, to_name, to_len, from_name, from_len)
    character(len=*, kind=c_k), intent(in) :: udt_name
    integer(i32), intent(in) :: sid
    logical(lk), intent(in) :: has_dec
    character(len=*, kind=c_k), intent(out) :: to_name
    integer(i32), intent(out) :: to_len
    character(len=*, kind=c_k), intent(out) :: from_name
    integer(i32), intent(out) :: from_len
    character(len=3, kind=c_k) :: tag3
    integer(i32) :: k, ntag, nbase

    if (sid >= STD_FORTRAN_II .and. sid <= STD_FORTRAN_66 .and. .not. has_dec) then
      ntag = 0_i32
      do k = 1_i32, len(udt_name, kind=i32)
        if (is_ascii_alnum(udt_name(k:k)) .and. ntag < 3_i32) then
          ntag = ntag + 1_i32
          tag3(ntag:ntag) = udt_name(k:k)
        end if
      end do
      do while (ntag < 3_i32)
        ntag = ntag + 1_i32
        tag3(ntag:ntag) = "X"
      end do
      call copy_name("U2J" // tag3(1:3), to_name, to_len)
      call copy_name("J2U" // tag3(1:3), from_name, from_len)
    else
      nbase = len(udt_name, kind=i32)
      if (sid <= STD_FORTRAN_77) then
        call to_upper_buf(udt_name(1:nbase) // "_TO_JSON", to_name, to_len)
        call to_upper_buf(udt_name(1:nbase) // "_FROM_JSON", from_name, from_len)
      else
        call copy_name(udt_name(1:nbase) // "_to_json", to_name, to_len)
        call copy_name(udt_name(1:nbase) // "_from_json", from_name, from_len)
      end if
    end if
  end subroutine codec_routine_names

  !> @brief Emit a single legacy (`FORTRAN I`..`FORTRAN 77`) UDT-to-JSON routine/subroutine.
  !> @param[in]    bundle  Canonical IR schema bundle.
  !> @param[in]    udt_idx 1-based UDT index in bundle.
  !> @param[in]    options Dialect options.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_legacy_to_json_proc(bundle, udt_idx, options, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    integer(i32), intent(in) :: udt_idx
    type(cdd_options_t), intent(in) :: options
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    type(cdd_dialect_caps_t) :: caps
    integer(i32) :: sid
    character(len=64, kind=c_k) :: to_nm, fr_nm
    integer(i32) :: to_len, fr_len

    sid = options%standard%id
    caps = get_dialect_capabilities(options%standard, options%allow_dec_structure)
    call codec_routine_names( &
      bundle%udts(udt_idx)%fortran_name%raw(1:bundle%udts(udt_idx)%fortran_name%len), &
      sid, caps%has_dec_structure, to_nm, to_len, fr_nm, fr_len)

    if (sid <= STD_FORTRAN_66 .and. .not. caps%has_dec_structure) then
      call emit_intbuf_to_json(bundle%udts(udt_idx), sid, to_nm(1:to_len), out_buf, err)
    else
      call emit_f77_to_json(bundle, udt_idx, caps%has_dec_structure, to_nm(1:to_len), out_buf, err)
    end if
  end subroutine emit_legacy_to_json_proc

  !> @brief Emit a single legacy (`FORTRAN I`..`FORTRAN 77`) JSON-to-UDT routine/subroutine.
  !> @param[in]    bundle  Canonical IR schema bundle.
  !> @param[in]    udt_idx 1-based UDT index in bundle.
  !> @param[in]    options Dialect options.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_legacy_from_json_proc(bundle, udt_idx, options, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    integer(i32), intent(in) :: udt_idx
    type(cdd_options_t), intent(in) :: options
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    type(cdd_dialect_caps_t) :: caps
    integer(i32) :: sid
    character(len=64, kind=c_k) :: to_nm, fr_nm
    integer(i32) :: to_len, fr_len

    sid = options%standard%id
    caps = get_dialect_capabilities(options%standard, options%allow_dec_structure)
    call codec_routine_names( &
      bundle%udts(udt_idx)%fortran_name%raw(1:bundle%udts(udt_idx)%fortran_name%len), &
      sid, caps%has_dec_structure, to_nm, to_len, fr_nm, fr_len)

    if (sid <= STD_FORTRAN_66 .and. .not. caps%has_dec_structure) then
      call emit_intbuf_from_json(bundle%udts(udt_idx), sid, fr_nm(1:fr_len), out_buf, err)
    else
      call emit_f77_from_json(bundle, udt_idx, caps%has_dec_structure, fr_nm(1:fr_len), out_buf, err)
    end if
  end subroutine emit_legacy_from_json_proc

  ! ===========================================================================
  ! Tier 1: FORTRAN I, II, III, IV, 66 Integer-ASCII-Buffer JSON Codecs
  !         (Strictly fixed-form cols 7-72, zero CHARACTER, zero block-IF!)
  ! ===========================================================================

  !> @brief Emit pre-F77 (`FORTRAN I`..`66`) integer-array `TO_JSON` serializer.
  !> @param[in]    udt     Source UDT definition.
  !> @param[in]    sid     Standard constant (STD_FORTRAN_I..STD_FORTRAN_66).
  !> @param[in]    sub_nm  Routine or 6-char subroutine name.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_intbuf_to_json(udt, sid, sub_nm, out_buf, err)
    type(cdd_ir_udt_t), intent(in) :: udt
    integer(i32), intent(in) :: sid
    character(len=*, kind=c_k), intent(in) :: sub_nm
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: f, k, lbl, err_lbl, loop_lbl, skip_lbl, nwords, ub
    logical(lk) :: has_jmax
    character(len=32, kind=c_k) :: ref_expr
    integer(i32) :: ref_len
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    has_jmax = (sid >= STD_FORTRAN_IV)
    lbl = 8010_i32
    err_lbl = 8490_i32

    if (sid == STD_FORTRAN_I) then
      call buf_append_str(out_buf, "C @cdd-begin-routine: " // sub_nm // LF, err)
      call buf_append_str(out_buf, " 8000 IERR = 0" // LF, err)
      call buf_append_str(out_buf, "      JLEN = 0" // LF, err)
    else
      if (has_jmax) then
        call buf_append_str(out_buf, "      SUBROUTINE " // sub_nm // &
          "(JBUF, JMAX, JLEN, IERR)" // LF, err)
      else
        call buf_append_str(out_buf, "      SUBROUTINE " // sub_nm // &
          "(JBUF, JLEN, IERR)" // LF, err)
      end if
      call emit_legacy_sub_decls(udt, sid, .true._lk, out_buf, err)
      call buf_append_str(out_buf, "      IERR = 0" // LF, err)
      call buf_append_str(out_buf, "      JLEN = 0" // LF, err)
      if (has_jmax) then
        call buf_append_str(out_buf, "      IF (JMAX .LT. 2) GO TO 8490" // LF, err)
      end if
    end if

    call emit_ibuf_push(out_buf, 123_i32, has_jmax, err_lbl, err)

    do f = 1_i32, udt%field_count
      if (f > 1_i32) call emit_ibuf_push(out_buf, 44_i32, has_jmax, err_lbl, err)
      call emit_ibuf_push(out_buf, 34_i32, has_jmax, err_lbl, err)
      do k = 1_i32, udt%fields(f)%json_name%len
        call emit_ibuf_push(out_buf, &
          iachar(udt%fields(f)%json_name%raw(k:k), i32), has_jmax, err_lbl, err)
      end do
      call emit_ibuf_push(out_buf, 34_i32, has_jmax, err_lbl, err)
      call emit_ibuf_push(out_buf, 58_i32, has_jmax, err_lbl, err)

      if (udt%fields(f)%type_kind == IR_STRING_FIXED .or. &
          udt%fields(f)%type_kind == IR_STRING_DEFERRED) then
        if (sid <= STD_FORTRAN_III) then
          nwords = max(1_i32, (max(6_i32, udt%fields(f)%char_len) + 5_i32) / 6_i32)
        else
          nwords = max(1_i32, (max(4_i32, udt%fields(f)%char_len) + 3_i32) / 4_i32)
        end if
        call emit_ibuf_packed_str_out( &
          udt%fields(f)%fortran_name%raw(1:udt%fields(f)%fortran_name%len), &
          nwords, sid, has_jmax, err_lbl, lbl, out_buf, err)
      else if (udt%fields(f)%rank > 0_i32) then
        ub = max(1_i32, udt%fields(f)%dims(1)%upper_bound)
        call emit_ibuf_push(out_buf, 91_i32, has_jmax, err_lbl, err)
        lbl = lbl + 1_i32
        loop_lbl = lbl
        lbl = lbl + 1_i32
        skip_lbl = lbl
        call buf_append_str(out_buf, "      DO ", err)
        call buf_append_i64(out_buf, int(loop_lbl, i64), err)
        call buf_append_str(out_buf, " JIDX = 1, ", err)
        call buf_append_i64(out_buf, int(ub, i64), err)
        call buf_append_char(out_buf, LF, err)
        call buf_append_str(out_buf, "      IF (JIDX .EQ. 1) GO TO ", err)
        call buf_append_i64(out_buf, int(skip_lbl, i64), err)
        call buf_append_char(out_buf, LF, err)
        call emit_ibuf_push(out_buf, 44_i32, has_jmax, err_lbl, err)
        call emit_lbl_continue(out_buf, skip_lbl, err)
        call copy_name(udt%fields(f)%fortran_name%raw(1:udt%fields(f)%fortran_name%len) // &
          "(JIDX)", ref_expr, ref_len)
        call emit_ibuf_scalar_out(ref_expr(1:ref_len), udt%fields(f)%type_kind, sid, &
          has_jmax, err_lbl, lbl, out_buf, err)
        call emit_lbl_continue(out_buf, loop_lbl, err)
        call emit_ibuf_push(out_buf, 93_i32, has_jmax, err_lbl, err)
      else
        call copy_name(udt%fields(f)%fortran_name%raw(1:udt%fields(f)%fortran_name%len), &
          ref_expr, ref_len)
        call emit_ibuf_scalar_out(ref_expr(1:ref_len), udt%fields(f)%type_kind, sid, &
          has_jmax, err_lbl, lbl, out_buf, err)
      end if
    end do

    call emit_ibuf_push(out_buf, 125_i32, has_jmax, err_lbl, err)
    if (sid == STD_FORTRAN_I) then
      call buf_append_str(out_buf, "      GO TO IJSRET" // LF, err)
      call buf_append_str(out_buf, "C @cdd-end-routine: " // sub_nm // LF, err)
    else
      if (has_jmax) then
        call buf_append_str(out_buf, "      RETURN" // LF, err)
        call buf_append_str(out_buf, " 8490 IERR = 2" // LF, err)
      end if
      call buf_append_str(out_buf, "      RETURN" // LF, err)
      call buf_append_str(out_buf, "      END" // LF, err)
    end if
  end subroutine emit_intbuf_to_json

  !> @brief Emit pre-F77 (`FORTRAN I`..`66`) integer-array `FROM_JSON` deserializer.
  !> @param[in]    udt     Target UDT definition.
  !> @param[in]    sid     Standard constant (STD_FORTRAN_I..STD_FORTRAN_66).
  !> @param[in]    sub_nm  Routine or 6-char subroutine name.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_intbuf_from_json(udt, sid, sub_nm, out_buf, err)
    type(cdd_ir_udt_t), intent(in) :: udt
    integer(i32), intent(in) :: sid
    character(len=*, kind=c_k), intent(in) :: sub_nm
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: f, k, lbl, err_lbl, done_lbl, top_lbl, after_lbl, nxt_f_lbl
    integer(i32) :: arr_loop, arr_ok, nwords, ub
    character(len=32, kind=c_k) :: ref_expr
    integer(i32) :: ref_len
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    lbl = 8520_i32
    top_lbl = 8505_i32
    after_lbl = 8970_i32
    done_lbl = 8980_i32
    err_lbl = 8990_i32

    if (sid == STD_FORTRAN_I) then
      call buf_append_str(out_buf, "C @cdd-begin-routine: " // sub_nm // LF, err)
      call buf_append_str(out_buf, " 8500 IERR = 0" // LF, err)
    else
      call buf_append_str(out_buf, "      SUBROUTINE " // sub_nm // &
        "(JBUF, JLEN, IERR)" // LF, err)
      call emit_legacy_sub_decls(udt, sid, .false._lk, out_buf, err)
      call buf_append_str(out_buf, "      IERR = 0" // LF, err)
    end if
    call buf_append_str(out_buf, "      JPOS = 1" // LF, err)

    call emit_ibuf_skip_ws(out_buf, lbl, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO 8990" // LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .NE. 123) GO TO 8990" // LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)

    call emit_lbl_continue(out_buf, top_lbl, err)
    call emit_ibuf_skip_ws(out_buf, lbl, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO 8990" // LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 125) GO TO 8980" // LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .NE. 34) GO TO 8990" // LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
    call buf_append_str(out_buf, "      JKB = JPOS" // LF, err)

    lbl = lbl + 2_i32
    call emit_lbl_continue(out_buf, lbl - 1_i32, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO 8990" // LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 34) GO TO ", err)
    call buf_append_i64(out_buf, int(lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
    call buf_append_str(out_buf, "      GO TO ", err)
    call buf_append_i64(out_buf, int(lbl - 1_i32, i64), err)
    call buf_append_char(out_buf, LF, err)
    call emit_lbl_continue(out_buf, lbl, err)
    call buf_append_str(out_buf, "      JKL = JPOS - JKB" // LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)

    call emit_ibuf_skip_ws(out_buf, lbl, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO 8990" // LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .NE. 58) GO TO 8990" // LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
    call emit_ibuf_skip_ws(out_buf, lbl, err)

    do f = 1_i32, udt%field_count
      lbl = lbl + 1_i32
      nxt_f_lbl = lbl
      call buf_append_str(out_buf, "      IF (JKL .NE. ", err)
      call buf_append_i64(out_buf, int(udt%fields(f)%json_name%len, i64), err)
      call buf_append_str(out_buf, ") GO TO ", err)
      call buf_append_i64(out_buf, int(nxt_f_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
      do k = 1_i32, udt%fields(f)%json_name%len
        call buf_append_str(out_buf, "      IF (JBUF(JKB + ", err)
        call buf_append_i64(out_buf, int(k - 1_i32, i64), err)
        call buf_append_str(out_buf, ") .NE. ", err)
        call buf_append_i64(out_buf, int(iachar(udt%fields(f)%json_name%raw(k:k), i32), i64), err)
        call buf_append_str(out_buf, ") GO TO ", err)
        call buf_append_i64(out_buf, int(nxt_f_lbl, i64), err)
        call buf_append_char(out_buf, LF, err)
      end do

      if (udt%fields(f)%type_kind == IR_STRING_FIXED .or. &
          udt%fields(f)%type_kind == IR_STRING_DEFERRED) then
        if (sid <= STD_FORTRAN_III) then
          nwords = max(1_i32, (max(6_i32, udt%fields(f)%char_len) + 5_i32) / 6_i32)
        else
          nwords = max(1_i32, (max(4_i32, udt%fields(f)%char_len) + 3_i32) / 4_i32)
        end if
        call emit_ibuf_packed_str_in( &
          udt%fields(f)%fortran_name%raw(1:udt%fields(f)%fortran_name%len), &
          nwords, sid, err_lbl, lbl, out_buf, err)
      else if (udt%fields(f)%rank > 0_i32) then
        ub = max(1_i32, udt%fields(f)%dims(1)%upper_bound)
        call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO 8990" // LF, err)
        call buf_append_str(out_buf, "      IF (JBUF(JPOS) .NE. 91) GO TO 8990" // LF, err)
        call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
        lbl = lbl + 2_i32
        arr_loop = lbl - 1_i32
        arr_ok = lbl
        call buf_append_str(out_buf, "      DO ", err)
        call buf_append_i64(out_buf, int(arr_loop, i64), err)
        call buf_append_str(out_buf, " JIDX = 1, ", err)
        call buf_append_i64(out_buf, int(ub, i64), err)
        call buf_append_char(out_buf, LF, err)
        call emit_ibuf_skip_ws(out_buf, lbl, err)
        call buf_append_str(out_buf, "      IF (JIDX .EQ. 1) GO TO ", err)
        call buf_append_i64(out_buf, int(arr_ok, i64), err)
        call buf_append_char(out_buf, LF, err)
        call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO 8990" // LF, err)
        call buf_append_str(out_buf, "      IF (JBUF(JPOS) .NE. 44) GO TO 8990" // LF, err)
        call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
        call emit_ibuf_skip_ws(out_buf, lbl, err)
        call emit_lbl_continue(out_buf, arr_ok, err)
        call copy_name(udt%fields(f)%fortran_name%raw(1:udt%fields(f)%fortran_name%len) // &
          "(JIDX)", ref_expr, ref_len)
        call emit_ibuf_scalar_in(ref_expr(1:ref_len), udt%fields(f)%type_kind, sid, &
          err_lbl, lbl, out_buf, err)
        call emit_lbl_continue(out_buf, arr_loop, err)
        call emit_ibuf_skip_ws(out_buf, lbl, err)
        call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO 8990" // LF, err)
        call buf_append_str(out_buf, "      IF (JBUF(JPOS) .NE. 93) GO TO 8990" // LF, err)
        call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
      else
        call copy_name(udt%fields(f)%fortran_name%raw(1:udt%fields(f)%fortran_name%len), &
          ref_expr, ref_len)
        call emit_ibuf_scalar_in(ref_expr(1:ref_len), udt%fields(f)%type_kind, sid, &
          err_lbl, lbl, out_buf, err)
      end if
      call buf_append_str(out_buf, "      GO TO ", err)
      call buf_append_i64(out_buf, int(after_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
      call emit_lbl_continue(out_buf, nxt_f_lbl, err)
    end do

    call emit_ibuf_skip_unknown_val(out_buf, err_lbl, after_lbl, lbl, err)

    call emit_lbl_continue(out_buf, after_lbl, err)
    call emit_ibuf_skip_ws(out_buf, lbl, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO 8990" // LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 125) GO TO ", err)
    call buf_append_i64(out_buf, int(done_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .NE. 44) GO TO 8990" // LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
    call buf_append_str(out_buf, "      GO TO ", err)
    call buf_append_i64(out_buf, int(top_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)

    call emit_lbl_continue(out_buf, done_lbl, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
    if (sid == STD_FORTRAN_I) then
      call buf_append_str(out_buf, "      GO TO IJSRET" // LF, err)
      call buf_append_str(out_buf, " 8990 IERR = 1" // LF, err)
      call buf_append_str(out_buf, "      GO TO IJSRET" // LF, err)
      call buf_append_str(out_buf, "C @cdd-end-routine: " // sub_nm // LF, err)
    else
      call buf_append_str(out_buf, "      RETURN" // LF, err)
      call buf_append_str(out_buf, " 8990 IERR = 1" // LF, err)
      call buf_append_str(out_buf, "      RETURN" // LF, err)
      call buf_append_str(out_buf, "      END" // LF, err)
    end if
  end subroutine emit_intbuf_from_json

  !> @brief Emit specification declarations and `COMMON` inside `FORTRAN II`..`66` subroutines.
  !> @param[in]    udt     UDT definition.
  !> @param[in]    sid     Standard constant (STD_FORTRAN_II..STD_FORTRAN_66).
  !> @param[in]    is_to   True for `TO_JSON` (includes JMAX in IV/66), false for `FROM_JSON`.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_legacy_sub_decls(udt, sid, is_to, out_buf, err)
    type(cdd_ir_udt_t), intent(in) :: udt
    integer(i32), intent(in) :: sid
    logical(lk), intent(in) :: is_to
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: i, nwords, col_len, fn_len, bn_len, bc_len
    character(len=6, kind=c_k) :: blk_num, blk_chr
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    if (sid <= STD_FORTRAN_III) then
      call buf_append_str(out_buf, "      DIMENSION JBUF(*)" // LF, err)
      if (sid == STD_FORTRAN_III) then
        call buf_append_str(out_buf, "C FORTRAN III BOOLEAN WORD VALIDATION" // LF, err)
      end if
      do i = 1_i32, udt%field_count
        if (udt%fields(i)%type_kind == IR_STRING_FIXED .or. &
            udt%fields(i)%type_kind == IR_STRING_DEFERRED) then
          nwords = max(1_i32, (max(6_i32, udt%fields(i)%char_len) + 5_i32) / 6_i32)
          call buf_append_str(out_buf, "      DIMENSION " // &
            udt%fields(i)%fortran_name%raw(1:udt%fields(i)%fortran_name%len) // "(", err)
          call buf_append_i64(out_buf, int(nwords, i64), err)
          call buf_append_str(out_buf, ")" // LF, err)
        else if (udt%fields(i)%rank > 0_i32) then
          call buf_append_str(out_buf, "      DIMENSION " // &
            udt%fields(i)%fortran_name%raw(1:udt%fields(i)%fortran_name%len) // "(", err)
          call buf_append_i64(out_buf, int(max(1_i32, udt%fields(i)%dims(1)%upper_bound), i64), err)
          call buf_append_str(out_buf, ")" // LF, err)
        end if
      end do
      if (udt%field_count > 0_i32) then
        call buf_append_str(out_buf, "      COMMON ", err)
        col_len = 13_i32
        do i = 1_i32, udt%field_count
          if (i > 1_i32) then
            call buf_append_str(out_buf, ", ", err)
            col_len = col_len + 2_i32
          end if
          fn_len = udt%fields(i)%fortran_name%len
          if (col_len + fn_len > 70_i32) then
            call buf_append_str(out_buf, LF // "     1 ", err)
            col_len = 7_i32
          end if
          call buf_append_str(out_buf, udt%fields(i)%fortran_name%raw(1:fn_len), err)
          col_len = col_len + fn_len
        end do
        call buf_append_char(out_buf, LF, err)
      end if
      return
    end if

    if (is_to) then
      call buf_append_str(out_buf, &
        "      INTEGER JBUF(*), JMAX, JLEN, IERR, JIV, JPOW, JCH" // LF, err)
    else
      call buf_append_str(out_buf, &
        "      INTEGER JBUF(*), JLEN, IERR, JPOS, JKB, JKL, JIV, JPOW, JCH" // LF, err)
    end if
    call buf_append_str(out_buf, &
      "      INTEGER JSGN, JW, JMUL, JIDX, JEND, JCUR, JWI, JCI, JDEP" // LF, err)
    call buf_append_str(out_buf, "      REAL XVAL, XSCL" // LF, err)

    do i = 1_i32, udt%field_count
      select case (udt%fields(i)%type_kind)
      case (IR_REAL32)
        call buf_append_str(out_buf, "      REAL ", err)
      case (IR_REAL64, IR_REAL128)
        call buf_append_str(out_buf, "      DOUBLE PRECISION ", err)
      case (IR_COMPLEX64, IR_COMPLEX128)
        call buf_append_str(out_buf, "      COMPLEX ", err)
      case (IR_BOOL)
        call buf_append_str(out_buf, "      LOGICAL ", err)
      case default
        call buf_append_str(out_buf, "      INTEGER ", err)
      end select
      call buf_append_str(out_buf, &
        udt%fields(i)%fortran_name%raw(1:udt%fields(i)%fortran_name%len), err)
      if (udt%fields(i)%type_kind == IR_STRING_FIXED .or. &
          udt%fields(i)%type_kind == IR_STRING_DEFERRED) then
        nwords = max(1_i32, (max(4_i32, udt%fields(i)%char_len) + 3_i32) / 4_i32)
        call buf_append_char(out_buf, "(", err)
        call buf_append_i64(out_buf, int(nwords, i64), err)
        call buf_append_char(out_buf, ")", err)
      else if (udt%fields(i)%rank > 0_i32) then
        call buf_append_char(out_buf, "(", err)
        call buf_append_i64(out_buf, int(max(1_i32, udt%fields(i)%dims(1)%upper_bound), i64), err)
        call buf_append_char(out_buf, ")", err)
      end if
      call buf_append_char(out_buf, LF, err)
    end do

    if (udt%field_count > 0_i32) then
      call make_common_names(udt%fortran_name%raw(1:udt%fortran_name%len), .false._lk, &
        blk_num, bn_len, blk_chr, bc_len)
      call emit_common_line(udt, blk_num(1:bn_len), .false._lk, .false._lk, out_buf, err)
    end if
  end subroutine emit_legacy_sub_decls

  !> @brief Append one ASCII byte push into `JBUF` with optional `JMAX` overflow guard.
  !> @param[inout] out_buf  Target buffer.
  !> @param[in]    ch_code  ASCII integer code to append.
  !> @param[in]    has_jmax True when `JMAX` capacity check is emitted.
  !> @param[in]    err_lbl  Overflow jump label.
  !> @param[inout] err      Error status.
  pure subroutine emit_ibuf_push(out_buf, ch_code, has_jmax, err_lbl, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: ch_code
    logical(lk), intent(in) :: has_jmax
    integer(i32), intent(in) :: err_lbl
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    if (has_jmax) then
      call buf_append_str(out_buf, "      IF (JLEN .GE. JMAX) GO TO ", err)
      call buf_append_i64(out_buf, int(err_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
    end if
    call buf_append_str(out_buf, "      JLEN = JLEN + 1" // LF, err)
    call buf_append_str(out_buf, "      JBUF(JLEN) = ", err)
    call buf_append_i64(out_buf, int(ch_code, i64), err)
    call buf_append_char(out_buf, LF, err)
  end subroutine emit_ibuf_push

  !> @brief Emit packed-word string serializer for `FORTRAN I`..`66` (base 64 or base 128).
  !> @param[in]    var_nm   Fortran array variable name.
  !> @param[in]    nwords   Word count.
  !> @param[in]    sid      Standard ID.
  !> @param[in]    has_jmax True for JMAX bounds check.
  !> @param[in]    err_lbl  Overflow jump label.
  !> @param[inout] lbl      Running statement label counter.
  !> @param[inout] out_buf  Target buffer.
  !> @param[inout] err      Error status.
  pure subroutine emit_ibuf_packed_str_out(var_nm, nwords, sid, has_jmax, err_lbl, lbl, &
      out_buf, err)
    character(len=*, kind=c_k), intent(in) :: var_nm
    integer(i32), intent(in) :: nwords
    integer(i32), intent(in) :: sid
    logical(lk), intent(in) :: has_jmax
    integer(i32), intent(in) :: err_lbl
    integer(i32), intent(inout) :: lbl
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: cpw, radix, l_w1, l_c1, l_w2, l_c2, l_done
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    if (sid <= STD_FORTRAN_III) then
      cpw = 5_i32
      radix = 64_i32
    else
      cpw = 4_i32
      radix = 128_i32
    end if

    call emit_ibuf_push(out_buf, 34_i32, has_jmax, err_lbl, err)
    lbl = lbl + 5_i32
    l_w1 = lbl - 4_i32
    l_c1 = lbl - 3_i32
    l_w2 = lbl - 2_i32
    l_c2 = lbl - 1_i32
    l_done = lbl

    call buf_append_str(out_buf, "      JEND = 0" // LF // "      JCUR = 0" // LF, err)
    call buf_append_str(out_buf, "      DO ", err)
    call buf_append_i64(out_buf, int(l_w1, i64), err)
    call buf_append_str(out_buf, " JWI = 1, ", err)
    call buf_append_i64(out_buf, int(nwords, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      JW = " // var_nm // "(JWI)" // LF, err)
    call buf_append_str(out_buf, "      DO ", err)
    call buf_append_i64(out_buf, int(l_c1, i64), err)
    call buf_append_str(out_buf, " JCI = 1, ", err)
    call buf_append_i64(out_buf, int(cpw, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      JCUR = JCUR + 1" // LF, err)
    call buf_append_str(out_buf, "      JCH = JW - (JW / ", err)
    call buf_append_i64(out_buf, int(radix, i64), err)
    call buf_append_str(out_buf, ") * ", err)
    call buf_append_i64(out_buf, int(radix, i64), err)
    call buf_append_char(out_buf, LF, err)
    if (radix == 64_i32) then
      call buf_append_str(out_buf, "      JCH = JCH + 32" // LF, err)
    else
      call buf_append_str(out_buf, "      IF (JCH .LE. 0) JCH = 32" // LF, err)
    end if
    call buf_append_str(out_buf, "      IF (JCH .NE. 32) JEND = JCUR" // LF, err)
    call buf_append_str(out_buf, "      JW = JW / ", err)
    call buf_append_i64(out_buf, int(radix, i64), err)
    call buf_append_char(out_buf, LF, err)
    call emit_lbl_continue(out_buf, l_c1, err)
    call emit_lbl_continue(out_buf, l_w1, err)

    call buf_append_str(out_buf, "      IF (JEND .EQ. 0) GO TO ", err)
    call buf_append_i64(out_buf, int(l_done, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      JCUR = 0" // LF, err)
    call buf_append_str(out_buf, "      DO ", err)
    call buf_append_i64(out_buf, int(l_w2, i64), err)
    call buf_append_str(out_buf, " JWI = 1, ", err)
    call buf_append_i64(out_buf, int(nwords, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      JW = " // var_nm // "(JWI)" // LF, err)
    call buf_append_str(out_buf, "      DO ", err)
    call buf_append_i64(out_buf, int(l_c2, i64), err)
    call buf_append_str(out_buf, " JCI = 1, ", err)
    call buf_append_i64(out_buf, int(cpw, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      JCUR = JCUR + 1" // LF, err)
    call buf_append_str(out_buf, "      IF (JCUR .GT. JEND) GO TO ", err)
    call buf_append_i64(out_buf, int(l_done, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      JCH = JW - (JW / ", err)
    call buf_append_i64(out_buf, int(radix, i64), err)
    call buf_append_str(out_buf, ") * ", err)
    call buf_append_i64(out_buf, int(radix, i64), err)
    call buf_append_char(out_buf, LF, err)
    if (radix == 64_i32) then
      call buf_append_str(out_buf, "      JCH = JCH + 32" // LF, err)
    else
      call buf_append_str(out_buf, "      IF (JCH .LE. 0) JCH = 32" // LF, err)
    end if
    if (has_jmax) then
      call buf_append_str(out_buf, "      IF (JLEN .GE. JMAX) GO TO ", err)
      call buf_append_i64(out_buf, int(err_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
    end if
    call buf_append_str(out_buf, "      JLEN = JLEN + 1" // LF // "      JBUF(JLEN) = JCH" // LF, err)
    call buf_append_str(out_buf, "      JW = JW / ", err)
    call buf_append_i64(out_buf, int(radix, i64), err)
    call buf_append_char(out_buf, LF, err)
    call emit_lbl_continue(out_buf, l_c2, err)
    call emit_lbl_continue(out_buf, l_w2, err)
    call emit_lbl_continue(out_buf, l_done, err)
    call emit_ibuf_push(out_buf, 34_i32, has_jmax, err_lbl, err)
  end subroutine emit_ibuf_packed_str_out

  !> @brief Emit packed-word string parser for `FORTRAN I`..`66` (base 64 or base 128).
  !> @param[in]    var_nm  Fortran array variable name.
  !> @param[in]    nwords  Word count.
  !> @param[in]    sid     Standard ID.
  !> @param[in]    err_lbl Parse error jump label.
  !> @param[inout] lbl     Running statement label counter.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_ibuf_packed_str_in(var_nm, nwords, sid, err_lbl, lbl, out_buf, err)
    character(len=*, kind=c_k), intent(in) :: var_nm
    integer(i32), intent(in) :: nwords
    integer(i32), intent(in) :: sid
    integer(i32), intent(in) :: err_lbl
    integer(i32), intent(inout) :: lbl
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: cpw, radix, l_w, l_c, l_pad, l_sto, l_rem, l_end
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    if (sid <= STD_FORTRAN_III) then
      cpw = 5_i32
      radix = 64_i32
    else
      cpw = 4_i32
      radix = 128_i32
    end if

    lbl = lbl + 6_i32
    l_w = lbl - 5_i32
    l_c = lbl - 4_i32
    l_pad = lbl - 3_i32
    l_sto = lbl - 2_i32
    l_rem = lbl - 1_i32
    l_end = lbl

    call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
    call buf_append_i64(out_buf, int(err_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .NE. 34) GO TO ", err)
    call buf_append_i64(out_buf, int(err_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF // "      JEND = 0" // LF, err)

    call buf_append_str(out_buf, "      DO ", err)
    call buf_append_i64(out_buf, int(l_w, i64), err)
    call buf_append_str(out_buf, " JWI = 1, ", err)
    call buf_append_i64(out_buf, int(nwords, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      JW = 0" // LF // "      JMUL = 1" // LF, err)
    call buf_append_str(out_buf, "      DO ", err)
    call buf_append_i64(out_buf, int(l_c, i64), err)
    call buf_append_str(out_buf, " JCI = 1, ", err)
    call buf_append_i64(out_buf, int(cpw, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JEND .EQ. 1) GO TO ", err)
    call buf_append_i64(out_buf, int(l_pad, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
    call buf_append_i64(out_buf, int(err_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .NE. 34) GO TO ", err)
    call buf_append_i64(out_buf, int(l_sto, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      JEND = 1" // LF // "      JPOS = JPOS + 1" // LF, err)
    call emit_lbl_continue(out_buf, l_pad, err)
    call buf_append_str(out_buf, "      JCH = 32" // LF // "      GO TO ", err)
    call buf_append_i64(out_buf, int(l_c, i64), err)
    call buf_append_char(out_buf, LF, err)
    call emit_lbl_continue(out_buf, l_sto, err)
    call buf_append_str(out_buf, "      JCH = JBUF(JPOS)" // LF // "      JPOS = JPOS + 1" // LF, err)
    if (radix == 64_i32) then
      call buf_append_str(out_buf, "      IF (JCH .GE. 97) JCH = JCH - 32" // LF, err)
      call buf_append_str(out_buf, "      JCH = JCH - 32" // LF, err)
      call buf_append_str(out_buf, "      IF (JCH .LT. 0) JCH = 0" // LF, err)
      call buf_append_str(out_buf, "      IF (JCH .GT. 63) JCH = 63" // LF, err)
    end if
    call buf_append_str(out_buf, "      JW = JW + JCH * JMUL" // LF, err)
    call buf_append_str(out_buf, "      JMUL = JMUL * ", err)
    call buf_append_i64(out_buf, int(radix, i64), err)
    call buf_append_char(out_buf, LF, err)
    call emit_lbl_continue(out_buf, l_c, err)
    call buf_append_str(out_buf, "      " // var_nm // "(JWI) = JW" // LF, err)
    call emit_lbl_continue(out_buf, l_w, err)

    call emit_lbl_continue(out_buf, l_rem, err)
    call buf_append_str(out_buf, "      IF (JEND .EQ. 1) GO TO ", err)
    call buf_append_i64(out_buf, int(l_end, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
    call buf_append_i64(out_buf, int(err_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 34) JEND = 1" // LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
    call buf_append_str(out_buf, "      GO TO ", err)
    call buf_append_i64(out_buf, int(l_rem, i64), err)
    call buf_append_char(out_buf, LF, err)
    call emit_lbl_continue(out_buf, l_end, err)
  end subroutine emit_ibuf_packed_str_in

  !> @brief Emit scalar serializer (`INTEGER`, `REAL`, `LOGICAL`) into `JBUF` for `FORTRAN I`..`66`.
  !> @param[in]    expr     Scalar or indexed Fortran expression.
  !> @param[in]    tkind    Canonical IR_* type kind.
  !> @param[in]    sid      Standard ID.
  !> @param[in]    has_jmax True when JMAX check is emitted.
  !> @param[in]    err_lbl  Overflow jump label.
  !> @param[inout] lbl      Running statement label counter.
  !> @param[inout] out_buf  Target buffer.
  !> @param[inout] err      Error status.
  pure subroutine emit_ibuf_scalar_out(expr, tkind, sid, has_jmax, err_lbl, lbl, out_buf, err)
    character(len=*, kind=c_k), intent(in) :: expr
    integer(i32), intent(in) :: tkind
    integer(i32), intent(in) :: sid
    logical(lk), intent(in) :: has_jmax
    integer(i32), intent(in) :: err_lbl
    integer(i32), intent(inout) :: lbl
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: l_t, l_e, l_pos, l_p1, l_p2, l_fp1, l_fp2
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    if (tkind == IR_BOOL) then
      lbl = lbl + 2_i32
      l_t = lbl - 1_i32
      l_e = lbl
      if (sid <= STD_FORTRAN_III) then
        if (sid == STD_FORTRAN_III) then
          call buf_append_str(out_buf, "      IF ((" // expr // " - (" // expr // &
            " / 2) * 2) .NE. 0) GO TO ", err)
        else
          call buf_append_str(out_buf, "      IF (" // expr // " .NE. 0) GO TO ", err)
        end if
      else
        call buf_append_str(out_buf, "      IF (" // expr // ") GO TO ", err)
      end if
      call buf_append_i64(out_buf, int(l_t, i64), err)
      call buf_append_char(out_buf, LF, err)
      call emit_ibuf_push(out_buf, 102_i32, has_jmax, err_lbl, err)
      call emit_ibuf_push(out_buf, 97_i32, has_jmax, err_lbl, err)
      call emit_ibuf_push(out_buf, 108_i32, has_jmax, err_lbl, err)
      call emit_ibuf_push(out_buf, 115_i32, has_jmax, err_lbl, err)
      call emit_ibuf_push(out_buf, 101_i32, has_jmax, err_lbl, err)
      call buf_append_str(out_buf, "      GO TO ", err)
      call buf_append_i64(out_buf, int(l_e, i64), err)
      call buf_append_char(out_buf, LF, err)
      call emit_lbl_continue(out_buf, l_t, err)
      call emit_ibuf_push(out_buf, 116_i32, has_jmax, err_lbl, err)
      call emit_ibuf_push(out_buf, 114_i32, has_jmax, err_lbl, err)
      call emit_ibuf_push(out_buf, 117_i32, has_jmax, err_lbl, err)
      call emit_ibuf_push(out_buf, 101_i32, has_jmax, err_lbl, err)
      call emit_lbl_continue(out_buf, l_e, err)
    else if (tkind >= IR_REAL32 .and. tkind <= IR_REAL128) then
      lbl = lbl + 5_i32
      l_pos = lbl - 4_i32
      l_p1 = lbl - 3_i32
      l_p2 = lbl - 2_i32
      l_fp1 = lbl - 1_i32
      l_fp2 = lbl
      call buf_append_str(out_buf, "      XVAL = " // expr // LF, err)
      call buf_append_str(out_buf, "      IF (XVAL .GE. 0.0) GO TO ", err)
      call buf_append_i64(out_buf, int(l_pos, i64), err)
      call buf_append_char(out_buf, LF, err)
      call emit_ibuf_push(out_buf, 45_i32, has_jmax, err_lbl, err)
      call buf_append_str(out_buf, "      XVAL = -XVAL" // LF, err)
      call emit_lbl_continue(out_buf, l_pos, err)
      call buf_append_str(out_buf, "      JEND = INT(XVAL * 10000.0 + 0.5)" // LF, err)
      call buf_append_str(out_buf, "      JIV = JEND / 10000" // LF, err)
      call emit_ibuf_pos_int_digits(out_buf, l_p1, l_p2, has_jmax, err_lbl, err)
      call emit_ibuf_push(out_buf, 46_i32, has_jmax, err_lbl, err)
      call buf_append_str(out_buf, "      JIV = JEND - (JEND / 10000) * 10000" // LF, err)
      call buf_append_str(out_buf, "      JPOW = 1000" // LF, err)
      call emit_lbl_continue(out_buf, l_fp1, err)
      call buf_append_str(out_buf, "      JCH = (JIV / JPOW) + 48" // LF, err)
      if (has_jmax) then
        call buf_append_str(out_buf, "      IF (JLEN .GE. JMAX) GO TO ", err)
        call buf_append_i64(out_buf, int(err_lbl, i64), err)
        call buf_append_char(out_buf, LF, err)
      end if
      call buf_append_str(out_buf, "      JLEN = JLEN + 1" // LF // "      JBUF(JLEN) = JCH" // LF, err)
      call buf_append_str(out_buf, "      JIV = JIV - (JIV / JPOW) * JPOW" // LF, err)
      call buf_append_str(out_buf, "      JPOW = JPOW / 10" // LF, err)
      call buf_append_str(out_buf, "      IF (JIV .EQ. 0) GO TO ", err)
      call buf_append_i64(out_buf, int(l_fp2, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      IF (JPOW .GT. 0) GO TO ", err)
      call buf_append_i64(out_buf, int(l_fp1, i64), err)
      call buf_append_char(out_buf, LF, err)
      call emit_lbl_continue(out_buf, l_fp2, err)
    else
      lbl = lbl + 3_i32
      l_pos = lbl - 2_i32
      l_p1 = lbl - 1_i32
      l_p2 = lbl
      if (tkind == IR_COMPLEX64 .or. tkind == IR_COMPLEX128) then
        call buf_append_str(out_buf, "      JIV = INT(REAL(" // expr // "))" // LF, err)
      else
        call buf_append_str(out_buf, "      JIV = " // expr // LF, err)
      end if
      call buf_append_str(out_buf, "      IF (JIV .GE. 0) GO TO ", err)
      call buf_append_i64(out_buf, int(l_pos, i64), err)
      call buf_append_char(out_buf, LF, err)
      call emit_ibuf_push(out_buf, 45_i32, has_jmax, err_lbl, err)
      call buf_append_str(out_buf, "      JIV = -JIV" // LF, err)
      call emit_lbl_continue(out_buf, l_pos, err)
      call emit_ibuf_pos_int_digits(out_buf, l_p1, l_p2, has_jmax, err_lbl, err)
    end if
  end subroutine emit_ibuf_scalar_out

  !> @brief Emit non-negative integer digit writer (`JIV >= 0`) using scalar `JPOW`.
  !> @param[inout] out_buf  Target buffer.
  !> @param[in]    l_p1     Power-of-10 scan label.
  !> @param[in]    l_p2     Digit-emit loop label.
  !> @param[in]    has_jmax True for JMAX bounds check.
  !> @param[in]    err_lbl  Overflow jump label.
  !> @param[inout] err      Error status.
  pure subroutine emit_ibuf_pos_int_digits(out_buf, l_p1, l_p2, has_jmax, err_lbl, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: l_p1
    integer(i32), intent(in) :: l_p2
    logical(lk), intent(in) :: has_jmax
    integer(i32), intent(in) :: err_lbl
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call buf_append_str(out_buf, "      JPOW = 1" // LF, err)
    call emit_lbl_continue(out_buf, l_p1, err)
    call buf_append_str(out_buf, "      IF (JIV / JPOW .LT. 10) GO TO ", err)
    call buf_append_i64(out_buf, int(l_p2, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      JPOW = JPOW * 10" // LF // "      GO TO ", err)
    call buf_append_i64(out_buf, int(l_p1, i64), err)
    call buf_append_char(out_buf, LF, err)
    call emit_lbl_continue(out_buf, l_p2, err)
    call buf_append_str(out_buf, "      JCH = (JIV / JPOW) + 48" // LF, err)
    if (has_jmax) then
      call buf_append_str(out_buf, "      IF (JLEN .GE. JMAX) GO TO ", err)
      call buf_append_i64(out_buf, int(err_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
    end if
    call buf_append_str(out_buf, "      JLEN = JLEN + 1" // LF // "      JBUF(JLEN) = JCH" // LF, err)
    call buf_append_str(out_buf, "      JIV = JIV - (JIV / JPOW) * JPOW" // LF, err)
    call buf_append_str(out_buf, "      JPOW = JPOW / 10" // LF, err)
    call buf_append_str(out_buf, "      IF (JPOW .GT. 0) GO TO ", err)
    call buf_append_i64(out_buf, int(l_p2, i64), err)
    call buf_append_char(out_buf, LF, err)
  end subroutine emit_ibuf_pos_int_digits

  !> @brief Emit scalar parser (`INTEGER`, `REAL`, `LOGICAL`) from `JBUF` for `FORTRAN I`..`66`.
  !> @param[in]    expr    Target variable or indexed array element.
  !> @param[in]    tkind   Canonical IR_* type kind.
  !> @param[in]    sid     Standard ID.
  !> @param[in]    err_lbl Parse error jump label.
  !> @param[inout] lbl     Running statement label counter.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_ibuf_scalar_in(expr, tkind, sid, err_lbl, lbl, out_buf, err)
    character(len=*, kind=c_k), intent(in) :: expr
    integer(i32), intent(in) :: tkind
    integer(i32), intent(in) :: sid
    integer(i32), intent(in) :: err_lbl
    integer(i32), intent(inout) :: lbl
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: l_f, l_ok, l_ip, l_id, l_fr, l_fd
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    if (tkind == IR_BOOL) then
      lbl = lbl + 2_i32
      l_f = lbl - 1_i32
      l_ok = lbl
      call buf_append_str(out_buf, "      IF (JPOS + 3 .GT. JLEN) GO TO ", err)
      call buf_append_i64(out_buf, int(err_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      IF (JBUF(JPOS) .NE. 116) GO TO ", err)
      call buf_append_i64(out_buf, int(l_f, i64), err)
      call buf_append_char(out_buf, LF, err)
      if (sid <= STD_FORTRAN_III) then
        call buf_append_str(out_buf, "      " // expr // " = 1" // LF, err)
      else
        call buf_append_str(out_buf, "      " // expr // " = .TRUE." // LF, err)
      end if
      call buf_append_str(out_buf, "      JPOS = JPOS + 4" // LF // "      GO TO ", err)
      call buf_append_i64(out_buf, int(l_ok, i64), err)
      call buf_append_char(out_buf, LF, err)
      call emit_lbl_continue(out_buf, l_f, err)
      call buf_append_str(out_buf, "      IF (JPOS + 4 .GT. JLEN) GO TO ", err)
      call buf_append_i64(out_buf, int(err_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      IF (JBUF(JPOS) .NE. 102) GO TO ", err)
      call buf_append_i64(out_buf, int(err_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
      if (sid <= STD_FORTRAN_III) then
        call buf_append_str(out_buf, "      " // expr // " = 0" // LF, err)
      else
        call buf_append_str(out_buf, "      " // expr // " = .FALSE." // LF, err)
      end if
      call buf_append_str(out_buf, "      JPOS = JPOS + 5" // LF, err)
      call emit_lbl_continue(out_buf, l_ok, err)
    else if (tkind >= IR_REAL32 .and. tkind <= IR_REAL128) then
      lbl = lbl + 5_i32
      l_f = lbl - 4_i32
      l_ip = lbl - 3_i32
      l_id = lbl - 2_i32
      l_fr = lbl - 1_i32
      l_fd = lbl
      call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
      call buf_append_i64(out_buf, int(err_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      JSGN = 1" // LF, err)
      call buf_append_str(out_buf, "      IF (JBUF(JPOS) .NE. 45) GO TO ", err)
      call buf_append_i64(out_buf, int(l_f, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      JSGN = -1" // LF // "      JPOS = JPOS + 1" // LF, err)
      call emit_lbl_continue(out_buf, l_f, err)
      call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
      call buf_append_i64(out_buf, int(err_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      IF (JBUF(JPOS) .LT. 48) GO TO ", err)
      call buf_append_i64(out_buf, int(err_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      IF (JBUF(JPOS) .GT. 57) GO TO ", err)
      call buf_append_i64(out_buf, int(err_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      XVAL = 0.0" // LF, err)
      call emit_lbl_continue(out_buf, l_ip, err)
      call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
      call buf_append_i64(out_buf, int(l_id, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      IF (JBUF(JPOS) .LT. 48) GO TO ", err)
      call buf_append_i64(out_buf, int(l_id, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      IF (JBUF(JPOS) .GT. 57) GO TO ", err)
      call buf_append_i64(out_buf, int(l_id, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      XVAL = XVAL * 10.0 + FLOAT(JBUF(JPOS) - 48)" // LF, err)
      call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF // "      GO TO ", err)
      call buf_append_i64(out_buf, int(l_ip, i64), err)
      call buf_append_char(out_buf, LF, err)
      call emit_lbl_continue(out_buf, l_id, err)
      call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
      call buf_append_i64(out_buf, int(l_fd, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      IF (JBUF(JPOS) .NE. 46) GO TO ", err)
      call buf_append_i64(out_buf, int(l_fd, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF // "      XSCL = 0.1" // LF, err)
      call emit_lbl_continue(out_buf, l_fr, err)
      call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
      call buf_append_i64(out_buf, int(l_fd, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      IF (JBUF(JPOS) .LT. 48) GO TO ", err)
      call buf_append_i64(out_buf, int(l_fd, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      IF (JBUF(JPOS) .GT. 57) GO TO ", err)
      call buf_append_i64(out_buf, int(l_fd, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      XVAL = XVAL + FLOAT(JBUF(JPOS) - 48) * XSCL" // LF, err)
      call buf_append_str(out_buf, "      XSCL = XSCL * 0.1" // LF, err)
      call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF // "      GO TO ", err)
      call buf_append_i64(out_buf, int(l_fr, i64), err)
      call buf_append_char(out_buf, LF, err)
      call emit_lbl_continue(out_buf, l_fd, err)
      call buf_append_str(out_buf, "      IF (JSGN .LT. 0) XVAL = -XVAL" // LF, err)
      call buf_append_str(out_buf, "      " // expr // " = XVAL" // LF, err)
    else
      lbl = lbl + 3_i32
      l_f = lbl - 2_i32
      l_ip = lbl - 1_i32
      l_id = lbl
      call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
      call buf_append_i64(out_buf, int(err_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      JSGN = 1" // LF, err)
      call buf_append_str(out_buf, "      IF (JBUF(JPOS) .NE. 45) GO TO ", err)
      call buf_append_i64(out_buf, int(l_f, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      JSGN = -1" // LF // "      JPOS = JPOS + 1" // LF, err)
      call emit_lbl_continue(out_buf, l_f, err)
      call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
      call buf_append_i64(out_buf, int(err_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      IF (JBUF(JPOS) .LT. 48) GO TO ", err)
      call buf_append_i64(out_buf, int(err_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      IF (JBUF(JPOS) .GT. 57) GO TO ", err)
      call buf_append_i64(out_buf, int(err_lbl, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      JIV = 0" // LF, err)
      call emit_lbl_continue(out_buf, l_ip, err)
      call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
      call buf_append_i64(out_buf, int(l_id, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      IF (JBUF(JPOS) .LT. 48) GO TO ", err)
      call buf_append_i64(out_buf, int(l_id, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      IF (JBUF(JPOS) .GT. 57) GO TO ", err)
      call buf_append_i64(out_buf, int(l_id, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      JIV = JIV * 10 + (JBUF(JPOS) - 48)" // LF, err)
      call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF // "      GO TO ", err)
      call buf_append_i64(out_buf, int(l_ip, i64), err)
      call buf_append_char(out_buf, LF, err)
      call emit_lbl_continue(out_buf, l_id, err)
      if (tkind == IR_COMPLEX64 .or. tkind == IR_COMPLEX128) then
        call buf_append_str(out_buf, "      " // expr // " = CMPLX(FLOAT(JIV * JSGN), 0.0)" // LF, err)
      else
        call buf_append_str(out_buf, "      " // expr // " = JIV * JSGN" // LF, err)
      end if
    end if
  end subroutine emit_ibuf_scalar_in

  !> @brief Emit ASCII whitespace skipper (`32, 9, 10, 13`) for `FORTRAN I`..`66`.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] lbl     Running statement label counter.
  !> @param[inout] err     Error status.
  pure subroutine emit_ibuf_skip_ws(out_buf, lbl, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(inout) :: lbl
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: l_top, l_adv, l_out
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    lbl = lbl + 3_i32
    l_top = lbl - 2_i32
    l_adv = lbl - 1_i32
    l_out = lbl
    call emit_lbl_continue(out_buf, l_top, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
    call buf_append_i64(out_buf, int(l_out, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 32) GO TO ", err)
    call buf_append_i64(out_buf, int(l_adv, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 9) GO TO ", err)
    call buf_append_i64(out_buf, int(l_adv, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 10) GO TO ", err)
    call buf_append_i64(out_buf, int(l_adv, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 13) GO TO ", err)
    call buf_append_i64(out_buf, int(l_adv, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      GO TO ", err)
    call buf_append_i64(out_buf, int(l_out, i64), err)
    call buf_append_char(out_buf, LF, err)
    call emit_lbl_continue(out_buf, l_adv, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF // "      GO TO ", err)
    call buf_append_i64(out_buf, int(l_top, i64), err)
    call buf_append_char(out_buf, LF, err)
    call emit_lbl_continue(out_buf, l_out, err)
  end subroutine emit_ibuf_skip_ws

  !> @brief Emit balanced unknown-key JSON value skipper for `FORTRAN I`..`66`.
  !> @param[inout] out_buf   Target buffer.
  !> @param[in]    err_lbl   Parse error jump label.
  !> @param[in]    after_lbl Post-field jump label.
  !> @param[inout] lbl       Running label counter.
  !> @param[inout] err       Error status.
  pure subroutine emit_ibuf_skip_unknown_val(out_buf, err_lbl, after_lbl, lbl, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: err_lbl
    integer(i32), intent(in) :: after_lbl
    integer(i32), intent(inout) :: lbl
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: l_s, l_sl, l_c, l_cl, l_p
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    lbl = lbl + 5_i32
    l_s = lbl - 4_i32
    l_sl = lbl - 3_i32
    l_c = lbl - 2_i32
    l_cl = lbl - 1_i32
    l_p = lbl

    call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
    call buf_append_i64(out_buf, int(err_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 34) GO TO ", err)
    call buf_append_i64(out_buf, int(l_s, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 91) GO TO ", err)
    call buf_append_i64(out_buf, int(l_c, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 123) GO TO ", err)
    call buf_append_i64(out_buf, int(l_c, i64), err)
    call buf_append_char(out_buf, LF, err)

    call emit_lbl_continue(out_buf, l_p, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
    call buf_append_i64(out_buf, int(err_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 44) GO TO ", err)
    call buf_append_i64(out_buf, int(after_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 125) GO TO ", err)
    call buf_append_i64(out_buf, int(after_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF // "      GO TO ", err)
    call buf_append_i64(out_buf, int(l_p, i64), err)
    call buf_append_char(out_buf, LF, err)

    call emit_lbl_continue(out_buf, l_s, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
    call emit_lbl_continue(out_buf, l_sl, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
    call buf_append_i64(out_buf, int(err_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 92) JPOS = JPOS + 2" // LF, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
    call buf_append_i64(out_buf, int(err_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .NE. 34) GO TO ", err)
    call buf_append_i64(out_buf, int(l_sl + 1000_i32, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF // "      GO TO ", err)
    call buf_append_i64(out_buf, int(after_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call emit_lbl_continue(out_buf, l_sl + 1000_i32, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF // "      GO TO ", err)
    call buf_append_i64(out_buf, int(l_sl, i64), err)
    call buf_append_char(out_buf, LF, err)

    call emit_lbl_continue(out_buf, l_c, err)
    call buf_append_str(out_buf, "      JDEP = 0" // LF, err)
    call emit_lbl_continue(out_buf, l_cl, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JLEN) GO TO ", err)
    call buf_append_i64(out_buf, int(err_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 91) JDEP = JDEP + 1" // LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 123) JDEP = JDEP + 1" // LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 93) JDEP = JDEP - 1" // LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS) .EQ. 125) JDEP = JDEP - 1" // LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
    call buf_append_str(out_buf, "      IF (JDEP .GT. 0) GO TO ", err)
    call buf_append_i64(out_buf, int(l_cl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      GO TO ", err)
    call buf_append_i64(out_buf, int(after_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
  end subroutine emit_ibuf_skip_unknown_val

  !> @brief Emit a 5-column fixed-form `<lbl> CONTINUE` statement line.
  !> @param[inout] out_buf Target buffer.
  !> @param[in]    lbl_num 4-digit statement label (1000..9999).
  !> @param[inout] err     Error status.
  pure subroutine emit_lbl_continue(out_buf, lbl_num, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: lbl_num
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call buf_append_char(out_buf, " ", err)
    call buf_append_i64(out_buf, int(lbl_num, i64), err)
    call buf_append_str(out_buf, " CONTINUE" // LF, err)
  end subroutine emit_lbl_continue

  ! ===========================================================================
  ! Tier 2: FORTRAN 77 & DEC STRUCTURE Native CHARACTER*(*) JSON Codecs
  !         (Structured block-IF, full string escape/unescape, arbitrary keys)
  ! ===========================================================================

  !> @brief Emit FORTRAN 77 (`COMMON` or DEC `RECORD`) `CHARACTER*(*)` serializer subroutine.
  !> @param[in]    bundle  Canonical IR schema bundle.
  !> @param[in]    udt_idx Target UDT index.
  !> @param[in]    has_dec True for DEC `RECORD /.../` argument mode.
  !> @param[in]    sub_nm  Subroutine name.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_f77_to_json(bundle, udt_idx, has_dec, sub_nm, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    integer(i32), intent(in) :: udt_idx
    logical(lk), intent(in) :: has_dec
    character(len=*, kind=c_k), intent(in) :: sub_nm
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: f, lbl, ub1, ub2, l_r1, l_r2, base_len
    character(len=48, kind=c_k) :: base_ref, elem_ref
    integer(i32) :: elem_len
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    lbl = 7010_i32
    if (has_dec) then
      call buf_append_str(out_buf, "      SUBROUTINE " // sub_nm // &
        "(REC_V, JBUF, JLEN, IERR)" // LF, err)
      call emit_dec_structure_defs_only(bundle, out_buf, err)
      call buf_append_str(out_buf, "      RECORD /" // &
        bundle%udts(udt_idx)%fortran_name%raw(1:bundle%udts(udt_idx)%fortran_name%len) // &
        "/ REC_V" // LF, err)
      call buf_append_str(out_buf, "      CHARACTER*2048 JSUB" // LF, err)
      call buf_append_str(out_buf, "      INTEGER JSLEN" // LF, err)
    else
      call buf_append_str(out_buf, "      SUBROUTINE " // sub_nm // &
        "(JBUF, JLEN, IERR)" // LF, err)
      call emit_f77_common_decls(bundle%udts(udt_idx), out_buf, err)
    end if

    call buf_append_str(out_buf, "      CHARACTER*(*) JBUF" // LF, err)
    call buf_append_str(out_buf, "      CHARACTER*32 JNBUF" // LF, err)
    call buf_append_str(out_buf, "      CHARACTER*1 JCH" // LF, err)
    call buf_append_str(out_buf, &
      "      INTEGER JLEN, IERR, JCAP, JK, JEND, JIDX, JIDX2, JLO" // LF, err)
    call buf_append_str(out_buf, "      IERR = 0" // LF // "      JLEN = 0" // LF, err)
    call buf_append_str(out_buf, "      JCAP = LEN(JBUF)" // LF, err)
    call buf_append_str(out_buf, "      JBUF = ' '" // LF, err)

    call emit_f77_append_lit(out_buf, "{", err)

    do f = 1_i32, bundle%udts(udt_idx)%field_count
      if (f > 1_i32) then
        call emit_f77_append_lit(out_buf, ",", err)
      end if
      call emit_f77_append_lit(out_buf, '"' // &
        bundle%udts(udt_idx)%fields(f)%json_name%raw( &
        1:bundle%udts(udt_idx)%fields(f)%json_name%len) // '":', err)

      if (has_dec) then
        call copy_name("REC_V." // bundle%udts(udt_idx)%fields(f)%fortran_name%raw( &
          1:bundle%udts(udt_idx)%fields(f)%fortran_name%len), base_ref, base_len)
      else
        call copy_name(bundle%udts(udt_idx)%fields(f)%fortran_name%raw( &
          1:bundle%udts(udt_idx)%fields(f)%fortran_name%len), base_ref, base_len)
      end if

      if (bundle%udts(udt_idx)%fields(f)%rank >= 2_i32) then
        ub1 = max(1_i32, bundle%udts(udt_idx)%fields(f)%dims(1)%upper_bound)
        ub2 = max(1_i32, bundle%udts(udt_idx)%fields(f)%dims(2)%upper_bound)
        call emit_f77_append_lit(out_buf, "[", err)
        lbl = lbl + 2_i32
        l_r1 = lbl - 1_i32
        l_r2 = lbl
        call buf_append_str(out_buf, "      DO ", err)
        call buf_append_i64(out_buf, int(l_r1, i64), err)
        call buf_append_str(out_buf, " JIDX = 1, ", err)
        call buf_append_i64(out_buf, int(ub1, i64), err)
        call buf_append_char(out_buf, LF, err)
        call buf_append_str(out_buf, "        IF (JIDX .GT. 1) THEN" // LF, err)
        call emit_f77_append_lit(out_buf, ",", err)
        call buf_append_str(out_buf, "        END IF" // LF, err)
        call emit_f77_append_lit(out_buf, "[", err)
        call buf_append_str(out_buf, "        DO ", err)
        call buf_append_i64(out_buf, int(l_r2, i64), err)
        call buf_append_str(out_buf, " JIDX2 = 1, ", err)
        call buf_append_i64(out_buf, int(ub2, i64), err)
        call buf_append_char(out_buf, LF, err)
        call buf_append_str(out_buf, "          IF (JIDX2 .GT. 1) THEN" // LF, err)
        call emit_f77_append_lit(out_buf, ",", err)
        call buf_append_str(out_buf, "          END IF" // LF, err)
        call copy_name(base_ref(1:base_len) // "(JIDX,JIDX2)", elem_ref, elem_len)
        call emit_f77_val_out(bundle, bundle%udts(udt_idx)%fields(f), elem_ref(1:elem_len), &
          lbl, out_buf, err)
        call emit_lbl_continue(out_buf, l_r2, err)
        call emit_f77_append_lit(out_buf, "]", err)
        call emit_lbl_continue(out_buf, l_r1, err)
        call emit_f77_append_lit(out_buf, "]", err)
      else if (bundle%udts(udt_idx)%fields(f)%rank == 1_i32) then
        ub1 = max(1_i32, bundle%udts(udt_idx)%fields(f)%dims(1)%upper_bound)
        call emit_f77_append_lit(out_buf, "[", err)
        lbl = lbl + 1_i32
        l_r1 = lbl
        call buf_append_str(out_buf, "      DO ", err)
        call buf_append_i64(out_buf, int(l_r1, i64), err)
        call buf_append_str(out_buf, " JIDX = 1, ", err)
        call buf_append_i64(out_buf, int(ub1, i64), err)
        call buf_append_char(out_buf, LF, err)
        call buf_append_str(out_buf, "        IF (JIDX .GT. 1) THEN" // LF, err)
        call emit_f77_append_lit(out_buf, ",", err)
        call buf_append_str(out_buf, "        END IF" // LF, err)
        call copy_name(base_ref(1:base_len) // "(JIDX)", elem_ref, elem_len)
        call emit_f77_val_out(bundle, bundle%udts(udt_idx)%fields(f), elem_ref(1:elem_len), &
          lbl, out_buf, err)
        call emit_lbl_continue(out_buf, l_r1, err)
        call emit_f77_append_lit(out_buf, "]", err)
      else
        call emit_f77_val_out(bundle, bundle%udts(udt_idx)%fields(f), base_ref(1:base_len), &
          lbl, out_buf, err)
      end if
    end do

    call emit_f77_append_lit(out_buf, "}", err)
    call buf_append_str(out_buf, "      RETURN" // LF // "      END" // LF, err)
  end subroutine emit_f77_to_json

  !> @brief Emit single FORTRAN 77 value serializer (`CHARACTER`, numeric, `LOGICAL`, DEC `RECORD`).
  !> @param[in]    bundle  Canonical IR bundle.
  !> @param[in]    fld     Field descriptor.
  !> @param[in]    expr    Variable/element expression.
  !> @param[inout] lbl     Running label counter.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_f77_val_out(bundle, fld, expr, lbl, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_field_t), intent(in) :: fld
    character(len=*, kind=c_k), intent(in) :: expr
    integer(i32), intent(inout) :: lbl
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: r_idx, l1, l2, to_len, fr_len
    character(len=64, kind=c_k) :: to_nm, fr_nm
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=1, kind=c_k), parameter :: BS = achar(92, kind=c_k)

    if (fld%type_kind == IR_STRING_FIXED .or. fld%type_kind == IR_STRING_DEFERRED .or. &
        fld%type_kind == IR_ENUM) then
      lbl = lbl + 2_i32
      l1 = lbl - 1_i32
      l2 = lbl
      call emit_f77_append_lit(out_buf, '"', err)
      call buf_append_str(out_buf, "      JEND = 0" // LF, err)
      call buf_append_str(out_buf, "      DO ", err)
      call buf_append_i64(out_buf, int(l1, i64), err)
      call buf_append_str(out_buf, " JK = 1, LEN(" // expr // ")" // LF, err)
      call buf_append_str(out_buf, "        IF (" // expr // "(JK:JK) .NE. ' ') JEND = JK" // LF, err)
      call emit_lbl_continue(out_buf, l1, err)
      call buf_append_str(out_buf, "      DO ", err)
      call buf_append_i64(out_buf, int(l2, i64), err)
      call buf_append_str(out_buf, " JK = 1, JEND" // LF, err)
      call buf_append_str(out_buf, "        JCH = " // expr // "(JK:JK)" // LF, err)
      call buf_append_str(out_buf, "        IF (JCH .EQ. '""' .OR. ICHAR(JCH) .EQ. 92) THEN" // LF, err)
      call emit_f77_append_lit(out_buf, BS, err)
      call emit_f77_append_jch(out_buf, err)
      call buf_append_str(out_buf, "        ELSE IF (ICHAR(JCH) .EQ. 10) THEN" // LF, err)
      call emit_f77_append_lit(out_buf, BS // "n", err)
      call buf_append_str(out_buf, "        ELSE IF (ICHAR(JCH) .EQ. 13) THEN" // LF, err)
      call emit_f77_append_lit(out_buf, BS // "r", err)
      call buf_append_str(out_buf, "        ELSE IF (ICHAR(JCH) .EQ. 9) THEN" // LF, err)
      call emit_f77_append_lit(out_buf, BS // "t", err)
      call buf_append_str(out_buf, "        ELSE" // LF, err)
      call emit_f77_append_jch(out_buf, err)
      call buf_append_str(out_buf, "        END IF" // LF, err)
      call emit_lbl_continue(out_buf, l2, err)
      call emit_f77_append_lit(out_buf, '"', err)
    else if (fld%type_kind == IR_BOOL) then
      call buf_append_str(out_buf, "      IF (" // expr // ") THEN" // LF, err)
      call emit_f77_append_lit(out_buf, "true", err)
      call buf_append_str(out_buf, "      ELSE" // LF, err)
      call emit_f77_append_lit(out_buf, "false", err)
      call buf_append_str(out_buf, "      END IF" // LF, err)
    else if (fld%type_kind == IR_NESTED_UDT) then
      r_idx = ir_bundle_find_udt(bundle, fld%ref_udt_name%raw(1:fld%ref_udt_name%len))
      if (r_idx > 0_i32) then
        call codec_routine_names( &
          bundle%udts(r_idx)%fortran_name%raw(1:bundle%udts(r_idx)%fortran_name%len), &
          STD_FORTRAN_77, .true._lk, to_nm, to_len, fr_nm, fr_len)
      else
        call codec_routine_names(fld%ref_udt_name%raw(1:fld%ref_udt_name%len), &
          STD_FORTRAN_77, .true._lk, to_nm, to_len, fr_nm, fr_len)
      end if
      call buf_append_str(out_buf, "      CALL " // to_nm(1:to_len) // "(" // expr // &
        ", JSUB, JSLEN, IERR)" // LF, err)
      call buf_append_str(out_buf, "      IF (IERR .NE. 0) RETURN" // LF, err)
      call buf_append_str(out_buf, "      IF (JLEN + JSLEN .GT. JCAP) THEN" // LF, err)
      call buf_append_str(out_buf, "        IERR = 2" // LF // "        RETURN" // LF, err)
      call buf_append_str(out_buf, "      END IF" // LF, err)
      call buf_append_str(out_buf, "      JBUF(JLEN + 1:JLEN + JSLEN) = JSUB(1:JSLEN)" // LF, err)
      call buf_append_str(out_buf, "      JLEN = JLEN + JSLEN" // LF, err)
    else if (fld%type_kind >= IR_REAL32 .and. fld%type_kind <= IR_REAL128) then
      lbl = lbl + 2_i32
      l1 = lbl - 1_i32
      l2 = lbl
      call buf_append_str(out_buf, "      WRITE(JNBUF, '(F24.6)') " // expr // LF, err)
      call buf_append_str(out_buf, "      JLO = 1" // LF, err)
      call emit_lbl_continue(out_buf, l1, err)
      call buf_append_str(out_buf, "      IF (JLO .LT. 24 .AND. JNBUF(JLO:JLO) .EQ. ' ') THEN" // LF, err)
      call buf_append_str(out_buf, "        JLO = JLO + 1" // LF // "        GO TO ", err)
      call buf_append_i64(out_buf, int(l1, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      END IF" // LF // "      JEND = 24" // LF, err)
      call emit_lbl_continue(out_buf, l2, err)
      call buf_append_str(out_buf, "      IF (JEND .GT. JLO + 1 .AND. " // &
        "JNBUF(JEND:JEND) .EQ. '0'" // LF // &
        "     1    .AND. JNBUF(JEND-1:JEND-1) .NE. '.') THEN" // LF, err)
      call buf_append_str(out_buf, "        JEND = JEND - 1" // LF // "        GO TO ", err)
      call buf_append_i64(out_buf, int(l2, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      END IF" // LF, err)
      call buf_append_str(out_buf, "      IF (JLEN + (JEND - JLO + 1) .GT. JCAP) THEN" // LF, err)
      call buf_append_str(out_buf, "        IERR = 2" // LF // "        RETURN" // LF, err)
      call buf_append_str(out_buf, "      END IF" // LF, err)
      call buf_append_str(out_buf, &
        "      JBUF(JLEN + 1:JLEN + JEND - JLO + 1) = JNBUF(JLO:JEND)" // LF, err)
      call buf_append_str(out_buf, "      JLEN = JLEN + JEND - JLO + 1" // LF, err)
    else
      lbl = lbl + 1_i32
      l1 = lbl
      if (fld%type_kind == IR_COMPLEX64 .or. fld%type_kind == IR_COMPLEX128) then
        call buf_append_str(out_buf, "      WRITE(JNBUF, '(I20)') INT(REAL(" // expr // "))" // LF, err)
      else
        call buf_append_str(out_buf, "      WRITE(JNBUF, '(I20)') " // expr // LF, err)
      end if
      call buf_append_str(out_buf, "      JLO = 1" // LF, err)
      call emit_lbl_continue(out_buf, l1, err)
      call buf_append_str(out_buf, "      IF (JLO .LT. 20 .AND. JNBUF(JLO:JLO) .EQ. ' ') THEN" // LF, err)
      call buf_append_str(out_buf, "        JLO = JLO + 1" // LF // "        GO TO ", err)
      call buf_append_i64(out_buf, int(l1, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "      END IF" // LF, err)
      call buf_append_str(out_buf, "      IF (JLEN + (21 - JLO) .GT. JCAP) THEN" // LF, err)
      call buf_append_str(out_buf, "        IERR = 2" // LF // "        RETURN" // LF, err)
      call buf_append_str(out_buf, "      END IF" // LF, err)
      call buf_append_str(out_buf, "      JBUF(JLEN + 1:JLEN + 21 - JLO) = JNBUF(JLO:20)" // LF, err)
      call buf_append_str(out_buf, "      JLEN = JLEN + 21 - JLO" // LF, err)
    end if
  end subroutine emit_f77_val_out

  !> @brief Emit FORTRAN 77 (`COMMON` or DEC `RECORD`) `CHARACTER*(*)` deserializer subroutine.
  !> @param[in]    bundle  Canonical IR schema bundle.
  !> @param[in]    udt_idx Target UDT index.
  !> @param[in]    has_dec True for DEC `RECORD /.../` argument mode.
  !> @param[in]    sub_nm  Subroutine name.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_f77_from_json(bundle, udt_idx, has_dec, sub_nm, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    integer(i32), intent(in) :: udt_idx
    logical(lk), intent(in) :: has_dec
    character(len=*, kind=c_k), intent(in) :: sub_nm
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: f, lbl, top_lbl, done_lbl, ub1, ub2, l_r1, l_r2, base_len, elem_len
    character(len=48, kind=c_k) :: base_ref, elem_ref
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    lbl = 7520_i32
    top_lbl = 7505_i32
    done_lbl = 7980_i32

    if (has_dec) then
      call buf_append_str(out_buf, "      SUBROUTINE " // sub_nm // &
        "(JBUF, REC_V, IERR)" // LF, err)
      call emit_dec_structure_defs_only(bundle, out_buf, err)
      call buf_append_str(out_buf, "      RECORD /" // &
        bundle%udts(udt_idx)%fortran_name%raw(1:bundle%udts(udt_idx)%fortran_name%len) // &
        "/ REC_V" // LF, err)
    else
      call buf_append_str(out_buf, "      SUBROUTINE " // sub_nm // &
        "(JBUF, IERR)" // LF, err)
      call emit_f77_common_decls(bundle%udts(udt_idx), out_buf, err)
    end if

    call buf_append_str(out_buf, "      CHARACTER*(*) JBUF" // LF, err)
    call buf_append_str(out_buf, "      CHARACTER*64 JKEY" // LF, err)
    call buf_append_str(out_buf, "      CHARACTER*256 JSVAL" // LF, err)
    call buf_append_str(out_buf, "      CHARACTER*1 JCH" // LF, err)
    call buf_append_str(out_buf, &
      "      INTEGER IERR, JN, JPOS, JKL, JSL, JIDX, JIDX2, JIOS" // LF, err)
    call buf_append_str(out_buf, &
      "      INTEGER JTE, JDEP, JSTRQ, JHEX, JHI, JHV, JIV" // LF, err)
    call buf_append_str(out_buf, "      IERR = 0" // LF // "      JN = LEN(JBUF)" // LF, err)
    call buf_append_str(out_buf, "      JPOS = 1" // LF, err)

    call emit_f77_skip_ws(out_buf, lbl, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JN) THEN" // LF, err)
    call buf_append_str(out_buf, "        IERR = 1" // LF // "        RETURN" // LF, err)
    call buf_append_str(out_buf, "      END IF" // LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS:JPOS) .NE. '{') THEN" // LF, err)
    call buf_append_str(out_buf, "        IERR = 1" // LF // "        RETURN" // LF, err)
    call buf_append_str(out_buf, "      END IF" // LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)

    call emit_lbl_continue(out_buf, top_lbl, err)
    call emit_f77_skip_ws(out_buf, lbl, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JN) THEN" // LF, err)
    call buf_append_str(out_buf, "        IERR = 1" // LF // "        RETURN" // LF, err)
    call buf_append_str(out_buf, "      END IF" // LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS:JPOS) .EQ. '}') GO TO 7980" // LF, err)
    call emit_f77_read_str_into(out_buf, "JKEY", "JKL", 64_i32, lbl, err)
    call emit_f77_skip_ws(out_buf, lbl, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JN) THEN" // LF, err)
    call buf_append_str(out_buf, "        IERR = 1" // LF // "        RETURN" // LF, err)
    call buf_append_str(out_buf, "      END IF" // LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS:JPOS) .NE. ':') THEN" // LF, err)
    call buf_append_str(out_buf, "        IERR = 1" // LF // "        RETURN" // LF, err)
    call buf_append_str(out_buf, "      END IF" // LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
    call emit_f77_skip_ws(out_buf, lbl, err)

    if (bundle%udts(udt_idx)%field_count == 0_i32) then
      call emit_f77_skip_val(out_buf, lbl, err)
    else
      do f = 1_i32, bundle%udts(udt_idx)%field_count
        if (f == 1_i32) then
          call buf_append_str(out_buf, "      IF (JKL .EQ. ", err)
        else
          call buf_append_str(out_buf, "      ELSE IF (JKL .EQ. ", err)
        end if
        call buf_append_i64(out_buf, int(bundle%udts(udt_idx)%fields(f)%json_name%len, i64), err)
        call buf_append_str(out_buf, LF // "     1    .AND. JKEY(1:JKL) .EQ. '" // &
          bundle%udts(udt_idx)%fields(f)%json_name%raw( &
          1:bundle%udts(udt_idx)%fields(f)%json_name%len) // "') THEN" // LF, err)

        if (has_dec) then
          call copy_name("REC_V." // bundle%udts(udt_idx)%fields(f)%fortran_name%raw( &
            1:bundle%udts(udt_idx)%fields(f)%fortran_name%len), base_ref, base_len)
        else
          call copy_name(bundle%udts(udt_idx)%fields(f)%fortran_name%raw( &
            1:bundle%udts(udt_idx)%fields(f)%fortran_name%len), base_ref, base_len)
        end if

        if (bundle%udts(udt_idx)%fields(f)%rank >= 2_i32) then
          ub1 = max(1_i32, bundle%udts(udt_idx)%fields(f)%dims(1)%upper_bound)
          ub2 = max(1_i32, bundle%udts(udt_idx)%fields(f)%dims(2)%upper_bound)
          call emit_f77_expect_ch(out_buf, "[", err)
          lbl = lbl + 2_i32
          l_r1 = lbl - 1_i32
          l_r2 = lbl
          call buf_append_str(out_buf, "        DO ", err)
          call buf_append_i64(out_buf, int(l_r1, i64), err)
          call buf_append_str(out_buf, " JIDX = 1, ", err)
          call buf_append_i64(out_buf, int(ub1, i64), err)
          call buf_append_char(out_buf, LF, err)
          call emit_f77_skip_ws(out_buf, lbl, err)
          call buf_append_str(out_buf, "          IF (JIDX .GT. 1) THEN" // LF, err)
          call emit_f77_expect_ch(out_buf, ",", err)
          call emit_f77_skip_ws(out_buf, lbl, err)
          call buf_append_str(out_buf, "          END IF" // LF, err)
          call emit_f77_expect_ch(out_buf, "[", err)
          call buf_append_str(out_buf, "          DO ", err)
          call buf_append_i64(out_buf, int(l_r2, i64), err)
          call buf_append_str(out_buf, " JIDX2 = 1, ", err)
          call buf_append_i64(out_buf, int(ub2, i64), err)
          call buf_append_char(out_buf, LF, err)
          call emit_f77_skip_ws(out_buf, lbl, err)
          call buf_append_str(out_buf, "            IF (JIDX2 .GT. 1) THEN" // LF, err)
          call emit_f77_expect_ch(out_buf, ",", err)
          call emit_f77_skip_ws(out_buf, lbl, err)
          call buf_append_str(out_buf, "            END IF" // LF, err)
          call copy_name(base_ref(1:base_len) // "(JIDX,JIDX2)", elem_ref, elem_len)
          call emit_f77_val_in(bundle, bundle%udts(udt_idx)%fields(f), elem_ref(1:elem_len), &
            lbl, out_buf, err)
          call emit_lbl_continue(out_buf, l_r2, err)
          call emit_f77_skip_ws(out_buf, lbl, err)
          call emit_f77_expect_ch(out_buf, "]", err)
          call emit_lbl_continue(out_buf, l_r1, err)
          call emit_f77_skip_ws(out_buf, lbl, err)
          call emit_f77_expect_ch(out_buf, "]", err)
        else if (bundle%udts(udt_idx)%fields(f)%rank == 1_i32) then
          ub1 = max(1_i32, bundle%udts(udt_idx)%fields(f)%dims(1)%upper_bound)
          call emit_f77_expect_ch(out_buf, "[", err)
          lbl = lbl + 1_i32
          l_r1 = lbl
          call buf_append_str(out_buf, "        DO ", err)
          call buf_append_i64(out_buf, int(l_r1, i64), err)
          call buf_append_str(out_buf, " JIDX = 1, ", err)
          call buf_append_i64(out_buf, int(ub1, i64), err)
          call buf_append_char(out_buf, LF, err)
          call emit_f77_skip_ws(out_buf, lbl, err)
          call buf_append_str(out_buf, "          IF (JIDX .GT. 1) THEN" // LF, err)
          call emit_f77_expect_ch(out_buf, ",", err)
          call emit_f77_skip_ws(out_buf, lbl, err)
          call buf_append_str(out_buf, "          END IF" // LF, err)
          call copy_name(base_ref(1:base_len) // "(JIDX)", elem_ref, elem_len)
          call emit_f77_val_in(bundle, bundle%udts(udt_idx)%fields(f), elem_ref(1:elem_len), &
            lbl, out_buf, err)
          call emit_lbl_continue(out_buf, l_r1, err)
          call emit_f77_skip_ws(out_buf, lbl, err)
          call emit_f77_expect_ch(out_buf, "]", err)
        else
          call emit_f77_val_in(bundle, bundle%udts(udt_idx)%fields(f), base_ref(1:base_len), &
            lbl, out_buf, err)
        end if
      end do
      call buf_append_str(out_buf, "      ELSE" // LF, err)
      call emit_f77_skip_val(out_buf, lbl, err)
      call buf_append_str(out_buf, "      END IF" // LF, err)
    end if

    call emit_f77_skip_ws(out_buf, lbl, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JN) THEN" // LF, err)
    call buf_append_str(out_buf, "        IERR = 1" // LF // "        RETURN" // LF, err)
    call buf_append_str(out_buf, "      END IF" // LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS:JPOS) .EQ. '}') GO TO 7980" // LF, err)
    call emit_f77_expect_ch(out_buf, ",", err)
    call buf_append_str(out_buf, "      GO TO ", err)
    call buf_append_i64(out_buf, int(top_lbl, i64), err)
    call buf_append_char(out_buf, LF, err)
    call emit_lbl_continue(out_buf, done_lbl, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
    call buf_append_str(out_buf, "      RETURN" // LF // "      END" // LF, err)
  end subroutine emit_f77_from_json

  !> @brief Emit single FORTRAN 77 value parser (`CHARACTER`, numeric, `LOGICAL`, DEC `RECORD`).
  !> @param[in]    bundle  Canonical IR bundle.
  !> @param[in]    fld     Field descriptor.
  !> @param[in]    expr    Target variable/element expression.
  !> @param[inout] lbl     Running label counter.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_f77_val_in(bundle, fld, expr, lbl, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_field_t), intent(in) :: fld
    character(len=*, kind=c_k), intent(in) :: expr
    integer(i32), intent(inout) :: lbl
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: r_idx, l_lp, l_dn, to_len, fr_len
    character(len=64, kind=c_k) :: to_nm, fr_nm
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    if (fld%type_kind == IR_STRING_FIXED .or. fld%type_kind == IR_STRING_DEFERRED .or. &
        fld%type_kind == IR_ENUM) then
      call emit_f77_read_str_into(out_buf, "JSVAL", "JSL", 256_i32, lbl, err)
      call buf_append_str(out_buf, "        " // expr // " = ' '" // LF, err)
      call buf_append_str(out_buf, "        IF (JSL .GT. 0) " // expr // " = JSVAL(1:JSL)" // LF, err)
    else if (fld%type_kind == IR_BOOL) then
      call buf_append_str(out_buf, "        IF (JPOS + 3 .LE. JN" // LF // &
        "     1      .AND. JBUF(JPOS:JPOS+3) .EQ. 'true') THEN" // LF, err)
      call buf_append_str(out_buf, "          " // expr // " = .TRUE." // LF, err)
      call buf_append_str(out_buf, "          JPOS = JPOS + 4" // LF, err)
      call buf_append_str(out_buf, "        ELSE IF (JPOS + 4 .LE. JN" // LF // &
        "     1      .AND. JBUF(JPOS:JPOS+4) .EQ. 'false') THEN" // LF, err)
      call buf_append_str(out_buf, "          " // expr // " = .FALSE." // LF, err)
      call buf_append_str(out_buf, "          JPOS = JPOS + 5" // LF, err)
      call buf_append_str(out_buf, "        ELSE" // LF, err)
      call buf_append_str(out_buf, "          IERR = 1" // LF // "          RETURN" // LF, err)
      call buf_append_str(out_buf, "        END IF" // LF, err)
    else if (fld%type_kind == IR_NESTED_UDT) then
      r_idx = ir_bundle_find_udt(bundle, fld%ref_udt_name%raw(1:fld%ref_udt_name%len))
      if (r_idx > 0_i32) then
        call codec_routine_names( &
          bundle%udts(r_idx)%fortran_name%raw(1:bundle%udts(r_idx)%fortran_name%len), &
          STD_FORTRAN_77, .true._lk, to_nm, to_len, fr_nm, fr_len)
      else
        call codec_routine_names(fld%ref_udt_name%raw(1:fld%ref_udt_name%len), &
          STD_FORTRAN_77, .true._lk, to_nm, to_len, fr_nm, fr_len)
      end if
      lbl = lbl + 2_i32
      l_lp = lbl - 1_i32
      l_dn = lbl
      call buf_append_str(out_buf, "        IF (JPOS .GT. JN .OR. " // &
        "JBUF(JPOS:JPOS) .NE. '{') THEN" // LF, err)
      call buf_append_str(out_buf, "          IERR = 1" // LF // "          RETURN" // LF, err)
      call buf_append_str(out_buf, "        END IF" // LF, err)
      call buf_append_str(out_buf, "        JTE = JPOS" // LF // "        JDEP = 0" // LF, err)
      call buf_append_str(out_buf, "        JSTRQ = 0" // LF, err)
      call emit_lbl_continue(out_buf, l_lp, err)
      call buf_append_str(out_buf, "        IF (JTE .GT. JN) THEN" // LF, err)
      call buf_append_str(out_buf, "          IERR = 1" // LF // "          RETURN" // LF, err)
      call buf_append_str(out_buf, "        END IF" // LF, err)
      call buf_append_str(out_buf, "        JCH = JBUF(JTE:JTE)" // LF, err)
      call buf_append_str(out_buf, "        IF (JSTRQ .EQ. 1) THEN" // LF, err)
      call buf_append_str(out_buf, "          IF (ICHAR(JCH) .EQ. 92) JTE = JTE + 1" // LF, err)
      call buf_append_str(out_buf, "          IF (JCH .EQ. '""') JSTRQ = 0" // LF, err)
      call buf_append_str(out_buf, "        ELSE" // LF, err)
      call buf_append_str(out_buf, "          IF (JCH .EQ. '""') JSTRQ = 1" // LF, err)
      call buf_append_str(out_buf, "          IF (JCH .EQ. '{') JDEP = JDEP + 1" // LF, err)
      call buf_append_str(out_buf, "          IF (JCH .EQ. '}') JDEP = JDEP - 1" // LF, err)
      call buf_append_str(out_buf, "          IF (JDEP .EQ. 0) GO TO ", err)
      call buf_append_i64(out_buf, int(l_dn, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "        END IF" // LF, err)
      call buf_append_str(out_buf, "        JTE = JTE + 1" // LF // "        GO TO ", err)
      call buf_append_i64(out_buf, int(l_lp, i64), err)
      call buf_append_char(out_buf, LF, err)
      call emit_lbl_continue(out_buf, l_dn, err)
      call buf_append_str(out_buf, "        CALL " // fr_nm(1:fr_len) // &
        "(JBUF(JPOS:JTE), " // expr // ", IERR)" // LF, err)
      call buf_append_str(out_buf, "        IF (IERR .NE. 0) RETURN" // LF, err)
      call buf_append_str(out_buf, "        JPOS = JTE + 1" // LF, err)
    else
      lbl = lbl + 2_i32
      l_lp = lbl - 1_i32
      l_dn = lbl
      call buf_append_str(out_buf, "        JTE = JPOS" // LF, err)
      call emit_lbl_continue(out_buf, l_lp, err)
      call buf_append_str(out_buf, "        IF (JTE .GT. JN) GO TO ", err)
      call buf_append_i64(out_buf, int(l_dn, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "        JCH = JBUF(JTE:JTE)" // LF, err)
      call buf_append_str(out_buf, "        IF ((JCH .GE. '0' .AND. JCH .LE. '9')" // LF // &
        "     1      .OR. JCH .EQ. '-' .OR. JCH .EQ. '+' .OR. JCH .EQ. '.'" // LF // &
        "     2      .OR. JCH .EQ. 'e' .OR. JCH .EQ. 'E') THEN" // LF, err)
      call buf_append_str(out_buf, "          JTE = JTE + 1" // LF // "          GO TO ", err)
      call buf_append_i64(out_buf, int(l_lp, i64), err)
      call buf_append_char(out_buf, LF, err)
      call buf_append_str(out_buf, "        END IF" // LF, err)
      call emit_lbl_continue(out_buf, l_dn, err)
      call buf_append_str(out_buf, "        IF (JTE .LE. JPOS) THEN" // LF, err)
      call buf_append_str(out_buf, "          IERR = 1" // LF // "          RETURN" // LF, err)
      call buf_append_str(out_buf, "        END IF" // LF, err)
      if (fld%type_kind == IR_COMPLEX64 .or. fld%type_kind == IR_COMPLEX128) then
        call buf_append_str(out_buf, "        READ(JBUF(JPOS:JTE-1), *, IOSTAT=JIOS) JIV" // LF, err)
        call buf_append_str(out_buf, "        " // expr // " = CMPLX(REAL(JIV), 0.0)" // LF, err)
      else
        call buf_append_str(out_buf, "        READ(JBUF(JPOS:JTE-1), *, IOSTAT=JIOS) " // expr // LF, err)
      end if
      call buf_append_str(out_buf, "        IF (JIOS .NE. 0) THEN" // LF, err)
      call buf_append_str(out_buf, "          IERR = 1" // LF // "          RETURN" // LF, err)
      call buf_append_str(out_buf, "        END IF" // LF, err)
      call buf_append_str(out_buf, "        JPOS = JTE" // LF, err)
    end if
  end subroutine emit_f77_val_in

  !> @brief Emit FORTRAN 77 field declarations, split/single `COMMON`, and `SAVE` inside codecs.
  !> @param[in]    udt     UDT definition.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_f77_common_decls(udt, out_buf, err)
    type(cdd_ir_udt_t), intent(in) :: udt
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: i, clen, bn_len, bc_len
    logical(lk) :: has_char, has_num, split_cc
    character(len=6, kind=c_k) :: blk_num, blk_chr
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    has_char = .false._lk
    has_num = .false._lk
    do i = 1_i32, udt%field_count
      if (udt%fields(i)%type_kind == IR_STRING_FIXED .or. &
          udt%fields(i)%type_kind == IR_STRING_DEFERRED .or. &
          udt%fields(i)%type_kind == IR_ENUM) then
        has_char = .true._lk
        clen = max(1_i32, udt%fields(i)%char_len)
        if (udt%fields(i)%type_kind == IR_ENUM) clen = 32_i32
        call buf_append_str(out_buf, "      CHARACTER*", err)
        call buf_append_i64(out_buf, int(clen, i64), err)
        call buf_append_char(out_buf, " ", err)
      else
        has_num = .true._lk
        select case (udt%fields(i)%type_kind)
        case (IR_REAL32)
          call buf_append_str(out_buf, "      REAL ", err)
        case (IR_REAL64, IR_REAL128)
          call buf_append_str(out_buf, "      DOUBLE PRECISION ", err)
        case (IR_COMPLEX64, IR_COMPLEX128)
          call buf_append_str(out_buf, "      COMPLEX ", err)
        case (IR_BOOL)
          call buf_append_str(out_buf, "      LOGICAL ", err)
        case default
          call buf_append_str(out_buf, "      INTEGER ", err)
        end select
      end if
      call buf_append_str(out_buf, &
        udt%fields(i)%fortran_name%raw(1:udt%fields(i)%fortran_name%len), err)
      if (udt%fields(i)%rank >= 2_i32) then
        call buf_append_char(out_buf, "(", err)
        call buf_append_i64(out_buf, int(max(1_i32, udt%fields(i)%dims(1)%upper_bound), i64), err)
        call buf_append_char(out_buf, ",", err)
        call buf_append_i64(out_buf, int(max(1_i32, udt%fields(i)%dims(2)%upper_bound), i64), err)
        call buf_append_char(out_buf, ")", err)
      else if (udt%fields(i)%rank == 1_i32) then
        call buf_append_char(out_buf, "(", err)
        call buf_append_i64(out_buf, int(max(1_i32, udt%fields(i)%dims(1)%upper_bound), i64), err)
        call buf_append_char(out_buf, ")", err)
      end if
      call buf_append_char(out_buf, LF, err)
    end do

    split_cc = has_char .and. has_num
    call make_common_names(udt%fortran_name%raw(1:udt%fortran_name%len), split_cc, &
      blk_num, bn_len, blk_chr, bc_len)
    if (split_cc) then
      call emit_common_line(udt, blk_num(1:bn_len), .true._lk, .false._lk, out_buf, err)
      call emit_common_line(udt, blk_chr(1:bc_len), .true._lk, .true._lk, out_buf, err)
      call buf_append_str(out_buf, "      SAVE /" // blk_num(1:bn_len) // "/, /" // &
        blk_chr(1:bc_len) // "/" // LF, err)
    else if (udt%field_count > 0_i32) then
      call emit_common_line(udt, blk_num(1:bn_len), .false._lk, .false._lk, out_buf, err)
      call buf_append_str(out_buf, "      SAVE /" // blk_num(1:bn_len) // "/" // LF, err)
    end if
  end subroutine emit_f77_common_decls

  !> @brief Emit bounded literal append into `JBUF` in FORTRAN 77 (`IERR = 2` on overflow).
  !> @param[inout] out_buf Target buffer.
  !> @param[in]    lit     Literal string to append (single quotes doubled automatically).
  !> @param[inout] err     Error status.
  pure subroutine emit_f77_append_lit(out_buf, lit, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    character(len=*, kind=c_k), intent(in) :: lit
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: n, k
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    n = len(lit, kind=i32)
    call buf_append_str(out_buf, "      IF (JLEN + ", err)
    call buf_append_i64(out_buf, int(n, i64), err)
    call buf_append_str(out_buf, " .GT. JCAP) THEN" // LF, err)
    call buf_append_str(out_buf, "        IERR = 2" // LF // "        RETURN" // LF, err)
    call buf_append_str(out_buf, "      END IF" // LF, err)
    call buf_append_str(out_buf, "      JBUF(JLEN + 1:JLEN + ", err)
    call buf_append_i64(out_buf, int(n, i64), err)
    call buf_append_str(out_buf, ") = '", err)
    do k = 1_i32, n
      if (lit(k:k) == "'") call buf_append_char(out_buf, "'", err)
      call buf_append_char(out_buf, lit(k:k), err)
    end do
    call buf_append_str(out_buf, "'" // LF // "      JLEN = JLEN + ", err)
    call buf_append_i64(out_buf, int(n, i64), err)
    call buf_append_char(out_buf, LF, err)
  end subroutine emit_f77_append_lit

  !> @brief Emit single-char `JCH` append into `JBUF` in FORTRAN 77 (`IERR = 2` on overflow).
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_f77_append_jch(out_buf, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call buf_append_str(out_buf, "          IF (JLEN + 1 .GT. JCAP) THEN" // LF, err)
    call buf_append_str(out_buf, "            IERR = 2" // LF // "            RETURN" // LF, err)
    call buf_append_str(out_buf, "          END IF" // LF, err)
    call buf_append_str(out_buf, "          JLEN = JLEN + 1" // LF, err)
    call buf_append_str(out_buf, "          JBUF(JLEN:JLEN) = JCH" // LF, err)
  end subroutine emit_f77_append_jch

  !> @brief Emit single-character expectation check (`JBUF(JPOS:JPOS) == ch`) in FORTRAN 77.
  !> @param[inout] out_buf Target buffer.
  !> @param[in]    ch      Expected single ASCII character.
  !> @param[inout] err     Error status.
  pure subroutine emit_f77_expect_ch(out_buf, ch, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    character(len=1, kind=c_k), intent(in) :: ch
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call buf_append_str(out_buf, "      IF (JPOS .GT. JN) THEN" // LF, err)
    call buf_append_str(out_buf, "        IERR = 1" // LF // "        RETURN" // LF, err)
    call buf_append_str(out_buf, "      END IF" // LF, err)
    call buf_append_str(out_buf, "      IF (JBUF(JPOS:JPOS) .NE. '" // ch // "') THEN" // LF, err)
    call buf_append_str(out_buf, "        IERR = 1" // LF // "        RETURN" // LF, err)
    call buf_append_str(out_buf, "      END IF" // LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
  end subroutine emit_f77_expect_ch

  !> @brief Emit FORTRAN 77 whitespace skipper (`' '`, TAB, LF, CR).
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] lbl     Running label counter.
  !> @param[inout] err     Error status.
  pure subroutine emit_f77_skip_ws(out_buf, lbl, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(inout) :: lbl
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: l_ws
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    lbl = lbl + 1_i32
    l_ws = lbl
    call emit_lbl_continue(out_buf, l_ws, err)
    call buf_append_str(out_buf, "      IF (JPOS .LE. JN) THEN" // LF, err)
    call buf_append_str(out_buf, "        JCH = JBUF(JPOS:JPOS)" // LF, err)
    call buf_append_str(out_buf, "        IF (JCH .EQ. ' ' .OR. ICHAR(JCH) .EQ. 9" // LF // &
      "     1      .OR. ICHAR(JCH) .EQ. 10" // LF // &
      "     2      .OR. ICHAR(JCH) .EQ. 13) THEN" // LF, err)
    call buf_append_str(out_buf, "          JPOS = JPOS + 1" // LF // "          GO TO ", err)
    call buf_append_i64(out_buf, int(l_ws, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "        END IF" // LF, err)
    call buf_append_str(out_buf, "      END IF" // LF, err)
  end subroutine emit_f77_skip_ws

  !> @brief Emit RFC 8259 JSON string reader/unescaper (including `uXXXX`) in FORTRAN 77.
  !> @param[inout] out_buf Target buffer.
  !> @param[in]    dst_var Destination CHARACTER variable (`JKEY` or `JSVAL`).
  !> @param[in]    len_var Destination decoded length variable (`JKL` or `JSL`).
  !> @param[in]    max_cap Capacity of dst_var.
  !> @param[inout] lbl     Running label counter.
  !> @param[inout] err     Error status.
  pure subroutine emit_f77_read_str_into(out_buf, dst_var, len_var, max_cap, lbl, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    character(len=*, kind=c_k), intent(in) :: dst_var
    character(len=*, kind=c_k), intent(in) :: len_var
    integer(i32), intent(in) :: max_cap
    integer(i32), intent(inout) :: lbl
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: l_ch, l_hx, l_dn
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    lbl = lbl + 3_i32
    l_ch = lbl - 2_i32
    l_hx = lbl - 1_i32
    l_dn = lbl

    call buf_append_str(out_buf, "      IF (JPOS .GT. JN .OR. JBUF(JPOS:JPOS) .NE. '""') THEN" // LF, err)
    call buf_append_str(out_buf, "        IERR = 1" // LF // "        RETURN" // LF, err)
    call buf_append_str(out_buf, "      END IF" // LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
    call buf_append_str(out_buf, "      " // dst_var // " = ' '" // LF, err)
    call buf_append_str(out_buf, "      " // len_var // " = 0" // LF, err)

    call emit_lbl_continue(out_buf, l_ch, err)
    call buf_append_str(out_buf, "      IF (JPOS .GT. JN) THEN" // LF, err)
    call buf_append_str(out_buf, "        IERR = 1" // LF // "        RETURN" // LF, err)
    call buf_append_str(out_buf, "      END IF" // LF, err)
    call buf_append_str(out_buf, "      JCH = JBUF(JPOS:JPOS)" // LF, err)
    call buf_append_str(out_buf, "      JPOS = JPOS + 1" // LF, err)
    call buf_append_str(out_buf, "      IF (JCH .EQ. '""') GO TO ", err)
    call buf_append_i64(out_buf, int(l_dn, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "      IF (ICHAR(JCH) .EQ. 92) THEN" // LF, err)
    call buf_append_str(out_buf, "        IF (JPOS .GT. JN) THEN" // LF, err)
    call buf_append_str(out_buf, "          IERR = 1" // LF // "          RETURN" // LF, err)
    call buf_append_str(out_buf, "        END IF" // LF, err)
    call buf_append_str(out_buf, "        JCH = JBUF(JPOS:JPOS)" // LF, err)
    call buf_append_str(out_buf, "        JPOS = JPOS + 1" // LF, err)
    call buf_append_str(out_buf, "        IF (JCH .EQ. 'n') THEN" // LF, err)
    call buf_append_str(out_buf, "          JCH = CHAR(10)" // LF, err)
    call buf_append_str(out_buf, "        ELSE IF (JCH .EQ. 'r') THEN" // LF, err)
    call buf_append_str(out_buf, "          JCH = CHAR(13)" // LF, err)
    call buf_append_str(out_buf, "        ELSE IF (JCH .EQ. 't') THEN" // LF, err)
    call buf_append_str(out_buf, "          JCH = CHAR(9)" // LF, err)
    call buf_append_str(out_buf, "        ELSE IF (JCH .EQ. 'b') THEN" // LF, err)
    call buf_append_str(out_buf, "          JCH = CHAR(8)" // LF, err)
    call buf_append_str(out_buf, "        ELSE IF (JCH .EQ. 'f') THEN" // LF, err)
    call buf_append_str(out_buf, "          JCH = CHAR(12)" // LF, err)
    call buf_append_str(out_buf, "        ELSE IF (JCH .EQ. 'u') THEN" // LF, err)
    call buf_append_str(out_buf, "          IF (JPOS + 3 .GT. JN) THEN" // LF, err)
    call buf_append_str(out_buf, "            IERR = 1" // LF // "            RETURN" // LF, err)
    call buf_append_str(out_buf, "          END IF" // LF, err)
    call buf_append_str(out_buf, "          JHEX = 0" // LF, err)
    call buf_append_str(out_buf, "          DO ", err)
    call buf_append_i64(out_buf, int(l_hx, i64), err)
    call buf_append_str(out_buf, " JHI = 0, 3" // LF, err)
    call buf_append_str(out_buf, "            JHV = ICHAR(JBUF(JPOS+JHI:JPOS+JHI))" // LF, err)
    call buf_append_str(out_buf, "            IF (JHV .GE. 48 .AND. JHV .LE. 57) THEN" // LF, err)
    call buf_append_str(out_buf, "              JHV = JHV - 48" // LF, err)
    call buf_append_str(out_buf, "            ELSE IF (JHV .GE. 65 .AND. JHV .LE. 70) THEN" // LF, err)
    call buf_append_str(out_buf, "              JHV = JHV - 55" // LF, err)
    call buf_append_str(out_buf, "            ELSE IF (JHV .GE. 97 .AND. JHV .LE. 102) THEN" // LF, err)
    call buf_append_str(out_buf, "              JHV = JHV - 87" // LF, err)
    call buf_append_str(out_buf, "            ELSE" // LF, err)
    call buf_append_str(out_buf, "              IERR = 1" // LF // "              RETURN" // LF, err)
    call buf_append_str(out_buf, "            END IF" // LF, err)
    call buf_append_str(out_buf, "            JHEX = JHEX * 16 + JHV" // LF, err)
    call emit_lbl_continue(out_buf, l_hx, err)
    call buf_append_str(out_buf, "          JCH = CHAR(MOD(JHEX, 128))" // LF, err)
    call buf_append_str(out_buf, "          JPOS = JPOS + 4" // LF, err)
    call buf_append_str(out_buf, "        END IF" // LF, err)
    call buf_append_str(out_buf, "      END IF" // LF, err)
    call buf_append_str(out_buf, "      IF (" // len_var // " .LT. ", err)
    call buf_append_i64(out_buf, int(max_cap, i64), err)
    call buf_append_str(out_buf, ") THEN" // LF, err)
    call buf_append_str(out_buf, "        " // len_var // " = " // len_var // " + 1" // LF, err)
    call buf_append_str(out_buf, "        " // dst_var // "(" // len_var // ":" // len_var // &
      ") = JCH" // LF, err)
    call buf_append_str(out_buf, "      END IF" // LF, err)
    call buf_append_str(out_buf, "      GO TO ", err)
    call buf_append_i64(out_buf, int(l_ch, i64), err)
    call buf_append_char(out_buf, LF, err)
    call emit_lbl_continue(out_buf, l_dn, err)
  end subroutine emit_f77_read_str_into

  !> @brief Emit balanced unknown-key JSON value skipper in FORTRAN 77.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] lbl     Running label counter.
  !> @param[inout] err     Error status.
  pure subroutine emit_f77_skip_val(out_buf, lbl, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(inout) :: lbl
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: l_c, l_p, l_dn
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    lbl = lbl + 3_i32
    l_c = lbl - 2_i32
    l_p = lbl - 1_i32
    l_dn = lbl

    call buf_append_str(out_buf, "        IF (JPOS .GT. JN) THEN" // LF, err)
    call buf_append_str(out_buf, "          IERR = 1" // LF // "          RETURN" // LF, err)
    call buf_append_str(out_buf, "        END IF" // LF, err)
    call buf_append_str(out_buf, "        IF (JBUF(JPOS:JPOS) .EQ. '""') THEN" // LF, err)
    call emit_f77_read_str_into(out_buf, "JSVAL", "JSL", 256_i32, lbl, err)
    call buf_append_str(out_buf, "        ELSE IF (JBUF(JPOS:JPOS) .EQ. '{'" // LF // &
      "     1      .OR. JBUF(JPOS:JPOS) .EQ. '[') THEN" // LF, err)
    call buf_append_str(out_buf, "          JDEP = 0" // LF // "          JSTRQ = 0" // LF, err)
    call emit_lbl_continue(out_buf, l_c, err)
    call buf_append_str(out_buf, "          IF (JPOS .GT. JN) THEN" // LF, err)
    call buf_append_str(out_buf, "            IERR = 1" // LF // "            RETURN" // LF, err)
    call buf_append_str(out_buf, "          END IF" // LF, err)
    call buf_append_str(out_buf, "          JCH = JBUF(JPOS:JPOS)" // LF, err)
    call buf_append_str(out_buf, "          IF (JSTRQ .EQ. 1) THEN" // LF, err)
    call buf_append_str(out_buf, "            IF (ICHAR(JCH) .EQ. 92) JPOS = JPOS + 1" // LF, err)
    call buf_append_str(out_buf, "            IF (JCH .EQ. '""') JSTRQ = 0" // LF, err)
    call buf_append_str(out_buf, "          ELSE" // LF, err)
    call buf_append_str(out_buf, "            IF (JCH .EQ. '""') JSTRQ = 1" // LF, err)
    call buf_append_str(out_buf, &
      "            IF (JCH .EQ. '{' .OR. JCH .EQ. '[') JDEP = JDEP + 1" // LF, err)
    call buf_append_str(out_buf, &
      "            IF (JCH .EQ. '}' .OR. JCH .EQ. ']') JDEP = JDEP - 1" // LF, err)
    call buf_append_str(out_buf, "          END IF" // LF, err)
    call buf_append_str(out_buf, "          JPOS = JPOS + 1" // LF, err)
    call buf_append_str(out_buf, "          IF (JDEP .GT. 0) GO TO ", err)
    call buf_append_i64(out_buf, int(l_c, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "        ELSE" // LF, err)
    call emit_lbl_continue(out_buf, l_p, err)
    call buf_append_str(out_buf, "          IF (JPOS .GT. JN) GO TO ", err)
    call buf_append_i64(out_buf, int(l_dn, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "          JCH = JBUF(JPOS:JPOS)" // LF, err)
    call buf_append_str(out_buf, "          IF (JCH .EQ. ',' .OR. JCH .EQ. '}'" // LF // &
      "     1        .OR. JCH .EQ. ' ') GO TO ", err)
    call buf_append_i64(out_buf, int(l_dn, i64), err)
    call buf_append_char(out_buf, LF, err)
    call buf_append_str(out_buf, "          JPOS = JPOS + 1" // LF // "          GO TO ", err)
    call buf_append_i64(out_buf, int(l_p, i64), err)
    call buf_append_char(out_buf, LF, err)
    call emit_lbl_continue(out_buf, l_dn, err)
    call buf_append_str(out_buf, "        END IF" // LF, err)
  end subroutine emit_f77_skip_val

  ! ===========================================================================
  ! Tier 3: Modern Fortran (F90, F95, F2003, F2008, F2018, F2023) JSON Codecs
  !         (Doxygen headers, PURE serializers, TBPs, SUBMODULEs, OOP EXTENDS)
  ! ===========================================================================

  !> @brief Emit a modern Fortran (`F90`..`F2023`) `<udt>_to_json` serializer subroutine.
  !> @param[in]    bundle    Canonical IR bundle.
  !> @param[in]    udt_idx   Target UDT index.
  !> @param[in]    options   Dialect options.
  !> @param[in]    is_submod True when emitting inside a F2008+ `SUBMODULE`.
  !> @param[inout] out_buf   Target buffer.
  !> @param[inout] err       Error status.
  pure subroutine emit_modern_to_json_proc(bundle, udt_idx, options, is_submod, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    integer(i32), intent(in) :: udt_idx
    type(cdd_options_t), intent(in) :: options
    logical(lk), intent(in) :: is_submod
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    type(cdd_dialect_caps_t) :: caps
    integer(i32) :: sid, ind, f, fn_len
    logical(lk) :: up, is_f03, is_poly
    character(len=64, kind=c_k) :: sub_nm, fr_nm
    integer(i32) :: sub_len, fr_len
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    sid = options%standard%id
    caps = get_dialect_capabilities(options%standard, options%allow_dec_structure)
    up = options%uppercase_keywords
    ind = max(2_i32, options%indent_width)
    is_f03 = (sid >= STD_FORTRAN_2003)
    is_poly = is_f03 .and. options%emit_type_bound_procs .and. &
              .not. bundle%udts(udt_idx)%is_bind_c
    fn_len = bundle%udts(udt_idx)%fortran_name%len
    call codec_routine_names(bundle%udts(udt_idx)%fortran_name%raw(1:fn_len), sid, &
      .false._lk, sub_nm, sub_len, fr_nm, fr_len)

    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "!> @brief Serialize " // &
      bundle%udts(udt_idx)%fortran_name%raw(1:fn_len) // " to RFC 8259 JSON." // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "!> @param[in]  self     UDT instance to serialize." // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "!> @param[out] json_str Output JSON string buffer." // LF, err)
    if (.not. is_f03) then
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "!> @param[out] json_len Written JSON byte length." // LF, err)
    end if
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "!> @param[out] ierr     Status (0=OK, 2=overflow)." // LF, err)
    if (is_f03) then
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "!> @param[out] errmsg   Optional diagnostic text." // LF, err)
    end if

    call emit_sp(out_buf, ind, err)
    if (caps%has_pure_procs) call emit_kw(out_buf, "pure ", up, err)
    call emit_kw(out_buf, "recursive ", up, err)
    if (is_submod) call emit_kw(out_buf, "module ", up, err)
    call emit_kw(out_buf, "subroutine ", up, err)
    call buf_append_str(out_buf, sub_nm(1:sub_len), err)
    if (is_f03) then
      call buf_append_str(out_buf, "(self, json_str, ierr, errmsg)" // LF, err)
    else
      call buf_append_str(out_buf, "(self, json_str, json_len, ierr)" // LF, err)
    end if

    call emit_sp(out_buf, ind * 2_i32, err)
    if (is_poly) then
      call emit_kw(out_buf, "class(", up, err)
    else
      call emit_kw(out_buf, "type(", up, err)
    end if
    call buf_append_str(out_buf, bundle%udts(udt_idx)%fortran_name%raw(1:fn_len), err)
    call emit_kw(out_buf, "), intent(in) :: self", up, err)
    call buf_append_char(out_buf, LF, err)

    call emit_sp(out_buf, ind * 2_i32, err)
    if (is_f03) then
      call emit_kw(out_buf, "character(len=:), allocatable, intent(out) :: json_str", up, err)
      call buf_append_char(out_buf, LF, err)
    else
      call emit_kw(out_buf, "character(len=*), intent(out) :: json_str", up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_sp(out_buf, ind * 2_i32, err)
      call emit_kw(out_buf, "integer(int32), intent(out) :: json_len", up, err)
      call buf_append_char(out_buf, LF, err)
    end if
    call emit_sp(out_buf, ind * 2_i32, err)
    call emit_kw(out_buf, "integer(int32), intent(out) :: ierr", up, err)
    call buf_append_char(out_buf, LF, err)
    if (is_f03) then
      call emit_sp(out_buf, ind * 2_i32, err)
      call emit_kw(out_buf, &
        "character(len=:), allocatable, intent(out), optional :: errmsg", up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_sp(out_buf, ind * 2_i32, err)
      call emit_kw(out_buf, "character(len=:), allocatable :: jsub", up, err)
      call buf_append_char(out_buf, LF, err)
    else
      call emit_sp(out_buf, ind * 2_i32, err)
      call emit_kw(out_buf, "character(len=4096) :: jsub", up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_sp(out_buf, ind * 2_i32, err)
      call emit_kw(out_buf, "integer(int32) :: jslen, jcap", up, err)
      call buf_append_char(out_buf, LF, err)
    end if
    call emit_sp(out_buf, ind * 2_i32, err)
    call emit_kw(out_buf, "character(len=64) :: jnbuf", up, err)
    call buf_append_char(out_buf, LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call emit_kw(out_buf, "character(len=1) :: jch", up, err)
    call buf_append_char(out_buf, LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call emit_kw(out_buf, "integer(int32) :: jk, jend, jidx, jidx2, jhas", up, err)
    call buf_append_char(out_buf, LF, err)

    ! Initialize all scratch vars cleanly so -Wall -Wextra -Werror never warns:
    call emit_sp(out_buf, ind * 2_i32, err)
    call buf_append_str(out_buf, "ierr = 0_int32; jk = 0_int32; jend = 0_int32; " // &
      "jidx = 0_int32; jidx2 = 0_int32; jhas = 0_int32" // LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call buf_append_str(out_buf, "jnbuf = ' '; jch = ' '" // LF, err)
    if (is_f03) then
      call emit_sp(out_buf, ind * 2_i32, err)
      call buf_append_str(out_buf, "jsub = ''; if (present(errmsg)) errmsg = ''" // LF, err)
      if (is_poly .and. bundle%udts(udt_idx)%field_count == 0_i32 .and. &
          .not. bundle%udts(udt_idx)%has_extends) then
        call emit_sp(out_buf, ind * 2_i32, err)
        call buf_append_str(out_buf, "if (.not. same_type_as(self, self)) ierr = 0_int32" // LF, err)
      end if
      if (bundle%udts(udt_idx)%has_extends .and. caps%has_oop_class_extends) then
        call emit_sp(out_buf, ind * 2_i32, err)
        call buf_append_str(out_buf, "call self%" // &
          bundle%udts(udt_idx)%extends_name%raw(1:bundle%udts(udt_idx)%extends_name%len) // &
          "%to_json(jsub, ierr, errmsg)" // LF, err)
        call emit_sp(out_buf, ind * 2_i32, err)
        call buf_append_str(out_buf, "if (ierr /= 0_int32) return" // LF, err)
        call emit_sp(out_buf, ind * 2_i32, err)
        call buf_append_str(out_buf, "if (len(jsub) > 2) then" // LF, err)
        call emit_sp(out_buf, ind * 3_i32, err)
        call buf_append_str(out_buf, "json_str = jsub(1:len(jsub)-1)" // LF, err)
        call emit_sp(out_buf, ind * 3_i32, err)
        call buf_append_str(out_buf, "jhas = 1_int32" // LF, err)
        call emit_sp(out_buf, ind * 2_i32, err)
        call buf_append_str(out_buf, "else" // LF, err)
        call emit_sp(out_buf, ind * 3_i32, err)
        call buf_append_str(out_buf, "json_str = '{'" // LF, err)
        call emit_sp(out_buf, ind * 2_i32, err)
        call buf_append_str(out_buf, "end if" // LF, err)
      else
        call emit_sp(out_buf, ind * 2_i32, err)
        call buf_append_str(out_buf, "json_str = '{'" // LF, err)
      end if
    else
      call emit_sp(out_buf, ind * 2_i32, err)
      call buf_append_str(out_buf, "jsub = ' '; jslen = 0_int32; json_len = 0_int32; " // &
        "json_str = ' '; jcap = int(len(json_str), int32)" // LF, err)
      call emit_mod_append_expr(out_buf, ind * 2_i32, is_f03, "'{'", err)
    end if
    call emit_sp(out_buf, ind * 2_i32, err)
    call buf_append_str(out_buf, "if (jk < 0_int32 .or. jend < 0_int32 .or. " // &
      "jidx < 0_int32 .or. jidx2 < 0_int32) ierr = 0_int32" // LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call buf_append_str(out_buf, "if (jnbuf(1:1) == char(0) .or. jch == char(0) .or. " // &
      "len(jsub) < 0) ierr = 0_int32" // LF, err)
    if (.not. is_f03) then
      call emit_sp(out_buf, ind * 2_i32, err)
      call buf_append_str(out_buf, "if (jslen < 0_int32) ierr = 0_int32" // LF, err)
    end if

    do f = 1_i32, bundle%udts(udt_idx)%field_count
      call emit_mod_field_out(bundle, bundle%udts(udt_idx)%fields(f), sid, caps, &
        ind * 2_i32, is_f03, out_buf, err)
    end do

    call emit_mod_append_expr(out_buf, ind * 2_i32, is_f03, "'}'", err)
    call emit_sp(out_buf, ind, err)
    call emit_kw(out_buf, "end subroutine ", up, err)
    call buf_append_str(out_buf, sub_nm(1:sub_len) // LF, err)
  end subroutine emit_modern_to_json_proc

  !> @brief Emit a pure F2003+ convenience function `<udt>_to_json_str(self) result(json_str)`.
  !> @param[in]    udt     Source UDT definition.
  !> @param[in]    options Dialect options.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_modern_to_json_func(udt, options, out_buf, err)
    type(cdd_ir_udt_t), intent(in) :: udt
    type(cdd_options_t), intent(in) :: options
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: ind, fn_len
    logical(lk) :: up
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    ind = max(2_i32, options%indent_width)
    up = options%uppercase_keywords
    fn_len = udt%fortran_name%len

    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "!> @brief Pure function wrapper returning JSON string for " // &
      udt%fortran_name%raw(1:fn_len) // "." // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "!> @param[in] self UDT instance." // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "!> @return Serialized JSON string." // LF, err)
    call emit_sp(out_buf, ind, err)
    call emit_kw(out_buf, "pure function ", up, err)
    call buf_append_str(out_buf, udt%fortran_name%raw(1:fn_len) // &
      "_to_json_str(self) result(json_str)" // LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call emit_kw(out_buf, "class(", up, err)
    call buf_append_str(out_buf, udt%fortran_name%raw(1:fn_len), err)
    call emit_kw(out_buf, "), intent(in) :: self", up, err)
    call buf_append_char(out_buf, LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call emit_kw(out_buf, "character(len=:), allocatable :: json_str", up, err)
    call buf_append_char(out_buf, LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call emit_kw(out_buf, "integer(int32) :: ierr", up, err)
    call buf_append_char(out_buf, LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call buf_append_str(out_buf, "call self%to_json(json_str, ierr)" // LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call buf_append_str(out_buf, "if (ierr /= 0_int32) json_str = '{}'" // LF, err)
    call emit_sp(out_buf, ind, err)
    call emit_kw(out_buf, "end function ", up, err)
    call buf_append_str(out_buf, udt%fortran_name%raw(1:fn_len) // "_to_json_str" // LF, err)
  end subroutine emit_modern_to_json_func

  !> @brief Emit serializer statements for one field in modern Fortran (`F90`..`F2023`).
  !> @param[in]    bundle  Canonical IR bundle.
  !> @param[in]    fld     Field descriptor.
  !> @param[in]    sid     Standard ID.
  !> @param[in]    caps    Dialect capabilities.
  !> @param[in]    ind     Indentation width.
  !> @param[in]    is_f03  True for deferred-length F2003+ string mode.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_mod_field_out(bundle, fld, sid, caps, ind, is_f03, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_field_t), intent(in) :: fld
    integer(i32), intent(in) :: sid
    type(cdd_dialect_caps_t), intent(in) :: caps
    integer(i32), intent(in) :: ind
    logical(lk), intent(in) :: is_f03
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    logical(lk) :: use_alloc, use_ptr
    character(len=80, kind=c_k) :: ref_b, ref_e
    integer(i32) :: rb_len, re_len
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call field_storage_flags(fld, caps, use_alloc, use_ptr)
    call copy_name("self%" // fld%fortran_name%raw(1:fld%fortran_name%len), ref_b, rb_len)

    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "if (jhas == 1_int32) then" // LF, err)
    call emit_mod_append_expr(out_buf, ind + 2_i32, is_f03, "','", err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "jhas = 1_int32" // LF, err)
    call emit_mod_append_expr(out_buf, ind, is_f03, "'" // '"' // &
      fld%json_name%raw(1:fld%json_name%len) // '":' // "'", err)

    if (use_alloc) then
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "if (.not. allocated(" // ref_b(1:rb_len) // ")) then" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 2_i32, is_f03, "'null'", err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "else" // LF, err)
    else if (use_ptr) then
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "if (.not. associated(" // ref_b(1:rb_len) // ")) then" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 2_i32, is_f03, "'null'", err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "else" // LF, err)
    end if

    if (fld%rank >= 2_i32) then
      call emit_mod_append_expr(out_buf, ind + 2_i32, is_f03, "'['", err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "do jidx = int(lbound(" // ref_b(1:rb_len) // &
        ", 1), int32), int(ubound(" // ref_b(1:rb_len) // ", 1), int32)" // LF, err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "if (jidx > int(lbound(" // ref_b(1:rb_len) // &
        ", 1), int32)) then" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 6_i32, is_f03, "','", err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "end if" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 4_i32, is_f03, "'['", err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "do jidx2 = int(lbound(" // ref_b(1:rb_len) // &
        ", 2), int32), int(ubound(" // ref_b(1:rb_len) // ", 2), int32)" // LF, err)
      call emit_sp(out_buf, ind + 6_i32, err)
      call buf_append_str(out_buf, "if (jidx2 > int(lbound(" // ref_b(1:rb_len) // &
        ", 2), int32)) then" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 8_i32, is_f03, "','", err)
      call emit_sp(out_buf, ind + 6_i32, err)
      call buf_append_str(out_buf, "end if" // LF, err)
      call copy_name(ref_b(1:rb_len) // "(jidx, jidx2)", ref_e, re_len)
      call emit_mod_val_out(bundle, fld, ref_e(1:re_len), sid, ind + 6_i32, is_f03, &
        .false._lk, out_buf, err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "end do" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 4_i32, is_f03, "']'", err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "end do" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 2_i32, is_f03, "']'", err)
    else if (fld%rank == 1_i32) then
      call emit_mod_append_expr(out_buf, ind + 2_i32, is_f03, "'['", err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "do jidx = int(lbound(" // ref_b(1:rb_len) // &
        ", 1), int32), int(ubound(" // ref_b(1:rb_len) // ", 1), int32)" // LF, err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "if (jidx > int(lbound(" // ref_b(1:rb_len) // &
        ", 1), int32)) then" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 6_i32, is_f03, "','", err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "end if" // LF, err)
      call copy_name(ref_b(1:rb_len) // "(jidx)", ref_e, re_len)
      call emit_mod_val_out(bundle, fld, ref_e(1:re_len), sid, ind + 4_i32, is_f03, &
        .false._lk, out_buf, err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "end do" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 2_i32, is_f03, "']'", err)
    else
      call emit_mod_val_out(bundle, fld, ref_b(1:rb_len), sid, ind + 2_i32, is_f03, &
        (fld%type_kind == IR_STRING_DEFERRED .and. caps%has_deferred_char), out_buf, err)
    end if

    if (use_alloc .or. use_ptr) then
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "end if" // LF, err)
    end if
  end subroutine emit_mod_field_out

  !> @brief Emit scalar/element JSON value serializer in modern Fortran (`F90`..`F2023`).
  !> @param[in]    bundle     Canonical IR bundle.
  !> @param[in]    fld        Field descriptor.
  !> @param[in]    expr       Fortran scalar/element expression.
  !> @param[in]    sid        Standard ID.
  !> @param[in]    ind        Indentation width.
  !> @param[in]    is_f03     True for deferred-length F2003+ string mode.
  !> @param[in]    exact_len  True when `len(expr)` should be used instead of `len_trim(expr)`.
  !> @param[inout] out_buf    Target buffer.
  !> @param[inout] err        Error status.
  pure subroutine emit_mod_val_out(bundle, fld, expr, sid, ind, is_f03, exact_len, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_field_t), intent(in) :: fld
    character(len=*, kind=c_k), intent(in) :: expr
    integer(i32), intent(in) :: sid
    integer(i32), intent(in) :: ind
    logical(lk), intent(in) :: is_f03
    logical(lk), intent(in) :: exact_len
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: e_idx, r_idx, k, to_len, fr_len
    character(len=64, kind=c_k) :: to_nm, fr_nm
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=1, kind=c_k), parameter :: BS = achar(92, kind=c_k)

    if (fld%type_kind == IR_STRING_FIXED .or. fld%type_kind == IR_STRING_DEFERRED) then
      call emit_mod_append_expr(out_buf, ind, is_f03, "'" // '"' // "'", err)
      call emit_sp(out_buf, ind, err)
      if (exact_len) then
        call buf_append_str(out_buf, "jend = int(len(" // expr // "), int32)" // LF, err)
      else
        call buf_append_str(out_buf, "jend = int(len_trim(" // expr // "), int32)" // LF, err)
      end if
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "do jk = 1_int32, jend" // LF, err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "jch = " // expr // "(jk:jk)" // LF, err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "if (jch == '""' .or. iachar(jch) == 92) then" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 4_i32, is_f03, "achar(92) // jch", err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "else if (iachar(jch) == 10) then" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 4_i32, is_f03, "'" // BS // "n'", err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "else if (iachar(jch) == 13) then" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 4_i32, is_f03, "'" // BS // "r'", err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "else if (iachar(jch) == 9) then" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 4_i32, is_f03, "'" // BS // "t'", err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "else" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 4_i32, is_f03, "jch", err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "end if" // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "end do" // LF, err)
      call emit_mod_append_expr(out_buf, ind, is_f03, "'" // '"' // "'", err)
    else if (fld%type_kind == IR_ENUM) then
      e_idx = ir_bundle_find_enum(bundle, fld%enum_name%raw(1:fld%enum_name%len))
      if (e_idx > 0_i32 .and. bundle%enums(max(1_i32, e_idx))%val_count > 0_i32) then
        do k = 1_i32, bundle%enums(e_idx)%val_count
          call emit_sp(out_buf, ind, err)
          if (k == 1_i32) then
            call buf_append_str(out_buf, "if (", err)
          else
            call buf_append_str(out_buf, "else if (", err)
          end if
          call buf_append_str(out_buf, expr // " == " // &
            bundle%enums(e_idx)%f_vals(k)%raw(1:bundle%enums(e_idx)%f_vals(k)%len) // &
            ") then" // LF, err)
          call emit_mod_append_expr(out_buf, ind + 2_i32, is_f03, "'" // '"' // &
            bundle%enums(e_idx)%json_vals(k)%raw(1:bundle%enums(e_idx)%json_vals(k)%len) // &
            '"' // "'", err)
        end do
        call emit_sp(out_buf, ind, err)
        call buf_append_str(out_buf, "else" // LF, err)
        call emit_mod_append_expr(out_buf, ind + 2_i32, is_f03, "'" // '"' // &
          bundle%enums(e_idx)%json_vals(1)%raw(1:bundle%enums(e_idx)%json_vals(1)%len) // &
          '"' // "'", err)
        call emit_sp(out_buf, ind, err)
        call buf_append_str(out_buf, "end if" // LF, err)
      else
        call emit_sp(out_buf, ind, err)
        call buf_append_str(out_buf, "write(jnbuf, '(I20)') " // expr // LF, err)
        call emit_mod_append_expr(out_buf, ind, is_f03, "trim(adjustl(jnbuf))", err)
      end if
    else if (fld%type_kind == IR_BOOL) then
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "if (" // expr // ") then" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 2_i32, is_f03, "'true'", err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "else" // LF, err)
      call emit_mod_append_expr(out_buf, ind + 2_i32, is_f03, "'false'", err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "end if" // LF, err)
    else if (fld%type_kind == IR_NESTED_UDT) then
      r_idx = ir_bundle_find_udt(bundle, fld%ref_udt_name%raw(1:fld%ref_udt_name%len))
      if (r_idx > 0_i32) then
        call codec_routine_names( &
          bundle%udts(r_idx)%fortran_name%raw(1:bundle%udts(r_idx)%fortran_name%len), &
          sid, .false._lk, to_nm, to_len, fr_nm, fr_len)
      else
        call codec_routine_names(fld%ref_udt_name%raw(1:fld%ref_udt_name%len), &
          sid, .false._lk, to_nm, to_len, fr_nm, fr_len)
      end if
      call emit_sp(out_buf, ind, err)
      if (is_f03) then
        call buf_append_str(out_buf, "call " // to_nm(1:to_len) // "(" // expr // &
          ", jsub, ierr, errmsg)" // LF, err)
        call emit_sp(out_buf, ind, err)
        call buf_append_str(out_buf, "if (ierr /= 0_int32) return" // LF, err)
        call emit_mod_append_expr(out_buf, ind, is_f03, "jsub", err)
      else
        call buf_append_str(out_buf, "call " // to_nm(1:to_len) // "(" // expr // &
          ", jsub, jslen, ierr)" // LF, err)
        call emit_sp(out_buf, ind, err)
        call buf_append_str(out_buf, "if (ierr /= 0_int32) return" // LF, err)
        call emit_mod_append_expr(out_buf, ind, is_f03, "jsub(1:jslen)", err)
      end if
    else if (fld%type_kind >= IR_REAL32 .and. fld%type_kind <= IR_REAL128) then
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "write(jnbuf, '(F32.6)') " // expr // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "jnbuf = adjustl(jnbuf)" // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "jend = int(len_trim(jnbuf), int32)" // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "do while (jend > 2_int32 .and. jnbuf(jend:jend) == '0' " // &
        ".and. jnbuf(jend-1:jend-1) /= '.')" // LF, err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "jend = jend - 1_int32" // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "end do" // LF, err)
      call emit_mod_append_expr(out_buf, ind, is_f03, "jnbuf(1:jend)", err)
    else if (fld%type_kind == IR_COMPLEX64 .or. fld%type_kind == IR_COMPLEX128) then
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "write(jnbuf, '(I20)') int(real(" // expr // "), int64)" // LF, err)
      call emit_mod_append_expr(out_buf, ind, is_f03, "trim(adjustl(jnbuf))", err)
    else
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "write(jnbuf, '(I24)') " // expr // LF, err)
      call emit_mod_append_expr(out_buf, ind, is_f03, "trim(adjustl(jnbuf))", err)
    end if
  end subroutine emit_mod_val_out

  !> @brief Emit character/substring append to `json_str` (allocatable in F2003+, bounded in F90/95).
  !> @param[inout] out_buf  Target buffer.
  !> @param[in]    ind      Indentation spaces.
  !> @param[in]    is_f03   True for F2003+ deferred-length `json_str = json_str // ...`.
  !> @param[in]    str_expr Fortran character expression to append.
  !> @param[inout] err      Error status.
  pure subroutine emit_mod_append_expr(out_buf, ind, is_f03, str_expr, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: ind
    logical(lk), intent(in) :: is_f03
    character(len=*, kind=c_k), intent(in) :: str_expr
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call emit_sp(out_buf, ind, err)
    if (is_f03) then
      call buf_append_str(out_buf, "json_str = json_str // (" // str_expr // ")" // LF, err)
    else
      call buf_append_str(out_buf, "if (json_len + int(len(" // str_expr // &
        "), int32) > jcap) then" // LF, err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "ierr = 2_int32; return" // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "end if" // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "json_str(json_len + 1_int32:json_len + int(len(" // &
        str_expr // "), int32)) = " // str_expr // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "json_len = json_len + int(len(" // str_expr // &
        "), int32)" // LF, err)
    end if
  end subroutine emit_mod_append_expr

  !> @brief Emit a modern Fortran (`F90`..`F2023`) `<udt>_from_json` deserializer subroutine.
  !> @param[in]    bundle    Canonical IR bundle.
  !> @param[in]    udt_idx   Target UDT index.
  !> @param[in]    options   Dialect options.
  !> @param[in]    is_submod True when emitting inside a F2008+ `SUBMODULE`.
  !> @param[inout] out_buf   Target buffer.
  !> @param[inout] err       Error status.
  pure subroutine emit_modern_from_json_proc(bundle, udt_idx, options, is_submod, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    integer(i32), intent(in) :: udt_idx
    type(cdd_options_t), intent(in) :: options
    logical(lk), intent(in) :: is_submod
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    type(cdd_dialect_caps_t) :: caps
    integer(i32) :: sid, ind, f, fn_len
    logical(lk) :: up, is_f03, is_poly, is_pure
    character(len=64, kind=c_k) :: to_nm, sub_nm
    integer(i32) :: to_len, sub_len
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    sid = options%standard%id
    caps = get_dialect_capabilities(options%standard, options%allow_dec_structure)
    up = options%uppercase_keywords
    ind = max(2_i32, options%indent_width)
    is_f03 = (sid >= STD_FORTRAN_2003)
    is_poly = is_f03 .and. options%emit_type_bound_procs .and. &
              .not. bundle%udts(udt_idx)%is_bind_c
    is_pure = caps%has_pure_procs .and. &
              (is_submod .or. .not. udt_uses_pointer(bundle, udt_idx, caps))
    fn_len = bundle%udts(udt_idx)%fortran_name%len
    call codec_routine_names(bundle%udts(udt_idx)%fortran_name%raw(1:fn_len), sid, &
      .false._lk, to_nm, to_len, sub_nm, sub_len)

    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "!> @brief Deserialize RFC 8259 JSON into " // &
      bundle%udts(udt_idx)%fortran_name%raw(1:fn_len) // "." // LF, err)
    call emit_sp(out_buf, ind, err)
    if (is_poly) then
      call buf_append_str(out_buf, "!> @param[inout] self     Target UDT instance." // LF, err)
    else
      call buf_append_str(out_buf, "!> @param[out]   self     Target UDT instance." // LF, err)
    end if
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "!> @param[in]    json_str Input JSON string." // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "!> @param[out]   ierr     Status (0=OK, 1=parse error)." // LF, err)
    if (is_f03) then
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "!> @param[out]   errmsg   Optional diagnostic text." // LF, err)
    end if

    call emit_sp(out_buf, ind, err)
    if (is_pure) call emit_kw(out_buf, "pure ", up, err)
    call emit_kw(out_buf, "recursive ", up, err)
    if (is_submod) call emit_kw(out_buf, "module ", up, err)
    call emit_kw(out_buf, "subroutine ", up, err)
    call buf_append_str(out_buf, sub_nm(1:sub_len), err)
    if (is_f03) then
      call buf_append_str(out_buf, "(self, json_str, ierr, errmsg)" // LF, err)
    else
      call buf_append_str(out_buf, "(self, json_str, ierr)" // LF, err)
    end if

    call emit_sp(out_buf, ind * 2_i32, err)
    if (is_poly) then
      call emit_kw(out_buf, "class(", up, err)
      call buf_append_str(out_buf, bundle%udts(udt_idx)%fortran_name%raw(1:fn_len), err)
      call emit_kw(out_buf, "), intent(inout) :: self", up, err)
    else
      call emit_kw(out_buf, "type(", up, err)
      call buf_append_str(out_buf, bundle%udts(udt_idx)%fortran_name%raw(1:fn_len), err)
      call emit_kw(out_buf, "), intent(out) :: self", up, err)
    end if
    call buf_append_char(out_buf, LF, err)

    call emit_sp(out_buf, ind * 2_i32, err)
    call emit_kw(out_buf, "character(len=*), intent(in) :: json_str", up, err)
    call buf_append_char(out_buf, LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call emit_kw(out_buf, "integer(int32), intent(out) :: ierr", up, err)
    call buf_append_char(out_buf, LF, err)
    if (is_f03) then
      call emit_sp(out_buf, ind * 2_i32, err)
      call emit_kw(out_buf, &
        "character(len=:), allocatable, intent(out), optional :: errmsg", up, err)
      call buf_append_char(out_buf, LF, err)
    end if

    call emit_sp(out_buf, ind * 2_i32, err)
    call emit_kw(out_buf, "character(len=128) :: jkey", up, err)
    call buf_append_char(out_buf, LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call emit_kw(out_buf, "character(len=1024) :: jsval", up, err)
    call buf_append_char(out_buf, LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call emit_kw(out_buf, "character(len=1) :: jch", up, err)
    call buf_append_char(out_buf, LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call emit_kw(out_buf, "integer :: jios", up, err)
    call buf_append_char(out_buf, LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call emit_kw(out_buf, "integer(int32) :: jn, jpos, jkl, jsl, jidx, jidx2, jte, " // &
      "jdep, jstrq, jhex, jhi, jhv, jcnt, jcnt2, jscan", up, err)
    call buf_append_char(out_buf, LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call emit_kw(out_buf, "integer(int64) :: jiv64", up, err)
    call buf_append_char(out_buf, LF, err)

    call emit_sp(out_buf, ind * 2_i32, err)
    call buf_append_str(out_buf, "ierr = 0_int32; jn = int(len(json_str), int32); " // &
      "jpos = 1_int32; jkl = 0_int32; jsl = 0_int32" // LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call buf_append_str(out_buf, "jidx = 0_int32; jidx2 = 0_int32; jte = 0_int32; " // &
      "jdep = 0_int32; jstrq = 0_int32; jhex = 0_int32" // LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call buf_append_str(out_buf, "jhi = 0_int32; jhv = 0_int32; jcnt = 0_int32; " // &
      "jcnt2 = 0_int32; jscan = 0_int32; jios = 0; jiv64 = 0_int64" // LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call buf_append_str(out_buf, "jkey = ' '; jsval = ' '; jch = ' '" // LF, err)
    if (is_f03) then
      call emit_sp(out_buf, ind * 2_i32, err)
      call buf_append_str(out_buf, "if (present(errmsg)) errmsg = ''" // LF, err)
      if (is_poly .and. bundle%udts(udt_idx)%field_count == 0_i32 .and. &
          .not. bundle%udts(udt_idx)%has_extends) then
        call emit_sp(out_buf, ind * 2_i32, err)
        call buf_append_str(out_buf, "if (.not. same_type_as(self, self)) ierr = 0_int32" // LF, err)
      end if
      if (bundle%udts(udt_idx)%has_extends .and. caps%has_oop_class_extends) then
        call emit_sp(out_buf, ind * 2_i32, err)
        call buf_append_str(out_buf, "call self%" // &
          bundle%udts(udt_idx)%extends_name%raw(1:bundle%udts(udt_idx)%extends_name%len) // &
          "%from_json(json_str, ierr, errmsg)" // LF, err)
        call emit_sp(out_buf, ind * 2_i32, err)
        call buf_append_str(out_buf, "if (ierr /= 0_int32) return" // LF, err)
      end if
    end if
    call emit_sp(out_buf, ind * 2_i32, err)
    call buf_append_str(out_buf, "if (jidx < 0_int32 .or. jidx2 < 0_int32 .or. " // &
      "jte < 0_int32 .or. jcnt < 0_int32 .or. jcnt2 < 0_int32) ierr = 0_int32" // LF, err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call buf_append_str(out_buf, "if (jscan < 0_int32 .or. jios < -999 .or. " // &
      "jiv64 < -9_int64 .or. jsval(1:1) == char(0)) ierr = 0_int32" // LF, err)

    ! Initialize pointers in F90/95/non-poly intent(out) so associated() is well-defined:
    do f = 1_i32, bundle%udts(udt_idx)%field_count
      if (bundle%udts(udt_idx)%fields(f)%is_pointer .or. &
          ((bundle%udts(udt_idx)%fields(f)%is_allocatable .or. &
            bundle%udts(udt_idx)%fields(f)%is_nullable) .and. .not. caps%has_allocatable_comp)) then
        call emit_sp(out_buf, ind * 2_i32, err)
        call buf_append_str(out_buf, "nullify(self%" // &
          bundle%udts(udt_idx)%fields(f)%fortran_name%raw( &
          1:bundle%udts(udt_idx)%fields(f)%fortran_name%len) // ")" // LF, err)
      end if
    end do

    call emit_mod_skip_ws(out_buf, ind * 2_i32, err)
    call emit_mod_expect_ch(out_buf, ind * 2_i32, is_f03, "{", err)

    call emit_sp(out_buf, ind * 2_i32, err)
    call buf_append_str(out_buf, "do" // LF, err)
    call emit_mod_skip_ws(out_buf, ind * 3_i32, err)
    call emit_sp(out_buf, ind * 3_i32, err)
    call buf_append_str(out_buf, "if (jpos > jn) then" // LF, err)
    call emit_mod_fail(out_buf, ind * 4_i32, is_f03, "Unexpected EOF in JSON object", err)
    call emit_sp(out_buf, ind * 3_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind * 3_i32, err)
    call buf_append_str(out_buf, "if (json_str(jpos:jpos) == '}') then" // LF, err)
    call emit_sp(out_buf, ind * 4_i32, err)
    call buf_append_str(out_buf, "jpos = jpos + 1_int32; exit" // LF, err)
    call emit_sp(out_buf, ind * 3_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)

    call emit_mod_read_str(out_buf, ind * 3_i32, is_f03, "jkey", "jkl", 128_i32, err)
    call emit_mod_skip_ws(out_buf, ind * 3_i32, err)
    call emit_mod_expect_ch(out_buf, ind * 3_i32, is_f03, ":", err)
    call emit_mod_skip_ws(out_buf, ind * 3_i32, err)

    if (bundle%udts(udt_idx)%field_count == 0_i32) then
      call emit_mod_skip_val(out_buf, ind * 3_i32, is_f03, err)
    else
      do f = 1_i32, bundle%udts(udt_idx)%field_count
        call emit_sp(out_buf, ind * 3_i32, err)
        if (f == 1_i32) then
          call buf_append_str(out_buf, "if (jkl == ", err)
        else
          call buf_append_str(out_buf, "else if (jkl == ", err)
        end if
        call buf_append_i64(out_buf, int(bundle%udts(udt_idx)%fields(f)%json_name%len, i64), err)
        call buf_append_str(out_buf, "_int32 .and. jkey(1:jkl) == '" // &
          bundle%udts(udt_idx)%fields(f)%json_name%raw( &
          1:bundle%udts(udt_idx)%fields(f)%json_name%len) // "') then" // LF, err)
        call emit_mod_field_in(bundle, bundle%udts(udt_idx)%fields(f), sid, caps, &
          ind * 4_i32, is_f03, out_buf, err)
      end do
      call emit_sp(out_buf, ind * 3_i32, err)
      call buf_append_str(out_buf, "else" // LF, err)
      call emit_mod_skip_val(out_buf, ind * 4_i32, is_f03, err)
      call emit_sp(out_buf, ind * 3_i32, err)
      call buf_append_str(out_buf, "end if" // LF, err)
    end if

    call emit_mod_skip_ws(out_buf, ind * 3_i32, err)
    call emit_sp(out_buf, ind * 3_i32, err)
    call buf_append_str(out_buf, "if (jpos > jn) then" // LF, err)
    call emit_mod_fail(out_buf, ind * 4_i32, is_f03, "Unterminated JSON object", err)
    call emit_sp(out_buf, ind * 3_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind * 3_i32, err)
    call buf_append_str(out_buf, "if (json_str(jpos:jpos) == '}') then" // LF, err)
    call emit_sp(out_buf, ind * 4_i32, err)
    call buf_append_str(out_buf, "jpos = jpos + 1_int32; exit" // LF, err)
    call emit_sp(out_buf, ind * 3_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_mod_expect_ch(out_buf, ind * 3_i32, is_f03, ",", err)
    call emit_sp(out_buf, ind * 2_i32, err)
    call buf_append_str(out_buf, "end do" // LF, err)

    call emit_sp(out_buf, ind, err)
    call emit_kw(out_buf, "end subroutine ", up, err)
    call buf_append_str(out_buf, sub_nm(1:sub_len) // LF, err)
  end subroutine emit_modern_from_json_proc

  !> @brief Emit field deserializer statements (including null/allocatable/pointer/1D/2D arrays).
  !> @param[in]    bundle  Canonical IR bundle.
  !> @param[in]    fld     Field descriptor.
  !> @param[in]    sid     Standard ID.
  !> @param[in]    caps    Dialect capabilities.
  !> @param[in]    ind     Indentation width.
  !> @param[in]    is_f03  True for F2003+ mode.
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_mod_field_in(bundle, fld, sid, caps, ind, is_f03, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_field_t), intent(in) :: fld
    integer(i32), intent(in) :: sid
    type(cdd_dialect_caps_t), intent(in) :: caps
    integer(i32), intent(in) :: ind
    logical(lk), intent(in) :: is_f03
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    logical(lk) :: use_alloc, use_ptr
    integer(i32) :: ub1, ub2, rb_len, re_len
    character(len=80, kind=c_k) :: ref_b, ref_e
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call field_storage_flags(fld, caps, use_alloc, use_ptr)
    call copy_name("self%" // fld%fortran_name%raw(1:fld%fortran_name%len), ref_b, rb_len)

    if (use_alloc .or. use_ptr) then
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "if (jpos + 3_int32 <= jn .and. " // &
        "json_str(jpos:min(jn, jpos + 3_int32)) == 'null') then" // LF, err)
      call emit_sp(out_buf, ind + 2_i32, err)
      if (use_alloc) then
        call buf_append_str(out_buf, "if (allocated(" // ref_b(1:rb_len) // &
          ")) deallocate(" // ref_b(1:rb_len) // ")" // LF, err)
      else
        call buf_append_str(out_buf, "nullify(" // ref_b(1:rb_len) // ")" // LF, err)
      end if
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "jpos = jpos + 4_int32" // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "else" // LF, err)
    end if

    if (fld%rank >= 2_i32) then
      call emit_mod_expect_ch(out_buf, ind + 2_i32, is_f03, "[", err)
      call emit_mod_skip_ws(out_buf, ind + 2_i32, err)
      if (use_alloc .or. use_ptr) then
        call emit_mod_count_2d(out_buf, ind + 2_i32, err)
        call emit_sp(out_buf, ind + 2_i32, err)
        if (use_alloc) then
          call buf_append_str(out_buf, "if (allocated(" // ref_b(1:rb_len) // &
            ")) deallocate(" // ref_b(1:rb_len) // ")" // LF, err)
          call emit_sp(out_buf, ind + 2_i32, err)
        end if
        call buf_append_str(out_buf, "allocate(" // ref_b(1:rb_len) // "(jcnt, jcnt2))" // LF, err)
      else
        ub1 = max(1_i32, fld%dims(1)%upper_bound)
        ub2 = max(1_i32, fld%dims(2)%upper_bound)
        call emit_sp(out_buf, ind + 2_i32, err)
        call buf_append_str(out_buf, "jcnt = ", err)
        call buf_append_i64(out_buf, int(ub1, i64), err)
        call buf_append_str(out_buf, "_int32; jcnt2 = ", err)
        call buf_append_i64(out_buf, int(ub2, i64), err)
        call buf_append_str(out_buf, "_int32" // LF, err)
      end if
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "do jidx = 1_int32, jcnt" // LF, err)
      call emit_mod_skip_ws(out_buf, ind + 4_i32, err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "if (jidx > 1_int32) then" // LF, err)
      call emit_mod_expect_ch(out_buf, ind + 6_i32, is_f03, ",", err)
      call emit_mod_skip_ws(out_buf, ind + 6_i32, err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "end if" // LF, err)
      call emit_mod_expect_ch(out_buf, ind + 4_i32, is_f03, "[", err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "do jidx2 = 1_int32, jcnt2" // LF, err)
      call emit_mod_skip_ws(out_buf, ind + 6_i32, err)
      call emit_sp(out_buf, ind + 6_i32, err)
      call buf_append_str(out_buf, "if (jidx2 > 1_int32) then" // LF, err)
      call emit_mod_expect_ch(out_buf, ind + 8_i32, is_f03, ",", err)
      call emit_mod_skip_ws(out_buf, ind + 8_i32, err)
      call emit_sp(out_buf, ind + 6_i32, err)
      call buf_append_str(out_buf, "end if" // LF, err)
      call copy_name(ref_b(1:rb_len) // "(jidx, jidx2)", ref_e, re_len)
      call emit_mod_val_in(bundle, fld, ref_e(1:re_len), sid, ind + 6_i32, is_f03, &
        .false._lk, out_buf, err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "end do" // LF, err)
      call emit_mod_skip_ws(out_buf, ind + 4_i32, err)
      call emit_mod_expect_ch(out_buf, ind + 4_i32, is_f03, "]", err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "end do" // LF, err)
      call emit_mod_skip_ws(out_buf, ind + 2_i32, err)
      call emit_mod_expect_ch(out_buf, ind + 2_i32, is_f03, "]", err)
    else if (fld%rank == 1_i32) then
      call emit_mod_expect_ch(out_buf, ind + 2_i32, is_f03, "[", err)
      call emit_mod_skip_ws(out_buf, ind + 2_i32, err)
      if (use_alloc .or. use_ptr) then
        call emit_mod_count_1d(out_buf, ind + 2_i32, err)
        call emit_sp(out_buf, ind + 2_i32, err)
        if (use_alloc) then
          call buf_append_str(out_buf, "if (allocated(" // ref_b(1:rb_len) // &
            ")) deallocate(" // ref_b(1:rb_len) // ")" // LF, err)
          call emit_sp(out_buf, ind + 2_i32, err)
        end if
        call buf_append_str(out_buf, "allocate(" // ref_b(1:rb_len) // "(jcnt))" // LF, err)
      else
        ub1 = max(1_i32, fld%dims(1)%upper_bound)
        call emit_sp(out_buf, ind + 2_i32, err)
        call buf_append_str(out_buf, "jcnt = ", err)
        call buf_append_i64(out_buf, int(ub1, i64), err)
        call buf_append_str(out_buf, "_int32" // LF, err)
      end if
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "do jidx = 1_int32, jcnt" // LF, err)
      call emit_mod_skip_ws(out_buf, ind + 4_i32, err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "if (jidx > 1_int32) then" // LF, err)
      call emit_mod_expect_ch(out_buf, ind + 6_i32, is_f03, ",", err)
      call emit_mod_skip_ws(out_buf, ind + 6_i32, err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "end if" // LF, err)
      call copy_name(ref_b(1:rb_len) // "(jidx)", ref_e, re_len)
      call emit_mod_val_in(bundle, fld, ref_e(1:re_len), sid, ind + 4_i32, is_f03, &
        .false._lk, out_buf, err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "end do" // LF, err)
      call emit_mod_skip_ws(out_buf, ind + 2_i32, err)
      call emit_mod_expect_ch(out_buf, ind + 2_i32, is_f03, "]", err)
    else
      if (use_alloc .and. fld%type_kind /= IR_STRING_DEFERRED) then
        call emit_sp(out_buf, ind + 2_i32, err)
        call buf_append_str(out_buf, "if (.not. allocated(" // ref_b(1:rb_len) // &
          ")) allocate(" // ref_b(1:rb_len) // ")" // LF, err)
      else if (use_ptr) then
        call emit_sp(out_buf, ind + 2_i32, err)
        call buf_append_str(out_buf, "allocate(" // ref_b(1:rb_len) // ")" // LF, err)
      end if
      call emit_mod_val_in(bundle, fld, ref_b(1:rb_len), sid, ind + 2_i32, is_f03, &
        (fld%type_kind == IR_STRING_DEFERRED .and. caps%has_deferred_char), out_buf, err)
    end if

    if (use_alloc .or. use_ptr) then
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "end if" // LF, err)
    end if
  end subroutine emit_mod_field_in

  !> @brief Emit scalar/element JSON value parser in modern Fortran (`F90`..`F2023`).
  !> @param[in]    bundle   Canonical IR bundle.
  !> @param[in]    fld      Field descriptor.
  !> @param[in]    expr     Target Fortran scalar/element expression.
  !> @param[in]    sid      Standard ID.
  !> @param[in]    ind      Indentation spaces.
  !> @param[in]    is_f03   True for F2003+ mode.
  !> @param[in]    is_def_c True for deferred-length allocatable character assignment.
  !> @param[inout] out_buf  Target buffer.
  !> @param[inout] err      Error status.
  pure subroutine emit_mod_val_in(bundle, fld, expr, sid, ind, is_f03, is_def_c, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_field_t), intent(in) :: fld
    character(len=*, kind=c_k), intent(in) :: expr
    integer(i32), intent(in) :: sid
    integer(i32), intent(in) :: ind
    logical(lk), intent(in) :: is_f03
    logical(lk), intent(in) :: is_def_c
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: e_idx, r_idx, k, to_len, fr_len
    character(len=64, kind=c_k) :: to_nm, fr_nm
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    if (fld%type_kind == IR_STRING_FIXED .or. fld%type_kind == IR_STRING_DEFERRED) then
      call emit_mod_read_str(out_buf, ind, is_f03, "jsval", "jsl", 1024_i32, err)
      call emit_sp(out_buf, ind, err)
      if (is_def_c) then
        call buf_append_str(out_buf, "if (jsl > 0_int32) then" // LF, err)
        call emit_sp(out_buf, ind + 2_i32, err)
        call buf_append_str(out_buf, expr // " = jsval(1:jsl)" // LF, err)
        call emit_sp(out_buf, ind, err)
        call buf_append_str(out_buf, "else" // LF, err)
        call emit_sp(out_buf, ind + 2_i32, err)
        call buf_append_str(out_buf, expr // " = ''" // LF, err)
        call emit_sp(out_buf, ind, err)
        call buf_append_str(out_buf, "end if" // LF, err)
      else
        call buf_append_str(out_buf, expr // " = ' '" // LF, err)
        call emit_sp(out_buf, ind, err)
        call buf_append_str(out_buf, "if (jsl > 0_int32) " // expr // " = jsval(1:jsl)" // LF, err)
      end if
    else if (fld%type_kind == IR_ENUM) then
      e_idx = ir_bundle_find_enum(bundle, fld%enum_name%raw(1:fld%enum_name%len))
      if (e_idx > 0_i32 .and. bundle%enums(max(1_i32, e_idx))%val_count > 0_i32) then
        call emit_mod_read_str(out_buf, ind, is_f03, "jsval", "jsl", 1024_i32, err)
        do k = 1_i32, bundle%enums(e_idx)%val_count
          call emit_sp(out_buf, ind, err)
          if (k == 1_i32) then
            call buf_append_str(out_buf, "if (jsl == ", err)
          else
            call buf_append_str(out_buf, "else if (jsl == ", err)
          end if
          call buf_append_i64(out_buf, int(bundle%enums(e_idx)%json_vals(k)%len, i64), err)
          call buf_append_str(out_buf, "_int32 .and. jsval(1:jsl) == '" // &
            bundle%enums(e_idx)%json_vals(k)%raw(1:bundle%enums(e_idx)%json_vals(k)%len) // &
            "') then" // LF, err)
          call emit_sp(out_buf, ind + 2_i32, err)
          call buf_append_str(out_buf, expr // " = " // &
            bundle%enums(e_idx)%f_vals(k)%raw(1:bundle%enums(e_idx)%f_vals(k)%len) // LF, err)
        end do
        call emit_sp(out_buf, ind, err)
        call buf_append_str(out_buf, "else" // LF, err)
        call emit_mod_fail(out_buf, ind + 2_i32, is_f03, "Invalid enum literal", err)
        call emit_sp(out_buf, ind, err)
        call buf_append_str(out_buf, "end if" // LF, err)
      else
        call emit_mod_num_in(out_buf, ind, is_f03, expr, .false._lk, err)
      end if
    else if (fld%type_kind == IR_BOOL) then
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "if (jpos + 3_int32 <= jn .and. " // &
        "json_str(jpos:min(jn, jpos + 3_int32)) == 'true') then" // LF, err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, expr // " = .true.; jpos = jpos + 4_int32" // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "else if (jpos + 4_int32 <= jn .and. " // &
        "json_str(jpos:min(jn, jpos + 4_int32)) == 'false') then" // LF, err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, expr // " = .false.; jpos = jpos + 5_int32" // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "else" // LF, err)
      call emit_mod_fail(out_buf, ind + 2_i32, is_f03, "Expected boolean true/false", err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "end if" // LF, err)
    else if (fld%type_kind == IR_NESTED_UDT) then
      r_idx = ir_bundle_find_udt(bundle, fld%ref_udt_name%raw(1:fld%ref_udt_name%len))
      if (r_idx > 0_i32) then
        call codec_routine_names( &
          bundle%udts(r_idx)%fortran_name%raw(1:bundle%udts(r_idx)%fortran_name%len), &
          sid, .false._lk, to_nm, to_len, fr_nm, fr_len)
      else
        call codec_routine_names(fld%ref_udt_name%raw(1:fld%ref_udt_name%len), &
          sid, .false._lk, to_nm, to_len, fr_nm, fr_len)
      end if
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "if (jpos > jn) then" // LF, err)
      call emit_mod_fail(out_buf, ind + 2_i32, is_f03, "Expected nested JSON object", err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "end if" // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "if (json_str(jpos:jpos) /= '{') then" // LF, err)
      call emit_mod_fail(out_buf, ind + 2_i32, is_f03, "Expected '{' for nested object", err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "end if" // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "jte = jpos; jdep = 0_int32; jstrq = 0_int32" // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "do" // LF, err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "if (jte > jn) then" // LF, err)
      call emit_mod_fail(out_buf, ind + 4_i32, is_f03, "Unterminated nested object", err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "end if" // LF, err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "jch = json_str(jte:jte)" // LF, err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "if (jstrq == 1_int32) then" // LF, err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "if (iachar(jch) == 92) jte = jte + 1_int32" // LF, err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "if (jch == '""') jstrq = 0_int32" // LF, err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "else" // LF, err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "if (jch == '""') jstrq = 1_int32" // LF, err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "if (jch == '{') jdep = jdep + 1_int32" // LF, err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "if (jch == '}') jdep = jdep - 1_int32" // LF, err)
      call emit_sp(out_buf, ind + 4_i32, err)
      call buf_append_str(out_buf, "if (jdep == 0_int32) exit" // LF, err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "end if" // LF, err)
      call emit_sp(out_buf, ind + 2_i32, err)
      call buf_append_str(out_buf, "jte = jte + 1_int32" // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "end do" // LF, err)
      call emit_sp(out_buf, ind, err)
      if (is_f03) then
        call buf_append_str(out_buf, "call " // fr_nm(1:fr_len) // "(" // expr // &
          ", json_str(jpos:jte), ierr, errmsg)" // LF, err)
      else
        call buf_append_str(out_buf, "call " // fr_nm(1:fr_len) // "(" // expr // &
          ", json_str(jpos:jte), ierr)" // LF, err)
      end if
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "if (ierr /= 0_int32) return" // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, "jpos = jte + 1_int32" // LF, err)
    else
      call emit_mod_num_in(out_buf, ind, is_f03, expr, &
        (fld%type_kind == IR_COMPLEX64 .or. fld%type_kind == IR_COMPLEX128), err)
    end if
  end subroutine emit_mod_val_in

  !> @brief Emit numeric token scanner + internal `read(..., iostat=jios)` in modern Fortran.
  !> @param[inout] out_buf Target buffer.
  !> @param[in]    ind     Indentation width.
  !> @param[in]    is_f03  True for F2003+ errmsg mode.
  !> @param[in]    expr    Target variable expression.
  !> @param[in]    is_cplx True when target has Fortran COMPLEX type.
  !> @param[inout] err     Error status.
  pure subroutine emit_mod_num_in(out_buf, ind, is_f03, expr, is_cplx, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: ind
    logical(lk), intent(in) :: is_f03
    character(len=*, kind=c_k), intent(in) :: expr
    logical(lk), intent(in) :: is_cplx
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "jte = jpos" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "do while (jte <= jn)" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "jch = json_str(jte:jte)" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "if ((jch >= '0' .and. jch <= '9') .or. jch == '-' .or. &" // &
      LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "jch == '+' .or. jch == '.' .or. jch == 'e' .or. " // &
      "jch == 'E') then" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "jte = jte + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "else" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "exit" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "end do" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "if (jte <= jpos) then" // LF, err)
    call emit_mod_fail(out_buf, ind + 2_i32, is_f03, "Expected numeric value", err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind, err)
    if (is_cplx) then
      call buf_append_str(out_buf, "read(json_str(jpos:jte-1_int32), *, iostat=jios) jiv64" // LF, err)
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, expr // " = cmplx(real(jiv64, real32), 0.0_real32)" // LF, err)
    else
      call buf_append_str(out_buf, "read(json_str(jpos:jte-1_int32), *, iostat=jios) " // expr // LF, err)
    end if
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "if (jios /= 0) then" // LF, err)
    call emit_mod_fail(out_buf, ind + 2_i32, is_f03, "Malformed numeric literal", err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "jpos = jte" // LF, err)
  end subroutine emit_mod_num_in

  !> @brief Emit dynamic 1D JSON array element counter (`jcnt`) starting right after `[`.
  !> @param[inout] out_buf Target buffer.
  !> @param[in]    ind     Indentation width.
  !> @param[inout] err     Error status.
  pure subroutine emit_mod_count_1d(out_buf, ind, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: ind
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "jcnt = 0_int32" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "if (jpos <= jn) then" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "if (json_str(jpos:jpos) /= ']') then" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "jcnt = 1_int32; jscan = jpos; jdep = 0_int32; jstrq = 0_int32" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "do while (jscan <= jn)" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "jch = json_str(jscan:jscan)" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "if (jstrq == 1_int32) then" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "if (iachar(jch) == 92) jscan = jscan + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "if (jch == '""') jstrq = 0_int32" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "else" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "if (jch == '""') jstrq = 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "if (jch == '[' .or. jch == '{') jdep = jdep + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "if (jch == ']' .and. jdep == 0_int32) exit" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "if (jch == ']' .or. jch == '}') jdep = jdep - 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "if (jch == ',' .and. jdep == 0_int32) jcnt = jcnt + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "jscan = jscan + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "end do" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "end if" // LF, err)
  end subroutine emit_mod_count_1d

  !> @brief Emit dynamic 2D JSON nested-array shape counter (`jcnt`, `jcnt2`) after outer `[`.
  !> @param[inout] out_buf Target buffer.
  !> @param[in]    ind     Indentation width.
  !> @param[inout] err     Error status.
  pure subroutine emit_mod_count_2d(out_buf, ind, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: ind
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call emit_mod_count_1d(out_buf, ind, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "jcnt2 = 0_int32; jscan = jpos" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, &
      "do while (jscan <= jn .and. json_str(min(jn,jscan):min(jn,jscan)) /= '[')" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "jscan = jscan + 1_int32" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "end do" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "jscan = jscan + 1_int32" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, &
      "do while (jscan <= jn .and. json_str(min(jn,jscan):min(jn,jscan)) == ' ')" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "jscan = jscan + 1_int32" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "end do" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "if (jcnt > 0_int32 .and. jscan <= jn) then" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "if (json_str(jscan:jscan) /= ']') then" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "jcnt2 = 1_int32; jdep = 0_int32; jstrq = 0_int32" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "do while (jscan <= jn)" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "jch = json_str(jscan:jscan)" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "if (jstrq == 1_int32) then" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "if (iachar(jch) == 92) jscan = jscan + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "if (jch == '""') jstrq = 0_int32" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "else" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "if (jch == '""') jstrq = 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "if (jch == '[' .or. jch == '{') jdep = jdep + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "if (jch == ']' .and. jdep == 0_int32) exit" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "if (jch == ']' .or. jch == '}') jdep = jdep - 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "if (jch == ',' .and. jdep == 0_int32) jcnt2 = jcnt2 + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "jscan = jscan + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "end do" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "end if" // LF, err)
  end subroutine emit_mod_count_2d

  !> @brief Emit modern Fortran whitespace skipper (`' '`, TAB, LF, CR).
  !> @param[inout] out_buf Target buffer.
  !> @param[in]    ind     Indentation width.
  !> @param[inout] err     Error status.
  pure subroutine emit_mod_skip_ws(out_buf, ind, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: ind
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "do while (jpos <= jn)" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "jch = json_str(jpos:jpos)" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "if (jch == ' ' .or. iachar(jch) == 9 .or. " // &
      "iachar(jch) == 10 .or. iachar(jch) == 13) then" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "jpos = jpos + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "else" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "exit" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "end do" // LF, err)
  end subroutine emit_mod_skip_ws

  !> @brief Emit single-character token check (`json_str(jpos:jpos) == ch`) in modern Fortran.
  !> @param[inout] out_buf Target buffer.
  !> @param[in]    ind     Indentation width.
  !> @param[in]    is_f03  True for F2003+ errmsg mode.
  !> @param[in]    ch      Expected character.
  !> @param[inout] err     Error status.
  pure subroutine emit_mod_expect_ch(out_buf, ind, is_f03, ch, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: ind
    logical(lk), intent(in) :: is_f03
    character(len=1, kind=c_k), intent(in) :: ch
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "if (jpos > jn) then" // LF, err)
    call emit_mod_fail(out_buf, ind + 2_i32, is_f03, "Unexpected end of JSON input", err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "if (json_str(jpos:jpos) /= '" // ch // "') then" // LF, err)
    call emit_mod_fail(out_buf, ind + 2_i32, is_f03, "Unexpected JSON delimiter", err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "jpos = jpos + 1_int32" // LF, err)
  end subroutine emit_mod_expect_ch

  !> @brief Emit RFC 8259 string reader and unescaper (including unicode hex uXXXX) in F90..2023.
  !> @param[inout] out_buf Target buffer.
  !> @param[in]    ind     Indentation width.
  !> @param[in]    is_f03  True for F2003+ errmsg mode.
  !> @param[in]    dst_v   Destination string variable (`jkey` or `jsval`).
  !> @param[in]    len_v   Destination length variable (`jkl` or `jsl`).
  !> @param[in]    max_cap Buffer capacity of dst_v.
  !> @param[inout] err     Error status.
  pure subroutine emit_mod_read_str(out_buf, ind, is_f03, dst_v, len_v, max_cap, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: ind
    logical(lk), intent(in) :: is_f03
    character(len=*, kind=c_k), intent(in) :: dst_v
    character(len=*, kind=c_k), intent(in) :: len_v
    integer(i32), intent(in) :: max_cap
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call emit_mod_expect_ch(out_buf, ind, is_f03, '"', err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, dst_v // " = ' '; " // len_v // " = 0_int32" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "do" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "if (jpos > jn) then" // LF, err)
    call emit_mod_fail(out_buf, ind + 4_i32, is_f03, "Unterminated JSON string", err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "jch = json_str(jpos:jpos); jpos = jpos + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "if (jch == '""') exit" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "if (iachar(jch) == 92) then" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "if (jpos > jn) then" // LF, err)
    call emit_mod_fail(out_buf, ind + 6_i32, is_f03, "Truncated string escape", err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "jch = json_str(jpos:jpos); jpos = jpos + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "if (jch == 'n') then" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "jch = achar(10)" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "else if (jch == 'r') then" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "jch = achar(13)" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "else if (jch == 't') then" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "jch = achar(9)" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "else if (jch == 'b') then" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "jch = achar(8)" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "else if (jch == 'f') then" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "jch = achar(12)" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "else if (jch == 'u') then" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "if (jpos + 3_int32 > jn) then" // LF, err)
    call emit_mod_fail(out_buf, ind + 8_i32, is_f03, "Truncated unicode escape", err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "jhex = 0_int32" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "do jhi = 0_int32, 3_int32" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "jhv = int(iachar(json_str(jpos+jhi:jpos+jhi)), int32)" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "if (jhv >= 48_int32 .and. jhv <= 57_int32) then" // LF, err)
    call emit_sp(out_buf, ind + 10_i32, err)
    call buf_append_str(out_buf, "jhv = jhv - 48_int32" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "else if (jhv >= 65_int32 .and. jhv <= 70_int32) then" // LF, err)
    call emit_sp(out_buf, ind + 10_i32, err)
    call buf_append_str(out_buf, "jhv = jhv - 55_int32" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "else if (jhv >= 97_int32 .and. jhv <= 102_int32) then" // LF, err)
    call emit_sp(out_buf, ind + 10_i32, err)
    call buf_append_str(out_buf, "jhv = jhv - 87_int32" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "else" // LF, err)
    call emit_mod_fail(out_buf, ind + 10_i32, is_f03, "Invalid unicode hex digit", err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind + 8_i32, err)
    call buf_append_str(out_buf, "jhex = jhex * 16_int32 + jhv" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "end do" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "jch = achar(mod(jhex, 128_int32)); jpos = jpos + 4_int32" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "if (" // len_v // " < ", err)
    call buf_append_i64(out_buf, int(max_cap, i64), err)
    call buf_append_str(out_buf, "_int32) then" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, len_v // " = " // len_v // " + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, dst_v // "(" // len_v // ":" // len_v // ") = jch" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "end do" // LF, err)
  end subroutine emit_mod_read_str

  !> @brief Emit balanced unknown-key JSON value skipper in modern Fortran (`F90`..`F2023`).
  !> @param[inout] out_buf Target buffer.
  !> @param[in]    ind     Indentation width.
  !> @param[in]    is_f03  True for F2003+ errmsg mode.
  !> @param[inout] err     Error status.
  pure subroutine emit_mod_skip_val(out_buf, ind, is_f03, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: ind
    logical(lk), intent(in) :: is_f03
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "if (jpos > jn) then" // LF, err)
    call emit_mod_fail(out_buf, ind + 2_i32, is_f03, "Missing JSON field value", err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "if (json_str(jpos:jpos) == '""') then" // LF, err)
    call emit_mod_read_str(out_buf, ind + 2_i32, is_f03, "jsval", "jsl", 1024_i32, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "else if (json_str(jpos:jpos) == '{' .or. " // &
      "json_str(jpos:jpos) == '[') then" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "jdep = 0_int32; jstrq = 0_int32" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "do" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "if (jpos > jn) then" // LF, err)
    call emit_mod_fail(out_buf, ind + 6_i32, is_f03, "Unterminated skipped container", err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "jch = json_str(jpos:jpos)" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "if (jstrq == 1_int32) then" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "if (iachar(jch) == 92) jpos = jpos + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "if (jch == '""') jstrq = 0_int32" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "else" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "if (jch == '""') jstrq = 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "if (jch == '{' .or. jch == '[') jdep = jdep + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 6_i32, err)
    call buf_append_str(out_buf, "if (jch == '}' .or. jch == ']') jdep = jdep - 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "end if" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "jpos = jpos + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "if (jdep <= 0_int32) exit" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "end do" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "else" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "do while (jpos <= jn)" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "jch = json_str(jpos:jpos)" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "if (jch == ',' .or. jch == '}' .or. jch == ' ' .or. " // &
      "iachar(jch) == 9 .or. iachar(jch) == 10 .or. iachar(jch) == 13) exit" // LF, err)
    call emit_sp(out_buf, ind + 4_i32, err)
    call buf_append_str(out_buf, "jpos = jpos + 1_int32" // LF, err)
    call emit_sp(out_buf, ind + 2_i32, err)
    call buf_append_str(out_buf, "end do" // LF, err)
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "end if" // LF, err)
  end subroutine emit_mod_skip_val

  !> @brief Emit parse-error return (`ierr = 1`, optional `errmsg`) in modern Fortran.
  !> @param[inout] out_buf Target buffer.
  !> @param[in]    ind     Indentation width.
  !> @param[in]    is_f03  True when optional `errmsg` dummy argument exists.
  !> @param[in]    msg     Diagnostic message literal.
  !> @param[inout] err     Error status.
  pure subroutine emit_mod_fail(out_buf, ind, is_f03, msg, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: ind
    logical(lk), intent(in) :: is_f03
    character(len=*, kind=c_k), intent(in) :: msg
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "ierr = 1_int32" // LF, err)
    if (is_f03) then
      call emit_sp(out_buf, ind, err)
      call buf_append_str(out_buf, 'if (present(errmsg)) errmsg = "' // msg // '"' // LF, err)
    end if
    call emit_sp(out_buf, ind, err)
    call buf_append_str(out_buf, "return" // LF, err)
  end subroutine emit_mod_fail

  !> @brief Compute exact `ALLOCATABLE` vs `POINTER` storage flags matching `cdd_emit_udt`.
  !> @param[in]  fld       Field descriptor.
  !> @param[in]  caps      Dialect capabilities.
  !> @param[out] use_alloc True when component is declared `ALLOCATABLE`.
  !> @param[out] use_ptr   True when component is declared `POINTER`.
  pure subroutine field_storage_flags(fld, caps, use_alloc, use_ptr)
    type(cdd_ir_field_t), intent(in) :: fld
    type(cdd_dialect_caps_t), intent(in) :: caps
    logical(lk), intent(out) :: use_alloc
    logical(lk), intent(out) :: use_ptr

    use_alloc = fld%is_allocatable .and. caps%has_allocatable_comp
    use_ptr = fld%is_pointer .or. ((fld%is_allocatable .or. fld%is_nullable) .and. &
                                   .not. caps%has_allocatable_comp)
    if (fld%type_kind == IR_STRING_DEFERRED .and. caps%has_deferred_char) then
      use_alloc = .true._lk
      use_ptr = .false._lk
    end if
  end subroutine field_storage_flags

  !> @brief Check whether a UDT (or any nested UDT in the bundle) uses `POINTER` components.
  !> @param[in] bundle  Canonical IR bundle.
  !> @param[in] udt_idx Target UDT index (unused per-UDT filter; checks bundle closure).
  !> @param[in] caps    Dialect capabilities.
  !> @return True when any component in bundle uses `POINTER` (disallowing `PURE` `allocate`).
  pure function udt_uses_pointer(bundle, udt_idx, caps) result(has_ptr)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    integer(i32), intent(in) :: udt_idx
    type(cdd_dialect_caps_t), intent(in) :: caps
    logical(lk) :: has_ptr
    logical(lk) :: ua, up_f
    integer(i32) :: u, f

    associate (unused_u => udt_idx)
    end associate
    has_ptr = .false._lk
    do u = 1_i32, bundle%udt_count
      do f = 1_i32, bundle%udts(u)%field_count
        call field_storage_flags(bundle%udts(u)%fields(f), caps, ua, up_f)
        if (up_f) then
          has_ptr = .true._lk
          return
        end if
      end do
    end do
  end function udt_uses_pointer

  !> @brief Copy a bounded identifier/expression into a fixed-length character buffer.
  !> @param[in]  src     Source string.
  !> @param[out] dst     Destination character buffer.
  !> @param[out] out_len Copied character length.
  pure subroutine copy_name(src, dst, out_len)
    character(len=*, kind=c_k), intent(in) :: src
    character(len=*, kind=c_k), intent(out) :: dst
    integer(i32), intent(out) :: out_len
    integer(i32) :: k

    out_len = min(len(src, kind=i32), len(dst, kind=i32))
    do k = 1_i32, out_len
      dst(k:k) = src(k:k)
    end do
  end subroutine copy_name

  !> @brief Append keyword text in lowercase or uppercase per `up` flag.
  !> @param[inout] out_buf Target buffer.
  !> @param[in]    kw      Lowercase keyword string.
  !> @param[in]    up      True to convert ASCII letters to uppercase.
  !> @param[inout] err     Error status.
  pure subroutine emit_kw(out_buf, kw, up, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    character(len=*, kind=c_k), intent(in) :: kw
    logical(lk), intent(in) :: up
    type(cdd_error_t), intent(inout) :: err
    character(len=256, kind=c_k) :: ubuf
    integer(i32) :: ulen

    if (up) then
      call to_upper_buf(kw, ubuf, ulen)
      call buf_append_str(out_buf, ubuf(1:ulen), err)
    else
      call buf_append_str(out_buf, kw, err)
    end if
  end subroutine emit_kw

  !> @brief Append `n` ASCII space characters for indentation.
  !> @param[inout] out_buf Target buffer.
  !> @param[in]    n       Number of spaces.
  !> @param[inout] err     Error status.
  pure subroutine emit_sp(out_buf, n, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: n
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: k

    do k = 1_i32, n
      call buf_append_char(out_buf, " ", err)
    end do
  end subroutine emit_sp

end module cdd_emit_codec
