!> @file cdd_emit_udt.f90
!> @brief Operation 0: Emit idiomatic Fortran UDT definitions from IR across FORTRAN I..2023.

!> @brief Multi-standard Fortran UDT code generator supporting all 12 historical and modern dialects.
module cdd_emit_udt
  use cdd_kinds, only: i32, i64, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set_simple, CDD_ERR_INVALID_OPTION
  use cdd_string, only: cdd_buffer_t, buf_init, buf_append_char, buf_append_str, buf_append_i64, &
    to_upper_buf, str_eq_nocase
  use cdd_standard, only: STD_FORTRAN_I, STD_FORTRAN_II, STD_FORTRAN_III, STD_FORTRAN_IV, &
    STD_FORTRAN_66, STD_FORTRAN_77, STD_FORTRAN_90, STD_FORTRAN_95, STD_FORTRAN_2003, &
    STD_FORTRAN_2008, STD_FORTRAN_2018, STD_FORTRAN_2023, &
    cdd_options_t, cdd_dialect_caps_t, get_dialect_capabilities, resolve_effective_form
  use cdd_ir_types, only: CDD_MAX_UDTS, IR_INT8, IR_INT16, IR_INT32, IR_INT64, IR_REAL32, &
    IR_REAL64, IR_REAL128, IR_COMPLEX64, IR_COMPLEX128, IR_BOOL, IR_STRING_FIXED, &
    IR_STRING_DEFERRED, IR_ENUM, IR_NESTED_UDT, DIM_FIXED, cdd_ir_field_t, cdd_ir_enum_t, &
    cdd_ir_udt_t, cdd_ir_schema_bundle_t, ir_bundle_find_udt, ir_bundle_find_enum
  use cdd_ir_namemap, only: format_cdd_field_comment
  implicit none (type, external)
  private

  public :: emit_udt_fortran, emit_udt_types_section, emit_single_modern_field_line
  public :: emit_modern_module_header, emit_dec_structures, emit_dec_structure_defs_only
  public :: make_common_names, emit_common_line, compute_udt_topo_order

contains

  !> @brief Emit complete dialect-pure Fortran UDT code for bundle into out_buf (Operation 0).
  !> @param[in]  bundle  Canonical IR schema bundle.
  !> @param[in]  options Target Fortran standard, source form, and formatting options.
  !> @param[out] out_buf Generated Fortran source code buffer.
  !> @param[out] err     Structured error status.
  pure subroutine emit_udt_fortran(bundle, options, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_options_t), intent(in) :: options
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: form, sid
    type(cdd_dialect_caps_t) :: caps
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call buf_init(out_buf)
    call cdd_error_clear(err)
    if (bundle%main_udt_idx < 1_i32 .or. bundle%main_udt_idx > bundle%udt_count) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Empty IR bundle in emit_udt_fortran")
      return
    end if

    call resolve_effective_form(options, form, err)
    if (err%code /= 0_i32) return

    caps = get_dialect_capabilities(options%standard, options%allow_dec_structure)
    sid = options%standard%id

    if (sid == STD_FORTRAN_I) then
      call emit_f57_udt(bundle%udts(bundle%main_udt_idx), out_buf, err)
    else if (sid == STD_FORTRAN_II .or. sid == STD_FORTRAN_III) then
      call emit_f58_udt(bundle%udts(bundle%main_udt_idx), (sid == STD_FORTRAN_III), out_buf, err)
    else if (sid <= STD_FORTRAN_77) then
      if (caps%has_dec_structure) then
        call emit_dec_structures(bundle, out_buf, err)
      else
        call emit_f66_f77_common(bundle%udts(bundle%main_udt_idx), sid, out_buf, err)
      end if
    else
      call emit_modern_module_header(bundle, options, form, out_buf, err)
      call emit_udt_types_section(bundle, options, .false._lk, out_buf, err)
      if (options%emit_submodules .and. caps%has_submodules .and. &
          options%emit_type_bound_procs) then
        call emit_submodule_codec_interfaces(bundle, options, out_buf, err)
      end if
      call emit_kw(out_buf, "end module ", options%uppercase_keywords, err)
      call buf_append_str(out_buf, bundle%module_name%raw(1:bundle%module_name%len), err)
      call buf_append_char(out_buf, LF, err)
    end if
  end subroutine emit_udt_fortran

  !> @brief Emit modern Fortran (F90..F2023) enum and derived-type definitions without module wrapper.
  !> @param[in]    bundle        Canonical IR bundle.
  !> @param[in]    options       Dialect options.
  !> @param[in]    with_tbp_bind True when codec procedures are emitted in same module CONTAINS.
  !> @param[inout] out_buf       Target buffer.
  !> @param[inout] err           Error status.
  pure subroutine emit_udt_types_section(bundle, options, with_tbp_bind, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_options_t), intent(in) :: options
    logical(lk), intent(in) :: with_tbp_bind
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    type(cdd_dialect_caps_t) :: caps
    integer(i32) :: i, ind, order(CDD_MAX_UDTS)
    logical(lk) :: emit_tbps

    caps = get_dialect_capabilities(options%standard, options%allow_dec_structure)
    ind = max(2_i32, options%indent_width)
    emit_tbps = caps%has_oop_class_extends .and. options%emit_type_bound_procs .and. &
                (with_tbp_bind .or. (options%emit_submodules .and. caps%has_submodules))

    do i = 1_i32, bundle%enum_count
      call emit_modern_enum(bundle%enums(i), options%standard%id, ind, &
        options%uppercase_keywords, out_buf, err)
    end do

    call compute_udt_topo_order(bundle, order)
    do i = 1_i32, bundle%udt_count
      call emit_modern_type_def(bundle, bundle%udts(order(i)), options, caps, ind, &
        emit_tbps, out_buf, err)
    end do
  end subroutine emit_udt_types_section

  !> @brief Emit FORTRAN (I) (1957) fixed-form DIMENSION + EQUIVALENCE layout block.
  !> @param[in]    udt     Source UDT definition.
  !> @param[inout] out_buf Destination buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_f57_udt(udt, out_buf, err)
    type(cdd_ir_udt_t), intent(in) :: udt
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: i, first_int, first_real, nwords
    type(cdd_buffer_t) :: cbuf
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call emit_fixed_udt_banner("C", udt, out_buf, err)
    first_int = 0_i32
    first_real = 0_i32

    do i = 1_i32, udt%field_count
      call format_cdd_field_comment("C", udt%fields(i), cbuf, err)
      call buf_append_str(out_buf, cbuf%raw(1:min(72_i32, cbuf%len)), err)
      call buf_append_char(out_buf, LF, err)
      if (udt%fields(i)%description%len > 0_i32) then
        call buf_append_str(out_buf, "C< ", err)
        call buf_append_str(out_buf, &
          udt%fields(i)%description%raw(1:min(68_i32, udt%fields(i)%description%len)), err)
        call buf_append_char(out_buf, LF, err)
      end if

      if (udt%fields(i)%type_kind == IR_STRING_FIXED .or. &
          udt%fields(i)%type_kind == IR_STRING_DEFERRED) then
        nwords = max(1_i32, (max(6_i32, udt%fields(i)%char_len) + 5_i32) / 6_i32)
        call buf_append_str(out_buf, "      DIMENSION ", err)
        call buf_append_str(out_buf, &
          udt%fields(i)%fortran_name%raw(1:udt%fields(i)%fortran_name%len), err)
        call buf_append_char(out_buf, "(", err)
        call buf_append_i64(out_buf, int(nwords, i64), err)
        call buf_append_char(out_buf, ")", err)
        call buf_append_char(out_buf, LF, err)
      else if (udt%fields(i)%rank > 0_i32) then
        call buf_append_str(out_buf, "      DIMENSION ", err)
        call buf_append_str(out_buf, &
          udt%fields(i)%fortran_name%raw(1:udt%fields(i)%fortran_name%len), err)
        call append_fixed_dims(udt%fields(i), out_buf, err)
        call buf_append_char(out_buf, LF, err)
      end if

      if (udt%fields(i)%type_kind >= IR_REAL32 .and. &
          udt%fields(i)%type_kind <= IR_COMPLEX128) then
        if (first_real == 0_i32) first_real = i
      else
        if (first_int == 0_i32) first_int = i
      end if
    end do

    if (first_int > 0_i32) then
      call buf_append_str(out_buf, "      DIMENSION IUDTBK(", err)
      call buf_append_i64(out_buf, int(max(1_i32, udt%field_count * 4_i32), i64), err)
      call buf_append_str(out_buf, ")" // LF, err)
      call buf_append_str(out_buf, "      EQUIVALENCE (IUDTBK(1), ", err)
      call buf_append_str(out_buf, &
        udt%fields(first_int)%fortran_name%raw(1:udt%fields(first_int)%fortran_name%len), err)
      call buf_append_str(out_buf, ")" // LF, err)
    end if
    if (first_real > 0_i32) then
      call buf_append_str(out_buf, "      DIMENSION XUDTBK(", err)
      call buf_append_i64(out_buf, int(max(1_i32, udt%field_count * 4_i32), i64), err)
      call buf_append_str(out_buf, ")" // LF, err)
      call buf_append_str(out_buf, "      EQUIVALENCE (XUDTBK(1), ", err)
      call buf_append_str(out_buf, &
        udt%fields(first_real)%fortran_name%raw(1:udt%fields(first_real)%fortran_name%len), err)
      call buf_append_str(out_buf, ")" // LF, err)
    end if
  end subroutine emit_f57_udt

  !> @brief Emit FORTRAN II / III (1958) fixed-form DIMENSION + blank COMMON layout block.
  !> @param[in]    udt       Source UDT definition.
  !> @param[in]    is_f3     True when emitting FORTRAN III boolean-word dialect comment.
  !> @param[inout] out_buf   Destination buffer.
  !> @param[inout] err       Error status.
  pure subroutine emit_f58_udt(udt, is_f3, out_buf, err)
    type(cdd_ir_udt_t), intent(in) :: udt
    logical(lk), intent(in) :: is_f3
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: i, nwords, col_len, fn_len
    type(cdd_buffer_t) :: cbuf
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call emit_fixed_udt_banner("C", udt, out_buf, err)
    if (is_f3) then
      call buf_append_str(out_buf, "C FORTRAN III BOOLEAN WORD MASK LAYOUT" // LF, err)
    end if

    do i = 1_i32, udt%field_count
      call format_cdd_field_comment("C", udt%fields(i), cbuf, err)
      call buf_append_str(out_buf, cbuf%raw(1:min(72_i32, cbuf%len)), err)
      call buf_append_char(out_buf, LF, err)
      if (udt%fields(i)%type_kind == IR_STRING_FIXED .or. &
          udt%fields(i)%type_kind == IR_STRING_DEFERRED) then
        nwords = max(1_i32, (max(6_i32, udt%fields(i)%char_len) + 5_i32) / 6_i32)
        call buf_append_str(out_buf, "      DIMENSION ", err)
        call buf_append_str(out_buf, &
          udt%fields(i)%fortran_name%raw(1:udt%fields(i)%fortran_name%len), err)
        call buf_append_char(out_buf, "(", err)
        call buf_append_i64(out_buf, int(nwords, i64), err)
        call buf_append_str(out_buf, ")" // LF, err)
      else if (udt%fields(i)%rank > 0_i32) then
        call buf_append_str(out_buf, "      DIMENSION ", err)
        call buf_append_str(out_buf, &
          udt%fields(i)%fortran_name%raw(1:udt%fields(i)%fortran_name%len), err)
        call append_fixed_dims(udt%fields(i), out_buf, err)
        call buf_append_char(out_buf, LF, err)
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
  end subroutine emit_f58_udt

  !> @brief Emit FORTRAN IV, 66, and 77 explicit declarations + named COMMON (+ F77 char split & BLOCK DATA).
  !> @param[in]    udt     Source UDT definition.
  !> @param[in]    sid     Standard constant (STD_FORTRAN_IV, 66, or 77).
  !> @param[inout] out_buf Destination buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_f66_f77_common(udt, sid, out_buf, err)
    type(cdd_ir_udt_t), intent(in) :: udt
    integer(i32), intent(in) :: sid
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: i, nwords, num_cnt, chr_cnt, any_def
    logical(lk) :: is_f77, is_chr, split_common
    type(cdd_buffer_t) :: cbuf
    character(len=6, kind=c_k) :: blk_num, blk_chr
    integer(i32) :: bn_len, bc_len
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    is_f77 = (sid == STD_FORTRAN_77)
    call emit_fixed_udt_banner("C", udt, out_buf, err)

    num_cnt = 0_i32
    chr_cnt = 0_i32
    any_def = 0_i32

    do i = 1_i32, udt%field_count
      call format_cdd_field_comment("C", udt%fields(i), cbuf, err)
      call buf_append_str(out_buf, cbuf%raw(1:min(72_i32, cbuf%len)), err)
      call buf_append_char(out_buf, LF, err)
      if (udt%fields(i)%has_default) any_def = any_def + 1_i32

      is_chr = is_f77 .and. (udt%fields(i)%type_kind == IR_STRING_FIXED .or. &
                             udt%fields(i)%type_kind == IR_STRING_DEFERRED .or. &
                             udt%fields(i)%type_kind == IR_ENUM)
      if (is_chr) then
        chr_cnt = chr_cnt + 1_i32
        call buf_append_str(out_buf, "      CHARACTER*", err)
        call buf_append_i64(out_buf, int(max(1_i32, udt%fields(i)%char_len), i64), err)
        call buf_append_char(out_buf, " ", err)
        call buf_append_str(out_buf, &
          udt%fields(i)%fortran_name%raw(1:udt%fields(i)%fortran_name%len), err)
        if (udt%fields(i)%rank > 0_i32) call append_fixed_dims(udt%fields(i), out_buf, err)
        call buf_append_char(out_buf, LF, err)
      else
        num_cnt = num_cnt + 1_i32
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
        if (.not. is_f77 .and. (udt%fields(i)%type_kind == IR_STRING_FIXED .or. &
                                udt%fields(i)%type_kind == IR_STRING_DEFERRED)) then
          nwords = max(1_i32, (max(4_i32, udt%fields(i)%char_len) + 3_i32) / 4_i32)
          call buf_append_char(out_buf, "(", err)
          call buf_append_i64(out_buf, int(nwords, i64), err)
          call buf_append_char(out_buf, ")", err)
        else if (udt%fields(i)%rank > 0_i32) then
          call append_fixed_dims(udt%fields(i), out_buf, err)
        end if
        call buf_append_char(out_buf, LF, err)
      end if
    end do

    split_common = (is_f77 .and. num_cnt > 0_i32 .and. chr_cnt > 0_i32)
    call make_common_names(udt%fortran_name%raw(1:udt%fortran_name%len), split_common, &
      blk_num, bn_len, blk_chr, bc_len)

    if (num_cnt > 0_i32) then
      call emit_common_line(udt, blk_num(1:bn_len), is_f77, .false._lk, out_buf, err)
      if (is_f77) then
        call buf_append_str(out_buf, "      SAVE /" // blk_num(1:bn_len) // "/" // LF, err)
      end if
    end if
    if (chr_cnt > 0_i32) then
      call emit_common_line(udt, blk_chr(1:bc_len), is_f77, .true._lk, out_buf, err)
      call buf_append_str(out_buf, "      SAVE /" // blk_chr(1:bc_len) // "/" // LF, err)
    end if

    if (any_def > 0_i32 .and. sid == STD_FORTRAN_66) then
      call emit_f66_block_data(udt, blk_num(1:bn_len), out_buf, err)
    end if
  end subroutine emit_f66_f77_common

  !> @brief Emit DEC STRUCTURE /name/ ... END STRUCTURE and companion RECORD declaration.
  !> @param[in]    bundle  Canonical IR bundle.
  !> @param[inout] out_buf Destination buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_dec_structures(bundle, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call emit_dec_structure_defs_only(bundle, out_buf, err)
    call buf_append_str(out_buf, "      RECORD /", err)
    call buf_append_str(out_buf, bundle%udts(bundle%main_udt_idx)%fortran_name%raw( &
      1:bundle%udts(bundle%main_udt_idx)%fortran_name%len), err)
    call buf_append_str(out_buf, "/ REC_VAL" // LF, err)
  end subroutine emit_dec_structures

  !> @brief Emit DEC STRUCTURE /name/ ... END STRUCTURE definitions in topological order.
  !> @param[in]    bundle  Canonical IR bundle.
  !> @param[inout] out_buf Destination buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_dec_structure_defs_only(bundle, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: k, u, f, r_idx, order(CDD_MAX_UDTS)
    type(cdd_buffer_t) :: cbuf
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call compute_udt_topo_order(bundle, order)
    do k = 1_i32, bundle%udt_count
      u = order(k)
      call emit_fixed_udt_banner("C", bundle%udts(u), out_buf, err)
      call buf_append_str(out_buf, "      STRUCTURE /", err)
      call buf_append_str(out_buf, &
        bundle%udts(u)%fortran_name%raw(1:bundle%udts(u)%fortran_name%len), err)
      call buf_append_str(out_buf, "/" // LF, err)

      do f = 1_i32, bundle%udts(u)%field_count
        call format_cdd_field_comment("C", bundle%udts(u)%fields(f), cbuf, err)
        call buf_append_str(out_buf, cbuf%raw(1:min(72_i32, cbuf%len)), err)
        call buf_append_char(out_buf, LF, err)
        select case (bundle%udts(u)%fields(f)%type_kind)
        case (IR_INT8, IR_INT16)
          call buf_append_str(out_buf, "        INTEGER*2 ", err)
        case (IR_INT64)
          call buf_append_str(out_buf, "        INTEGER*8 ", err)
        case (IR_REAL32)
          call buf_append_str(out_buf, "        REAL*4 ", err)
        case (IR_REAL64, IR_REAL128)
          call buf_append_str(out_buf, "        REAL*8 ", err)
        case (IR_BOOL)
          call buf_append_str(out_buf, "        LOGICAL*4 ", err)
        case (IR_STRING_FIXED, IR_STRING_DEFERRED, IR_ENUM)
          call buf_append_str(out_buf, "        CHARACTER*", err)
          call buf_append_i64(out_buf, &
            int(max(1_i32, bundle%udts(u)%fields(f)%char_len), i64), err)
          call buf_append_char(out_buf, " ", err)
        case (IR_NESTED_UDT)
          r_idx = ir_bundle_find_udt(bundle, bundle%udts(u)%fields(f)%ref_udt_name%raw( &
            1:bundle%udts(u)%fields(f)%ref_udt_name%len))
          call buf_append_str(out_buf, "        RECORD /", err)
          if (r_idx > 0_i32) then
            call buf_append_str(out_buf, &
              bundle%udts(r_idx)%fortran_name%raw(1:bundle%udts(r_idx)%fortran_name%len), err)
          else
            call buf_append_str(out_buf, bundle%udts(u)%fields(f)%ref_udt_name%raw( &
              1:bundle%udts(u)%fields(f)%ref_udt_name%len), err)
          end if
          call buf_append_str(out_buf, "/ ", err)
        case default
          call buf_append_str(out_buf, "        INTEGER*4 ", err)
        end select
        call buf_append_str(out_buf, bundle%udts(u)%fields(f)%fortran_name%raw( &
          1:bundle%udts(u)%fields(f)%fortran_name%len), err)
        if (bundle%udts(u)%fields(f)%rank > 0_i32) then
          call append_fixed_dims(bundle%udts(u)%fields(f), out_buf, err)
        end if
        call buf_append_char(out_buf, LF, err)
      end do
      call buf_append_str(out_buf, "      END STRUCTURE" // LF, err)
    end do
  end subroutine emit_dec_structure_defs_only

  !> @brief Emit modern Fortran (F90..F2023) module header, USE kind imports, and IMPLICIT NONE.
  !> @param[in]    bundle  IR bundle.
  !> @param[in]    options Dialect options.
  !> @param[in]    form    Effective source form.
  !> @param[inout] out_buf Destination buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_modern_module_header(bundle, options, form, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_options_t), intent(in) :: options
    integer(i32), intent(in) :: form
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: sid, ind
    logical(lk) :: up
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    associate (unused_form => form)
    end associate
    sid = options%standard%id
    up = options%uppercase_keywords
    ind = max(2_i32, options%indent_width)

    call buf_append_str(out_buf, "!> @brief Generated module ", err)
    call buf_append_str(out_buf, bundle%module_name%raw(1:bundle%module_name%len), err)
    call buf_append_char(out_buf, LF, err)

    call emit_kw(out_buf, "module ", up, err)
    call buf_append_str(out_buf, bundle%module_name%raw(1:bundle%module_name%len), err)
    call buf_append_char(out_buf, LF, err)

    if (sid >= STD_FORTRAN_2008) then
      call emit_indent(out_buf, ind, err)
      call emit_kw(out_buf, &
        "use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, " // &
        "real32, real64, real128", up, err)
      call buf_append_char(out_buf, LF, err)
    end if
    if (sid >= STD_FORTRAN_2003 .and. sid < STD_FORTRAN_2023 .and. bundle%enum_count > 0_i32) then
      call emit_indent(out_buf, ind, err)
      call emit_kw(out_buf, "use, intrinsic :: iso_c_binding, only: c_int", up, err)
      call buf_append_char(out_buf, LF, err)
    end if

    call emit_indent(out_buf, ind, err)
    if (sid >= STD_FORTRAN_2018) then
      call emit_kw(out_buf, "implicit none (type, external)", up, err)
    else
      call emit_kw(out_buf, "implicit none", up, err)
    end if
    call buf_append_char(out_buf, LF, err)

    if (sid < STD_FORTRAN_2008) then
      call emit_indent(out_buf, ind, err)
      call emit_kw(out_buf, "integer, parameter :: int8 = selected_int_kind(2)", up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_indent(out_buf, ind, err)
      call emit_kw(out_buf, "integer, parameter :: int16 = selected_int_kind(4)", up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_indent(out_buf, ind, err)
      call emit_kw(out_buf, "integer, parameter :: int32 = selected_int_kind(9)", up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_indent(out_buf, ind, err)
      call emit_kw(out_buf, "integer, parameter :: int64 = selected_int_kind(18)", up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_indent(out_buf, ind, err)
      call emit_kw(out_buf, "integer, parameter :: real32 = selected_real_kind(6, 37)", up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_indent(out_buf, ind, err)
      call emit_kw(out_buf, "integer, parameter :: real64 = selected_real_kind(15, 307)", up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_indent(out_buf, ind, err)
      call emit_kw(out_buf, "integer, parameter :: real128 = selected_real_kind(15, 307)", up, err)
      call buf_append_char(out_buf, LF, err)
    end if
  end subroutine emit_modern_module_header

  !> @brief Emit an enumeration in F90/95 (constants), F2003..2018 (BIND(C)), or F2023 (ENUMERATION TYPE).
  !> @param[in]    edef    IR enumeration definition.
  !> @param[in]    sid     Standard constant.
  !> @param[in]    ind     Indentation spaces.
  !> @param[in]    up      Uppercase keywords toggle.
  !> @param[inout] out_buf Destination buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_modern_enum(edef, sid, ind, up, out_buf, err)
    type(cdd_ir_enum_t), intent(in) :: edef
    integer(i32), intent(in) :: sid
    integer(i32), intent(in) :: ind
    logical(lk), intent(in) :: up
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: k
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    if (edef%description%len > 0_i32) then
      call emit_indent(out_buf, ind, err)
      call buf_append_str(out_buf, "!> @brief ", err)
      call buf_append_str(out_buf, edef%description%raw(1:edef%description%len), err)
      call buf_append_char(out_buf, LF, err)
    end if

    if (sid >= STD_FORTRAN_2023) then
      call emit_indent(out_buf, ind, err)
      call emit_kw(out_buf, "enumeration type, public :: ", up, err)
      call buf_append_str(out_buf, edef%fortran_name%raw(1:edef%fortran_name%len), err)
      call buf_append_char(out_buf, LF, err)
      do k = 1_i32, edef%val_count
        call emit_indent(out_buf, ind * 2_i32, err)
        call emit_kw(out_buf, "enumerator :: ", up, err)
        call buf_append_str(out_buf, edef%f_vals(k)%raw(1:edef%f_vals(k)%len), err)
        call buf_append_str(out_buf, " !< " // edef%json_vals(k)%raw(1:edef%json_vals(k)%len) // LF, err)
      end do
      call emit_indent(out_buf, ind, err)
      call emit_kw(out_buf, "end enumeration type ", up, err)
      call buf_append_str(out_buf, edef%fortran_name%raw(1:edef%fortran_name%len), err)
      call buf_append_char(out_buf, LF, err)
    else if (sid >= STD_FORTRAN_2003) then
      call emit_indent(out_buf, ind, err)
      call buf_append_str(out_buf, "! @cdd-enum: " // &
        edef%json_name%raw(1:edef%json_name%len) // LF, err)
      call emit_indent(out_buf, ind, err)
      call emit_kw(out_buf, "e" // "num, bind(c)", up, err)
      call buf_append_char(out_buf, LF, err)
      do k = 1_i32, edef%val_count
        call emit_indent(out_buf, ind * 2_i32, err)
        call emit_kw(out_buf, "enumerator :: ", up, err)
        call buf_append_str(out_buf, edef%f_vals(k)%raw(1:edef%f_vals(k)%len), err)
        call buf_append_str(out_buf, " = ", err)
        call buf_append_i64(out_buf, int(k - 1_i32, i64), err)
        call buf_append_str(out_buf, " !< " // edef%json_vals(k)%raw(1:edef%json_vals(k)%len) // LF, err)
      end do
      call emit_indent(out_buf, ind, err)
      call emit_kw(out_buf, "end e" // "num", up, err)
      call buf_append_char(out_buf, LF, err)
    else
      do k = 1_i32, edef%val_count
        call emit_indent(out_buf, ind, err)
        call emit_kw(out_buf, "integer(int32), parameter, public :: ", up, err)
        call buf_append_str(out_buf, edef%f_vals(k)%raw(1:edef%f_vals(k)%len), err)
        call buf_append_str(out_buf, " = ", err)
        call buf_append_i64(out_buf, int(k - 1_i32, i64), err)
        call buf_append_char(out_buf, LF, err)
      end do
    end if
  end subroutine emit_modern_enum

  !> @brief Emit a modern Fortran derived `TYPE :: <name>_t ... END TYPE` definition.
  !> @param[in]    bundle    Enclosing IR bundle.
  !> @param[in]    udt       Source UDT.
  !> @param[in]    options   SDK options.
  !> @param[in]    caps      Dialect capability descriptor.
  !> @param[in]    ind       Indentation width.
  !> @param[in]    emit_tbps True to emit CONTAINS TBP bindings.
  !> @param[inout] out_buf   Destination buffer.
  !> @param[inout] err       Error status.
  pure subroutine emit_modern_type_def(bundle, udt, options, caps, ind, emit_tbps, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_udt_t), intent(in) :: udt
    type(cdd_options_t), intent(in) :: options
    type(cdd_dialect_caps_t), intent(in) :: caps
    integer(i32), intent(in) :: ind
    logical(lk), intent(in) :: emit_tbps
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: f
    logical(lk) :: up
    type(cdd_buffer_t) :: cbuf
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    up = options%uppercase_keywords
    call emit_indent(out_buf, ind, err)
    call buf_append_str(out_buf, "! @cdd-udt: " // udt%json_name%raw(1:udt%json_name%len) // LF, err)
    call emit_indent(out_buf, ind, err)
    call buf_append_str(out_buf, "!> @brief ", err)
    if (udt%description%len > 0_i32) then
      call buf_append_str(out_buf, udt%description%raw(1:udt%description%len), err)
    else
      call buf_append_str(out_buf, "Generated UDT " // udt%json_name%raw(1:udt%json_name%len), err)
    end if
    call buf_append_char(out_buf, LF, err)

    call emit_indent(out_buf, ind, err)
    call emit_kw(out_buf, "type, public", up, err)
    if (udt%has_extends .and. caps%has_oop_class_extends) then
      call emit_kw(out_buf, ", extends(", up, err)
      call buf_append_str(out_buf, udt%extends_name%raw(1:udt%extends_name%len), err)
      call buf_append_char(out_buf, ")", err)
    end if
    if (udt%is_bind_c .and. caps%has_oop_class_extends) then
      call emit_kw(out_buf, ", bind(c)", up, err)
    end if
    call buf_append_str(out_buf, " :: ", err)
    call buf_append_str(out_buf, udt%fortran_name%raw(1:udt%fortran_name%len), err)
    call buf_append_char(out_buf, LF, err)

    do f = 1_i32, udt%field_count
      call emit_indent(out_buf, ind * 2_i32, err)
      call format_cdd_field_comment("!", udt%fields(f), cbuf, err)
      call buf_append_str(out_buf, cbuf%raw(1:cbuf%len), err)
      call buf_append_char(out_buf, LF, err)
      call emit_single_modern_field_line(bundle, udt%fields(f), options%standard%id, caps, &
        ind * 2_i32, up, .true._lk, LF, out_buf, err)
    end do

    if (emit_tbps .and. .not. udt%is_bind_c) then
      call emit_indent(out_buf, ind, err)
      call emit_kw(out_buf, "contains", up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_indent(out_buf, ind * 2_i32, err)
      call emit_kw(out_buf, "procedure :: to_json => ", up, err)
      call buf_append_str(out_buf, udt%fortran_name%raw(1:udt%fortran_name%len) // "_to_json" // LF, err)
      call emit_indent(out_buf, ind * 2_i32, err)
      call emit_kw(out_buf, "procedure :: from_json => ", up, err)
      call buf_append_str(out_buf, udt%fortran_name%raw(1:udt%fortran_name%len) // "_from_json" // LF, err)
    end if

    call emit_indent(out_buf, ind, err)
    call emit_kw(out_buf, "end type ", up, err)
    call buf_append_str(out_buf, udt%fortran_name%raw(1:udt%fortran_name%len), err)
    call buf_append_char(out_buf, LF, err)
  end subroutine emit_modern_type_def

  !> @brief Emit a single modern Fortran component declaration line (reused by surgical patcher!).
  !> @param[in]    bundle   Enclosing IR bundle.
  !> @param[in]    fld      Field descriptor to emit.
  !> @param[in]    sid      Fortran standard ID.
  !> @param[in]    caps     Dialect capabilities.
  !> @param[in]    ind      Leading space indentation count.
  !> @param[in]    up       Uppercase keywords toggle.
  !> @param[in]    use_dcol True to emit ' :: ' separator.
  !> @param[in]    nl_seq   Line ending string (LF or CRLF).
  !> @param[inout] out_buf  Destination buffer.
  !> @param[inout] err      Error status.
  pure subroutine emit_single_modern_field_line(bundle, fld, sid, caps, ind, up, use_dcol, &
      nl_seq, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_field_t), intent(in) :: fld
    integer(i32), intent(in) :: sid
    type(cdd_dialect_caps_t), intent(in) :: caps
    integer(i32), intent(in) :: ind
    logical(lk), intent(in) :: up
    logical(lk), intent(in) :: use_dcol
    character(len=*, kind=c_k), intent(in) :: nl_seq
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: r_idx, e_idx, r
    logical(lk) :: use_alloc, use_ptr

    call emit_indent(out_buf, ind, err)
    use_alloc = fld%is_allocatable .and. caps%has_allocatable_comp
    use_ptr = fld%is_pointer .or. ((fld%is_allocatable .or. fld%is_nullable) .and. &
                                   .not. caps%has_allocatable_comp)

    select case (fld%type_kind)
    case (IR_INT8)
      call emit_kw(out_buf, "integer(int8)", up, err)
    case (IR_INT16)
      call emit_kw(out_buf, "integer(int16)", up, err)
    case (IR_INT32)
      call emit_kw(out_buf, "integer(int32)", up, err)
    case (IR_INT64)
      call emit_kw(out_buf, "integer(int64)", up, err)
    case (IR_REAL32)
      call emit_kw(out_buf, "real(real32)", up, err)
    case (IR_REAL64)
      call emit_kw(out_buf, "real(real64)", up, err)
    case (IR_REAL128)
      call emit_kw(out_buf, "real(real128)", up, err)
    case (IR_COMPLEX64)
      call emit_kw(out_buf, "complex(real32)", up, err)
    case (IR_COMPLEX128)
      call emit_kw(out_buf, "complex(real64)", up, err)
    case (IR_BOOL)
      call emit_kw(out_buf, "logical", up, err)
    case (IR_STRING_FIXED)
      call emit_kw(out_buf, "character(len=", up, err)
      call buf_append_i64(out_buf, int(max(1_i32, fld%char_len), i64), err)
      call buf_append_char(out_buf, ")", err)
    case (IR_STRING_DEFERRED)
      if (caps%has_deferred_char) then
        call emit_kw(out_buf, "character(len=:)", up, err)
        use_alloc = .true._lk
        use_ptr = .false._lk
      else
        call emit_kw(out_buf, "character(len=", up, err)
        call buf_append_i64(out_buf, int(max(1_i32, fld%char_len), i64), err)
        call buf_append_char(out_buf, ")", err)
      end if
    case (IR_ENUM)
      e_idx = ir_bundle_find_enum(bundle, fld%enum_name%raw(1:fld%enum_name%len))
      if (sid >= STD_FORTRAN_2023 .and. e_idx > 0_i32) then
        call emit_kw(out_buf, "type(", up, err)
        call buf_append_str(out_buf, &
          bundle%enums(e_idx)%fortran_name%raw(1:bundle%enums(e_idx)%fortran_name%len), err)
        call buf_append_char(out_buf, ")", err)
      else if (sid >= STD_FORTRAN_2003) then
        call emit_kw(out_buf, "integer(c_int)", up, err)
      else
        call emit_kw(out_buf, "integer(int32)", up, err)
      end if
    case default
      r_idx = ir_bundle_find_udt(bundle, fld%ref_udt_name%raw(1:fld%ref_udt_name%len))
      call emit_kw(out_buf, "type(", up, err)
      if (r_idx > 0_i32) then
        call buf_append_str(out_buf, &
          bundle%udts(r_idx)%fortran_name%raw(1:bundle%udts(r_idx)%fortran_name%len), err)
      else
        call buf_append_str(out_buf, fld%ref_udt_name%raw(1:fld%ref_udt_name%len), err)
      end if
      call buf_append_char(out_buf, ")", err)
    end select

    if (use_alloc) then
      call emit_kw(out_buf, ", allocatable", up, err)
    else if (use_ptr) then
      call emit_kw(out_buf, ", pointer", up, err)
    end if

    if (use_dcol) then
      call buf_append_str(out_buf, " :: ", err)
    else
      call buf_append_char(out_buf, " ", err)
    end if
    call buf_append_str(out_buf, fld%fortran_name%raw(1:fld%fortran_name%len), err)

    if (fld%rank > 0_i32) then
      call buf_append_char(out_buf, "(", err)
      do r = 1_i32, fld%rank
        if (r > 1_i32) call buf_append_char(out_buf, ",", err)
        if (fld%dims(r)%kind == DIM_FIXED .and. .not. use_alloc .and. .not. use_ptr) then
          if (fld%dims(r)%lower_bound /= 1_i32) then
            call buf_append_i64(out_buf, int(fld%dims(r)%lower_bound, i64), err)
            call buf_append_char(out_buf, ":", err)
          end if
          call buf_append_i64(out_buf, int(fld%dims(r)%upper_bound, i64), err)
        else
          call buf_append_char(out_buf, ":", err)
        end if
      end do
      call buf_append_char(out_buf, ")", err)
    end if

    if (caps%has_default_comp_init) then
      if (use_ptr) then
        call emit_kw(out_buf, " => null()", up, err)
      else if (fld%has_default .and. .not. use_alloc .and. fld%rank == 0_i32) then
        call emit_default_init_expr(fld, up, out_buf, err)
      end if
    end if

    if (fld%description%len > 0_i32) then
      call buf_append_str(out_buf, " !< ", err)
      call buf_append_str(out_buf, fld%description%raw(1:fld%description%len), err)
    else
      call buf_append_str(out_buf, " !< Field " // fld%json_name%raw(1:fld%json_name%len), err)
    end if
    call buf_append_str(out_buf, nl_seq, err)
  end subroutine emit_single_modern_field_line

  !> @brief Emit F2008+ `module subroutine` interface stubs when `options%emit_submodules` is enabled.
  !> @param[in]    bundle  IR bundle.
  !> @param[in]    options Dialect options.
  !> @param[inout] out_buf Destination buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_submodule_codec_interfaces(bundle, options, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_options_t), intent(in) :: options
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: u, ind, fn_len
    logical(lk) :: up
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    ind = max(2_i32, options%indent_width)
    up = options%uppercase_keywords
    call emit_indent(out_buf, ind, err)
    call emit_kw(out_buf, "interface", up, err)
    call buf_append_char(out_buf, LF, err)

    do u = 1_i32, bundle%udt_count
      fn_len = bundle%udts(u)%fortran_name%len
      call emit_indent(out_buf, ind * 2_i32, err)
      call emit_kw(out_buf, "pure recursive module subroutine ", up, err)
      call buf_append_str(out_buf, bundle%udts(u)%fortran_name%raw(1:fn_len) // &
        "_to_json(self, json_str, ierr, errmsg)" // LF, err)
      call emit_indent(out_buf, ind * 3_i32, err)
      if (bundle%udts(u)%is_bind_c) then
        call emit_kw(out_buf, "type(", up, err)
      else
        call emit_kw(out_buf, "class(", up, err)
      end if
      call buf_append_str(out_buf, bundle%udts(u)%fortran_name%raw(1:fn_len) // &
        "), intent(in) :: self" // LF, err)
      call emit_indent(out_buf, ind * 3_i32, err)
      call emit_kw(out_buf, "character(len=:), allocatable, intent(out) :: json_str", up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_indent(out_buf, ind * 3_i32, err)
      call emit_kw(out_buf, "integer(int32), intent(out) :: ierr", up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_indent(out_buf, ind * 3_i32, err)
      call emit_kw(out_buf, "character(len=:), allocatable, intent(out), optional :: errmsg", &
        up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_indent(out_buf, ind * 2_i32, err)
      call emit_kw(out_buf, "end subroutine ", up, err)
      call buf_append_str(out_buf, bundle%udts(u)%fortran_name%raw(1:fn_len) // "_to_json" // LF, err)

      call emit_indent(out_buf, ind * 2_i32, err)
      call emit_kw(out_buf, "pure recursive module subroutine ", up, err)
      call buf_append_str(out_buf, bundle%udts(u)%fortran_name%raw(1:fn_len) // &
        "_from_json(self, json_str, ierr, errmsg)" // LF, err)
      call emit_indent(out_buf, ind * 3_i32, err)
      if (bundle%udts(u)%is_bind_c) then
        call emit_kw(out_buf, "type(", up, err)
        call buf_append_str(out_buf, bundle%udts(u)%fortran_name%raw(1:fn_len), err)
        call emit_kw(out_buf, "), intent(out) :: self", up, err)
      else
        call emit_kw(out_buf, "class(", up, err)
        call buf_append_str(out_buf, bundle%udts(u)%fortran_name%raw(1:fn_len), err)
        call emit_kw(out_buf, "), intent(inout) :: self", up, err)
      end if
      call buf_append_char(out_buf, LF, err)
      call emit_indent(out_buf, ind * 3_i32, err)
      call emit_kw(out_buf, "character(len=*), intent(in) :: json_str", up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_indent(out_buf, ind * 3_i32, err)
      call emit_kw(out_buf, "integer(int32), intent(out) :: ierr", up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_indent(out_buf, ind * 3_i32, err)
      call emit_kw(out_buf, "character(len=:), allocatable, intent(out), optional :: errmsg", &
        up, err)
      call buf_append_char(out_buf, LF, err)
      call emit_indent(out_buf, ind * 2_i32, err)
      call emit_kw(out_buf, "end subroutine ", up, err)
      call buf_append_str(out_buf, bundle%udts(u)%fortran_name%raw(1:fn_len) // "_from_json" // LF, err)
    end do

    call emit_indent(out_buf, ind, err)
    call emit_kw(out_buf, "end interface", up, err)
    call buf_append_char(out_buf, LF, err)
  end subroutine emit_submodule_codec_interfaces

  !> @brief Emit F95+ inline default component initializer `= <lit>`.
  !> @param[in]    fld     Field descriptor with has_default == .true.
  !> @param[in]    up      Uppercase toggle.
  !> @param[inout] out_buf Destination buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_default_init_expr(fld, up, out_buf, err)
    type(cdd_ir_field_t), intent(in) :: fld
    logical(lk), intent(in) :: up
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: dlen

    dlen = fld%default_lexeme%len
    if (dlen == 0_i32) return
    if (fld%type_kind == IR_BOOL) then
      if (str_eq_nocase(fld%default_lexeme%raw(1:dlen), "true")) then
        call emit_kw(out_buf, " = .true.", up, err)
      else
        call emit_kw(out_buf, " = .false.", up, err)
      end if
    else if (fld%type_kind == IR_STRING_FIXED) then
      call buf_append_str(out_buf, ' = "' // fld%default_lexeme%raw(1:dlen) // '"', err)
    else if (fld%type_kind >= IR_INT8 .and. fld%type_kind <= IR_REAL128) then
      call buf_append_str(out_buf, " = " // fld%default_lexeme%raw(1:dlen), err)
    end if
  end subroutine emit_default_init_expr

  !> @brief Emit fixed-form `@@cdd-udt:` banner and optional description.
  !> @param[in]    lead    Comment character ("C").
  !> @param[in]    udt     UDT descriptor.
  !> @param[inout] out_buf Destination buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_fixed_udt_banner(lead, udt, out_buf, err)
    character(len=*, kind=c_k), intent(in) :: lead
    type(cdd_ir_udt_t), intent(in) :: udt
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call buf_append_str(out_buf, lead // " @cdd-udt: " // &
      udt%json_name%raw(1:udt%json_name%len) // LF, err)
    if (udt%description%len > 0_i32) then
      call buf_append_str(out_buf, lead // "> " // &
        udt%description%raw(1:min(68_i32, udt%description%len)) // LF, err)
    end if
  end subroutine emit_fixed_udt_banner

  !> @brief Append fixed-form `(d1, d2, ...)` array bounds for fld.
  !> @param[in]    fld     Field descriptor.
  !> @param[inout] out_buf Destination buffer.
  !> @param[inout] err     Error status.
  pure subroutine append_fixed_dims(fld, out_buf, err)
    type(cdd_ir_field_t), intent(in) :: fld
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: r, ub

    call buf_append_char(out_buf, "(", err)
    do r = 1_i32, fld%rank
      if (r > 1_i32) call buf_append_char(out_buf, ",", err)
      ub = max(1_i32, fld%dims(r)%upper_bound)
      call buf_append_i64(out_buf, int(ub, i64), err)
    end do
    call buf_append_char(out_buf, ")", err)
  end subroutine append_fixed_dims

  !> @brief Compute 6-char named COMMON block labels (with N/C suffixes if F77 split).
  !> @param[in]  base_nm  Base UDT <=6-char name.
  !> @param[in]  do_split True when F77 character/numeric split is active.
  !> @param[out] blk_num  Numeric COMMON block name.
  !> @param[out] bn_len   Length of blk_num.
  !> @param[out] blk_chr  Character COMMON block name.
  !> @param[out] bc_len   Length of blk_chr.
  pure subroutine make_common_names(base_nm, do_split, blk_num, bn_len, blk_chr, bc_len)
    character(len=*, kind=c_k), intent(in) :: base_nm
    logical(lk), intent(in) :: do_split
    character(len=6, kind=c_k), intent(out) :: blk_num
    integer(i32), intent(out) :: bn_len
    character(len=6, kind=c_k), intent(out) :: blk_chr
    integer(i32), intent(out) :: bc_len
    integer(i32) :: k, keep_n

    if (do_split) then
      keep_n = min(5_i32, len(base_nm, kind=i32))
      do k = 1_i32, keep_n
        blk_num(k:k) = base_nm(k:k)
        blk_chr(k:k) = base_nm(k:k)
      end do
      bn_len = keep_n + 1_i32
      bc_len = keep_n + 1_i32
      blk_num(bn_len:bn_len) = "N"
      blk_chr(bc_len:bc_len) = "C"
    else
      keep_n = min(6_i32, len(base_nm, kind=i32))
      do k = 1_i32, keep_n
        blk_num(k:k) = base_nm(k:k)
        blk_chr(k:k) = base_nm(k:k)
      end do
      bn_len = keep_n
      bc_len = keep_n
    end if
  end subroutine make_common_names

  !> @brief Emit a named `COMMON /blk/ ...` line (filtering character vs non-character in F77).
  !> @param[in]    udt       UDT definition.
  !> @param[in]    blk_name  COMMON block name.
  !> @param[in]    is_f77    True when F77 CHARACTER type is active.
  !> @param[in]    want_char True to emit CHARACTER members, false for numeric/logical members.
  !> @param[inout] out_buf   Destination buffer.
  !> @param[inout] err       Error status.
  pure subroutine emit_common_line(udt, blk_name, is_f77, want_char, out_buf, err)
    type(cdd_ir_udt_t), intent(in) :: udt
    character(len=*, kind=c_k), intent(in) :: blk_name
    logical(lk), intent(in) :: is_f77
    logical(lk), intent(in) :: want_char
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: i, emitted, col_len, fn_len
    logical(lk) :: is_chr
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call buf_append_str(out_buf, "      COMMON /" // blk_name // "/ ", err)
    col_len = 16_i32 + len(blk_name, kind=i32)
    emitted = 0_i32
    do i = 1_i32, udt%field_count
      is_chr = is_f77 .and. (udt%fields(i)%type_kind == IR_STRING_FIXED .or. &
                             udt%fields(i)%type_kind == IR_STRING_DEFERRED .or. &
                             udt%fields(i)%type_kind == IR_ENUM)
      if (is_chr .eqv. want_char) then
        if (emitted > 0_i32) then
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
        emitted = emitted + 1_i32
      end if
    end do
    call buf_append_char(out_buf, LF, err)
  end subroutine emit_common_line

  !> @brief Emit ANSI FORTRAN 66 `BLOCK DATA` unit with Hollerith `nH` string defaults.
  !> @param[in]    udt      UDT definition.
  !> @param[in]    blk_name COMMON block name.
  !> @param[inout] out_buf  Destination buffer.
  !> @param[inout] err      Error status.
  pure subroutine emit_f66_block_data(udt, blk_name, out_buf, err)
    type(cdd_ir_udt_t), intent(in) :: udt
    character(len=*, kind=c_k), intent(in) :: blk_name
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: i, dlen
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call buf_append_str(out_buf, "      BLOCK DATA BD" // &
      blk_name(1:min(4_i32, len(blk_name, i32))) // LF, err)
    call emit_common_line(udt, blk_name, .false._lk, .false._lk, out_buf, err)
    do i = 1_i32, udt%field_count
      dlen = udt%fields(i)%default_lexeme%len
      if (udt%fields(i)%has_default .and. dlen > 0_i32) then
        call buf_append_str(out_buf, "      DATA ", err)
        call buf_append_str(out_buf, &
          udt%fields(i)%fortran_name%raw(1:udt%fields(i)%fortran_name%len), err)
        call buf_append_str(out_buf, " /", err)
        if (udt%fields(i)%type_kind == IR_STRING_FIXED .or. &
            udt%fields(i)%type_kind == IR_STRING_DEFERRED) then
          call buf_append_i64(out_buf, int(dlen, i64), err)
          call buf_append_str(out_buf, "H" // udt%fields(i)%default_lexeme%raw(1:dlen), err)
        else if (udt%fields(i)%type_kind == IR_BOOL) then
          if (str_eq_nocase(udt%fields(i)%default_lexeme%raw(1:dlen), "true")) then
            call buf_append_str(out_buf, ".TRUE.", err)
          else
            call buf_append_str(out_buf, ".FALSE.", err)
          end if
        else
          call buf_append_str(out_buf, udt%fields(i)%default_lexeme%raw(1:dlen), err)
        end if
        call buf_append_str(out_buf, "/" // LF, err)
      end if
    end do
    call buf_append_str(out_buf, "      END" // LF, err)
  end subroutine emit_f66_block_data

  !> @brief Append `count` leading spaces to out_buf.
  !> @param[inout] out_buf Destination buffer.
  !> @param[in]    count   Number of spaces.
  !> @param[inout] err     Error status.
  pure subroutine emit_indent(out_buf, count, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: count
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: k

    do k = 1_i32, count
      call buf_append_char(out_buf, " ", err)
    end do
  end subroutine emit_indent

  !> @brief Append keyword text to out_buf in lowercase or UPPERCASE per `up`.
  !> @param[inout] out_buf Destination buffer.
  !> @param[in]    kw      Lowercase keyword phrase.
  !> @param[in]    up      True to emit in UPPERCASE.
  !> @param[inout] err     Error status.
  pure subroutine emit_kw(out_buf, kw, up, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    character(len=*, kind=c_k), intent(in) :: kw
    logical(lk), intent(in) :: up
    type(cdd_error_t), intent(inout) :: err
    character(len=128, kind=c_k) :: ubuf
    integer(i32) :: ulen

    if (up) then
      call to_upper_buf(kw, ubuf, ulen)
      call buf_append_str(out_buf, ubuf(1:ulen), err)
    else
      call buf_append_str(out_buf, kw, err)
    end if
  end subroutine emit_kw

  !> @brief Compute dependency-first topological ordering of UDT indices 1..bundle%udt_count.
  !> @param[in]  bundle Canonical IR bundle.
  !> @param[out] order  Ordered 1-based UDT indices in order(1:bundle%udt_count).
  pure subroutine compute_udt_topo_order(bundle, order)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    integer(i32), intent(out) :: order(CDD_MAX_UDTS)
    logical(lk) :: seen(CDD_MAX_UDTS)
    integer(i32) :: u, cnt

    order = 0_i32
    seen = .false._lk
    cnt = 0_i32
    do u = 1_i32, bundle%udt_count
      if (.not. seen(u)) then
        call visit_udt_deps(bundle, u, seen, order, cnt)
      end if
    end do
  end subroutine compute_udt_topo_order

  !> @brief Depth-first dependency visitor for compute_udt_topo_order.
  !> @param[in]    bundle Canonical IR bundle.
  !> @param[in]    u      Current UDT index.
  !> @param[inout] seen   Visited markers (breaks self/mutual cycles).
  !> @param[inout] order  Output topological order array.
  !> @param[inout] cnt    Number of UDTs appended so far.
  recursive pure subroutine visit_udt_deps(bundle, u, seen, order, cnt)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    integer(i32), intent(in) :: u
    logical(lk), intent(inout) :: seen(CDD_MAX_UDTS)
    integer(i32), intent(inout) :: order(CDD_MAX_UDTS)
    integer(i32), intent(inout) :: cnt
    integer(i32) :: f, dep_u

    seen(u) = .true._lk
    if (bundle%udts(u)%has_extends) then
      dep_u = ir_bundle_find_udt(bundle, bundle%udts(u)%extends_name%raw( &
        1:bundle%udts(u)%extends_name%len))
      if (dep_u > 0_i32) then
        if (.not. seen(dep_u)) call visit_udt_deps(bundle, dep_u, seen, order, cnt)
      end if
    end if

    do f = 1_i32, bundle%udts(u)%field_count
      if (bundle%udts(u)%fields(f)%type_kind == IR_NESTED_UDT) then
        dep_u = ir_bundle_find_udt(bundle, bundle%udts(u)%fields(f)%ref_udt_name%raw( &
          1:bundle%udts(u)%fields(f)%ref_udt_name%len))
        if (dep_u > 0_i32) then
          if (.not. seen(dep_u)) call visit_udt_deps(bundle, dep_u, seen, order, cnt)
        end if
      end if
    end do

    cnt = cnt + 1_i32
    order(cnt) = u
  end subroutine visit_udt_deps

end module cdd_emit_udt
