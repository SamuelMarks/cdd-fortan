!> @file cdd_standard.f90
!> @brief 12-standard Fortran dialect registry, syntactic capability matrix, and SDK options.

!> @brief Strongly-typed definitions for FORTRAN (I) through Fortran 2023 and SDK options.
module cdd_standard
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set_simple, CDD_ERR_INVALID_OPTION
  use cdd_string, only: cdd_string_t, str_clear, trim_bounds, to_lower_buf, str_eq
  implicit none (type, external)
  private

  public :: STD_FORTRAN_I, STD_FORTRAN_II, STD_FORTRAN_III, STD_FORTRAN_IV
  public :: STD_FORTRAN_66, STD_FORTRAN_77, STD_FORTRAN_90, STD_FORTRAN_95
  public :: STD_FORTRAN_2003, STD_FORTRAN_2008, STD_FORTRAN_2018, STD_FORTRAN_2023
  public :: FORM_AUTO, FORM_FIXED, FORM_FREE
  public :: JSON_MODE_AUTO, JSON_MODE_SCHEMA, JSON_MODE_SAMPLE
  public :: cdd_standard_t, cdd_dialect_caps_t, cdd_options_t
  public :: parse_standard_name, standard_display_name
  public :: get_dialect_capabilities, cdd_default_options, resolve_effective_form

  integer(i32), parameter :: STD_FORTRAN_I = 1_i32     !< FORTRAN (I) (1957, IBM 704).
  integer(i32), parameter :: STD_FORTRAN_II = 2_i32    !< FORTRAN II (1958).
  integer(i32), parameter :: STD_FORTRAN_III = 3_i32   !< FORTRAN III (1958).
  integer(i32), parameter :: STD_FORTRAN_IV = 4_i32    !< FORTRAN IV (1962).
  integer(i32), parameter :: STD_FORTRAN_66 = 5_i32    !< FORTRAN 66 (ANSI X3.9-1966).
  integer(i32), parameter :: STD_FORTRAN_77 = 6_i32    !< FORTRAN 77 (ANSI X3.9-1978).
  integer(i32), parameter :: STD_FORTRAN_90 = 7_i32    !< Fortran 90 (ISO/IEC 1539:1991).
  integer(i32), parameter :: STD_FORTRAN_95 = 8_i32    !< Fortran 95 (ISO/IEC 1539-1:1997).
  integer(i32), parameter :: STD_FORTRAN_2003 = 9_i32  !< Fortran 2003 (ISO/IEC 1539-1:2004).
  integer(i32), parameter :: STD_FORTRAN_2008 = 10_i32 !< Fortran 2008 (ISO/IEC 1539-1:2010).
  integer(i32), parameter :: STD_FORTRAN_2018 = 11_i32 !< Fortran 2018 (ISO/IEC 1539-1:2018).
  integer(i32), parameter :: STD_FORTRAN_2023 = 12_i32 !< Fortran 2023 (ISO/IEC 1539-1:2023).

  integer(i32), parameter :: FORM_AUTO = 0_i32         !< Auto-select fixed/free form by standard.
  integer(i32), parameter :: FORM_FIXED = 1_i32        !< Fixed punch-card source form (cols 1-80).
  integer(i32), parameter :: FORM_FREE = 2_i32         !< Free source form (F90+).

  integer(i32), parameter :: JSON_MODE_AUTO = 0_i32    !< Auto-detect JSON Schema vs sample JSON.
  integer(i32), parameter :: JSON_MODE_SCHEMA = 1_i32  !< Emit/consume JSON Schema Draft 2020-12.
  integer(i32), parameter :: JSON_MODE_SAMPLE = 2_i32  !< Emit/consume raw sample JSON payload.

  !> @brief Strongly-typed wrapper for a validated Fortran standard identifier.
  type :: cdd_standard_t
    integer(i32) :: id = STD_FORTRAN_2023 !< Canonical standard constant (STD_FORTRAN_I..2023).
  end type cdd_standard_t

  !> @brief Capability descriptor governing lexing, parsing, UDT layout, and codec codegen.
  type :: cdd_dialect_caps_t
    integer(i32) :: default_form = FORM_FREE           !< Default source form (FORM_FIXED or FORM_FREE).
    logical(lk) :: supports_free_form = .true._lk      !< True if free-form is permitted by standard.
    integer(i32) :: max_line_width = 10000_i32         !< Max statement characters per line.
    integer(i32) :: max_continuations = 255_i32        !< Max continuation lines per statement.
    integer(i32) :: max_ident_len = 63_i32             !< Max identifier length in characters.
    logical(lk) :: allows_underscore = .true._lk       !< True if '_' is allowed inside identifiers.
    logical(lk) :: implicit_only_typing = .false._lk   !< True if I-N / A-H,O-Z implicit typing is mandatory.
    logical(lk) :: has_subprograms = .true._lk         !< False in strict 1957 FORTRAN I (uses ASSIGN GO TO).
    logical(lk) :: has_blank_common = .true._lk        !< True in FORTRAN II and later.
    logical(lk) :: has_named_common = .true._lk        !< True in FORTRAN IV and later.
    logical(lk) :: has_logical_type = .true._lk        !< True in FORTRAN IV and later.
    logical(lk) :: has_boolean_word_ops = .false._lk   !< True in transitional 1958 FORTRAN III.
    logical(lk) :: has_character_type = .true._lk      !< True in FORTRAN 77 and later.
    logical(lk) :: has_block_if = .true._lk            !< True in FORTRAN 77 and later.
    logical(lk) :: has_dec_structure = .false._lk      !< True when DEC STRUCTURE/RECORD opt-in is active.
    logical(lk) :: has_modules_and_type = .true._lk    !< True in Fortran 90 and later.
    logical(lk) :: has_default_comp_init = .true._lk   !< True in Fortran 95 and later.
    logical(lk) :: has_pure_procs = .true._lk          !< True in Fortran 95 and later.
    logical(lk) :: has_allocatable_comp = .true._lk    !< True in Fortran 2003 and later.
    logical(lk) :: has_deferred_char = .true._lk       !< True in Fortran 2003 and later.
    logical(lk) :: has_oop_class_extends = .true._lk   !< True in Fortran 2003 and later.
    logical(lk) :: has_submodules = .true._lk          !< True in Fortran 2008 and later.
    logical(lk) :: has_recursive_alloc = .true._lk     !< True in Fortran 2008 and later.
    logical(lk) :: has_implicit_none_ext = .true._lk   !< True in Fortran 2018 and later.
    logical(lk) :: has_enumeration_type = .true._lk    !< True in Fortran 2023.
  end type cdd_dialect_caps_t

  !> @brief Unified configuration options controlling SDK conversion, codegen, and patching.
  type :: cdd_options_t
    type(cdd_standard_t) :: standard                   !< Target/source Fortran standard.
    integer(i32) :: source_form = FORM_AUTO            !< Source form override (FORM_AUTO/FIXED/FREE).
    integer(i32) :: json_mode = JSON_MODE_AUTO         !< JSON representation mode.
    logical(lk) :: allow_dec_structure = .false._lk    !< Allow DEC STRUCTURE/RECORD in IV/77.
    logical(lk) :: emit_submodules = .false._lk        !< Emit F2008+ codec bodies in a SUBMODULE.
    logical(lk) :: emit_type_bound_procs = .true._lk   !< Emit F2003+ TBPs (to_json/from_json).
    logical(lk) :: include_codecs_in_sync = .true._lk  !< Update/create codecs during sync.
    integer(i32) :: indent_width = 2_i32               !< Indentation spaces in free-form/JSON output.
    logical(lk) :: uppercase_keywords = .false._lk     !< Force UPPERCASE Fortran keywords in free-form.
    type(cdd_string_t) :: target_udt_name              !< Optional explicit target UDT name.
  end type cdd_options_t

contains

  !> @brief Construct default SDK options targeting Fortran 2023 in free form.
  !> @param[out] opts Initialized options record.
  pure subroutine cdd_default_options(opts)
    type(cdd_options_t), intent(out) :: opts

    opts%standard%id = STD_FORTRAN_2023
    opts%source_form = FORM_AUTO
    opts%json_mode = JSON_MODE_AUTO
    opts%allow_dec_structure = .false._lk
    opts%emit_submodules = .false._lk
    opts%emit_type_bound_procs = .true._lk
    opts%include_codecs_in_sync = .true._lk
    opts%indent_width = 2_i32
    opts%uppercase_keywords = .false._lk
    call str_clear(opts%target_udt_name)
  end subroutine cdd_default_options

  !> @brief Parse a human-friendly or CLI-short Fortran standard name into cdd_standard_t.
  !> @param[in]  text Input standard string (e.g. "FORTRAN (I)", "f77", "Fortran 2023").
  !> @param[out] std  Parsed strongly-typed standard wrapper.
  !> @param[out] err  Error status (CDD_ERR_INVALID_OPTION if unrecognized).
  pure subroutine parse_standard_name(text, std, err)
    character(len=*, kind=c_k), intent(in) :: text
    type(cdd_standard_t), intent(out) :: std
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: lo, hi, n
    character(len=64, kind=c_k) :: norm

    call cdd_error_clear(err)
    std%id = STD_FORTRAN_2023
    call trim_bounds(text, lo, hi)
    if (lo > hi) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Empty Fortran standard name")
      return
    end if
    call to_lower_buf(text(lo:hi), norm, n)

    if (str_eq(norm(1:n), "f57") .or. str_eq(norm(1:n), "fortran1") .or. &
        str_eq(norm(1:n), "fortran-i") .or. str_eq(norm(1:n), "fortran i") .or. &
        str_eq(norm(1:n), "fortran (i)")) then
      std%id = STD_FORTRAN_I
    else if (str_eq(norm(1:n), "f58") .or. str_eq(norm(1:n), "fortran2") .or. &
             str_eq(norm(1:n), "fortran-ii") .or. str_eq(norm(1:n), "fortran ii")) then
      std%id = STD_FORTRAN_II
    else if (str_eq(norm(1:n), "f58-iii") .or. str_eq(norm(1:n), "f58b") .or. &
             str_eq(norm(1:n), "fortran3") .or. str_eq(norm(1:n), "fortran-iii") .or. &
             str_eq(norm(1:n), "fortran iii")) then
      std%id = STD_FORTRAN_III
    else if (str_eq(norm(1:n), "f62") .or. str_eq(norm(1:n), "fortran4") .or. &
             str_eq(norm(1:n), "fortran-iv") .or. str_eq(norm(1:n), "fortran iv")) then
      std%id = STD_FORTRAN_IV
    else if (str_eq(norm(1:n), "f66") .or. str_eq(norm(1:n), "fortran66") .or. &
             str_eq(norm(1:n), "fortran-66") .or. str_eq(norm(1:n), "fortran 66")) then
      std%id = STD_FORTRAN_66
    else if (str_eq(norm(1:n), "f77") .or. str_eq(norm(1:n), "fortran77") .or. &
             str_eq(norm(1:n), "fortran-77") .or. str_eq(norm(1:n), "fortran 77")) then
      std%id = STD_FORTRAN_77
    else if (str_eq(norm(1:n), "f90") .or. str_eq(norm(1:n), "fortran90") .or. &
             str_eq(norm(1:n), "fortran-90") .or. str_eq(norm(1:n), "fortran 90")) then
      std%id = STD_FORTRAN_90
    else if (str_eq(norm(1:n), "f95") .or. str_eq(norm(1:n), "fortran95") .or. &
             str_eq(norm(1:n), "fortran-95") .or. str_eq(norm(1:n), "fortran 95")) then
      std%id = STD_FORTRAN_95
    else if (str_eq(norm(1:n), "f2003") .or. str_eq(norm(1:n), "f03") .or. &
             str_eq(norm(1:n), "fortran2003") .or. str_eq(norm(1:n), "fortran-2003") .or. &
             str_eq(norm(1:n), "fortran 2003")) then
      std%id = STD_FORTRAN_2003
    else if (str_eq(norm(1:n), "f2008") .or. str_eq(norm(1:n), "f08") .or. &
             str_eq(norm(1:n), "fortran2008") .or. str_eq(norm(1:n), "fortran-2008") .or. &
             str_eq(norm(1:n), "fortran 2008")) then
      std%id = STD_FORTRAN_2008
    else if (str_eq(norm(1:n), "f2018") .or. str_eq(norm(1:n), "f18") .or. &
             str_eq(norm(1:n), "fortran2018") .or. str_eq(norm(1:n), "fortran-2018") .or. &
             str_eq(norm(1:n), "fortran 2018")) then
      std%id = STD_FORTRAN_2018
    else if (str_eq(norm(1:n), "f2023") .or. str_eq(norm(1:n), "f23") .or. &
             str_eq(norm(1:n), "fortran2023") .or. str_eq(norm(1:n), "fortran-2023") .or. &
             str_eq(norm(1:n), "fortran 2023")) then
      std%id = STD_FORTRAN_2023
    else
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Unknown Fortran standard name")
    end if
  end subroutine parse_standard_name

  !> @brief Return the canonical specification display title for a Fortran standard.
  !> @param[in]  std      Standard wrapper.
  !> @param[out] name_buf Output string buffer (at least 16 chars recommended).
  !> @param[out] name_len Active character count written.
  pure subroutine standard_display_name(std, name_buf, name_len)
    type(cdd_standard_t), intent(in) :: std
    character(len=*, kind=c_k), intent(out) :: name_buf
    integer(i32), intent(out) :: name_len

    select case (std%id)
    case (STD_FORTRAN_I)
      call copy_title("FORTRAN (I)", name_buf, name_len)
    case (STD_FORTRAN_II)
      call copy_title("FORTRAN II", name_buf, name_len)
    case (STD_FORTRAN_III)
      call copy_title("FORTRAN III", name_buf, name_len)
    case (STD_FORTRAN_IV)
      call copy_title("FORTRAN IV", name_buf, name_len)
    case (STD_FORTRAN_66)
      call copy_title("FORTRAN 66", name_buf, name_len)
    case (STD_FORTRAN_77)
      call copy_title("FORTRAN 77", name_buf, name_len)
    case (STD_FORTRAN_90)
      call copy_title("Fortran 90", name_buf, name_len)
    case (STD_FORTRAN_95)
      call copy_title("Fortran 95", name_buf, name_len)
    case (STD_FORTRAN_2003)
      call copy_title("Fortran 2003", name_buf, name_len)
    case (STD_FORTRAN_2008)
      call copy_title("Fortran 2008", name_buf, name_len)
    case (STD_FORTRAN_2018)
      call copy_title("Fortran 2018", name_buf, name_len)
    case default
      call copy_title("Fortran 2023", name_buf, name_len)
    end select
  end subroutine standard_display_name

  !> @brief Compute the complete syntactic and semantic capability matrix for a standard.
  !> @param[in] std                 Target Fortran standard.
  !> @param[in] allow_dec_structure True if DEC STRUCTURE/RECORD extension is enabled.
  !> @return Dialect capability matrix record.
  pure function get_dialect_capabilities(std, allow_dec_structure) result(caps)
    type(cdd_standard_t), intent(in) :: std
    logical(lk), intent(in) :: allow_dec_structure
    type(cdd_dialect_caps_t) :: caps
    integer(i32) :: id

    id = std%id
    if (id < STD_FORTRAN_90) then
      caps%default_form = FORM_FIXED
      caps%supports_free_form = .false._lk
      caps%max_line_width = 72_i32
    else
      caps%default_form = FORM_FREE
      caps%supports_free_form = .true._lk
      if (id >= STD_FORTRAN_2023) then
        caps%max_line_width = 10000_i32
      else
        caps%max_line_width = 132_i32
      end if
    end if

    if (id == STD_FORTRAN_I) then
      caps%max_continuations = 9_i32
    else if (id <= STD_FORTRAN_77) then
      caps%max_continuations = 19_i32
    else if (id <= STD_FORTRAN_95) then
      caps%max_continuations = 39_i32
    else
      caps%max_continuations = 255_i32
    end if

    caps%has_dec_structure = allow_dec_structure .and. &
      (id == STD_FORTRAN_IV .or. id == STD_FORTRAN_77)

    if (id <= STD_FORTRAN_77) then
      if (caps%has_dec_structure) then
        caps%max_ident_len = 31_i32
        caps%allows_underscore = .true._lk
      else
        caps%max_ident_len = 6_i32
        caps%allows_underscore = .false._lk
      end if
    else if (id <= STD_FORTRAN_95) then
      caps%max_ident_len = 31_i32
      caps%allows_underscore = .true._lk
    else
      caps%max_ident_len = 63_i32
      caps%allows_underscore = .true._lk
    end if

    caps%implicit_only_typing = (id <= STD_FORTRAN_III)
    caps%has_subprograms = (id >= STD_FORTRAN_II)
    caps%has_blank_common = (id >= STD_FORTRAN_II)
    caps%has_named_common = (id >= STD_FORTRAN_IV)
    caps%has_logical_type = (id >= STD_FORTRAN_IV)
    caps%has_boolean_word_ops = (id == STD_FORTRAN_III)
    caps%has_character_type = (id >= STD_FORTRAN_77)
    caps%has_block_if = (id >= STD_FORTRAN_77)
    caps%has_modules_and_type = (id >= STD_FORTRAN_90)
    caps%has_default_comp_init = (id >= STD_FORTRAN_95)
    caps%has_pure_procs = (id >= STD_FORTRAN_95)
    caps%has_allocatable_comp = (id >= STD_FORTRAN_2003)
    caps%has_deferred_char = (id >= STD_FORTRAN_2003)
    caps%has_oop_class_extends = (id >= STD_FORTRAN_2003)
    caps%has_submodules = (id >= STD_FORTRAN_2008)
    caps%has_recursive_alloc = (id >= STD_FORTRAN_2008)
    caps%has_implicit_none_ext = (id >= STD_FORTRAN_2018)
    caps%has_enumeration_type = (id >= STD_FORTRAN_2023)
  end function get_dialect_capabilities

  !> @brief Resolve the effective source form (FORM_FIXED vs FORM_FREE) and validate against standard.
  !> @param[in]  opts SDK options record.
  !> @param[out] form Resolved source form (FORM_FIXED or FORM_FREE).
  !> @param[out] err  Error status if FORM_FREE is requested for a fixed-only legacy dialect.
  pure subroutine resolve_effective_form(opts, form, err)
    type(cdd_options_t), intent(in) :: opts
    integer(i32), intent(out) :: form
    type(cdd_error_t), intent(out) :: err
    type(cdd_dialect_caps_t) :: caps

    call cdd_error_clear(err)
    caps = get_dialect_capabilities(opts%standard, opts%allow_dec_structure)
    if (opts%source_form == FORM_AUTO) then
      form = caps%default_form
    else if (opts%source_form == FORM_FIXED) then
      form = FORM_FIXED
    else if (opts%source_form == FORM_FREE) then
      if (.not. caps%supports_free_form) then
        form = FORM_FIXED
        call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, &
          "Free source form is not supported prior to Fortran 90")
      else
        form = FORM_FREE
      end if
    else
      form = caps%default_form
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Invalid source_form code")
    end if
  end subroutine resolve_effective_form

  !> @brief Copy a literal title string into dst(1:out_len) without hidden padding branches.
  !> @param[in]  lit     Title literal.
  !> @param[out] dst     Destination buffer.
  !> @param[out] out_len Characters copied.
  pure subroutine copy_title(lit, dst, out_len)
    character(len=*, kind=c_k), intent(in) :: lit
    character(len=*, kind=c_k), intent(out) :: dst
    integer(i32), intent(out) :: out_len
    integer(i32) :: i, n, cap

    n = len(lit, kind=i32)
    cap = len(dst, kind=i32)
    if (n > cap) then
      n = cap
    end if
    out_len = n
    do i = 1_i32, n
      dst(i:i) = lit(i:i)
    end do
  end subroutine copy_title

end module cdd_standard
