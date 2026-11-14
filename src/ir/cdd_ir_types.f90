!> @file cdd_ir_types.f90
!> @brief Canonical Intermediate Representation (IR) for Fortran UDTs, fields, arrays, and enums.

!> @brief Strongly-typed IR structures bridging JSON Schema / JSON samples and FORTRAN I..2023 ASTs.
module cdd_ir_types
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set_simple, &
    CDD_ERR_BUFFER_OVERFLOW, CDD_ERR_INVALID_OPTION
  use cdd_string, only: cdd_string_t, str_clear, str_eq, str_eq_nocase
  implicit none (type, external)
  private

  public :: CDD_IDENT_MAX, CDD_MAX_RANK, CDD_MAX_FIELDS, CDD_MAX_UDTS, CDD_MAX_ENUMS, CDD_MAX_ENUM_VALS
  public :: IR_INT8, IR_INT16, IR_INT32, IR_INT64
  public :: IR_REAL32, IR_REAL64, IR_REAL128
  public :: IR_COMPLEX64, IR_COMPLEX128
  public :: IR_BOOL, IR_STRING_FIXED, IR_STRING_DEFERRED, IR_ENUM, IR_NESTED_UDT
  public :: DIM_FIXED, DIM_DEFERRED, DIM_POINTER, DIM_ASSUMED
  public :: cdd_ident_t, cdd_ir_dim_t, cdd_ir_field_t, cdd_ir_enum_t, cdd_ir_udt_t, cdd_ir_schema_bundle_t
  public :: ident_clear, ident_set, ident_eq
  public :: ir_dim_init, ir_field_init, ir_enum_init, ir_udt_init, ir_bundle_init
  public :: ir_bundle_add_udt, ir_bundle_find_udt, ir_udt_add_field, ir_udt_find_field
  public :: ir_bundle_add_enum, ir_bundle_find_enum
  public :: ir_type_to_tag, ir_tag_to_type

  integer(i32), parameter :: CDD_IDENT_MAX = 64_i32      !< Max identifier / JSON property key length.
  integer(i32), parameter :: CDD_MAX_RANK = 15_i32       !< Max Fortran array rank (ISO Fortran 2008+).
  integer(i32), parameter :: CDD_MAX_FIELDS = 48_i32     !< Max fields per UDT in static arena.
  integer(i32), parameter :: CDD_MAX_UDTS = 16_i32       !< Max UDTs per schema bundle.
  integer(i32), parameter :: CDD_MAX_ENUMS = 16_i32      !< Max enumerations per schema bundle.
  integer(i32), parameter :: CDD_MAX_ENUM_VALS = 16_i32  !< Max enumerators per enumeration.

  integer(i32), parameter :: IR_INT8 = 1_i32             !< 8-bit signed integer scalar/element.
  integer(i32), parameter :: IR_INT16 = 2_i32            !< 16-bit signed integer scalar/element.
  integer(i32), parameter :: IR_INT32 = 3_i32            !< 32-bit signed integer scalar/element.
  integer(i32), parameter :: IR_INT64 = 4_i32            !< 64-bit signed integer scalar/element.
  integer(i32), parameter :: IR_REAL32 = 5_i32           !< 32-bit single-precision float.
  integer(i32), parameter :: IR_REAL64 = 6_i32           !< 64-bit double-precision float.
  integer(i32), parameter :: IR_REAL128 = 7_i32          !< 128-bit quadruple-precision float.
  integer(i32), parameter :: IR_COMPLEX64 = 8_i32        !< Single-precision complex number.
  integer(i32), parameter :: IR_COMPLEX128 = 9_i32       !< Double-precision complex number.
  integer(i32), parameter :: IR_BOOL = 10_i32            !< Boolean / logical value.
  integer(i32), parameter :: IR_STRING_FIXED = 11_i32    !< Bounded-length string (char_len >= 1).
  integer(i32), parameter :: IR_STRING_DEFERRED = 12_i32 !< Deferred-length allocatable string.
  integer(i32), parameter :: IR_ENUM = 13_i32            !< Enumeration value referencing enum_name.
  integer(i32), parameter :: IR_NESTED_UDT = 14_i32      !< Nested child UDT referencing ref_udt_name.

  integer(i32), parameter :: DIM_FIXED = 1_i32           !< Explicit bounds lower_bound:upper_bound.
  integer(i32), parameter :: DIM_DEFERRED = 2_i32        !< Deferred allocatable dimension (:).
  integer(i32), parameter :: DIM_POINTER = 3_i32         !< Deferred pointer dimension (:).
  integer(i32), parameter :: DIM_ASSUMED = 4_i32         !< Assumed-size dimension (*).

  !> @brief Compact 64-byte identifier container for property keys and Fortran names.
  type :: cdd_ident_t
    integer(i32) :: len = 0_i32                  !< Active byte length in raw(1:len).
    character(len=CDD_IDENT_MAX, kind=c_k) :: raw !< Fixed 64-byte character array.
  end type cdd_ident_t

  !> @brief Single array dimension specification within a field declaration.
  type :: cdd_ir_dim_t
    integer(i32) :: kind = DIM_FIXED   !< Bound category (DIM_FIXED/DEFERRED/POINTER/ASSUMED).
    integer(i32) :: lower_bound = 1_i32 !< Explicit lower bound when kind == DIM_FIXED.
    integer(i32) :: upper_bound = 1_i32 !< Explicit upper bound when kind == DIM_FIXED.
  end type cdd_ir_dim_t

  !> @brief Canonical representation of one UDT member/field and its JSON/Fortran metadata.
  type :: cdd_ir_field_t
    type(cdd_ident_t) :: json_name              !< Exact original JSON property name.
    type(cdd_ident_t) :: fortran_name           !< Dialect-legal Fortran component/variable name.
    integer(i32) :: type_kind = IR_INT32        !< Canonical IR type constant (IR_*).
    integer(i32) :: char_len = 32_i32           !< Fixed string length when IR_STRING_FIXED.
    type(cdd_ident_t) :: ref_udt_name           !< Target UDT name when IR_NESTED_UDT.
    type(cdd_ident_t) :: enum_name              !< Target enum name when IR_ENUM.
    integer(i32) :: rank = 0_i32                !< Array rank (0 = scalar, 1..15 = array).
    type(cdd_ir_dim_t) :: dims(CDD_MAX_RANK)    !< Per-dimension bounds for 1..rank.
    logical(lk) :: is_required = .true._lk      !< True if listed in JSON Schema 'required'.
    logical(lk) :: is_nullable = .false._lk     !< True if JSON allows null value.
    logical(lk) :: is_allocatable = .false._lk  !< True if Fortran component is ALLOCATABLE.
    logical(lk) :: is_pointer = .false._lk      !< True if Fortran component is POINTER.
    logical(lk) :: has_default = .false._lk     !< True if default_lexeme is populated.
    type(cdd_ident_t) :: default_lexeme         !< Canonical literal representation of default.
    type(cdd_string_t) :: description           !< Bidirectional Doxygen / Schema description.
  end type cdd_ir_field_t

  !> @brief Canonical enumeration definition mapped between JSON enum and Fortran ENUM.
  type :: cdd_ir_enum_t
    type(cdd_ident_t) :: json_name                     !< Original JSON enum schema name.
    type(cdd_ident_t) :: fortran_name                  !< Fortran enum type identifier.
    integer(i32) :: val_count = 0_i32                  !< Number of enumerators.
    integer(i32) :: max_vals = CDD_MAX_ENUM_VALS       !< Configurable enumerator limit.
    type(cdd_ident_t) :: json_vals(CDD_MAX_ENUM_VALS)  !< Original JSON string enumerators.
    type(cdd_ident_t) :: f_vals(CDD_MAX_ENUM_VALS)     !< Sanitized Fortran enumerator names.
    type(cdd_string_t) :: description                  !< Optional enumeration docstring.
  end type cdd_ir_enum_t

  !> @brief Canonical User-Defined Type (struct/record/class/common) definition.
  type :: cdd_ir_udt_t
    type(cdd_ident_t) :: json_name                 !< Original JSON title / schema name.
    type(cdd_ident_t) :: fortran_name              !< Dialect-legal UDT / COMMON / STRUCTURE name.
    logical(lk) :: has_extends = .false._lk        !< True if derived type extends parent UDT.
    type(cdd_ident_t) :: extends_name              !< Parent UDT name when has_extends is true.
    logical(lk) :: is_bind_c = .false._lk          !< True if BIND(C) interop struct.
    integer(i32) :: field_count = 0_i32            !< Number of active fields.
    integer(i32) :: max_fields = CDD_MAX_FIELDS    !< Configurable field quota for testing.
    type(cdd_ir_field_t) :: fields(CDD_MAX_FIELDS) !< Ordered member field definitions.
    type(cdd_string_t) :: description              !< UDT-level Doxygen / Schema description.
  end type cdd_ir_udt_t

  !> @brief Top-level container of all UDTs and enumerations in a module/schema unit.
  type :: cdd_ir_schema_bundle_t
    type(cdd_ident_t) :: module_name             !< Enclosing Fortran module identifier.
    integer(i32) :: main_udt_idx = 0_i32         !< 1-based primary UDT index (0 if empty).
    integer(i32) :: udt_count = 0_i32            !< Active UDT count in udts(1:udt_count).
    integer(i32) :: max_udts = CDD_MAX_UDTS      !< Configurable UDT quota for testing.
    type(cdd_ir_udt_t) :: udts(CDD_MAX_UDTS)     !< UDT definitions ordered leaf-to-root or decl.
    integer(i32) :: enum_count = 0_i32           !< Active enumeration count.
    integer(i32) :: max_enums = CDD_MAX_ENUMS    !< Configurable enum quota for testing.
    type(cdd_ir_enum_t) :: enums(CDD_MAX_ENUMS)  !< Enumeration definitions.
  end type cdd_ir_schema_bundle_t

contains

  !> @brief Clear a cdd_ident_t to zero length.
  !> @param[out] id Identifier container to clear.
  pure subroutine ident_clear(id)
    type(cdd_ident_t), intent(out) :: id

    id%len = 0_i32
  end subroutine ident_clear

  !> @brief Assign text into cdd_ident_t, reporting CDD_ERR_BUFFER_OVERFLOW if > CDD_IDENT_MAX.
  !> @param[out] id  Destination identifier container.
  !> @param[in]  txt Source string slice.
  !> @param[out] err Error status record.
  pure subroutine ident_set(id, txt, err)
    type(cdd_ident_t), intent(out) :: id
    character(len=*, kind=c_k), intent(in) :: txt
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: n, i

    call cdd_error_clear(err)
    n = len(txt, kind=i32)
    if (n > CDD_IDENT_MAX) then
      id%len = 0_i32
      call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "Identifier exceeds CDD_IDENT_MAX")
      return
    end if
    id%len = n
    do i = 1_i32, n
      id%raw(i:i) = txt(i:i)
    end do
  end subroutine ident_set

  !> @brief Compare a cdd_ident_t against a character slice for exact equality.
  !> @param[in] id  Identifier container.
  !> @param[in] txt Expected string slice.
  !> @return True if id%raw(1:id%len) matches txt byte-for-byte.
  pure function ident_eq(id, txt) result(ok)
    type(cdd_ident_t), intent(in) :: id
    character(len=*, kind=c_k), intent(in) :: txt
    logical(lk) :: ok

    ok = str_eq(id%raw(1:id%len), txt)
  end function ident_eq

  !> @brief Initialize an array dimension descriptor to explicit 1:1 bounds.
  !> @param[out] d Dimension descriptor to reset.
  pure subroutine ir_dim_init(d)
    type(cdd_ir_dim_t), intent(out) :: d

    d%kind = DIM_FIXED
    d%lower_bound = 1_i32
    d%upper_bound = 1_i32
  end subroutine ir_dim_init

  !> @brief Initialize a field record to clean scalar IR_INT32 defaults.
  !> @param[out] f Field record to reset.
  pure subroutine ir_field_init(f)
    type(cdd_ir_field_t), intent(out) :: f
    integer(i32) :: k

    call ident_clear(f%json_name)
    call ident_clear(f%fortran_name)
    f%type_kind = IR_INT32
    f%char_len = 32_i32
    call ident_clear(f%ref_udt_name)
    call ident_clear(f%enum_name)
    f%rank = 0_i32
    do k = 1_i32, CDD_MAX_RANK
      call ir_dim_init(f%dims(k))
    end do
    f%is_required = .true._lk
    f%is_nullable = .false._lk
    f%is_allocatable = .false._lk
    f%is_pointer = .false._lk
    f%has_default = .false._lk
    call ident_clear(f%default_lexeme)
    call str_clear(f%description)
  end subroutine ir_field_init

  !> @brief Initialize an enumeration record to empty defaults.
  !> @param[out] e Enumeration record to reset.
  pure subroutine ir_enum_init(e)
    type(cdd_ir_enum_t), intent(out) :: e
    integer(i32) :: k

    call ident_clear(e%json_name)
    call ident_clear(e%fortran_name)
    e%val_count = 0_i32
    e%max_vals = CDD_MAX_ENUM_VALS
    do k = 1_i32, CDD_MAX_ENUM_VALS
      call ident_clear(e%json_vals(k))
      call ident_clear(e%f_vals(k))
    end do
    call str_clear(e%description)
  end subroutine ir_enum_init

  !> @brief Initialize a UDT record to empty defaults.
  !> @param[out] u UDT record to reset.
  pure subroutine ir_udt_init(u)
    type(cdd_ir_udt_t), intent(out) :: u

    call ident_clear(u%json_name)
    call ident_clear(u%fortran_name)
    u%has_extends = .false._lk
    call ident_clear(u%extends_name)
    u%is_bind_c = .false._lk
    u%field_count = 0_i32
    u%max_fields = CDD_MAX_FIELDS
    call str_clear(u%description)
  end subroutine ir_udt_init

  !> @brief Initialize a schema bundle to empty defaults.
  !> @param[out] b Schema bundle to reset.
  pure subroutine ir_bundle_init(b)
    type(cdd_ir_schema_bundle_t), intent(out) :: b

    call ident_clear(b%module_name)
    b%main_udt_idx = 0_i32
    b%udt_count = 0_i32
    b%max_udts = CDD_MAX_UDTS
    b%enum_count = 0_i32
    b%max_enums = CDD_MAX_ENUMS
  end subroutine ir_bundle_init

  !> @brief Allocate a new UDT slot in bundle and initialize its json_name.
  !> @param[inout] b       Target schema bundle.
  !> @param[in]    jname   JSON/schema name of the UDT.
  !> @param[out]   udt_idx 1-based allocated index in b%udts.
  !> @param[out]   err     Error status if bundle UDT arena is full or name too long.
  pure subroutine ir_bundle_add_udt(b, jname, udt_idx, err)
    type(cdd_ir_schema_bundle_t), intent(inout) :: b
    character(len=*, kind=c_k), intent(in) :: jname
    integer(i32), intent(out) :: udt_idx
    type(cdd_error_t), intent(out) :: err

    call cdd_error_clear(err)
    udt_idx = 0_i32
    if (b%udt_count >= b%max_udts) then
      call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "IR UDT arena full")
      return
    end if
    b%udt_count = b%udt_count + 1_i32
    udt_idx = b%udt_count
    call ir_udt_init(b%udts(udt_idx))
    call ident_set(b%udts(udt_idx)%json_name, jname, err)
    if (err%code /= 0_i32) then
      b%udt_count = b%udt_count - 1_i32
      udt_idx = 0_i32
    end if
  end subroutine ir_bundle_add_udt

  !> @brief Find a UDT index in bundle matching name (case-insensitive on Fortran or exact JSON).
  !> @param[in] b    Schema bundle to search.
  !> @param[in] name Target UDT name.
  !> @return 1-based UDT index, or 0 if not found.
  pure function ir_bundle_find_udt(b, name) result(idx)
    type(cdd_ir_schema_bundle_t), intent(in) :: b
    character(len=*, kind=c_k), intent(in) :: name
    integer(i32) :: idx
    integer(i32) :: i, jlen, flen

    idx = 0_i32
    do i = 1_i32, b%udt_count
      jlen = b%udts(i)%json_name%len
      flen = b%udts(i)%fortran_name%len
      if (str_eq(b%udts(i)%json_name%raw(1:jlen), name) .or. &
          (flen > 0_i32 .and. str_eq_nocase(b%udts(i)%fortran_name%raw(1:flen), name))) then
        idx = i
        return
      end if
    end do
  end function ir_bundle_find_udt

  !> @brief Append a prepared cdd_ir_field_t to a UDT.
  !> @param[inout] u         Target UDT definition.
  !> @param[in]    field_in  Field record to append.
  !> @param[out]   field_idx 1-based allocated field index.
  !> @param[out]   err       Error status if field arena is full.
  pure subroutine ir_udt_add_field(u, field_in, field_idx, err)
    type(cdd_ir_udt_t), intent(inout) :: u
    type(cdd_ir_field_t), intent(in) :: field_in
    integer(i32), intent(out) :: field_idx
    type(cdd_error_t), intent(out) :: err

    call cdd_error_clear(err)
    field_idx = 0_i32
    if (u%field_count >= u%max_fields) then
      call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "IR UDT field arena full")
      return
    end if
    u%field_count = u%field_count + 1_i32
    field_idx = u%field_count
    u%fields(field_idx) = field_in
  end subroutine ir_udt_add_field

  !> @brief Find a field in u matching json_name (exact) or fortran_name (case-insensitive).
  !> @param[in] u    UDT definition to search.
  !> @param[in] name Field name to locate.
  !> @return 1-based field index, or 0 if absent.
  pure function ir_udt_find_field(u, name) result(idx)
    type(cdd_ir_udt_t), intent(in) :: u
    character(len=*, kind=c_k), intent(in) :: name
    integer(i32) :: idx
    integer(i32) :: i, jlen, flen

    idx = 0_i32
    do i = 1_i32, u%field_count
      jlen = u%fields(i)%json_name%len
      flen = u%fields(i)%fortran_name%len
      if (str_eq(u%fields(i)%json_name%raw(1:jlen), name) .or. &
          (flen > 0_i32 .and. str_eq_nocase(u%fields(i)%fortran_name%raw(1:flen), name))) then
        idx = i
        return
      end if
    end do
  end function ir_udt_find_field

  !> @brief Allocate a new enumeration slot in bundle and set its json_name.
  !> @param[inout] b        Target schema bundle.
  !> @param[in]    jname    JSON/schema name of the enumeration.
  !> @param[out]   enum_idx 1-based allocated index in b%enums.
  !> @param[out]   err      Error status if enum arena is full.
  pure subroutine ir_bundle_add_enum(b, jname, enum_idx, err)
    type(cdd_ir_schema_bundle_t), intent(inout) :: b
    character(len=*, kind=c_k), intent(in) :: jname
    integer(i32), intent(out) :: enum_idx
    type(cdd_error_t), intent(out) :: err

    call cdd_error_clear(err)
    enum_idx = 0_i32
    if (b%enum_count >= b%max_enums) then
      call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "IR enum arena full")
      return
    end if
    b%enum_count = b%enum_count + 1_i32
    enum_idx = b%enum_count
    call ir_enum_init(b%enums(enum_idx))
    call ident_set(b%enums(enum_idx)%json_name, jname, err)
    if (err%code /= 0_i32) then
      b%enum_count = b%enum_count - 1_i32
      enum_idx = 0_i32
    end if
  end subroutine ir_bundle_add_enum

  !> @brief Find an enumeration index in bundle by JSON or Fortran name.
  !> @param[in] b    Schema bundle to search.
  !> @param[in] name Target enum name.
  !> @return 1-based enum index, or 0 if not found.
  pure function ir_bundle_find_enum(b, name) result(idx)
    type(cdd_ir_schema_bundle_t), intent(in) :: b
    character(len=*, kind=c_k), intent(in) :: name
    integer(i32) :: idx
    integer(i32) :: i, jlen, flen

    idx = 0_i32
    do i = 1_i32, b%enum_count
      jlen = b%enums(i)%json_name%len
      flen = b%enums(i)%fortran_name%len
      if (str_eq(b%enums(i)%json_name%raw(1:jlen), name) .or. &
          (flen > 0_i32 .and. str_eq_nocase(b%enums(i)%fortran_name%raw(1:flen), name))) then
        idx = i
        return
      end if
    end do
  end function ir_bundle_find_enum

  !> @brief Convert an IR_* type constant to its short reversible metadata tag name.
  !> @param[in]  type_kind IR_* type constant.
  !> @param[out] tag_buf   Output buffer (at least 16 chars recommended).
  !> @param[out] tag_len   Active length written.
  pure subroutine ir_type_to_tag(type_kind, tag_buf, tag_len)
    integer(i32), intent(in) :: type_kind
    character(len=*, kind=c_k), intent(out) :: tag_buf
    integer(i32), intent(out) :: tag_len

    select case (type_kind)
    case (IR_INT8)
      call write_tag("int8", tag_buf, tag_len)
    case (IR_INT16)
      call write_tag("int16", tag_buf, tag_len)
    case (IR_INT32)
      call write_tag("int32", tag_buf, tag_len)
    case (IR_INT64)
      call write_tag("int64", tag_buf, tag_len)
    case (IR_REAL32)
      call write_tag("real32", tag_buf, tag_len)
    case (IR_REAL64)
      call write_tag("real64", tag_buf, tag_len)
    case (IR_REAL128)
      call write_tag("real128", tag_buf, tag_len)
    case (IR_COMPLEX64)
      call write_tag("complex64", tag_buf, tag_len)
    case (IR_COMPLEX128)
      call write_tag("complex128", tag_buf, tag_len)
    case (IR_BOOL)
      call write_tag("bool", tag_buf, tag_len)
    case (IR_STRING_FIXED)
      call write_tag("string_fixed", tag_buf, tag_len)
    case (IR_STRING_DEFERRED)
      call write_tag("string_deferred", tag_buf, tag_len)
    case (IR_ENUM)
      call write_tag("enum", tag_buf, tag_len)
    case default
      call write_tag("nested_udt", tag_buf, tag_len)
    end select
  end subroutine ir_type_to_tag

  !> @brief Parse a reversible metadata tag string back into an IR_* type constant.
  !> @param[in]  tag       Tag string slice.
  !> @param[out] type_kind Parsed IR_* constant.
  !> @param[out] err       Error status if tag is unrecognized.
  pure subroutine ir_tag_to_type(tag, type_kind, err)
    character(len=*, kind=c_k), intent(in) :: tag
    integer(i32), intent(out) :: type_kind
    type(cdd_error_t), intent(out) :: err

    call cdd_error_clear(err)
    type_kind = IR_INT32
    if (str_eq_nocase(tag, "int8")) then
      type_kind = IR_INT8
    else if (str_eq_nocase(tag, "int16")) then
      type_kind = IR_INT16
    else if (str_eq_nocase(tag, "int32")) then
      type_kind = IR_INT32
    else if (str_eq_nocase(tag, "int64")) then
      type_kind = IR_INT64
    else if (str_eq_nocase(tag, "real32")) then
      type_kind = IR_REAL32
    else if (str_eq_nocase(tag, "real64")) then
      type_kind = IR_REAL64
    else if (str_eq_nocase(tag, "real128")) then
      type_kind = IR_REAL128
    else if (str_eq_nocase(tag, "complex64")) then
      type_kind = IR_COMPLEX64
    else if (str_eq_nocase(tag, "complex128")) then
      type_kind = IR_COMPLEX128
    else if (str_eq_nocase(tag, "bool")) then
      type_kind = IR_BOOL
    else if (str_eq_nocase(tag, "string_fixed")) then
      type_kind = IR_STRING_FIXED
    else if (str_eq_nocase(tag, "string_deferred")) then
      type_kind = IR_STRING_DEFERRED
    else if (str_eq_nocase(tag, "enum")) then
      type_kind = IR_ENUM
    else if (str_eq_nocase(tag, "nested_udt")) then
      type_kind = IR_NESTED_UDT
    else
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Unknown IR type tag")
    end if
  end subroutine ir_tag_to_type

  !> @brief Copy literal tag string into dst(1:out_len) clamped to dst capacity.
  !> @param[in]  lit     Literal tag text.
  !> @param[out] dst     Destination buffer.
  !> @param[out] out_len Number of bytes written.
  pure subroutine write_tag(lit, dst, out_len)
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
  end subroutine write_tag

end module cdd_ir_types
