!> @file cdd_json_infer.f90
!> @brief Bidirectional type inferencer and sample payload generator for raw JSON instances.

!> @brief Infers cdd_ir_schema_bundle_t from example JSON payloads and emits template JSON objects.
module cdd_json_infer
  use cdd_kinds, only: i32, i64, r64, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set_simple, CDD_ERR_JSON_INFER
  use cdd_string, only: str_eq_nocase, parse_i64_strict, parse_r64_strict
  use cdd_standard, only: cdd_options_t, cdd_dialect_caps_t, get_dialect_capabilities
  use cdd_json_dom, only: JVAL_NULL, JVAL_BOOL, JVAL_INT, JVAL_FLOAT, JVAL_STRING, &
    JVAL_ARRAY, JVAL_OBJECT, cdd_json_doc_t, jdoc_init, jdoc_add_null, jdoc_add_bool, &
    jdoc_add_int, jdoc_add_float, jdoc_add_string, jdoc_add_array, jdoc_add_object, &
    jdoc_array_append, jdoc_object_set, jdoc_node_str_bounds, jdoc_array_len, jdoc_array_item, &
    jdoc_object_len, jdoc_object_entry
  use cdd_ir_types, only: CDD_MAX_RANK, CDD_MAX_FIELDS, IR_INT8, IR_INT16, IR_INT32, IR_INT64, &
    IR_REAL32, IR_REAL64, IR_REAL128, IR_BOOL, IR_STRING_FIXED, IR_STRING_DEFERRED, &
    IR_ENUM, IR_NESTED_UDT, DIM_FIXED, DIM_DEFERRED, DIM_POINTER, cdd_ident_t, cdd_ir_field_t, &
    cdd_ir_udt_t, cdd_ir_schema_bundle_t, ident_set, ir_field_init, ir_bundle_init, &
    ir_bundle_add_udt, ir_bundle_find_udt, ir_udt_add_field, ir_udt_find_field, ir_bundle_find_enum
  use cdd_ir_namemap, only: sanitize_modern_ident, generate_legacy_ident, assign_udt_field_names
  implicit none (type, external)
  private

  public :: json_sample_to_ir, json_sample_to_ir_into, ir_to_json_sample

contains

  !> @brief Infer a canonical IR schema bundle from a raw JSON object or array of sample objects.
  !> @param[in]  doc     Parsed sample JSON DOM.
  !> @param[in]  options Target Fortran dialect and naming options.
  !> @param[out] bundle  Inferred canonical IR bundle.
  !> @param[out] err     Structured error status (CDD_ERR_JSON_INFER on incompatible types).
  pure subroutine json_sample_to_ir(doc, options, bundle, err)
    type(cdd_json_doc_t), intent(in) :: doc
    type(cdd_options_t), intent(in) :: options
    type(cdd_ir_schema_bundle_t), intent(out) :: bundle
    type(cdd_error_t), intent(out) :: err

    call ir_bundle_init(bundle)
    call json_sample_to_ir_into(doc, options, bundle, err)
  end subroutine json_sample_to_ir

  !> @brief Infer sample JSON into an existing/bounded cdd_ir_schema_bundle_t arena.
  !> @param[in]    doc     Parsed sample JSON DOM.
  !> @param[in]    options Target Fortran dialect and naming options.
  !> @param[inout] bundle  Pre-initialized IR bundle (preserves custom max_udts/max_fields).
  !> @param[out]   err     Structured error status.
  pure subroutine json_sample_to_ir_into(doc, options, bundle, err)
    type(cdd_json_doc_t), intent(in) :: doc
    type(cdd_options_t), intent(in) :: options
    type(cdd_ir_schema_bundle_t), intent(inout) :: bundle
    type(cdd_error_t), intent(out) :: err
    type(cdd_dialect_caps_t) :: caps
    integer(i32) :: root, main_idx, alen, k, elem
    character(len=64, kind=c_k) :: root_name
    integer(i32) :: rlen

    call cdd_error_clear(err)
    root = doc%root
    if (root < 1_i32 .or. root > doc%node_count) then
      call cdd_error_set_simple(err, CDD_ERR_JSON_INFER, "Empty JSON sample document")
      return
    end if

    caps = get_dialect_capabilities(options%standard, options%allow_dec_structure)
    if (options%target_udt_name%len > 0_i32) then
      rlen = min(64_i32, options%target_udt_name%len)
      do k = 1_i32, rlen
        root_name(k:k) = options%target_udt_name%raw(k:k)
      end do
    else
      root_name(1:6) = "Sample"
      rlen = 6_i32
    end if

    if (doc%nodes(root)%kind == JVAL_OBJECT) then
      call infer_object_into_udt(doc, root, root_name(1:rlen), .true._lk, caps, bundle, main_idx, err)
      if (err%code /= 0_i32) return
    else if (doc%nodes(root)%kind == JVAL_ARRAY) then
      alen = jdoc_array_len(doc, root)
      if (alen < 1_i32) then
        call cdd_error_set_simple(err, CDD_ERR_JSON_INFER, "Cannot infer UDT from empty root array")
        return
      end if
      do k = 1_i32, alen
        elem = jdoc_array_item(doc, root, k)
        if (doc%nodes(elem)%kind /= JVAL_OBJECT) then
          call cdd_error_set_simple(err, CDD_ERR_JSON_INFER, "Root sample array must contain objects")
          return
        end if
        call infer_object_into_udt(doc, elem, root_name(1:rlen), (k == 1_i32), &
          caps, bundle, main_idx, err)
        if (err%code /= 0_i32) return
      end do
    else
      call cdd_error_set_simple(err, CDD_ERR_JSON_INFER, "Sample JSON root must be object or array")
      return
    end if

    bundle%main_udt_idx = main_idx
    call finalize_inferred_names(bundle, caps, options%uppercase_keywords, err)
  end subroutine json_sample_to_ir_into

  !> @brief Generate a template/sample JSON document from a canonical cdd_ir_schema_bundle_t.
  !> @param[in]  bundle  Canonical IR bundle.
  !> @param[in]  options SDK options.
  !> @param[out] doc     Populated sample JSON DOM.
  !> @param[out] err     Structured error status.
  pure subroutine ir_to_json_sample(bundle, options, doc, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_options_t), intent(in) :: options
    type(cdd_json_doc_t), intent(out) :: doc
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: root_obj

    call jdoc_init(doc)
    call cdd_error_clear(err)
    associate (unused_indent => options%indent_width)
    end associate
    if (bundle%main_udt_idx < 1_i32 .or. bundle%main_udt_idx > bundle%udt_count) then
      call cdd_error_set_simple(err, CDD_ERR_JSON_INFER, "Empty IR bundle for sample JSON")
      return
    end if
    call build_sample_object(bundle, bundle%main_udt_idx, 1_i32, doc, root_obj, err)
    doc%root = root_obj
  end subroutine ir_to_json_sample

  !> @brief Infer or merge fields from a single JSON object node into bundle%udts(udt_idx).
  !> @param[in]    doc       JSON DOM.
  !> @param[in]    obj_node  JSON object node.
  !> @param[in]    udt_name  Desired UDT name.
  !> @param[in]    first_obs True on first sample object observation (controls required marking).
  !> @param[in]    caps      Dialect capability descriptor.
  !> @param[inout] bundle    Target IR bundle.
  !> @param[out]   udt_idx   1-based UDT index in bundle.
  !> @param[out]   err       Error status.
  recursive pure subroutine infer_object_into_udt(doc, obj_node, udt_name, first_obs, &
      caps, bundle, udt_idx, err)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: obj_node
    character(len=*, kind=c_k), intent(in) :: udt_name
    logical(lk), intent(in) :: first_obs
    type(cdd_dialect_caps_t), intent(in) :: caps
    type(cdd_ir_schema_bundle_t), intent(inout) :: bundle
    integer(i32), intent(out) :: udt_idx
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: pcount, i, klo, khi, vnode, existing_f, new_slot
    type(cdd_ir_field_t) :: fld
    logical(lk) :: seen(CDD_MAX_FIELDS)

    call cdd_error_clear(err)
    udt_idx = ir_bundle_find_udt(bundle, udt_name)
    if (udt_idx == 0_i32) then
      call ir_bundle_add_udt(bundle, udt_name, udt_idx, err)
      if (err%code /= 0_i32) return
    end if

    do i = 1_i32, bundle%udts(udt_idx)%field_count
      seen(i) = .false._lk
    end do

    pcount = jdoc_object_len(doc, obj_node)
    do i = 1_i32, pcount
      call jdoc_object_entry(doc, obj_node, i, klo, khi, vnode)
      if (khi >= klo) then
        call ir_field_init(fld)
        call ident_set(fld%json_name, doc%str_pool%raw(klo:khi), err)
        if (err%code /= 0_i32) return
        fld%is_required = first_obs
        call infer_value_type(doc, vnode, udt_name, doc%str_pool%raw(klo:khi), &
          caps, bundle, fld, err)
        if (err%code /= 0_i32) return

        existing_f = ir_udt_find_field(bundle%udts(udt_idx), doc%str_pool%raw(klo:khi))
        if (existing_f == 0_i32) then
          call ir_udt_add_field(bundle%udts(udt_idx), fld, new_slot, err)
          if (err%code /= 0_i32) return
          seen(new_slot) = .true._lk
        else
          seen(existing_f) = .true._lk
          call merge_inferred_field(bundle%udts(udt_idx)%fields(existing_f), fld, caps, err)
          if (err%code /= 0_i32) return
        end if
      end if
    end do

    if (.not. first_obs) then
      do i = 1_i32, bundle%udts(udt_idx)%field_count
        if (.not. seen(i)) then
          bundle%udts(udt_idx)%fields(i)%is_required = .false._lk
          bundle%udts(udt_idx)%fields(i)%is_nullable = .true._lk
        end if
      end do
    end if
  end subroutine infer_object_into_udt

  !> @brief Infer scalar, array, or nested UDT attributes for fld from JSON value node vnode.
  !> @param[in]    doc       JSON DOM.
  !> @param[in]    vnode     Value node index.
  !> @param[in]    parent_nm Enclosing UDT name.
  !> @param[in]    prop_nm   Property key.
  !> @param[in]    caps      Dialect capability descriptor.
  !> @param[inout] bundle    Target IR bundle.
  !> @param[inout] fld       Field descriptor to populate.
  !> @param[out]   err       Error status.
  recursive pure subroutine infer_value_type(doc, vnode, parent_nm, prop_nm, &
      caps, bundle, fld, err)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: vnode
    character(len=*, kind=c_k), intent(in) :: parent_nm
    character(len=*, kind=c_k), intent(in) :: prop_nm
    type(cdd_dialect_caps_t), intent(in) :: caps
    type(cdd_ir_schema_bundle_t), intent(inout) :: bundle
    type(cdd_ir_field_t), intent(inout) :: fld
    type(cdd_error_t), intent(out) :: err
    integer(i64) :: iv
    integer(i32) :: lo, hi, slen, alen, k, elem_node, child_idx
    character(len=64, kind=c_k) :: child_name
    integer(i32) :: clen
    type(cdd_ir_field_t) :: elem_fld

    call cdd_error_clear(err)
    select case (doc%nodes(vnode)%kind)
    case (JVAL_NULL)
      fld%type_kind = IR_STRING_FIXED
      fld%char_len = 32_i32
      fld%is_nullable = .true._lk
      fld%is_required = .false._lk
      if (caps%has_deferred_char) then
        fld%type_kind = IR_STRING_DEFERRED
        fld%is_allocatable = .true._lk
      end if
    case (JVAL_BOOL)
      fld%type_kind = IR_BOOL
    case (JVAL_INT)
      iv = doc%nodes(vnode)%int_val
      if (iv > 2147483647_i64 .or. iv < -2147483647_i64) then
        fld%type_kind = IR_INT64
      else
        fld%type_kind = IR_INT32
      end if
    case (JVAL_FLOAT)
      fld%type_kind = IR_REAL64
    case (JVAL_STRING)
      call jdoc_node_str_bounds(doc, vnode, lo, hi)
      slen = max(1_i32, hi - lo + 1_i32)
      if (caps%has_deferred_char) then
        fld%type_kind = IR_STRING_DEFERRED
        fld%is_allocatable = .true._lk
        fld%char_len = slen
      else
        fld%type_kind = IR_STRING_FIXED
        fld%char_len = max(16_i32, slen)
      end if
    case (JVAL_OBJECT)
      call make_child_udt_name(parent_nm, prop_nm, child_name, clen)
      call infer_object_into_udt(doc, vnode, child_name(1:clen), .true._lk, &
        caps, bundle, child_idx, err)
      if (err%code /= 0_i32) return
      fld%type_kind = IR_NESTED_UDT
      call ident_set(fld%ref_udt_name, child_name(1:clen), err)
    case default
      ! JVAL_ARRAY
      if (fld%rank >= CDD_MAX_RANK) then
        call cdd_error_set_simple(err, CDD_ERR_JSON_INFER, "Sample array exceeds max rank 15")
        return
      end if
      alen = jdoc_array_len(doc, vnode)
      fld%rank = fld%rank + 1_i32
      if (caps%has_allocatable_comp) then
        fld%dims(fld%rank)%kind = DIM_DEFERRED
        fld%is_allocatable = .true._lk
      else if (caps%has_modules_and_type) then
        fld%dims(fld%rank)%kind = DIM_POINTER
        fld%is_pointer = .true._lk
      else
        fld%dims(fld%rank)%kind = DIM_FIXED
        fld%dims(fld%rank)%lower_bound = 1_i32
        fld%dims(fld%rank)%upper_bound = max(1_i32, alen)
      end if

      if (alen == 0_i32) then
        fld%type_kind = IR_REAL64
        return
      end if

      do k = 1_i32, alen
        elem_node = jdoc_array_item(doc, vnode, k)
        call ir_field_init(elem_fld)
        elem_fld%rank = fld%rank
        elem_fld%dims = fld%dims
        elem_fld%is_allocatable = fld%is_allocatable
        elem_fld%is_pointer = fld%is_pointer
        if (doc%nodes(elem_node)%kind == JVAL_OBJECT) then
          call make_child_udt_name(parent_nm, prop_nm, child_name, clen)
          call infer_object_into_udt(doc, elem_node, child_name(1:clen), (k == 1_i32), &
            caps, bundle, child_idx, err)
          if (err%code /= 0_i32) return
          elem_fld%type_kind = IR_NESTED_UDT
          call ident_set(elem_fld%ref_udt_name, child_name(1:clen), err)
        else
          call infer_value_type(doc, elem_node, parent_nm, prop_nm, caps, bundle, elem_fld, err)
          if (err%code /= 0_i32) return
        end if
        if (k == 1_i32) then
          fld%type_kind = elem_fld%type_kind
          fld%char_len = elem_fld%char_len
          fld%ref_udt_name = elem_fld%ref_udt_name
          fld%rank = elem_fld%rank
          fld%dims = elem_fld%dims
          fld%is_nullable = elem_fld%is_nullable
        else
          call merge_inferred_field(fld, elem_fld, caps, err)
          if (err%code /= 0_i32) return
        end if
      end do
    end select
  end subroutine infer_value_type

  !> @brief Reconcile two inferred field observations (e.g. int + float -> float, null + T -> nullable T).
  !> @param[inout] acc  Accumulated field definition.
  !> @param[in]    obs  New observation.
  !> @param[in]    caps Dialect capabilities.
  !> @param[out]   err  Error status if types are irreconcilable (e.g. object vs number).
  pure subroutine merge_inferred_field(acc, obs, caps, err)
    type(cdd_ir_field_t), intent(inout) :: acc
    type(cdd_ir_field_t), intent(in) :: obs
    type(cdd_dialect_caps_t), intent(in) :: caps
    type(cdd_error_t), intent(out) :: err

    call cdd_error_clear(err)
    if (obs%is_nullable) then
      acc%is_nullable = .true._lk
      if (caps%has_allocatable_comp) acc%is_allocatable = .true._lk
      return
    end if
    if (acc%is_nullable .and. (acc%type_kind == IR_STRING_FIXED .or. &
        acc%type_kind == IR_STRING_DEFERRED) .and. .not. acc%is_required) then
      acc%type_kind = obs%type_kind
      acc%char_len = obs%char_len
      acc%ref_udt_name = obs%ref_udt_name
      return
    end if

    if (acc%type_kind == obs%type_kind) then
      if (obs%char_len > acc%char_len) acc%char_len = obs%char_len
      return
    end if

    if (acc%type_kind == IR_INT32 .and. obs%type_kind == IR_INT64) then
      acc%type_kind = IR_INT64
    else if (acc%type_kind == IR_INT64 .and. obs%type_kind == IR_INT32) then
      acc%type_kind = IR_INT64
    else if ((acc%type_kind == IR_INT32 .or. acc%type_kind == IR_INT64) .and. &
             obs%type_kind == IR_REAL64) then
      acc%type_kind = IR_REAL64
    else if (acc%type_kind == IR_REAL64 .and. &
             (obs%type_kind == IR_INT32 .or. obs%type_kind == IR_INT64)) then
      acc%type_kind = IR_REAL64
    else
      call cdd_error_set_simple(err, CDD_ERR_JSON_INFER, &
        "Conflicting heterogeneous types in JSON sample")
    end if
  end subroutine merge_inferred_field

  !> @brief Assign Fortran names to all inferred UDTs and fields in bundle.
  !> @param[inout] bundle    IR bundle.
  !> @param[in]    caps      Dialect capability descriptor.
  !> @param[in]    uppercase Uppercase flag.
  !> @param[out]   err       Error status.
  pure subroutine finalize_inferred_names(bundle, caps, uppercase, err)
    type(cdd_ir_schema_bundle_t), intent(inout) :: bundle
    type(cdd_dialect_caps_t), intent(in) :: caps
    logical(lk), intent(in) :: uppercase
    type(cdd_error_t), intent(out) :: err
    type(cdd_ident_t) :: used_udts(32)
    integer(i32) :: used_cnt, i, mod_max

    call cdd_error_clear(err)
    used_cnt = 0_i32
    do i = 1_i32, bundle%udt_count
      if (caps%max_ident_len <= 6_i32) then
        call generate_legacy_ident( &
          bundle%udts(i)%json_name%raw(1:bundle%udts(i)%json_name%len), &
          IR_NESTED_UDT, .false._lk, 100_i32, 32_i32, used_udts, used_cnt, &
          bundle%udts(i)%fortran_name, err)
      else
        call sanitize_modern_ident( &
          bundle%udts(i)%json_name%raw(1:bundle%udts(i)%json_name%len) // "_t", &
          caps%max_ident_len, uppercase, bundle%udts(i)%fortran_name, err)
      end if
      call assign_udt_field_names(caps%max_ident_len, caps%implicit_only_typing, &
        uppercase .or. (caps%max_ident_len <= 6_i32), CDD_MAX_FIELDS, &
        bundle%udts(i)%field_count, bundle%udts(i)%fields, err)
    end do

    mod_max = max(8_i32, caps%max_ident_len)
    call sanitize_modern_ident( &
      bundle%udts(bundle%main_udt_idx)%json_name%raw( &
        1:bundle%udts(bundle%main_udt_idx)%json_name%len) // "_mod", &
      mod_max, uppercase, bundle%module_name, err)
  end subroutine finalize_inferred_names

  !> @brief Build a sample JSON object for bundle%udts(udt_idx) (with cycle guard depth).
  !> @param[in]    bundle   Enclosing IR bundle.
  !> @param[in]    udt_idx  Target UDT index.
  !> @param[in]    depth    Recursion depth (stops recursive UDT cycles with null).
  !> @param[inout] doc      Target JSON DOM.
  !> @param[out]   obj_node Allocated object node.
  !> @param[out]   err      Error status.
  recursive pure subroutine build_sample_object(bundle, udt_idx, depth, doc, obj_node, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    integer(i32), intent(in) :: udt_idx
    integer(i32), intent(in) :: depth
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(out) :: obj_node
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: p_idx

    call cdd_error_clear(err)
    call jdoc_add_object(doc, obj_node, err)

    if (bundle%udts(udt_idx)%has_extends .and. depth <= 8_i32) then
      p_idx = ir_bundle_find_udt(bundle, &
        bundle%udts(udt_idx)%extends_name%raw(1:bundle%udts(udt_idx)%extends_name%len))
      if (p_idx > 0_i32) then
        call populate_udt_sample_fields(bundle, bundle%udts(p_idx), depth + 1_i32, doc, obj_node, err)
      end if
    end if

    call populate_udt_sample_fields(bundle, bundle%udts(udt_idx), depth, doc, obj_node, err)
  end subroutine build_sample_object

  !> @brief Populate sample key/value entries from udt%fields onto obj_node.
  !> @param[in]    bundle   Enclosing IR bundle.
  !> @param[in]    udt      Source UDT.
  !> @param[in]    depth    Current recursion depth.
  !> @param[inout] doc      Target JSON DOM.
  !> @param[in]    obj_node Target JSON object node.
  !> @param[out]   err      Error status.
  recursive pure subroutine populate_udt_sample_fields(bundle, udt, depth, doc, obj_node, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_udt_t), intent(in) :: udt
    integer(i32), intent(in) :: depth
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(in) :: obj_node
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: i, fval, r, arr_n

    call cdd_error_clear(err)
    do i = 1_i32, udt%field_count
      call build_scalar_sample_val(bundle, udt%fields(i), depth, doc, fval, err)
      do r = udt%fields(i)%rank, 1_i32, -1_i32
        call jdoc_add_array(doc, arr_n, err)
        call jdoc_array_append(doc, arr_n, fval, err)
        fval = arr_n
      end do
      call jdoc_object_set(doc, obj_node, &
        udt%fields(i)%json_name%raw(1:udt%fields(i)%json_name%len), fval, .false._lk, err)
    end do
  end subroutine populate_udt_sample_fields

  !> @brief Build a scalar or nested-object sample JSON node for fld.
  !> @param[in]    bundle Enclosing IR bundle.
  !> @param[in]    fld    Field descriptor.
  !> @param[in]    depth  Recursion depth.
  !> @param[inout] doc    Target JSON DOM.
  !> @param[out]   vnode  Allocated JSON value node.
  !> @param[out]   err    Error status.
  recursive pure subroutine build_scalar_sample_val(bundle, fld, depth, doc, vnode, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_field_t), intent(in) :: fld
    integer(i32), intent(in) :: depth
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(out) :: vnode
    type(cdd_error_t), intent(out) :: err
    integer(i64) :: iv
    real(r64) :: rv
    integer(i32) :: idx
    type(cdd_error_t) :: tmp_err

    call cdd_error_clear(err)
    if (fld%is_nullable .and. .not. fld%is_required .and. .not. fld%has_default) then
      call jdoc_add_null(doc, vnode, err)
      return
    end if

    select case (fld%type_kind)
    case (IR_INT8, IR_INT16, IR_INT32, IR_INT64)
      iv = 0_i64
      if (fld%has_default) then
        call parse_i64_strict(fld%default_lexeme%raw(1:fld%default_lexeme%len), iv, tmp_err)
      end if
      call jdoc_add_int(doc, iv, vnode, err)
    case (IR_REAL32, IR_REAL64, IR_REAL128)
      rv = 0.0_r64
      if (fld%has_default) then
        call parse_r64_strict(fld%default_lexeme%raw(1:fld%default_lexeme%len), rv, tmp_err)
      end if
      call jdoc_add_float(doc, rv, "", vnode, err)
    case (IR_BOOL)
      if (fld%has_default) then
        call jdoc_add_bool(doc, &
          str_eq_nocase(fld%default_lexeme%raw(1:fld%default_lexeme%len), "true"), vnode, err)
      else
        call jdoc_add_bool(doc, .false._lk, vnode, err)
      end if
    case (IR_ENUM)
      idx = ir_bundle_find_enum(bundle, fld%enum_name%raw(1:fld%enum_name%len))
      if (idx > 0_i32 .and. bundle%enums(max(1_i32, idx))%val_count > 0_i32) then
        call jdoc_add_string(doc, &
          bundle%enums(idx)%json_vals(1)%raw(1:bundle%enums(idx)%json_vals(1)%len), vnode, err)
      else
        call jdoc_add_string(doc, "", vnode, err)
      end if
    case (IR_NESTED_UDT)
      idx = ir_bundle_find_udt(bundle, fld%ref_udt_name%raw(1:fld%ref_udt_name%len))
      if (idx > 0_i32 .and. depth <= 4_i32) then
        call build_sample_object(bundle, idx, depth + 1_i32, doc, vnode, err)
      else
        call jdoc_add_null(doc, vnode, err)
      end if
    case default
      if (fld%has_default) then
        call jdoc_add_string(doc, fld%default_lexeme%raw(1:fld%default_lexeme%len), vnode, err)
      else
        call jdoc_add_string(doc, "", vnode, err)
      end if
    end select
  end subroutine build_scalar_sample_val

  !> @brief Synthesize "<Parent>_<prop>" child UDT name bounded to 64 chars.
  !> @param[in]  p_nm     Parent name.
  !> @param[in]  prop_nm  Property name.
  !> @param[out] out_name Synthesized name.
  !> @param[out] out_len  Active length.
  pure subroutine make_child_udt_name(p_nm, prop_nm, out_name, out_len)
    character(len=*, kind=c_k), intent(in) :: p_nm
    character(len=*, kind=c_k), intent(in) :: prop_nm
    character(len=64, kind=c_k), intent(out) :: out_name
    integer(i32), intent(out) :: out_len
    integer(i32) :: k, np, nq

    out_len = 0_i32
    np = min(30_i32, len(p_nm, kind=i32))
    do k = 1_i32, np
      out_len = out_len + 1_i32
      out_name(out_len:out_len) = p_nm(k:k)
    end do
    out_len = out_len + 1_i32
    out_name(out_len:out_len) = "_"
    nq = min(32_i32, len(prop_nm, kind=i32))
    do k = 1_i32, nq
      out_len = out_len + 1_i32
      out_name(out_len:out_len) = prop_nm(k:k)
    end do
  end subroutine make_child_udt_name

end module cdd_json_infer
