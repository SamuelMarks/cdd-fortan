!> @file cdd_json_schema.f90
!> @brief Bidirectional mapper between JSON Schema (Draft 2020-12 / OpenAPI 3.x) and cdd_ir_types.

!> @brief Schema auto-detection, $ref / allOf / nullable / enum / multi-rank mapper, and emitter.
module cdd_json_schema
  use cdd_kinds, only: i32, i64, r64, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set_simple, &
    CDD_ERR_JSON_SCHEMA, CDD_ERR_DIALECT_CONSTRAINT
  use cdd_string, only: cdd_string_t, str_clear, str_set, str_eq, str_eq_nocase, starts_with, &
    format_i64_to_str, format_r64_to_str, parse_i64_strict, parse_r64_strict
  use cdd_standard, only: cdd_options_t, cdd_dialect_caps_t, get_dialect_capabilities
  use cdd_json_dom, only: JVAL_NULL, JVAL_BOOL, JVAL_INT, JVAL_FLOAT, JVAL_STRING, &
    JVAL_ARRAY, JVAL_OBJECT, cdd_json_doc_t, jdoc_init, jdoc_add_bool, jdoc_add_int, &
    jdoc_add_float, jdoc_add_string, jdoc_add_array, jdoc_add_object, jdoc_array_append, &
    jdoc_object_set, jdoc_get_key, jdoc_has_key, jdoc_node_str_bounds, jdoc_array_len, &
    jdoc_array_item, jdoc_object_len, jdoc_object_entry
  use cdd_ir_types, only: CDD_MAX_RANK, CDD_MAX_FIELDS, IR_INT8, IR_INT16, IR_INT32, IR_INT64, &
    IR_REAL32, IR_REAL64, IR_REAL128, IR_BOOL, IR_STRING_FIXED, IR_STRING_DEFERRED, &
    IR_ENUM, IR_NESTED_UDT, DIM_FIXED, DIM_DEFERRED, DIM_POINTER, cdd_ident_t, &
    cdd_ir_field_t, cdd_ir_enum_t, cdd_ir_udt_t, cdd_ir_schema_bundle_t, ident_set, &
    ir_field_init, ir_bundle_init, ir_bundle_add_udt, ir_bundle_find_udt, ir_udt_add_field, &
    ir_bundle_add_enum, ir_bundle_find_enum
  use cdd_ir_namemap, only: sanitize_modern_ident, generate_legacy_ident, assign_udt_field_names
  implicit none (type, external)
  private

  public :: is_json_schema_doc, json_schema_to_ir, ir_to_json_schema

contains

  !> @brief Heuristic check returning true when doc represents a JSON Schema or OpenAPI spec.
  !> @param[in] doc Parsed JSON DOM document.
  !> @return True if root object contains JSON Schema / OpenAPI structural keywords.
  pure function is_json_schema_doc(doc) result(is_schema)
    type(cdd_json_doc_t), intent(in) :: doc
    logical(lk) :: is_schema
    integer(i32) :: root, tnode, tlen
    character(len=64, kind=c_k) :: tstr

    is_schema = .false._lk
    root = doc%root
    if (root < 1_i32 .or. root > doc%node_count) return
    if (doc%nodes(root)%kind /= JVAL_OBJECT) return

    if (jdoc_has_key(doc, root, "$schema") .or. &
        jdoc_has_key(doc, root, "$defs") .or. &
        jdoc_has_key(doc, root, "definitions") .or. &
        jdoc_has_key(doc, root, "components") .or. &
        jdoc_has_key(doc, root, "allOf")) then
      is_schema = .true._lk
      return
    end if

    if (jdoc_has_key(doc, root, "properties")) then
      tnode = jdoc_get_key(doc, root, "type")
      call get_opt_str(doc, tnode, tstr, tlen)
      if (tlen == 6_i32) then
        is_schema = str_eq(tstr(1:6), "object")
      end if
    end if
  end function is_json_schema_doc

  !> @brief Convert a JSON Schema (Draft-07 / 2020-12) or OpenAPI document into cdd_ir_schema_bundle_t.
  !> @param[in]  doc     Parsed JSON Schema DOM.
  !> @param[in]  options Target Fortran dialect and naming options.
  !> @param[out] bundle  Populated canonical IR bundle.
  !> @param[out] err     Structured error status.
  pure subroutine json_schema_to_ir(doc, options, bundle, err)
    type(cdd_json_doc_t), intent(in) :: doc
    type(cdd_options_t), intent(in) :: options
    type(cdd_ir_schema_bundle_t), intent(out) :: bundle
    type(cdd_error_t), intent(out) :: err
    type(cdd_dialect_caps_t) :: caps
    integer(i32) :: root, defs_node, comp_node, idx, count, klo, khi, sub_node
    integer(i32) :: main_idx, lo, hi
    character(len=64, kind=c_k) :: root_title
    integer(i32) :: rtitle_len

    call ir_bundle_init(bundle)
    call cdd_error_clear(err)
    root = doc%root
    if (root < 1_i32 .or. root > doc%node_count) then
      call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "Empty JSON document")
      return
    end if
    if (doc%nodes(root)%kind /= JVAL_OBJECT) then
      call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "JSON Schema root must be an object")
      return
    end if

    caps = get_dialect_capabilities(options%standard, options%allow_dec_structure)

    defs_node = jdoc_get_key(doc, root, "$defs")
    if (defs_node == 0_i32) defs_node = jdoc_get_key(doc, root, "definitions")
    if (defs_node == 0_i32) then
      comp_node = jdoc_get_key(doc, root, "components")
      if (comp_node > 0_i32) defs_node = jdoc_get_key(doc, comp_node, "schemas")
    end if

    if (defs_node > 0_i32) then
      count = jdoc_object_len(doc, defs_node)
      do idx = 1_i32, count
        call jdoc_object_entry(doc, defs_node, idx, klo, khi, sub_node)
        if (khi >= klo .and. doc%nodes(sub_node)%kind == JVAL_OBJECT) then
          if (jdoc_has_key(doc, sub_node, "enum")) then
            call register_enum_node(doc, sub_node, doc%str_pool%raw(klo:khi), &
              caps, options%uppercase_keywords, bundle, err)
          else
            call parse_object_schema(doc, defs_node, sub_node, doc%str_pool%raw(klo:khi), &
              caps, options, bundle, main_idx, err)
          end if
          if (err%code /= 0_i32) return
        end if
      end do
    end if

    if (jdoc_has_key(doc, root, "properties") .or. jdoc_has_key(doc, root, "allOf") .or. &
        defs_node == 0_i32) then
      call resolve_root_title(doc, root, options, root_title, rtitle_len)
      call parse_object_schema(doc, defs_node, root, root_title(1:rtitle_len), &
        caps, options, bundle, main_idx, err)
      if (err%code /= 0_i32) return
      bundle%main_udt_idx = main_idx
    else
      if (options%target_udt_name%len > 0_i32) then
        lo = 1_i32
        hi = options%target_udt_name%len
        main_idx = ir_bundle_find_udt(bundle, options%target_udt_name%raw(lo:hi))
        if (main_idx == 0_i32) then
          call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "Target schema not in components")
          return
        end if
        bundle%main_udt_idx = main_idx
      else if (bundle%udt_count > 0_i32) then
        bundle%main_udt_idx = bundle%udt_count
      else
        call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "No object schemas found in document")
        return
      end if
    end if

    call finalize_bundle_names(bundle, caps, options%uppercase_keywords, err)
  end subroutine json_schema_to_ir

  !> @brief Serialize a canonical cdd_ir_schema_bundle_t to a Draft 2020-12 JSON Schema DOM.
  !> @param[in]  bundle  Canonical IR bundle to serialize.
  !> @param[in]  options SDK options controlling output.
  !> @param[out] doc     Populated JSON Schema DOM document.
  !> @param[out] err     Structured error status.
  pure subroutine ir_to_json_schema(bundle, options, doc, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_options_t), intent(in) :: options
    type(cdd_json_doc_t), intent(out) :: doc
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: root_obj, s_node, defs_obj, i, sub_obj, main_idx

    call jdoc_init(doc)
    call cdd_error_clear(err)
    associate (unused_indent => options%indent_width)
    end associate
    main_idx = bundle%main_udt_idx
    if (main_idx < 1_i32 .or. main_idx > bundle%udt_count) then
      call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "Empty IR bundle for JSON Schema")
      return
    end if

    call jdoc_add_object(doc, root_obj, err)
    doc%root = root_obj

    call jdoc_add_string(doc, "https://json-schema.org/draft/2020-12/schema", s_node, err)
    call jdoc_object_set(doc, root_obj, "$schema", s_node, .false._lk, err)

    call build_udt_schema_obj(bundle, bundle%udts(main_idx), doc, root_obj, err)

    if (bundle%udt_count > 1_i32 .or. bundle%enum_count > 0_i32) then
      call jdoc_add_object(doc, defs_obj, err)
      do i = 1_i32, bundle%enum_count
        call build_enum_schema_obj(bundle%enums(i), doc, sub_obj, err)
        call jdoc_object_set(doc, defs_obj, &
          bundle%enums(i)%json_name%raw(1:bundle%enums(i)%json_name%len), &
          sub_obj, .false._lk, err)
      end do
      do i = 1_i32, bundle%udt_count
        if (i /= main_idx) then
          call jdoc_add_object(doc, sub_obj, err)
          call build_udt_schema_obj(bundle, bundle%udts(i), doc, sub_obj, err)
          call jdoc_object_set(doc, defs_obj, &
            bundle%udts(i)%json_name%raw(1:bundle%udts(i)%json_name%len), &
            sub_obj, .false._lk, err)
        end if
      end do
      call jdoc_object_set(doc, root_obj, "$defs", defs_obj, .false._lk, err)
    end if
  end subroutine ir_to_json_schema

  !> @brief Determine root UDT title from options%target_udt_name, "title", or "Record".
  !> @param[in]  doc      JSON DOM.
  !> @param[in]  root     Root object node.
  !> @param[in]  options  SDK options.
  !> @param[out] out_name Title buffer.
  !> @param[out] out_len  Active title length.
  pure subroutine resolve_root_title(doc, root, options, out_name, out_len)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: root
    type(cdd_options_t), intent(in) :: options
    character(len=64, kind=c_k), intent(out) :: out_name
    integer(i32), intent(out) :: out_len
    integer(i32) :: tnode, k

    if (options%target_udt_name%len > 0_i32) then
      out_len = options%target_udt_name%len
      if (out_len > 64_i32) out_len = 64_i32
      do k = 1_i32, out_len
        out_name(k:k) = options%target_udt_name%raw(k:k)
      end do
      return
    end if

    tnode = jdoc_get_key(doc, root, "title")
    call get_opt_str(doc, tnode, out_name, out_len)
    if (out_len > 0_i32) return

    out_name(1:6) = "Record"
    out_len = 6_i32
  end subroutine resolve_root_title

  !> @brief Register a string enum schema node into bundle%enums.
  !> @param[in]    doc       JSON DOM.
  !> @param[in]    snode     Schema object node containing "enum".
  !> @param[in]    ename     Enum JSON name.
  !> @param[in]    caps      Dialect capability descriptor.
  !> @param[in]    uppercase Uppercase flag.
  !> @param[inout] bundle    Target IR bundle.
  !> @param[out]   err       Error status.
  pure subroutine register_enum_node(doc, snode, ename, caps, uppercase, bundle, err)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: snode
    character(len=*, kind=c_k), intent(in) :: ename
    type(cdd_dialect_caps_t), intent(in) :: caps
    logical(lk), intent(in) :: uppercase
    type(cdd_ir_schema_bundle_t), intent(inout) :: bundle
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: arr_node, eidx, nvals, k, item_node, lo, hi, max_id

    call cdd_error_clear(err)
    if (ir_bundle_find_enum(bundle, ename) > 0_i32) return

    arr_node = jdoc_get_key(doc, snode, "enum")
    nvals = jdoc_array_len(doc, arr_node)
    if (nvals < 1_i32) then
      call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "Empty enum array in JSON Schema")
      return
    end if

    call ir_bundle_add_enum(bundle, ename, eidx, err)
    max_id = caps%max_ident_len
    call sanitize_modern_ident(ename, max_id, uppercase .or. (max_id <= 6_i32), &
      bundle%enums(eidx)%fortran_name, err)
    call extract_opt_description(doc, snode, bundle%enums(eidx)%description)

    do k = 1_i32, nvals
      item_node = jdoc_array_item(doc, arr_node, k)
      if (doc%nodes(item_node)%kind /= JVAL_STRING) then
        call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "Non-string value in enum schema")
        return
      end if
      call jdoc_node_str_bounds(doc, item_node, lo, hi)
      bundle%enums(eidx)%val_count = bundle%enums(eidx)%val_count + 1_i32
      if (hi >= lo) then
        call ident_set(bundle%enums(eidx)%json_vals(k), doc%str_pool%raw(lo:hi), err)
        if (err%code /= 0_i32) return
        call sanitize_modern_ident(doc%str_pool%raw(lo:hi), max_id, .true._lk, &
          bundle%enums(eidx)%f_vals(k), err)
      else
        call ident_set(bundle%enums(eidx)%json_vals(k), "", err)
        call ident_set(bundle%enums(eidx)%f_vals(k), "EMPTY_VAL", err)
      end if
    end do
  end subroutine register_enum_node

  !> @brief Recursively parse a JSON Schema object definition into a UDT in bundle.
  !> @param[in]    doc       JSON DOM.
  !> @param[in]    defs_node Schema definitions container object node (or 0).
  !> @param[in]    obj_node  Current schema object node.
  !> @param[in]    udt_name  Desired UDT JSON name.
  !> @param[in]    caps      Dialect capability descriptor.
  !> @param[in]    options   SDK options.
  !> @param[inout] bundle    Target IR bundle.
  !> @param[out]   udt_idx   1-based index of populated UDT in bundle.
  !> @param[out]   err       Error status.
  recursive pure subroutine parse_object_schema(doc, defs_node, obj_node, udt_name, &
      caps, options, bundle, udt_idx, err)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: defs_node
    integer(i32), intent(in) :: obj_node
    character(len=*, kind=c_k), intent(in) :: udt_name
    type(cdd_dialect_caps_t), intent(in) :: caps
    type(cdd_options_t), intent(in) :: options
    type(cdd_ir_schema_bundle_t), intent(inout) :: bundle
    integer(i32), intent(out) :: udt_idx
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: allof_node, acount, k, elem_node, ref_node
    integer(i32) :: parent_idx, pf
    character(len=64, kind=c_k) :: ref_target
    integer(i32) :: ref_len, f_dummy

    call cdd_error_clear(err)
    udt_idx = ir_bundle_find_udt(bundle, udt_name)
    if (udt_idx == 0_i32) then
      call ir_bundle_add_udt(bundle, udt_name, udt_idx, err)
    else if (bundle%udts(udt_idx)%field_count > 0_i32) then
      return
    end if

    call extract_opt_description(doc, obj_node, bundle%udts(udt_idx)%description)

    allof_node = jdoc_get_key(doc, obj_node, "allOf")
    if (allof_node > 0_i32) then
      acount = jdoc_array_len(doc, allof_node)
      do k = 1_i32, acount
        elem_node = jdoc_array_item(doc, allof_node, k)
        if (doc%nodes(elem_node)%kind == JVAL_OBJECT) then
          ref_node = jdoc_get_key(doc, elem_node, "$ref")
          if (ref_node > 0_i32) then
            call extract_ref_name(doc, ref_node, ref_target, ref_len, err)
            if (err%code /= 0_i32) return
            parent_idx = ir_bundle_find_udt(bundle, ref_target(1:ref_len))
            if (parent_idx == 0_i32 .and. defs_node > 0_i32) then
              ref_node = jdoc_get_key(doc, defs_node, ref_target(1:ref_len))
              if (ref_node > 0_i32) then
                call parse_object_schema(doc, defs_node, ref_node, ref_target(1:ref_len), &
                  caps, options, bundle, parent_idx, err)
                if (err%code /= 0_i32) return
              end if
            end if
            if (parent_idx == 0_i32) then
              call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "Unresolved $ref in allOf")
              return
            end if
            if (caps%has_oop_class_extends) then
              bundle%udts(udt_idx)%has_extends = .true._lk
              call ident_set(bundle%udts(udt_idx)%extends_name, ref_target(1:ref_len), err)
            else
              do pf = 1_i32, bundle%udts(parent_idx)%field_count
                call ir_udt_add_field(bundle%udts(udt_idx), &
                  bundle%udts(parent_idx)%fields(pf), f_dummy, err)
              end do
            end if
          else
            call parse_properties_into_udt(doc, defs_node, elem_node, udt_idx, &
              caps, options, bundle, err)
            if (err%code /= 0_i32) return
          end if
        end if
      end do
    end if

    call parse_properties_into_udt(doc, defs_node, obj_node, udt_idx, caps, options, bundle, err)
  end subroutine parse_object_schema

  !> @brief Parse "properties" and "required" from schema_obj into bundle%udts(udt_idx).
  !> @param[in]    doc        JSON DOM.
  !> @param[in]    defs_node  Definitions container node.
  !> @param[in]    schema_obj Object node holding "properties".
  !> @param[in]    udt_idx    Target UDT index in bundle.
  !> @param[in]    caps       Dialect capabilities.
  !> @param[in]    options    SDK options.
  !> @param[inout] bundle     Target IR bundle.
  !> @param[out]   err        Error status.
  recursive pure subroutine parse_properties_into_udt(doc, defs_node, schema_obj, udt_idx, &
      caps, options, bundle, err)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: defs_node
    integer(i32), intent(in) :: schema_obj
    integer(i32), intent(in) :: udt_idx
    type(cdd_dialect_caps_t), intent(in) :: caps
    type(cdd_options_t), intent(in) :: options
    type(cdd_ir_schema_bundle_t), intent(inout) :: bundle
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: props_node, req_node, pcount, idx, klo, khi, pschema, f_slot
    type(cdd_ir_field_t) :: fld

    call cdd_error_clear(err)
    props_node = jdoc_get_key(doc, schema_obj, "properties")
    if (props_node == 0_i32) return
    if (doc%nodes(props_node)%kind /= JVAL_OBJECT) then
      call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "'properties' must be a JSON object")
      return
    end if

    req_node = jdoc_get_key(doc, schema_obj, "required")
    pcount = jdoc_object_len(doc, props_node)
    do idx = 1_i32, pcount
      call jdoc_object_entry(doc, props_node, idx, klo, khi, pschema)
      if (khi >= klo) then
        call ir_field_init(fld)
        call ident_set(fld%json_name, doc%str_pool%raw(klo:khi), err)
        if (err%code /= 0_i32) return
        fld%is_required = is_in_string_array(doc, req_node, doc%str_pool%raw(klo:khi))
        call map_property_schema(doc, defs_node, pschema, udt_idx, &
          doc%str_pool%raw(klo:khi), caps, options, bundle, fld, err)
        if (err%code /= 0_i32) return
        call ir_udt_add_field(bundle%udts(udt_idx), fld, f_slot, err)
      end if
    end do
  end subroutine parse_properties_into_udt

  !> @brief Map a single JSON Schema property node into a cdd_ir_field_t.
  !> @param[in]    doc        JSON DOM.
  !> @param[in]    defs_node  Definitions node.
  !> @param[in]    pschema    Property schema object node.
  !> @param[in]    owner_udt  1-based index of enclosing UDT (for cycle detection).
  !> @param[in]    prop_name  Property key string.
  !> @param[in]    caps       Dialect capabilities.
  !> @param[in]    options    SDK options.
  !> @param[inout] bundle     Target IR bundle.
  !> @param[inout] fld        Field descriptor to populate.
  !> @param[out]   err        Error status.
  recursive pure subroutine map_property_schema(doc, defs_node, pschema, owner_udt, &
      prop_name, caps, options, bundle, fld, err)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: defs_node
    integer(i32), intent(in) :: pschema
    integer(i32), intent(in) :: owner_udt
    character(len=*, kind=c_k), intent(in) :: prop_name
    type(cdd_dialect_caps_t), intent(in) :: caps
    type(cdd_options_t), intent(in) :: options
    type(cdd_ir_schema_bundle_t), intent(inout) :: bundle
    type(cdd_ir_field_t), intent(inout) :: fld
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: eff_node, dnode, ref_node, union_node, ulen, k, sub_n
    integer(i32) :: tnode, fmt_node, maxl_node, items_node, def_node, child_udt, e_idx
    character(len=64, kind=c_k) :: ref_name, type_str, fmt_str, synth_name
    integer(i32) :: ref_len, tlen, flen, slen

    call cdd_error_clear(err)
    if (doc%nodes(pschema)%kind /= JVAL_OBJECT) then
      call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "Property schema must be an object")
      return
    end if

    eff_node = pschema
    call extract_opt_description(doc, pschema, fld%description)

    dnode = jdoc_get_key(doc, pschema, "nullable")
    if (dnode > 0_i32) then
      if (doc%nodes(dnode)%kind == JVAL_BOOL) then
        fld%is_nullable = doc%nodes(dnode)%bool_val
      end if
    end if

    union_node = jdoc_get_key(doc, pschema, "anyOf")
    if (union_node == 0_i32) union_node = jdoc_get_key(doc, pschema, "oneOf")
    if (union_node > 0_i32) then
      ulen = jdoc_array_len(doc, union_node)
      do k = 1_i32, ulen
        sub_n = jdoc_array_item(doc, union_node, k)
        if (is_null_type_schema(doc, sub_n)) then
          fld%is_nullable = .true._lk
        else
          eff_node = sub_n
        end if
      end do
    end if

    ref_node = jdoc_get_key(doc, eff_node, "$ref")
    if (ref_node > 0_i32) then
      call extract_ref_name(doc, ref_node, ref_name, ref_len, err)
      if (err%code /= 0_i32) return
      e_idx = ir_bundle_find_enum(bundle, ref_name(1:ref_len))
      if (e_idx > 0_i32) then
        fld%type_kind = IR_ENUM
        call ident_set(fld%enum_name, ref_name(1:ref_len), err)
        return
      end if
      child_udt = ir_bundle_find_udt(bundle, ref_name(1:ref_len))
      if (child_udt == 0_i32 .and. defs_node > 0_i32) then
        sub_n = jdoc_get_key(doc, defs_node, ref_name(1:ref_len))
        if (sub_n > 0_i32) then
          if (jdoc_has_key(doc, sub_n, "enum")) then
            call register_enum_node(doc, sub_n, ref_name(1:ref_len), caps, &
              options%uppercase_keywords, bundle, err)
            if (err%code /= 0_i32) return
            fld%type_kind = IR_ENUM
            call ident_set(fld%enum_name, ref_name(1:ref_len), err)
            return
          else
            call parse_object_schema(doc, defs_node, sub_n, ref_name(1:ref_len), &
              caps, options, bundle, child_udt, err)
            if (err%code /= 0_i32) return
          end if
        end if
      end if
      if (child_udt == 0_i32) then
        call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "Unresolved $ref target")
        return
      end if
      fld%type_kind = IR_NESTED_UDT
      call ident_set(fld%ref_udt_name, ref_name(1:ref_len), err)
      if (child_udt == owner_udt .or. fld%is_nullable) then
        if (caps%has_recursive_alloc) then
          fld%is_allocatable = .true._lk
        else if (caps%has_modules_and_type) then
          fld%is_pointer = .true._lk
        else if (child_udt == owner_udt) then
          call cdd_error_set_simple(err, CDD_ERR_DIALECT_CONSTRAINT, &
            "Recursive UDT requires Fortran 90+ POINTER or Fortran 2008+ ALLOCATABLE")
          return
        end if
      end if
      return
    end if

    if (jdoc_has_key(doc, eff_node, "enum")) then
      call build_inline_enum_name(bundle%udts(owner_udt)%json_name, prop_name, synth_name, slen)
      call register_enum_node(doc, eff_node, synth_name(1:slen), caps, &
        options%uppercase_keywords, bundle, err)
      if (err%code /= 0_i32) return
      fld%type_kind = IR_ENUM
      call ident_set(fld%enum_name, synth_name(1:slen), err)
      return
    end if

    tnode = jdoc_get_key(doc, eff_node, "type")
    call extract_schema_type_str(doc, tnode, type_str, tlen, fld%is_nullable, err)
    if (err%code /= 0_i32) return

    fmt_node = jdoc_get_key(doc, eff_node, "format")
    call get_opt_str(doc, fmt_node, fmt_str, flen)

    if (str_eq(type_str(1:tlen), "integer")) then
      if (str_eq(fmt_str(1:flen), "int8")) then
        fld%type_kind = IR_INT8
      else if (str_eq(fmt_str(1:flen), "int16")) then
        fld%type_kind = IR_INT16
      else if (str_eq(fmt_str(1:flen), "int64")) then
        fld%type_kind = IR_INT64
      else
        fld%type_kind = IR_INT32
      end if
    else if (str_eq(type_str(1:tlen), "number")) then
      if (str_eq(fmt_str(1:flen), "float")) then
        fld%type_kind = IR_REAL32
      else if (str_eq(fmt_str(1:flen), "quad") .or. str_eq(fmt_str(1:flen), "float128")) then
        fld%type_kind = IR_REAL128
      else
        fld%type_kind = IR_REAL64
      end if
    else if (str_eq(type_str(1:tlen), "boolean")) then
      fld%type_kind = IR_BOOL
    else if (str_eq(type_str(1:tlen), "string")) then
      maxl_node = jdoc_get_key(doc, eff_node, "maxLength")
      if (maxl_node > 0_i32 .and. doc%nodes(max(1_i32, maxl_node))%kind == JVAL_INT) then
        fld%type_kind = IR_STRING_FIXED
        fld%char_len = max(1_i32, int(doc%nodes(maxl_node)%int_val, kind=i32))
      else if (caps%has_deferred_char) then
        fld%type_kind = IR_STRING_DEFERRED
        fld%is_allocatable = .true._lk
      else
        fld%type_kind = IR_STRING_FIXED
        fld%char_len = 64_i32
      end if
    else if (str_eq(type_str(1:tlen), "array")) then
      items_node = eff_node
      do
        if (fld%rank >= CDD_MAX_RANK) then
          call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "Schema array exceeds max rank 15")
          return
        end if
        fld%rank = fld%rank + 1_i32
        maxl_node = jdoc_get_key(doc, items_node, "maxItems")
        if (maxl_node > 0_i32 .and. doc%nodes(max(1_i32, maxl_node))%kind == JVAL_INT) then
          fld%dims(fld%rank)%kind = DIM_FIXED
          fld%dims(fld%rank)%lower_bound = 1_i32
          fld%dims(fld%rank)%upper_bound = max(1_i32, int(doc%nodes(maxl_node)%int_val, i32))
        else if (caps%has_allocatable_comp) then
          fld%dims(fld%rank)%kind = DIM_DEFERRED
          fld%is_allocatable = .true._lk
        else if (caps%has_modules_and_type) then
          fld%dims(fld%rank)%kind = DIM_POINTER
          fld%is_pointer = .true._lk
        else
          fld%dims(fld%rank)%kind = DIM_FIXED
          fld%dims(fld%rank)%lower_bound = 1_i32
          fld%dims(fld%rank)%upper_bound = 8_i32
        end if
        sub_n = jdoc_get_key(doc, items_node, "items")
        if (sub_n == 0_i32) then
          call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "Array schema missing 'items'")
          return
        end if
        if (.not. is_array_type_schema(doc, sub_n)) then
          call map_property_schema(doc, defs_node, sub_n, owner_udt, prop_name, &
            caps, options, bundle, fld, err)
          return
        end if
        items_node = sub_n
      end do
    else if (str_eq(type_str(1:tlen), "object")) then
      call build_inline_enum_name(bundle%udts(owner_udt)%json_name, prop_name, synth_name, slen)
      call parse_object_schema(doc, defs_node, eff_node, synth_name(1:slen), &
        caps, options, bundle, child_udt, err)
      if (err%code /= 0_i32) return
      fld%type_kind = IR_NESTED_UDT
      call ident_set(fld%ref_udt_name, synth_name(1:slen), err)
    else
      call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "Unsupported JSON Schema property type")
      return
    end if

    if (fld%is_nullable .and. .not. fld%is_allocatable .and. .not. fld%is_pointer) then
      if (caps%has_allocatable_comp) then
        fld%is_allocatable = .true._lk
      else if (caps%has_modules_and_type) then
        fld%is_pointer = .true._lk
      end if
    end if

    def_node = jdoc_get_key(doc, pschema, "default")
    if (def_node > 0_i32) then
      call extract_default_literal(doc, def_node, fld, err)
    end if
  end subroutine map_property_schema

  !> @brief Populate dialect-valid Fortran identifiers across all UDTs, enums, and fields in bundle.
  !> @param[inout] bundle    IR schema bundle.
  !> @param[in]    caps      Dialect capability descriptor.
  !> @param[in]    uppercase Force uppercase flag.
  !> @param[out]   err       Error status.
  pure subroutine finalize_bundle_names(bundle, caps, uppercase, err)
    type(cdd_ir_schema_bundle_t), intent(inout) :: bundle
    type(cdd_dialect_caps_t), intent(in) :: caps
    logical(lk), intent(in) :: uppercase
    type(cdd_error_t), intent(out) :: err
    type(cdd_ident_t) :: used_udts(32)
    integer(i32) :: used_u_cnt, i, p_idx, mod_max

    call cdd_error_clear(err)
    used_u_cnt = 0_i32

    do i = 1_i32, bundle%udt_count
      if (caps%max_ident_len <= 6_i32) then
        call generate_legacy_ident( &
          bundle%udts(i)%json_name%raw(1:bundle%udts(i)%json_name%len), &
          IR_NESTED_UDT, .false._lk, 100_i32, 32_i32, used_udts, used_u_cnt, &
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
do i = 1_i32, bundle%udt_count
  if (bundle%udts(i)%has_extends) then
    p_idx = ir_bundle_find_udt(bundle, &
      bundle%udts(i)%extends_name%raw(1:bundle%udts(i)%extends_name%len))
    bundle%udts(i)%extends_name = bundle%udts(p_idx)%fortran_name
  end if
end do
    mod_max = max(8_i32, caps%max_ident_len)
    call sanitize_modern_ident( &
      bundle%udts(bundle%main_udt_idx)%json_name%raw( &
        1:bundle%udts(bundle%main_udt_idx)%json_name%len) // "_mod", &
      mod_max, uppercase, bundle%module_name, err)
  end subroutine finalize_bundle_names

  !> @brief Populate JSON Schema properties/required/allOf on obj_node for udt.
  !> @param[in]    bundle   Enclosing IR bundle.
  !> @param[in]    udt      Source UDT definition.
  !> @param[inout] doc      Target JSON DOM.
  !> @param[in]    obj_node Target JSON object node.
  !> @param[out]   err      Error status.
  pure subroutine build_udt_schema_obj(bundle, udt, doc, obj_node, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_udt_t), intent(in) :: udt
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(in) :: obj_node
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: vnode, props_obj, req_arr, req_cnt, i, fnode
    integer(i32) :: allof_arr, ref_obj, p_idx

    call cdd_error_clear(err)
    call jdoc_add_string(doc, udt%json_name%raw(1:udt%json_name%len), vnode, err)
    call jdoc_object_set(doc, obj_node, "title", vnode, .false._lk, err)

    if (udt%description%len > 0_i32) then
      call jdoc_add_string(doc, udt%description%raw(1:udt%description%len), vnode, err)
      call jdoc_object_set(doc, obj_node, "description", vnode, .false._lk, err)
    end if

    call jdoc_add_string(doc, "object", vnode, err)
    call jdoc_object_set(doc, obj_node, "type", vnode, .false._lk, err)

    if (udt%has_extends) then
      call jdoc_add_array(doc, allof_arr, err)
      call jdoc_add_object(doc, ref_obj, err)
      p_idx = ir_bundle_find_udt(bundle, udt%extends_name%raw(1:udt%extends_name%len))
      if (p_idx > 0_i32) then
        call jdoc_add_string(doc, "#/$defs/" // &
          bundle%udts(p_idx)%json_name%raw(1:bundle%udts(p_idx)%json_name%len), vnode, err)
      else
        call jdoc_add_string(doc, "#/$defs/" // &
          udt%extends_name%raw(1:udt%extends_name%len), vnode, err)
      end if
      call jdoc_object_set(doc, ref_obj, "$ref", vnode, .false._lk, err)
      call jdoc_array_append(doc, allof_arr, ref_obj, err)
      call jdoc_object_set(doc, obj_node, "allOf", allof_arr, .false._lk, err)
    end if

    call jdoc_add_object(doc, props_obj, err)
    call jdoc_add_array(doc, req_arr, err)
    req_cnt = 0_i32

    do i = 1_i32, udt%field_count
      call build_field_schema_obj(bundle, udt%fields(i), doc, fnode, err)
      call jdoc_object_set(doc, props_obj, &
        udt%fields(i)%json_name%raw(1:udt%fields(i)%json_name%len), fnode, .false._lk, err)
      if (udt%fields(i)%is_required) then
        req_cnt = req_cnt + 1_i32
        call jdoc_add_string(doc, &
          udt%fields(i)%json_name%raw(1:udt%fields(i)%json_name%len), vnode, err)
        call jdoc_array_append(doc, req_arr, vnode, err)
      end if
    end do

    call jdoc_object_set(doc, obj_node, "properties", props_obj, .false._lk, err)
    if (req_cnt > 0_i32) then
      call jdoc_object_set(doc, obj_node, "required", req_arr, .false._lk, err)
    end if
    call jdoc_add_bool(doc, .false._lk, vnode, err)
    call jdoc_object_set(doc, obj_node, "additionalProperties", vnode, .false._lk, err)
  end subroutine build_udt_schema_obj

  !> @brief Build a JSON Schema object node for a single cdd_ir_field_t (including arrays).
  !> @param[in]    bundle Enclosing IR bundle.
  !> @param[in]    fld    Field descriptor.
  !> @param[inout] doc    Target JSON DOM.
  !> @param[out]   fnode  Allocated JSON object node.
  !> @param[out]   err    Error status.
  pure subroutine build_field_schema_obj(bundle, fld, doc, fnode, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_field_t), intent(in) :: fld
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(out) :: fnode
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: leaf_node, cur_arr, next_arr, vnode, r, extent
    integer(i64) :: def_i
    real(r64) :: def_r
    type(cdd_error_t) :: tmp_err

    call cdd_error_clear(err)
    call build_leaf_type_schema(bundle, fld, doc, leaf_node, err)

    if (fld%rank == 0_i32) then
      fnode = leaf_node
    else
      cur_arr = leaf_node
      do r = fld%rank, 1_i32, -1_i32
        call jdoc_add_object(doc, next_arr, err)
        call jdoc_add_string(doc, "array", vnode, err)
        call jdoc_object_set(doc, next_arr, "type", vnode, .false._lk, err)
        if (fld%dims(r)%kind == DIM_FIXED) then
          extent = fld%dims(r)%upper_bound - fld%dims(r)%lower_bound + 1_i32
          call jdoc_add_int(doc, int(extent, i64), vnode, err)
          call jdoc_object_set(doc, next_arr, "minItems", vnode, .false._lk, err)
          call jdoc_add_int(doc, int(extent, i64), vnode, err)
          call jdoc_object_set(doc, next_arr, "maxItems", vnode, .false._lk, err)
        end if
        call jdoc_object_set(doc, next_arr, "items", cur_arr, .false._lk, err)
        cur_arr = next_arr
      end do
      fnode = cur_arr
    end if

    if (fld%description%len > 0_i32) then
      call jdoc_add_string(doc, fld%description%raw(1:fld%description%len), vnode, err)
      call jdoc_object_set(doc, fnode, "description", vnode, .false._lk, err)
    end if

    if (fld%is_nullable) then
      call jdoc_add_bool(doc, .true._lk, vnode, err)
      call jdoc_object_set(doc, fnode, "nullable", vnode, .false._lk, err)
    end if

    if (fld%has_default .and. fld%default_lexeme%len > 0_i32) then
      if (fld%type_kind == IR_BOOL) then
        call jdoc_add_bool(doc, &
          str_eq_nocase(fld%default_lexeme%raw(1:fld%default_lexeme%len), "true"), vnode, err)
      else if (fld%type_kind >= IR_INT8 .and. fld%type_kind <= IR_INT64) then
        call parse_i64_strict(fld%default_lexeme%raw(1:fld%default_lexeme%len), def_i, tmp_err)
        call jdoc_add_int(doc, def_i, vnode, err)
      else if (fld%type_kind >= IR_REAL32 .and. fld%type_kind <= IR_REAL128) then
        call parse_r64_strict(fld%default_lexeme%raw(1:fld%default_lexeme%len), def_r, tmp_err)
        call jdoc_add_float(doc, def_r, "", vnode, err)
      else
        call jdoc_add_string(doc, fld%default_lexeme%raw(1:fld%default_lexeme%len), vnode, err)
      end if
      call jdoc_object_set(doc, fnode, "default", vnode, .false._lk, err)
    end if
  end subroutine build_field_schema_obj

  !> @brief Build the scalar/element leaf JSON Schema object for a field's type_kind.
  !> @param[in]    bundle Enclosing IR bundle.
  !> @param[in]    fld    Field descriptor.
  !> @param[inout] doc    Target JSON DOM.
  !> @param[out]   lnode  Allocated leaf schema object node.
  !> @param[out]   err    Error status.
  pure subroutine build_leaf_type_schema(bundle, fld, doc, lnode, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_field_t), intent(in) :: fld
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(out) :: lnode
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: vnode, idx

    call jdoc_add_object(doc, lnode, err)

    select case (fld%type_kind)
    case (IR_INT8)
      call jdoc_add_string(doc, "integer", vnode, err)
      call jdoc_object_set(doc, lnode, "type", vnode, .false._lk, err)
      call jdoc_add_string(doc, "int8", vnode, err)
      call jdoc_object_set(doc, lnode, "format", vnode, .false._lk, err)
    case (IR_INT16)
      call jdoc_add_string(doc, "integer", vnode, err)
      call jdoc_object_set(doc, lnode, "type", vnode, .false._lk, err)
      call jdoc_add_string(doc, "int16", vnode, err)
      call jdoc_object_set(doc, lnode, "format", vnode, .false._lk, err)
    case (IR_INT32)
      call jdoc_add_string(doc, "integer", vnode, err)
      call jdoc_object_set(doc, lnode, "type", vnode, .false._lk, err)
      call jdoc_add_string(doc, "int32", vnode, err)
      call jdoc_object_set(doc, lnode, "format", vnode, .false._lk, err)
    case (IR_INT64)
      call jdoc_add_string(doc, "integer", vnode, err)
      call jdoc_object_set(doc, lnode, "type", vnode, .false._lk, err)
      call jdoc_add_string(doc, "int64", vnode, err)
      call jdoc_object_set(doc, lnode, "format", vnode, .false._lk, err)
    case (IR_REAL32)
      call jdoc_add_string(doc, "number", vnode, err)
      call jdoc_object_set(doc, lnode, "type", vnode, .false._lk, err)
      call jdoc_add_string(doc, "float", vnode, err)
      call jdoc_object_set(doc, lnode, "format", vnode, .false._lk, err)
    case (IR_REAL128)
      call jdoc_add_string(doc, "number", vnode, err)
      call jdoc_object_set(doc, lnode, "type", vnode, .false._lk, err)
      call jdoc_add_string(doc, "quad", vnode, err)
      call jdoc_object_set(doc, lnode, "format", vnode, .false._lk, err)
    case (IR_BOOL)
      call jdoc_add_string(doc, "boolean", vnode, err)
      call jdoc_object_set(doc, lnode, "type", vnode, .false._lk, err)
    case (IR_STRING_FIXED)
      call jdoc_add_string(doc, "string", vnode, err)
      call jdoc_object_set(doc, lnode, "type", vnode, .false._lk, err)
      call jdoc_add_int(doc, int(fld%char_len, i64), vnode, err)
      call jdoc_object_set(doc, lnode, "maxLength", vnode, .false._lk, err)
    case (IR_STRING_DEFERRED)
      call jdoc_add_string(doc, "string", vnode, err)
      call jdoc_object_set(doc, lnode, "type", vnode, .false._lk, err)
    case (IR_ENUM)
      idx = ir_bundle_find_enum(bundle, fld%enum_name%raw(1:fld%enum_name%len))
      if (idx > 0_i32) then
        call jdoc_add_string(doc, "#/$defs/" // &
          bundle%enums(idx)%json_name%raw(1:bundle%enums(idx)%json_name%len), vnode, err)
      else
        call jdoc_add_string(doc, "#/$defs/" // fld%enum_name%raw(1:fld%enum_name%len), vnode, err)
      end if
      call jdoc_object_set(doc, lnode, "$ref", vnode, .false._lk, err)
    case (IR_NESTED_UDT)
      idx = ir_bundle_find_udt(bundle, fld%ref_udt_name%raw(1:fld%ref_udt_name%len))
      if (idx > 0_i32) then
        call jdoc_add_string(doc, "#/$defs/" // &
          bundle%udts(idx)%json_name%raw(1:bundle%udts(idx)%json_name%len), vnode, err)
      else
        call jdoc_add_string(doc, "#/$defs/" // &
          fld%ref_udt_name%raw(1:fld%ref_udt_name%len), vnode, err)
      end if
      call jdoc_object_set(doc, lnode, "$ref", vnode, .false._lk, err)
    case default
      call jdoc_add_string(doc, "number", vnode, err)
      call jdoc_object_set(doc, lnode, "type", vnode, .false._lk, err)
      call jdoc_add_string(doc, "double", vnode, err)
      call jdoc_object_set(doc, lnode, "format", vnode, .false._lk, err)
    end select
  end subroutine build_leaf_type_schema

  !> @brief Build a JSON Schema enum object node from a cdd_ir_enum_t.
  !> @param[in]    edef  IR enum definition.
  !> @param[inout] doc   Target JSON DOM.
  !> @param[out]   enode Allocated enum schema object node.
  !> @param[out]   err   Error status.
  pure subroutine build_enum_schema_obj(edef, doc, enode, err)
    type(cdd_ir_enum_t), intent(in) :: edef
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(out) :: enode
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: vnode, arr_node, k

    call jdoc_add_object(doc, enode, err)
    call jdoc_add_string(doc, "string", vnode, err)
    call jdoc_object_set(doc, enode, "type", vnode, .false._lk, err)
    if (edef%description%len > 0_i32) then
      call jdoc_add_string(doc, edef%description%raw(1:edef%description%len), vnode, err)
      call jdoc_object_set(doc, enode, "description", vnode, .false._lk, err)
    end if
    call jdoc_add_array(doc, arr_node, err)
    do k = 1_i32, edef%val_count
      call jdoc_add_string(doc, edef%json_vals(k)%raw(1:edef%json_vals(k)%len), vnode, err)
      call jdoc_array_append(doc, arr_node, vnode, err)
    end do
    call jdoc_object_set(doc, enode, "enum", arr_node, .false._lk, err)
  end subroutine build_enum_schema_obj

  !> @brief Extract trailing schema name from "#/$defs/X", "#/definitions/X", or "#/components/schemas/X".
  !> @param[in]  doc      JSON DOM.
  !> @param[in]  ref_node String node containing $ref URI.
  !> @param[out] out_name Extracted target schema name.
  !> @param[out] out_len  Active length of out_name.
  !> @param[out] err      Error status if $ref URI is invalid.
  pure subroutine extract_ref_name(doc, ref_node, out_name, out_len, err)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: ref_node
    character(len=64, kind=c_k), intent(out) :: out_name
    integer(i32), intent(out) :: out_len
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: lo, hi, start_idx, k

    call cdd_error_clear(err)
    out_len = 0_i32
    if (doc%nodes(ref_node)%kind /= JVAL_STRING) then
      call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "$ref must be a string")
      return
    end if
    call jdoc_node_str_bounds(doc, ref_node, lo, hi)
    if (hi < lo) then
      call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "Empty $ref string")
      return
    end if

    if (starts_with(doc%str_pool%raw(lo:hi), "#/$defs/")) then
      start_idx = lo + 8_i32
    else if (starts_with(doc%str_pool%raw(lo:hi), "#/definitions/")) then
      start_idx = lo + 14_i32
    else if (starts_with(doc%str_pool%raw(lo:hi), "#/components/schemas/")) then
      start_idx = lo + 21_i32
    else
      call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "Unsupported external $ref URI")
      return
    end if

    out_len = hi - start_idx + 1_i32
    if (out_len < 1_i32 .or. out_len > 64_i32) then
      out_len = 0_i32
      call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "Invalid $ref schema identifier length")
      return
    end if
    do k = 1_i32, out_len
      out_name(k:k) = doc%str_pool%raw(start_idx + k - 1_i32:start_idx + k - 1_i32)
    end do
  end subroutine extract_ref_name

  !> @brief Extract primary type string from "type": "string" or "type": ["string", "null"].
  !> @param[in]    doc      JSON DOM.
  !> @param[in]    tnode    "type" property value node (or 0 if omitted).
  !> @param[out]   out_str  Primary type string.
  !> @param[out]   out_len  Active length.
  !> @param[inout] nullable Updated to true if "null" appears in type array.
  !> @param[out]   err      Error status.
  pure subroutine extract_schema_type_str(doc, tnode, out_str, out_len, nullable, err)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: tnode
    character(len=64, kind=c_k), intent(out) :: out_str
    integer(i32), intent(out) :: out_len
    logical(lk), intent(inout) :: nullable
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: alen, k, elem, elen
    character(len=64, kind=c_k) :: estr

    call cdd_error_clear(err)
    out_len = 0_i32
    if (tnode == 0_i32) then
      out_str(1:6) = "object"
      out_len = 6_i32
      return
    end if

    if (doc%nodes(tnode)%kind == JVAL_STRING) then
      call get_opt_str(doc, tnode, out_str, out_len)
      if (out_len > 0_i32) return
    else if (doc%nodes(tnode)%kind == JVAL_ARRAY) then
      alen = jdoc_array_len(doc, tnode)
      do k = 1_i32, alen
        elem = jdoc_array_item(doc, tnode, k)
        call get_opt_str(doc, elem, estr, elen)
        if (elen > 0_i32) then
          if (str_eq(estr(1:elen), "null")) then
            nullable = .true._lk
          else if (out_len == 0_i32) then
            call get_opt_str(doc, elem, out_str, out_len)
          end if
        end if
      end do
      if (out_len > 0_i32) return
    end if
    call cdd_error_set_simple(err, CDD_ERR_JSON_SCHEMA, "Invalid 'type' attribute in JSON Schema")
  end subroutine extract_schema_type_str

  !> @brief Copy a JVAL_STRING node's text into out_str(1:out_len) (0 if absent/non-string).
  !> @param[in]  doc     JSON DOM.
  !> @param[in]  snode   Node index (may be 0).
  !> @param[out] out_str Destination buffer (64 chars).
  !> @param[out] out_len Active byte count.
  pure subroutine get_opt_str(doc, snode, out_str, out_len)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: snode
    character(len=64, kind=c_k), intent(out) :: out_str
    integer(i32), intent(out) :: out_len
    integer(i32) :: lo, hi, k

    out_len = 0_i32
    if (snode < 1_i32) return
    if (doc%nodes(snode)%kind /= JVAL_STRING) return
    call jdoc_node_str_bounds(doc, snode, lo, hi)
    if (hi < lo) return
    out_len = hi - lo + 1_i32
    if (out_len > 64_i32) out_len = 64_i32
    do k = 1_i32, out_len
      out_str(k:k) = doc%str_pool%raw(lo + k - 1_i32:lo + k - 1_i32)
    end do
  end subroutine get_opt_str

  !> @brief Check whether snode is an object with "type": "null".
  !> @param[in] doc   JSON DOM.
  !> @param[in] snode Schema node index.
  !> @return True if schema represents JSON null.
  pure function is_null_type_schema(doc, snode) result(ok)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: snode
    logical(lk) :: ok
    integer(i32) :: tnode
    character(len=64, kind=c_k) :: tstr
    integer(i32) :: tlen

    ok = .false._lk
    if (doc%nodes(snode)%kind /= JVAL_OBJECT) return
    tnode = jdoc_get_key(doc, snode, "type")
    call get_opt_str(doc, tnode, tstr, tlen)
    if (tlen == 4_i32) then
      ok = str_eq(tstr(1:4), "null")
    end if
  end function is_null_type_schema

  !> @brief Check whether snode is an object with "type": "array".
  !> @param[in] doc   JSON DOM.
  !> @param[in] snode Schema node index.
  !> @return True if schema represents a nested JSON array.
  pure function is_array_type_schema(doc, snode) result(ok)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: snode
    logical(lk) :: ok
    integer(i32) :: tnode
    character(len=64, kind=c_k) :: tstr
    integer(i32) :: tlen

    ok = .false._lk
    if (doc%nodes(snode)%kind /= JVAL_OBJECT) return
    tnode = jdoc_get_key(doc, snode, "type")
    call get_opt_str(doc, tnode, tstr, tlen)
    if (tlen == 5_i32) then
      ok = str_eq(tstr(1:5), "array")
    end if
  end function is_array_type_schema

  !> @brief Test whether target string appears in a JSON string array node.
  !> @param[in] doc      JSON DOM.
  !> @param[in] arr_node Array node index (0 if absent).
  !> @param[in] target   String to locate.
  !> @return True if target is an element of arr_node.
  pure function is_in_string_array(doc, arr_node, target) result(found)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: arr_node
    character(len=*, kind=c_k), intent(in) :: target
    logical(lk) :: found
    integer(i32) :: cnt, k, item, elen
    character(len=64, kind=c_k) :: estr

    found = .false._lk
    if (arr_node < 1_i32) return
    cnt = jdoc_array_len(doc, arr_node)
    do k = 1_i32, cnt
      item = jdoc_array_item(doc, arr_node, k)
      call get_opt_str(doc, item, estr, elen)
      if (elen > 0_i32) then
        if (str_eq(estr(1:elen), target)) then
          found = .true._lk
          return
        end if
      end if
    end do
  end function is_in_string_array

  !> @brief Build a synthetic child UDT / enum name "<Parent>_<prop>".
  !> @param[in]  parent_id Parent UDT JSON name.
  !> @param[in]  prop_name Property key.
  !> @param[out] out_name  Synthesized name buffer (64 chars).
  !> @param[out] out_len   Active length.
  pure subroutine build_inline_enum_name(parent_id, prop_name, out_name, out_len)
    type(cdd_ident_t), intent(in) :: parent_id
    character(len=*, kind=c_k), intent(in) :: prop_name
    character(len=64, kind=c_k), intent(out) :: out_name
    integer(i32), intent(out) :: out_len
    integer(i32) :: k, pn, qn

    out_len = 0_i32
    pn = min(30_i32, parent_id%len)
    do k = 1_i32, pn
      out_len = out_len + 1_i32
      out_name(out_len:out_len) = parent_id%raw(k:k)
    end do
    out_len = out_len + 1_i32
    out_name(out_len:out_len) = "_"
    qn = min(32_i32, len(prop_name, kind=i32))
    do k = 1_i32, qn
      out_len = out_len + 1_i32
      out_name(out_len:out_len) = prop_name(k:k)
    end do
  end subroutine build_inline_enum_name

  !> @brief Serialize a scalar JSON "default" node into fld%default_lexeme.
  !> @param[in]    doc      JSON DOM.
  !> @param[in]    def_node Default value node.
  !> @param[inout] fld      Target field descriptor.
  !> @param[out]   err      Error status.
  pure subroutine extract_default_literal(doc, def_node, fld, err)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: def_node
    type(cdd_ir_field_t), intent(inout) :: fld
    type(cdd_error_t), intent(out) :: err
    character(len=32, kind=c_k) :: nbuf
    integer(i32) :: nlen, lo, hi

    call cdd_error_clear(err)
    select case (doc%nodes(def_node)%kind)
    case (JVAL_BOOL)
      fld%has_default = .true._lk
      if (doc%nodes(def_node)%bool_val) then
        call ident_set(fld%default_lexeme, "true", err)
      else
        call ident_set(fld%default_lexeme, "false", err)
      end if
    case (JVAL_INT)
      fld%has_default = .true._lk
      call format_i64_to_str(doc%nodes(def_node)%int_val, nbuf, nlen)
      call ident_set(fld%default_lexeme, nbuf(1:nlen), err)
    case (JVAL_FLOAT)
      fld%has_default = .true._lk
      call format_r64_to_str(doc%nodes(def_node)%float_val, nbuf, nlen)
      call ident_set(fld%default_lexeme, nbuf(1:nlen), err)
    case (JVAL_STRING)
      fld%has_default = .true._lk
      call jdoc_node_str_bounds(doc, def_node, lo, hi)
      if (hi >= lo) then
        call ident_set(fld%default_lexeme, doc%str_pool%raw(lo:hi), err)
      else
        call ident_set(fld%default_lexeme, "", err)
      end if
    case default
      fld%is_nullable = (doc%nodes(def_node)%kind == JVAL_NULL) .or. fld%is_nullable
    end select
  end subroutine extract_default_literal

  !> @brief Extract optional "description" string from schema node into dst_desc.
  !> @param[in]  doc      JSON DOM.
  !> @param[in]  snode    Schema object node.
  !> @param[out] dst_desc Destination cdd_string_t.
  pure subroutine extract_opt_description(doc, snode, dst_desc)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: snode
    type(cdd_string_t), intent(out) :: dst_desc
    integer(i32) :: dnode, lo, hi
    type(cdd_error_t) :: tmp_err

    call str_clear(dst_desc)
    dnode = jdoc_get_key(doc, snode, "description")
    if (dnode < 1_i32) return
    if (doc%nodes(dnode)%kind /= JVAL_STRING) return
    call jdoc_node_str_bounds(doc, dnode, lo, hi)
    if (hi >= lo) then
      call str_set(dst_desc, doc%str_pool%raw(lo:hi), tmp_err)
    end if
  end subroutine extract_opt_description

end module cdd_json_schema
