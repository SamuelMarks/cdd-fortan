!> @file cdd_patch_udt.f90
!> @brief Operation 3: Surgical trivia-preserving CST synchronization from JSON to Fortran.

!> @brief Patches Fortran UDTs and companion JSON codecs in place while preserving 100% of trivia.
module cdd_patch_udt
  use cdd_kinds, only: i32, i64, lk, c_k
  use cdd_errors, only: CDD_ERR_UDT_NOT_FOUND, cdd_error_t, cdd_error_clear
  use cdd_string, only: TERM_CRLF, cdd_buffer_t, buf_init, buf_append_char, buf_append_str, &
    buf_append_i64, str_eq, str_eq_nocase
  use cdd_standard, only: STD_FORTRAN_I, STD_FORTRAN_III, STD_FORTRAN_77, STD_FORTRAN_90, &
    FORM_FIXED, JSON_MODE_SCHEMA, JSON_MODE_SAMPLE, cdd_options_t, &
    cdd_dialect_caps_t, get_dialect_capabilities, resolve_effective_form
  use cdd_json_dom, only: cdd_json_doc_t
  use cdd_json_parser, only: json_parse
  use cdd_json_schema, only: is_json_schema_doc, json_schema_to_ir
  use cdd_json_infer, only: json_sample_to_ir
  use cdd_ir_types, only: CDD_MAX_FIELDS, IR_INT8, IR_INT16, IR_INT64, IR_REAL32, IR_REAL64, &
    IR_REAL128, IR_BOOL, IR_STRING_FIXED, IR_STRING_DEFERRED, &
    IR_ENUM, IR_NESTED_UDT, cdd_ir_field_t, cdd_ir_udt_t, cdd_ir_schema_bundle_t, &
    ir_bundle_find_udt
  use cdd_ir_namemap, only: format_cdd_field_comment
  use cdd_cst_trivia, only: CDD_MAX_TRIVIA, TRIVIA_WHITESPACE, TRIVIA_NEWLINE, &
    TRIVIA_DOXYGEN_COMMENT, TRIVIA_LINE_COMMENT
  use cdd_cst_tokens, only: FTOK_DCOLON, ftok_norm_eq
  use cdd_cst_nodes, only: CST_MODULE, CST_DERIVED_TYPE, CST_DEC_STRUCTURE, CST_COMMON_BLOCK, &
    CST_EQUIVALENCE_STMT, CST_DIMENSION_STMT, CST_SAVE_STMT, CST_FIELD_DECL, CST_SUBROUTINE, &
    CST_FUNCTION, CST_ASSIGNED_GOTO_BLOCK, CST_OPAQUE_STMT, cdd_cst_tree_t, cst_add_node, &
    cst_mark_deleted, cst_replace_node, cst_insert_before, cst_insert_after
  use cdd_cst_parser, only: cst_parse
  use cdd_cst_printer, only: cst_print
  use cdd_emit_udt, only: emit_udt_fortran, emit_udt_types_section, &
    emit_single_modern_field_line, make_common_names, emit_common_line
  use cdd_extract_udt, only: extract_udt_to_ir
  use cdd_emit_codec, only: emit_codec_fortran, emit_modern_to_json_proc, &
    emit_modern_from_json_proc, emit_legacy_to_json_proc, emit_legacy_from_json_proc, &
    codec_routine_names
  implicit none (type, external)
  private

  public :: patch_fortran_with_json

contains

  !> @brief Surgically synchronize an existing Fortran source text with a JSON Schema or sample.
  !> @param[in]  existing_src Original Fortran source code (fixed or free form).
  !> @param[in]  json_src     Target JSON Schema or JSON sample payload.
  !> @param[in]  options      Dialect, form, target UDT name, and codec-sync options.
  !> @param[out] patched_buf  Updated Fortran source buffer preserving surrounding trivia.
  !> @param[out] err          Structured error status.
  pure subroutine patch_fortran_with_json(existing_src, json_src, options, patched_buf, err)
    character(len=*, kind=c_k), intent(in) :: existing_src
    character(len=*, kind=c_k), intent(in) :: json_src
    type(cdd_options_t), intent(in) :: options
    type(cdd_buffer_t), intent(out) :: patched_buf
    type(cdd_error_t), intent(out) :: err
    type(cdd_json_doc_t) :: jdoc
    type(cdd_ir_schema_bundle_t) :: target_bundle, exist_bundle
    type(cdd_cst_tree_t) :: tree
    type(cdd_error_t) :: ext_err
    type(cdd_dialect_caps_t) :: caps
    integer(i32) :: form, sid, nl_len, udt_node
    character(len=2, kind=c_k) :: nl_seq

    call buf_init(patched_buf)
    call cdd_error_clear(err)

    call resolve_effective_form(options, form, err)
    if (err%code /= 0_i32) return

    call json_parse(json_src, jdoc, err)
    if (err%code /= 0_i32) return

    if (options%json_mode == JSON_MODE_SCHEMA) then
      call json_schema_to_ir(jdoc, options, target_bundle, err)
    else if (options%json_mode == JSON_MODE_SAMPLE) then
      call json_sample_to_ir(jdoc, options, target_bundle, err)
    else if (is_json_schema_doc(jdoc)) then
      call json_schema_to_ir(jdoc, options, target_bundle, err)
    else
      call json_sample_to_ir(jdoc, options, target_bundle, err)
    end if
    if (err%code /= 0_i32) return

    call cst_parse(existing_src, options, tree, err)
    if (err%code /= 0_i32) return

    call detect_file_newline(tree, nl_seq, nl_len)
    sid = options%standard%id
    caps = get_dialect_capabilities(options%standard, options%allow_dec_structure)

    call extract_udt_to_ir(tree, options, exist_bundle, ext_err)
    if (ext_err%code == CDD_ERR_UDT_NOT_FOUND) then
      call insert_missing_udt_and_codecs(tree, target_bundle, options, sid, &
        nl_seq(1:nl_len), err)
      call cst_print(tree, patched_buf, err)
      return
    end if

    call align_bundle_identifiers(exist_bundle, target_bundle)
    udt_node = find_target_udt_cst_node(tree, exist_bundle%udts(exist_bundle%main_udt_idx))

    if (udt_node > 0_i32) then
      call patch_structured_udt_node(tree, udt_node, &
        exist_bundle%udts(exist_bundle%main_udt_idx), target_bundle, &
        target_bundle%main_udt_idx, options, sid, caps, form, nl_seq(1:nl_len), err)
    else
      call patch_legacy_flat_udt(tree, exist_bundle%udts(exist_bundle%main_udt_idx), &
        target_bundle, target_bundle%main_udt_idx, options, sid, caps, nl_seq(1:nl_len), err)
    end if

    if (options%include_codecs_in_sync) then
      call patch_companion_codecs(tree, target_bundle, target_bundle%main_udt_idx, &
        options, sid, caps, nl_seq(1:nl_len), err)
    end if

    call cst_print(tree, patched_buf, err)
  end subroutine patch_fortran_with_json

  !> @brief Detect whether existing CST uses `CRLF` or `LF` line terminators.
  !> @param[in]  tree   Parsed CST tree.
  !> @param[out] nl_seq 2-byte newline buffer (`LF` or `CRLF`).
  !> @param[out] nl_len Active length (1 for `LF`, 2 for `CRLF`).
  pure subroutine detect_file_newline(tree, nl_seq, nl_len)
    type(cdd_cst_tree_t), intent(in) :: tree
    character(len=2, kind=c_k), intent(out) :: nl_seq
    integer(i32), intent(out) :: nl_len
    integer(i32) :: k

    nl_seq(1:1) = achar(10, kind=c_k)
    nl_seq(2:2) = " "
    nl_len = 1_i32
    do k = 1_i32, tree%stream%trivia_count
      if (tree%stream%trivia(k)%kind == TRIVIA_NEWLINE .and. &
          tree%stream%trivia(k)%term_kind == TERM_CRLF) then
        nl_seq(1:1) = achar(13, kind=c_k)
        nl_seq(2:2) = achar(10, kind=c_k)
        nl_len = 2_i32
        return
      end if
    end do
  end subroutine detect_file_newline

  !> @brief Preserve existing UDT and field Fortran identifiers when JSON keys match.
  !> @param[in]    exist_b  Extracted IR bundle from existing Fortran file.
  !> @param[inout] target_b Target IR bundle from JSON input.
  pure subroutine align_bundle_identifiers(exist_b, target_b)
    type(cdd_ir_schema_bundle_t), intent(in) :: exist_b
    type(cdd_ir_schema_bundle_t), intent(inout) :: target_b
    integer(i32) :: em, tm, tf, ef

    em = exist_b%main_udt_idx
    tm = target_b%main_udt_idx
    if (exist_b%module_name%len > 0_i32) target_b%module_name = exist_b%module_name
    target_b%udts(tm)%fortran_name = exist_b%udts(em)%fortran_name

    do tf = 1_i32, target_b%udts(tm)%field_count
      ef = match_field_in_udt(exist_b%udts(em), target_b%udts(tm)%fields(tf))
      if (ef > 0_i32) then
        target_b%udts(tm)%fields(tf)%fortran_name = exist_b%udts(em)%fields(ef)%fortran_name
      end if
    end do
  end subroutine align_bundle_identifiers

  !> @brief Match a field inside `udt` by exact `json_name` first, then case-insensitive identifier.
  !> @param[in] udt  UDT to search.
  !> @param[in] cand Candidate field to match.
  !> @return 1-based field index in `udt` or 0 if not present.
  pure function match_field_in_udt(udt, cand) result(idx)
    type(cdd_ir_udt_t), intent(in) :: udt
    type(cdd_ir_field_t), intent(in) :: cand
    integer(i32) :: idx
    integer(i32) :: k

    idx = 0_i32
    do k = 1_i32, udt%field_count
      if (str_eq(udt%fields(k)%json_name%raw(1:udt%fields(k)%json_name%len), &
                 cand%json_name%raw(1:cand%json_name%len))) then
        idx = k
        return
      end if
    end do
    do k = 1_i32, udt%field_count
      if (str_eq_nocase(udt%fields(k)%fortran_name%raw(1:udt%fields(k)%fortran_name%len), &
                        cand%fortran_name%raw(1:cand%fortran_name%len)) .or. &
          str_eq_nocase(udt%fields(k)%fortran_name%raw(1:udt%fields(k)%fortran_name%len), &
                        cand%json_name%raw(1:cand%json_name%len))) then
        idx = k
        return
      end if
    end do
  end function match_field_in_udt

  !> @brief Locate `CST_DERIVED_TYPE` or `CST_DEC_STRUCTURE` node matching `exist_udt` (0 if flat).
  !> @param[in] tree      Parsed CST tree.
  !> @param[in] exist_udt Extracted UDT record.
  !> @return CST node index or 0.
  pure function find_target_udt_cst_node(tree, exist_udt) result(nidx)
    type(cdd_cst_tree_t), intent(in) :: tree
    type(cdd_ir_udt_t), intent(in) :: exist_udt
    integer(i32) :: nidx
    integer(i32) :: i

    nidx = 0_i32
    do i = 1_i32, tree%node_count
      if (tree%nodes(i)%kind == CST_DERIVED_TYPE .or. &
          tree%nodes(i)%kind == CST_DEC_STRUCTURE) then
        if (str_eq_nocase(tree%nodes(i)%name%raw(1:tree%nodes(i)%name%len), &
                          exist_udt%fortran_name%raw(1:exist_udt%fortran_name%len))) then
          nidx = i
          return
        end if
      end if
    end do
  end function find_target_udt_cst_node

  !> @brief Insert generated UDT (and optional codecs) when target UDT does not yet exist in file.
  !> @param[inout] tree    CST tree.
  !> @param[in]    bundle  Target IR bundle.
  !> @param[in]    options Dialect options.
  !> @param[in]    sid     Standard ID.
  !> @param[in]    nl_seq  Target line terminator (`LF` or `CRLF`).
  !> @param[inout] err     Error status.
  pure subroutine insert_missing_udt_and_codecs(tree, bundle, options, sid, nl_seq, err)
    type(cdd_cst_tree_t), intent(inout) :: tree
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_options_t), intent(in) :: options
    integer(i32), intent(in) :: sid
    character(len=*, kind=c_k), intent(in) :: nl_seq
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: mod_node, k, cont_tok, end_mod_tok, anchor
    type(cdd_buffer_t) :: raw_b, udt_b, cod_b, norm_b

    mod_node = 0_i32
    do k = 1_i32, tree%node_count
      if (tree%nodes(k)%kind == CST_MODULE) then
        mod_node = k
        exit
      end if
    end do

    if (mod_node > 0_i32) then
      call emit_udt_types_section(bundle, options, &
        (options%emit_type_bound_procs .and. options%include_codecs_in_sync), udt_b, err)
      cont_tok = find_contains_tok_in_node(tree, mod_node)
      end_mod_tok = find_stmt_start_of_last_tok(tree, tree%nodes(mod_node)%last_tok)
      if (cont_tok > 0_i32) then
        call convert_lf_buf(udt_b%raw(1:udt_b%len), nl_seq, norm_b, err)
        call insert_before_tok(tree, cont_tok, norm_b%raw(1:norm_b%len), err)
        if (options%include_codecs_in_sync) then
          call buf_init(cod_b)
          call emit_modern_to_json_proc(bundle, bundle%main_udt_idx, options, .false._lk, cod_b, err)
          call emit_modern_from_json_proc(bundle, bundle%main_udt_idx, options, .false._lk, cod_b, err)
          call convert_lf_buf(cod_b%raw(1:cod_b%len), nl_seq, norm_b, err)
          call insert_before_tok(tree, end_mod_tok, norm_b%raw(1:norm_b%len), err)
        end if
      else
        if (options%include_codecs_in_sync) then
          call buf_append_str(udt_b, "contains" // achar(10, kind=c_k), err)
          call emit_modern_to_json_proc(bundle, bundle%main_udt_idx, options, .false._lk, udt_b, err)
          call emit_modern_from_json_proc(bundle, bundle%main_udt_idx, options, .false._lk, udt_b, err)
        end if
        call convert_lf_buf(udt_b%raw(1:udt_b%len), nl_seq, norm_b, err)
        call insert_before_tok(tree, end_mod_tok, norm_b%raw(1:norm_b%len), err)
      end if
      return
    end if

    if (sid >= STD_FORTRAN_90 .and. options%include_codecs_in_sync) then
      call emit_codec_fortran(bundle, options, raw_b, err)
      call convert_lf_buf(raw_b%raw(1:raw_b%len), nl_seq, norm_b, err)
      call cst_add_node(tree, CST_OPAQUE_STMT, 1_i32, tree%stream%token_count, &
        tree%stream%token_count, anchor, err)
      call cst_insert_after(tree, anchor, norm_b%raw(1:norm_b%len), err)
    else
      call emit_udt_fortran(bundle, options, udt_b, err)
      call convert_lf_buf(udt_b%raw(1:udt_b%len), nl_seq, norm_b, err)
      call insert_before_tok(tree, 1_i32, norm_b%raw(1:norm_b%len), err)
      if (options%include_codecs_in_sync) then
        call emit_codec_fortran(bundle, options, cod_b, err)
        call convert_lf_buf(cod_b%raw(1:cod_b%len), nl_seq, norm_b, err)
        call cst_add_node(tree, CST_OPAQUE_STMT, 1_i32, tree%stream%token_count, &
          tree%stream%token_count, anchor, err)
        call cst_insert_after(tree, anchor, norm_b%raw(1:norm_b%len), err)
      end if
    end if
  end subroutine insert_missing_udt_and_codecs

  !> @brief Surgically patch fields inside a `CST_DERIVED_TYPE` or `CST_DEC_STRUCTURE` node.
  !> @param[inout] tree         CST tree.
  !> @param[in]    udt_node     Target CST node index.
  !> @param[in]    exist_udt    Extracted existing UDT IR.
  !> @param[in]    target_b     Target IR bundle.
  !> @param[in]    target_u_idx Target UDT index in `target_b`.
  !> @param[in]    options      Dialect options.
  !> @param[in]    sid          Standard ID.
  !> @param[in]    caps         Dialect capabilities.
  !> @param[in]    form         Effective source form.
  !> @param[in]    nl_seq       File newline sequence (`LF` or `CRLF`).
  !> @param[inout] err          Error status.
  pure subroutine patch_structured_udt_node(tree, udt_node, exist_udt, target_b, target_u_idx, &
      options, sid, caps, form, nl_seq, err)
    type(cdd_cst_tree_t), intent(inout) :: tree
    integer(i32), intent(in) :: udt_node
    type(cdd_ir_udt_t), intent(in) :: exist_udt
    type(cdd_ir_schema_bundle_t), intent(in) :: target_b
    integer(i32), intent(in) :: target_u_idx
    type(cdd_options_t), intent(in) :: options
    integer(i32), intent(in) :: sid
    type(cdd_dialect_caps_t), intent(in) :: caps
    integer(i32), intent(in) :: form
    character(len=*, kind=c_k), intent(in) :: nl_seq
    type(cdd_error_t), intent(inout) :: err
    logical(lk) :: matched_t(CDD_MAX_FIELDS), up_kw, use_dcol, is_dec
    character(len=64, kind=c_k) :: ind_str
    integer(i32) :: ind_len, i, ef, tf, last_kept_node, hdr_end_tok, anchor
    type(cdd_buffer_t) :: pres_b, mod_b, add_b

    matched_t = .false._lk
    is_dec = (tree%nodes(udt_node)%kind == CST_DEC_STRUCTURE)
    call detect_udt_member_style(tree, udt_node, options, ind_str, ind_len, up_kw, use_dcol)
    last_kept_node = 0_i32

    do i = 2_i32, tree%node_count
      if (tree%nodes(i)%parent == udt_node .and. tree%nodes(i)%kind == CST_FIELD_DECL) then
        ef = find_field_by_cst_name(exist_udt, &
          tree%nodes(i)%name%raw(1:tree%nodes(i)%name%len))
        tf = match_field_in_udt(target_b%udts(target_u_idx), exist_udt%fields(ef))

        if (tf == 0_i32) then
          call extract_preserved_lead_trivia(tree, tree%nodes(i)%first_tok, pres_b, err)
          if (pres_b%len > 0_i32) then
            call cst_replace_node(tree, i, pres_b%raw(1:pres_b%len), &
              .false._lk, .false._lk, err)
          else
            call cst_mark_deleted(tree, i)
          end if
        else
          matched_t(tf) = .true._lk
          last_kept_node = i
          if (.not. fields_semantically_same(exist_udt%fields(ef), &
              target_b%udts(target_u_idx)%fields(tf), caps)) then
            call build_modified_field_replacement(tree, tree%nodes(i)%first_tok, target_b, &
              target_b%udts(target_u_idx)%fields(tf), sid, caps, form, is_dec, up_kw, use_dcol, &
              nl_seq, mod_b, err)
            call cst_replace_node(tree, i, mod_b%raw(1:mod_b%len), .false._lk, .true._lk, err)
          end if
        end if
      end if
    end do

    call buf_init(add_b)
    do tf = 1_i32, target_b%udts(target_u_idx)%field_count
      if (.not. matched_t(tf)) then
        call append_synthesized_field(target_b, target_b%udts(target_u_idx)%fields(tf), &
          sid, caps, form, is_dec, ind_str(1:ind_len), up_kw, use_dcol, nl_seq, add_b, err)
      end if
    end do

    if (add_b%len > 0_i32) then
      if (last_kept_node > 0_i32) then
        call cst_insert_after(tree, last_kept_node, add_b%raw(1:add_b%len), err)
      else
        hdr_end_tok = find_stmt_end_from(tree, tree%nodes(udt_node)%first_tok)
        call cst_add_node(tree, CST_OPAQUE_STMT, udt_node, hdr_end_tok, hdr_end_tok, anchor, err)
        call cst_insert_after(tree, anchor, add_b%raw(1:add_b%len), err)
      end if
    end if
  end subroutine patch_structured_udt_node

  !> @brief Surgically patch legacy flat / `COMMON` UDTs (`FORTRAN I`..`FORTRAN 77` non-DEC).
  !> @param[inout] tree         CST tree.
  !> @param[in]    exist_udt    Extracted existing UDT IR.
  !> @param[in]    target_b     Target IR bundle.
  !> @param[in]    target_u_idx Target UDT index.
  !> @param[in]    options      Dialect options.
  !> @param[in]    sid          Standard ID.
  !> @param[in]    caps         Dialect capabilities.
  !> @param[in]    nl_seq       File newline sequence (`LF` or `CRLF`).
  !> @param[inout] err          Error status.
  pure subroutine patch_legacy_flat_udt(tree, exist_udt, target_b, target_u_idx, options, &
      sid, caps, nl_seq, err)
    type(cdd_cst_tree_t), intent(inout) :: tree
    type(cdd_ir_udt_t), intent(in) :: exist_udt
    type(cdd_ir_schema_bundle_t), intent(in) :: target_b
    integer(i32), intent(in) :: target_u_idx
    type(cdd_options_t), intent(in) :: options
    integer(i32), intent(in) :: sid
    type(cdd_dialect_caps_t), intent(in) :: caps
    character(len=*, kind=c_k), intent(in) :: nl_seq
    type(cdd_error_t), intent(inout) :: err
    logical(lk) :: matched_t(CDD_MAX_FIELDS), changed
    integer(i32) :: i, ef, tf, last_kept_decl, first_com_node, first_udt_node
    type(cdd_buffer_t) :: pres_b, mod_b, add_b, com_b, raw_b, norm_b

    if (udt_all_fields_unchanged(exist_udt, target_b%udts(target_u_idx), caps)) return

    if (sid <= STD_FORTRAN_III) then
      first_udt_node = 0_i32
      do i = 2_i32, tree%node_count
        if (tree%nodes(i)%parent == 1_i32 .and. &
            (tree%nodes(i)%kind == CST_DIMENSION_STMT .or. &
             tree%nodes(i)%kind == CST_EQUIVALENCE_STMT .or. &
             tree%nodes(i)%kind == CST_COMMON_BLOCK)) then
          if (.not. ftok_norm_eq(tree%stream, tree%nodes(i)%first_tok + 1_i32, "JBUF")) then
            if (first_udt_node == 0_i32) then
              first_udt_node = i
            else
              call cst_mark_deleted(tree, i)
            end if
          end if
        end if
      end do
      call emit_udt_fortran(target_b, options, raw_b, err)
      call convert_lf_buf(raw_b%raw(1:raw_b%len), nl_seq, norm_b, err)
      if (first_udt_node > 0_i32) then
        call extract_preserved_lead_trivia(tree, tree%nodes(first_udt_node)%first_tok, &
          pres_b, err)
        call buf_append_str(pres_b, norm_b%raw(1:norm_b%len), err)
        call cst_replace_node(tree, first_udt_node, pres_b%raw(1:pres_b%len), &
          .false._lk, .false._lk, err)
      else
        call insert_before_tok(tree, 1_i32, norm_b%raw(1:norm_b%len), err)
      end if
      return
    end if

    matched_t = .false._lk
    changed = .false._lk
    last_kept_decl = 0_i32
    first_com_node = 0_i32

    do i = 2_i32, tree%node_count
      if (tree%nodes(i)%parent == 1_i32 .and. tree%nodes(i)%kind == CST_FIELD_DECL) then
        ef = find_field_by_cst_name(exist_udt, &
          tree%nodes(i)%name%raw(1:tree%nodes(i)%name%len))
        tf = match_field_in_udt(target_b%udts(target_u_idx), exist_udt%fields(ef))
        if (tf == 0_i32) then
          changed = .true._lk
          call extract_preserved_lead_trivia(tree, tree%nodes(i)%first_tok, pres_b, err)
          if (pres_b%len > 0_i32) then
            call cst_replace_node(tree, i, pres_b%raw(1:pres_b%len), &
              .false._lk, .false._lk, err)
          else
            call cst_mark_deleted(tree, i)
          end if
        else
          matched_t(tf) = .true._lk
          last_kept_decl = i
          if (.not. fields_semantically_same(exist_udt%fields(ef), &
              target_b%udts(target_u_idx)%fields(tf), caps)) then
            call build_modified_field_replacement(tree, tree%nodes(i)%first_tok, target_b, &
              target_b%udts(target_u_idx)%fields(tf), sid, caps, FORM_FIXED, .false._lk, &
              .true._lk, .false._lk, nl_seq, mod_b, err)
            call cst_replace_node(tree, i, mod_b%raw(1:mod_b%len), .false._lk, .true._lk, err)
          end if
        end if
      else if (tree%nodes(i)%parent == 1_i32 .and. &
               (tree%nodes(i)%kind == CST_COMMON_BLOCK .or. &
                tree%nodes(i)%kind == CST_SAVE_STMT)) then
        if (first_com_node == 0_i32) first_com_node = i
      end if
    end do

    call buf_init(add_b)
    do tf = 1_i32, target_b%udts(target_u_idx)%field_count
      if (.not. matched_t(tf)) then
        changed = .true._lk
        call append_synthesized_field(target_b, target_b%udts(target_u_idx)%fields(tf), &
          sid, caps, FORM_FIXED, .false._lk, "      ", .true._lk, .false._lk, nl_seq, add_b, err)
      end if
    end do

    if (add_b%len > 0_i32) then
      if (last_kept_decl > 0_i32) then
        call cst_insert_after(tree, last_kept_decl, add_b%raw(1:add_b%len), err)
      else
        call cst_insert_before(tree, first_com_node, add_b%raw(1:add_b%len), err)
      end if
    end if

    if (changed .and. first_com_node > 0_i32) then
      call rebuild_legacy_common_nodes(tree, target_b%udts(target_u_idx), sid, nl_seq, &
        com_b, err)
    end if
  end subroutine patch_legacy_flat_udt

  !> @brief Update `COMMON` (and F77 `SAVE`) CST nodes in place when legacy field membership changed.
  !> @param[inout] tree   CST tree.
  !> @param[in]    udt    Updated target UDT.
  !> @param[in]    sid    Standard ID.
  !> @param[in]    nl_seq File newline sequence.
  !> @param[out]   com_b  Formatted COMMON replacement buffer.
  !> @param[inout] err    Error status.
  pure subroutine rebuild_legacy_common_nodes(tree, udt, sid, nl_seq, com_b, err)
    type(cdd_cst_tree_t), intent(inout) :: tree
    type(cdd_ir_udt_t), intent(in) :: udt
    integer(i32), intent(in) :: sid
    character(len=*, kind=c_k), intent(in) :: nl_seq
    type(cdd_buffer_t), intent(out) :: com_b
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: i, first_com, bn_len, bc_len, k
    logical(lk) :: is_f77, has_chr, has_num, split_cc
    character(len=6, kind=c_k) :: blk_num, blk_chr
    type(cdd_buffer_t) :: raw_b, body_b

    call buf_init(raw_b)
    is_f77 = (sid == STD_FORTRAN_77)
    has_chr = .false._lk
    has_num = .false._lk
    do k = 1_i32, udt%field_count
      if (is_f77 .and. (udt%fields(k)%type_kind == IR_STRING_FIXED .or. &
                        udt%fields(k)%type_kind == IR_STRING_DEFERRED .or. &
                        udt%fields(k)%type_kind == IR_ENUM)) then
        has_chr = .true._lk
      else
        has_num = .true._lk
      end if
    end do
    split_cc = is_f77 .and. has_chr .and. has_num
    call make_common_names(udt%fortran_name%raw(1:udt%fortran_name%len), split_cc, &
      blk_num, bn_len, blk_chr, bc_len)

    if (split_cc) then
      call emit_common_line(udt, blk_num(1:bn_len), .true._lk, .false._lk, raw_b, err)
      call emit_common_line(udt, blk_chr(1:bc_len), .true._lk, .true._lk, raw_b, err)
      call buf_append_str(raw_b, "      SAVE /" // blk_num(1:bn_len) // "/, /" // &
        blk_chr(1:bc_len) // "/" // achar(10, kind=c_k), err)
    else if (udt%field_count > 0_i32) then
      call emit_common_line(udt, blk_num(1:bn_len), is_f77, .false._lk, raw_b, err)
      if (is_f77) then
        call buf_append_str(raw_b, "      SAVE /" // blk_num(1:bn_len) // "/" // &
          achar(10, kind=c_k), err)
      end if
    end if

    first_com = 0_i32
    do i = 2_i32, tree%node_count
      if (tree%nodes(i)%parent == 1_i32 .and. &
          (tree%nodes(i)%kind == CST_COMMON_BLOCK .or. &
           tree%nodes(i)%kind == CST_SAVE_STMT)) then
        if (first_com == 0_i32) then
          first_com = i
        else
          call cst_mark_deleted(tree, i)
        end if
      end if
    end do

    call buf_init(com_b)
    if (raw_b%len == 0_i32) then
      call cst_mark_deleted(tree, first_com)
    else
      call strip_proc_outer_wrapper(raw_b%raw(1:raw_b%len), .false._lk, nl_seq, body_b, err)
      com_b = body_b
      call cst_replace_node(tree, first_com, com_b%raw(1:com_b%len), &
        .true._lk, .true._lk, err)
    end if
  end subroutine rebuild_legacy_common_nodes

  !> @brief Surgically update `<udt>_to_json` and `<udt>_from_json` while keeping outer trivia!
  !> @param[inout] tree    CST tree.
  !> @param[in]    bundle  Updated target IR bundle.
  !> @param[in]    udt_idx Main UDT index in `bundle`.
  !> @param[in]    options Dialect options.
  !> @param[in]    sid     Standard ID.
  !> @param[in]    caps    Dialect capabilities.
  !> @param[in]    nl_seq  File newline sequence (`LF` or `CRLF`).
  !> @param[inout] err     Error status.
  pure subroutine patch_companion_codecs(tree, bundle, udt_idx, options, sid, caps, nl_seq, err)
    type(cdd_cst_tree_t), intent(inout) :: tree
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    integer(i32), intent(in) :: udt_idx
    type(cdd_options_t), intent(in) :: options
    integer(i32), intent(in) :: sid
    type(cdd_dialect_caps_t), intent(in) :: caps
    character(len=*, kind=c_k), intent(in) :: nl_seq
    type(cdd_error_t), intent(inout) :: err
    character(len=64, kind=c_k) :: to_nm, fr_nm
    integer(i32) :: to_len, fr_len, to_node, fr_node, mod_node, k, end_mod_tok, cont_tok, anchor
    type(cdd_buffer_t) :: raw_to, raw_fr, strip_to, strip_fr, ins_b, norm_b

    call codec_routine_names( &
      bundle%udts(udt_idx)%fortran_name%raw(1:bundle%udts(udt_idx)%fortran_name%len), &
      sid, caps%has_dec_structure, to_nm, to_len, fr_nm, fr_len)

    to_node = find_proc_cst_node(tree, to_nm(1:to_len))
    fr_node = find_proc_cst_node(tree, fr_nm(1:fr_len))

    call buf_init(raw_to)
    call buf_init(raw_fr)
    if (sid <= STD_FORTRAN_77) then
      call emit_legacy_to_json_proc(bundle, udt_idx, options, raw_to, err)
      call emit_legacy_from_json_proc(bundle, udt_idx, options, raw_fr, err)
    else
      call emit_modern_to_json_proc(bundle, udt_idx, options, .false._lk, raw_to, err)
      call emit_modern_from_json_proc(bundle, udt_idx, options, .false._lk, raw_fr, err)
    end if

    if (to_node > 0_i32) then
      call strip_proc_outer_wrapper(raw_to%raw(1:raw_to%len), (sid == STD_FORTRAN_I), &
        nl_seq, strip_to, err)
      call cst_replace_node(tree, to_node, strip_to%raw(1:strip_to%len), &
        .true._lk, .true._lk, err)
    end if

    if (fr_node > 0_i32) then
      call strip_proc_outer_wrapper(raw_fr%raw(1:raw_fr%len), (sid == STD_FORTRAN_I), &
        nl_seq, strip_fr, err)
      call cst_replace_node(tree, fr_node, strip_fr%raw(1:strip_fr%len), &
        .true._lk, .true._lk, err)
    end if

    if (to_node == 0_i32 .and. fr_node == 0_i32) then
      call buf_init(ins_b)
      mod_node = 0_i32
      do k = 1_i32, tree%node_count
        if (tree%nodes(k)%kind == CST_MODULE) then
          mod_node = k
          exit
        end if
      end do
      if (mod_node > 0_i32) then
        cont_tok = find_contains_tok_in_node(tree, mod_node)
        end_mod_tok = find_stmt_start_of_last_tok(tree, tree%nodes(mod_node)%last_tok)
        if (cont_tok == 0_i32) then
          call buf_append_str(ins_b, "contains" // achar(10, kind=c_k), err)
        end if
        call buf_append_str(ins_b, raw_to%raw(1:raw_to%len), err)
        call buf_append_str(ins_b, raw_fr%raw(1:raw_fr%len), err)
        call convert_lf_buf(ins_b%raw(1:ins_b%len), nl_seq, norm_b, err)
        call insert_before_tok(tree, end_mod_tok, norm_b%raw(1:norm_b%len), err)
      else if (sid > STD_FORTRAN_I) then
        call buf_append_str(ins_b, raw_to%raw(1:raw_to%len), err)
        call buf_append_str(ins_b, raw_fr%raw(1:raw_fr%len), err)
        call convert_lf_buf(ins_b%raw(1:ins_b%len), nl_seq, norm_b, err)
        call cst_add_node(tree, CST_OPAQUE_STMT, 1_i32, tree%stream%token_count, &
          tree%stream%token_count, anchor, err)
        call cst_insert_after(tree, anchor, norm_b%raw(1:norm_b%len), err)
      end if
    end if
  end subroutine patch_companion_codecs

  !> @brief Find a `CST_SUBROUTINE`, `CST_FUNCTION`, or `CST_ASSIGNED_GOTO_BLOCK` by name.
  !> @param[in] tree    CST tree.
  !> @param[in] proc_nm Target procedure name.
  !> @return CST node index or 0.
  pure function find_proc_cst_node(tree, proc_nm) result(nidx)
    type(cdd_cst_tree_t), intent(in) :: tree
    character(len=*, kind=c_k), intent(in) :: proc_nm
    integer(i32) :: nidx
    integer(i32) :: i

    nidx = 0_i32
    do i = 2_i32, tree%node_count
      if (tree%nodes(i)%kind == CST_SUBROUTINE .or. &
          tree%nodes(i)%kind == CST_FUNCTION .or. &
          tree%nodes(i)%kind == CST_ASSIGNED_GOTO_BLOCK) then
        if (str_eq_nocase(tree%nodes(i)%name%raw(1:tree%nodes(i)%name%len), proc_nm)) then
          nidx = i
          return
        end if
      end if
    end do
  end function find_proc_cst_node

  !> @brief Strip leading Doxygen/banner comments + indent and trailing newline from generated proc.
  !> @param[in]  raw_proc    Generated procedure text (with LF line endings).
  !> @param[in]  is_f57_goto True when `C @@cdd-end-routine:` footer should also be stripped.
  !> @param[in]  nl_seq      Target file newline sequence (`LF` or `CRLF`).
  !> @param[out] out_buf     Stripped interior body ready for `keep_lead=.true., keep_trail=.true.`.
  !> @param[inout] err       Error status.
  pure subroutine strip_proc_outer_wrapper(raw_proc, is_f57_goto, nl_seq, out_buf, err)
    character(len=*, kind=c_k), intent(in) :: raw_proc
    logical(lk), intent(in) :: is_f57_goto
    character(len=*, kind=c_k), intent(in) :: nl_seq
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: n, pos, ls, le, first_ns, end_limit
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call buf_init(out_buf)
    n = len(raw_proc, kind=i32)
    pos = 1_i32

    ! Skip leading comment lines (! or col-1 C) and find first code token on first code line
    do
      ls = pos
      le = ls
      do while (le <= n .and. raw_proc(min(n, le):min(n, le)) /= LF)
        le = le + 1_i32
      end do
      first_ns = ls
      do while (first_ns < le .and. &
                (raw_proc(min(n, first_ns):min(n, first_ns)) == " " .or. &
                 raw_proc(min(n, first_ns):min(n, first_ns)) == achar(9, kind=c_k)))
        first_ns = first_ns + 1_i32
      end do
      if ((ls < le .and. (raw_proc(min(n, ls):min(n, ls)) == "C" .or. &
                          raw_proc(min(n, ls):min(n, ls)) == "c")) .or. &
          (first_ns < le .and. raw_proc(min(n, first_ns):min(n, first_ns)) == "!")) then
        pos = le + 1_i32
      else
        pos = first_ns
        exit
      end if
    end do

    end_limit = n - 1_i32
    if (is_f57_goto) then
      do while (raw_proc(end_limit:end_limit) /= LF)
        end_limit = end_limit - 1_i32
      end do
      end_limit = end_limit - 1_i32
    end if

    call convert_lf_buf(raw_proc(pos:end_limit), nl_seq, out_buf, err)
  end subroutine strip_proc_outer_wrapper

  !> @brief Detect indentation string, keyword casing, and `::` style of an existing UDT CST node.
  !> @param[in]  tree     CST tree.
  !> @param[in]  udt_node CST_DERIVED_TYPE or CST_DEC_STRUCTURE node index.
  !> @param[in]  options  Fallback options.
  !> @param[out] ind_str  Detected indentation whitespace string (spaces or tabs).
  !> @param[out] ind_len  Length of `ind_str`.
  !> @param[out] up_kw    Detected uppercase-keyword preference.
  !> @param[out] use_dcol Detected `::` double-colon preference.
  pure subroutine detect_udt_member_style(tree, udt_node, options, ind_str, ind_len, up_kw, &
      use_dcol)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: udt_node
    type(cdd_options_t), intent(in) :: options
    character(len=64, kind=c_k), intent(out) :: ind_str
    integer(i32), intent(out) :: ind_len
    logical(lk), intent(out) :: up_kw
    logical(lk), intent(out) :: use_dcol
    integer(i32) :: i, ft, lt, t, tr_last, off, slen, k, u_ft
    character(len=1, kind=c_k) :: ch

    ind_str = " "
    ind_len = max(2_i32, options%indent_width) * 2_i32
    up_kw = options%uppercase_keywords
    use_dcol = .true._lk

    u_ft = tree%nodes(udt_node)%first_tok
    ch = tree%stream%source_buf%raw( &
      tree%stream%tokens(u_ft)%raw_off:tree%stream%tokens(u_ft)%raw_off)
    up_kw = (ch >= "A" .and. ch <= "Z")
    if (tree%stream%tokens(u_ft)%lead_count > 0_i32) then
      tr_last = tree%stream%tokens(u_ft)%lead_first + tree%stream%tokens(u_ft)%lead_count - 1_i32
      if (tree%stream%trivia(tr_last)%kind == TRIVIA_WHITESPACE) then
        slen = min(56_i32, tree%stream%trivia(tr_last)%src_len)
        off = tree%stream%trivia(tr_last)%src_off
        do k = 1_i32, slen
          ind_str(k:k) = tree%stream%source_buf%raw(off + k - 1_i32:off + k - 1_i32)
        end do
        do k = 1_i32, max(2_i32, options%indent_width)
          ind_str(slen + k:slen + k) = " "
        end do
        ind_len = slen + max(2_i32, options%indent_width)
      end if
    end if

    do i = 2_i32, tree%node_count
      if (tree%nodes(i)%parent == udt_node .and. tree%nodes(i)%kind == CST_FIELD_DECL) then
        ft = tree%nodes(i)%first_tok
        lt = tree%nodes(i)%last_tok
        ch = tree%stream%source_buf%raw( &
          tree%stream%tokens(ft)%raw_off:tree%stream%tokens(ft)%raw_off)
        up_kw = (ch >= "A" .and. ch <= "Z")
        use_dcol = .false._lk
        do t = ft, lt
          if (tree%stream%tokens(t)%kind == FTOK_DCOLON) use_dcol = .true._lk
        end do
        if (tree%stream%tokens(ft)%lead_count > 0_i32) then
          tr_last = tree%stream%tokens(ft)%lead_first + tree%stream%tokens(ft)%lead_count - 1_i32
          if (tree%stream%trivia(tr_last)%kind == TRIVIA_WHITESPACE) then
            ind_len = min(64_i32, tree%stream%trivia(tr_last)%src_len)
            off = tree%stream%trivia(tr_last)%src_off
            do k = 1_i32, ind_len
              ind_str(k:k) = tree%stream%source_buf%raw(off + k - 1_i32:off + k - 1_i32)
            end do
          end if
        end if
        return
      end if
    end do
  end subroutine detect_udt_member_style

  !> @brief Preserve blank lines, `#ifdef` directives, and banner comments above a deleted field!
  !> @param[in]  tree      CST tree.
  !> @param[in]  first_tok First token of the removed `CST_FIELD_DECL`.
  !> @param[out] pres_buf  Preserved non-member leading trivia bytes.
  !> @param[inout] err     Error status.
  pure subroutine extract_preserved_lead_trivia(tree, first_tok, pres_buf, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: first_tok
    type(cdd_buffer_t), intent(out) :: pres_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: tf, tc, nl_idx(0:CDD_MAX_TRIVIA), nl_cnt, k, ln_s, ln_e, keep_end_tr
    integer(i32) :: p, off, slen
    logical(lk) :: is_member_doc

    call buf_init(pres_buf)
    tf = tree%stream%tokens(first_tok)%lead_first
    tc = tree%stream%tokens(first_tok)%lead_count
    if (tc <= 0_i32) return

    nl_idx(0) = tf - 1_i32
    nl_cnt = 0_i32
    do k = tf, tf + tc - 1_i32
      if (tree%stream%trivia(k)%kind == TRIVIA_NEWLINE) then
        nl_cnt = nl_cnt + 1_i32
        nl_idx(nl_cnt) = k
      end if
    end do
    if (nl_cnt == 0_i32) return

    keep_end_tr = nl_idx(nl_cnt)
    do k = nl_cnt, 1_i32, -1_i32
      ln_s = nl_idx(k - 1_i32) + 1_i32
      ln_e = nl_idx(k)
      is_member_doc = .false._lk
      do p = ln_s, ln_e - 1_i32
        off = tree%stream%trivia(p)%src_off
        slen = tree%stream%trivia(p)%src_len
        if (tree%stream%trivia(p)%kind == TRIVIA_DOXYGEN_COMMENT) then
          is_member_doc = .true._lk
        else if (tree%stream%trivia(p)%kind == TRIVIA_LINE_COMMENT) then
          if (index(tree%stream%source_buf%raw(off:off + slen - 1_i32), "@cdd-field:") > 0 .or. &
              index(tree%stream%source_buf%raw(off:off + slen - 1_i32), "@cdd-udt:") > 0) then
            is_member_doc = .true._lk
          end if
        end if
      end do
      if (is_member_doc) then
        keep_end_tr = nl_idx(k - 1_i32)
      else
        exit
      end if
    end do

    do p = tf, keep_end_tr
      off = tree%stream%trivia(p)%src_off
      slen = tree%stream%trivia(p)%src_len
      call buf_append_str(pres_buf, tree%stream%source_buf%raw(off:off + slen - 1_i32), err)
    end do
  end subroutine extract_preserved_lead_trivia

  !> @brief Build replacement text for a type/bound-modified field keeping lead & inline trivia!
  !> @param[in]  tree      CST tree.
  !> @param[in]  first_tok First token of the modified `CST_FIELD_DECL`.
  !> @param[in]  bundle    Target IR bundle.
  !> @param[in]  fld       Target updated IR field.
  !> @param[in]  sid       Standard ID.
  !> @param[in]  caps      Dialect capabilities.
  !> @param[in]  form      Source form.
  !> @param[in]  is_dec    True for DEC STRUCTURE member.
  !> @param[in]  up_kw     Uppercase keyword flag.
  !> @param[in]  use_dcol  Double-colon flag.
  !> @param[in]  nl_seq    Newline sequence (unused for trailing; used if `@cdd-field` updated).
  !> @param[out] out_buf   Replacement bytes (`keep_lead=.false., keep_trail=.true.`).
  !> @param[inout] err     Error status.
  pure subroutine build_modified_field_replacement(tree, first_tok, bundle, fld, sid, caps, &
      form, is_dec, up_kw, use_dcol, nl_seq, out_buf, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: first_tok
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_field_t), intent(in) :: fld
    integer(i32), intent(in) :: sid
    type(cdd_dialect_caps_t), intent(in) :: caps
    integer(i32), intent(in) :: form
    logical(lk), intent(in) :: is_dec
    logical(lk), intent(in) :: up_kw
    logical(lk), intent(in) :: use_dcol
    character(len=*, kind=c_k), intent(in) :: nl_seq
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: tf, tc, p, off, slen, doc_pos
    type(cdd_buffer_t) :: cbuf, stmt_b

    associate (unused_nl => nl_seq)
    end associate
    call buf_init(out_buf)
    tf = tree%stream%tokens(first_tok)%lead_first
    tc = tree%stream%tokens(first_tok)%lead_count
    do p = tf, tf + tc - 1_i32
      off = tree%stream%trivia(p)%src_off
      slen = tree%stream%trivia(p)%src_len
      if (tree%stream%trivia(p)%kind == TRIVIA_LINE_COMMENT .and. &
          index(tree%stream%source_buf%raw(off:off + slen - 1_i32), "@cdd-field:") > 0) then
        if (form == FORM_FIXED) then
          call format_cdd_field_comment("C", fld, cbuf, err)
          call buf_append_str(out_buf, cbuf%raw(1:min(72_i32, cbuf%len)), err)
        else
          call format_cdd_field_comment("!", fld, cbuf, err)
          call buf_append_str(out_buf, cbuf%raw(1:cbuf%len), err)
        end if
      else
        call buf_append_str(out_buf, tree%stream%source_buf%raw(off:off + slen - 1_i32), err)
      end if
    end do

    call buf_init(stmt_b)
    if (form == FORM_FIXED) then
      call emit_fixed_field_stmt_body(bundle, fld, sid, is_dec, stmt_b, err)
      call buf_append_str(out_buf, stmt_b%raw(1:stmt_b%len), err)
    else
      call emit_single_modern_field_line(bundle, fld, sid, caps, 0_i32, up_kw, use_dcol, &
        "", stmt_b, err)
      doc_pos = index(stmt_b%raw(1:stmt_b%len), " !<")
      call buf_append_str(out_buf, stmt_b%raw(1:doc_pos - 1_i32), err)
    end if
  end subroutine build_modified_field_replacement

  !> @brief Append a newly added field (with `@@cdd-field:` metadata and detected style) to `out_buf`.
  !> @param[in]    bundle   Target IR bundle.
  !> @param[in]    fld      Newly added field descriptor.
  !> @param[in]    sid      Standard ID.
  !> @param[in]    caps     Dialect capabilities.
  !> @param[in]    form     Effective source form.
  !> @param[in]    is_dec   True for DEC STRUCTURE member.
  !> @param[in]    ind_str  Detected indentation whitespace string.
  !> @param[in]    up_kw    Uppercase keyword preference.
  !> @param[in]    use_dcol Double-colon preference.
  !> @param[in]    nl_seq   File newline sequence (`LF` or `CRLF`).
  !> @param[inout] out_buf  Destination buffer.
  !> @param[inout] err      Error status.
  pure subroutine append_synthesized_field(bundle, fld, sid, caps, form, is_dec, ind_str, &
      up_kw, use_dcol, nl_seq, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_field_t), intent(in) :: fld
    integer(i32), intent(in) :: sid
    type(cdd_dialect_caps_t), intent(in) :: caps
    integer(i32), intent(in) :: form
    logical(lk), intent(in) :: is_dec
    character(len=*, kind=c_k), intent(in) :: ind_str
    logical(lk), intent(in) :: up_kw
    logical(lk), intent(in) :: use_dcol
    character(len=*, kind=c_k), intent(in) :: nl_seq
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    type(cdd_buffer_t) :: cbuf, stmt_b

    if (form == FORM_FIXED) then
      call format_cdd_field_comment("C", fld, cbuf, err)
      call buf_append_str(out_buf, cbuf%raw(1:min(72_i32, cbuf%len)) // nl_seq, err)
      if (is_dec) then
        call buf_append_str(out_buf, "        ", err)
      else
        call buf_append_str(out_buf, "      ", err)
      end if
      call buf_init(stmt_b)
      call emit_fixed_field_stmt_body(bundle, fld, sid, is_dec, stmt_b, err)
      call buf_append_str(out_buf, stmt_b%raw(1:stmt_b%len) // nl_seq, err)
    else
      call format_cdd_field_comment("!", fld, cbuf, err)
      call buf_append_str(out_buf, ind_str // cbuf%raw(1:cbuf%len) // nl_seq, err)
      call buf_append_str(out_buf, ind_str, err)
      call emit_single_modern_field_line(bundle, fld, sid, caps, 0_i32, up_kw, use_dcol, &
        nl_seq, out_buf, err)
    end if
  end subroutine append_synthesized_field

  !> @brief Emit a fixed-form field statement body (without leading spaces or trailing newline).
  !> @param[in]    bundle  Target IR bundle.
  !> @param[in]    fld     Field descriptor.
  !> @param[in]    sid     Standard ID.
  !> @param[in]    is_dec  True for DEC STRUCTURE member (`INTEGER*4` / `RECORD /.../`).
  !> @param[inout] out_buf Target buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_fixed_field_stmt_body(bundle, fld, sid, is_dec, out_buf, err)
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_field_t), intent(in) :: fld
    integer(i32), intent(in) :: sid
    logical(lk), intent(in) :: is_dec
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: r, r_idx, r_len, nwords

    r_len = fld%ref_udt_name%len
    r_idx = 0_i32
    if (r_len > 0_i32) r_idx = ir_bundle_find_udt(bundle, fld%ref_udt_name%raw(1:r_len))

    if (is_dec) then
      select case (fld%type_kind)
      case (IR_INT8, IR_INT16)
        call buf_append_str(out_buf, "INTEGER*2 ", err)
      case (IR_INT64)
        call buf_append_str(out_buf, "INTEGER*8 ", err)
      case (IR_REAL32)
        call buf_append_str(out_buf, "REAL*4 ", err)
      case (IR_REAL64, IR_REAL128)
        call buf_append_str(out_buf, "REAL*8 ", err)
      case (IR_BOOL)
        call buf_append_str(out_buf, "LOGICAL*4 ", err)
      case (IR_STRING_FIXED, IR_STRING_DEFERRED, IR_ENUM)
        call buf_append_str(out_buf, "CHARACTER*", err)
        call buf_append_i64(out_buf, int(max(1_i32, fld%char_len), i64), err)
        call buf_append_char(out_buf, " ", err)
      case (IR_NESTED_UDT)
        call buf_append_str(out_buf, "RECORD /", err)
        call buf_append_str(out_buf, &
          bundle%udts(r_idx)%fortran_name%raw(1:bundle%udts(r_idx)%fortran_name%len), err)
        call buf_append_str(out_buf, "/ ", err)
      case default
        call buf_append_str(out_buf, "INTEGER*4 ", err)
      end select
    else if (sid == STD_FORTRAN_77 .and. (fld%type_kind == IR_STRING_FIXED .or. &
             fld%type_kind == IR_STRING_DEFERRED .or. fld%type_kind == IR_ENUM)) then
      call buf_append_str(out_buf, "CHARACTER*", err)
      call buf_append_i64(out_buf, int(max(1_i32, fld%char_len), i64), err)
      call buf_append_char(out_buf, " ", err)
    else
      select case (fld%type_kind)
      case (IR_REAL32)
        call buf_append_str(out_buf, "REAL ", err)
      case (IR_REAL64, IR_REAL128)
        call buf_append_str(out_buf, "DOUBLE PRECISION ", err)
      case (IR_BOOL)
        call buf_append_str(out_buf, "LOGICAL ", err)
      case default
        call buf_append_str(out_buf, "INTEGER ", err)
      end select
    end if

    call buf_append_str(out_buf, fld%fortran_name%raw(1:fld%fortran_name%len), err)
    if (sid < STD_FORTRAN_77 .and. .not. is_dec .and. &
        (fld%type_kind == IR_STRING_FIXED .or. fld%type_kind == IR_STRING_DEFERRED)) then
      nwords = max(1_i32, (max(4_i32, fld%char_len) + 3_i32) / 4_i32)
      call buf_append_char(out_buf, "(", err)
      call buf_append_i64(out_buf, int(nwords, i64), err)
      call buf_append_char(out_buf, ")", err)
    else if (fld%rank > 0_i32) then
      call buf_append_char(out_buf, "(", err)
      do r = 1_i32, fld%rank
        if (r > 1_i32) call buf_append_char(out_buf, ",", err)
        call buf_append_i64(out_buf, int(max(1_i32, fld%dims(r)%upper_bound), i64), err)
      end do
      call buf_append_char(out_buf, ")", err)
    end if
  end subroutine emit_fixed_field_stmt_body

  !> @brief Locate field index in `udt` matching `CST_FIELD_DECL` identifier `cst_nm`.
  !> @param[in] udt    UDT record.
  !> @param[in] cst_nm Declarator identifier from CST.
  !> @return 1-based field index in `udt`.
  pure function find_field_by_cst_name(udt, cst_nm) result(idx)
    type(cdd_ir_udt_t), intent(in) :: udt
    character(len=*, kind=c_k), intent(in) :: cst_nm
    integer(i32) :: idx
    integer(i32) :: k

    idx = udt%field_count
    do k = 1_i32, udt%field_count - 1_i32
      if (str_eq_nocase(udt%fields(k)%fortran_name%raw(1:udt%fields(k)%fortran_name%len), cst_nm)) then
        idx = k
        return
      end if
    end do
  end function find_field_by_cst_name

  !> @brief Check whether two fields have identical type, rank, bounds, string length, and storage.
  !> @param[in] a    Existing field.
  !> @param[in] b    Target field.
  !> @param[in] caps Dialect capabilities.
  !> @return True when original CST declaration can remain 100% untouched.
  pure function fields_semantically_same(a, b, caps) result(same)
    type(cdd_ir_field_t), intent(in) :: a
    type(cdd_ir_field_t), intent(in) :: b
    type(cdd_dialect_caps_t), intent(in) :: caps
    logical(lk) :: same
    integer(i32) :: r

    associate (unused_caps => caps)
    end associate
    same = .false._lk
    if (a%type_kind /= b%type_kind) return
    if (a%rank /= b%rank) return
    if (a%type_kind == IR_STRING_FIXED .and. a%char_len /= b%char_len) return
    do r = 1_i32, a%rank
      if (a%dims(r)%kind /= b%dims(r)%kind) return
      if (a%dims(r)%upper_bound /= b%dims(r)%upper_bound) return
    end do
    same = .true._lk
  end function fields_semantically_same

  !> @brief Check if an entire UDT has identical ordered field layout to `b`.
  !> @param[in] a    Existing UDT.
  !> @param[in] b    Target UDT.
  !> @param[in] caps Dialect capabilities.
  !> @return True when all fields match in count, order, names, types, and bounds.
  pure function udt_all_fields_unchanged(a, b, caps) result(same)
    type(cdd_ir_udt_t), intent(in) :: a
    type(cdd_ir_udt_t), intent(in) :: b
    type(cdd_dialect_caps_t), intent(in) :: caps
    logical(lk) :: same
    integer(i32) :: ka, kb

    same = .false._lk
    kb = 0_i32
    do ka = 1_i32, a%field_count
      if (str_eq_nocase(a%fields(ka)%fortran_name%raw(1:a%fields(ka)%fortran_name%len), &
                        "JBUF")) cycle
      kb = kb + 1_i32
      if (kb > b%field_count) return
      if (.not. str_eq(a%fields(ka)%json_name%raw(1:a%fields(ka)%json_name%len), &
                       b%fields(kb)%json_name%raw(1:b%fields(kb)%json_name%len))) return
      if (.not. fields_semantically_same(a%fields(ka), b%fields(kb), caps)) return
    end do
    if (kb /= b%field_count) return
    same = .true._lk
  end function udt_all_fields_unchanged

  !> @brief Find `CONTAINS` keyword token directly inside a `CST_MODULE` block (0 if absent).
  !> @param[in] tree     CST tree.
  !> @param[in] mod_node `CST_MODULE` node index.
  !> @return Token index of `CONTAINS` or 0.
  pure function find_contains_tok_in_node(tree, mod_node) result(tok_idx)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: mod_node
    integer(i32) :: tok_idx
    integer(i32) :: t, prev_end

    tok_idx = 0_i32
    do t = tree%nodes(mod_node)%first_tok + 1_i32, tree%nodes(mod_node)%last_tok
      prev_end = t - 1_i32
      if (tree%stream%tokens(prev_end)%stmt_end .and. &
          tree%stream%tokens(t)%stmt_end .and. &
          ftok_norm_eq(tree%stream, t, "CONTAINS")) then
        if (.not. is_inside_child_type(tree, mod_node, t)) then
          tok_idx = t
          return
        end if
      end if
    end do
  end function find_contains_tok_in_node

  !> @brief Check whether token `t` lies inside a child `CST_DERIVED_TYPE` of `mod_node`.
  !> @param[in] tree     CST tree.
  !> @param[in] mod_node Enclosing module node index.
  !> @param[in] t        Token index to test.
  !> @return True if `t` is inside a derived type (e.g. TBP `CONTAINS`).
  pure function is_inside_child_type(tree, mod_node, t) result(inside)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: mod_node
    integer(i32), intent(in) :: t
    logical(lk) :: inside
    integer(i32) :: i

    inside = .false._lk
    do i = 2_i32, tree%node_count
      if (tree%nodes(i)%parent == mod_node .and. tree%nodes(i)%kind == CST_DERIVED_TYPE) then
        if (t >= tree%nodes(i)%first_tok .and. t <= tree%nodes(i)%last_tok) then
          inside = .true._lk
          return
        end if
      end if
    end do
  end function is_inside_child_type

  !> @brief Find the first token of the final `END ...` statement ending at `end_last_tok`.
  !> @param[in] tree         CST tree.
  !> @param[in] end_last_tok Last token of a block's `END` statement.
  !> @return First token of that `END` statement.
  pure function find_stmt_start_of_last_tok(tree, end_last_tok) result(start_tok)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: end_last_tok
    integer(i32) :: start_tok

    start_tok = end_last_tok
    do while (.not. tree%stream%tokens(start_tok - 1_i32)%stmt_end)
      start_tok = start_tok - 1_i32
    end do
  end function find_stmt_start_of_last_tok

  !> @brief Find `stmt_end` token for the statement beginning at `first_tok`.
  !> @param[in] tree      CST tree.
  !> @param[in] first_tok Start token index.
  !> @return Statement-ending token index.
  pure function find_stmt_end_from(tree, first_tok) result(end_tok)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: first_tok
    integer(i32) :: end_tok

    end_tok = first_tok
    do while (.not. tree%stream%tokens(end_tok)%stmt_end)
      end_tok = end_tok + 1_i32
    end do
  end function find_stmt_end_from

  !> @brief Attach a `before` insertion at any arbitrary token `tok_idx` via an anchor node.
  !> @param[inout] tree     CST tree.
  !> @param[in]    tok_idx  Target token index.
  !> @param[in]    ins_text Text to insert before `tok_idx`.
  !> @param[inout] err      Error status.
  pure subroutine insert_before_tok(tree, tok_idx, ins_text, err)
    type(cdd_cst_tree_t), intent(inout) :: tree
    integer(i32), intent(in) :: tok_idx
    character(len=*, kind=c_k), intent(in) :: ins_text
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: anchor

    call cst_add_node(tree, CST_OPAQUE_STMT, 1_i32, tok_idx, tok_idx, anchor, err)
    call cst_insert_before(tree, anchor, ins_text, err)
  end subroutine insert_before_tok

  !> @brief Convert `LF` line terminators in `src` to `nl_seq` (`LF` or `CRLF`).
  !> @param[in]    src     Input string with `LF` terminators.
  !> @param[in]    nl_seq  Target newline sequence.
  !> @param[out]   out_buf Converted buffer.
  !> @param[inout] err     Error status.
  pure subroutine convert_lf_buf(src, nl_seq, out_buf, err)
    character(len=*, kind=c_k), intent(in) :: src
    character(len=*, kind=c_k), intent(in) :: nl_seq
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: k
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call buf_init(out_buf)
    if (len(nl_seq, kind=i32) == 1_i32) then
      call buf_append_str(out_buf, src, err)
      return
    end if
    do k = 1_i32, len(src, kind=i32)
      if (src(k:k) == LF) then
        call buf_append_str(out_buf, nl_seq, err)
      else
        call buf_append_char(out_buf, src(k:k), err)
      end if
    end do
  end subroutine convert_lf_buf

end module cdd_patch_udt
