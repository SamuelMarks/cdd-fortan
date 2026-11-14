!> @file cdd_extract_udt.f90
!> @brief Operation 1: Extract canonical IR schema bundles from CST across FORTRAN I..2023.

!> @brief Multi-standard Fortran UDT, STRUCTURE, ENUM, COMMON, and DIMENSION CST-to-IR extractor.
module cdd_extract_udt
  use cdd_kinds, only: i32, i64, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set_simple, CDD_ERR_UDT_NOT_FOUND
  use cdd_string, only: cdd_string_t, str_set, str_eq_nocase, starts_with, ends_with, &
    trim_bounds, parse_i64_strict
  use cdd_standard, only: cdd_options_t
  use cdd_ir_types, only: IR_INT32, IR_REAL32, IR_STRING_FIXED, &
    IR_ENUM, IR_NESTED_UDT, DIM_FIXED, cdd_ident_t, cdd_ir_field_t, cdd_ir_udt_t, &
    cdd_ir_schema_bundle_t, ident_set, ir_field_init, ir_bundle_init, &
    ir_bundle_add_udt, ir_bundle_find_udt, ir_udt_add_field, ir_udt_find_field, &
    ir_bundle_add_enum, ir_bundle_find_enum
  use cdd_ir_namemap, only: is_implicit_int_letter, parse_cdd_field_comment
  use cdd_cst_trivia, only: TRIVIA_DOXYGEN_COMMENT, TRIVIA_LINE_COMMENT
  use cdd_cst_tokens, only: FTOK_IDENT, FTOK_INT_LIT, FTOK_HOLLERITH, FTOK_LPAREN, &
    FTOK_RPAREN, FTOK_SLASH, ftok_norm_eq
  use cdd_cst_nodes, only: CST_MODULE, CST_DERIVED_TYPE, CST_DEC_STRUCTURE, CST_ENUM_BIND_C, &
    CST_ENUMERATION_TYPE, CST_COMMON_BLOCK, CST_DIMENSION_STMT, CST_DATA_STMT, CST_FIELD_DECL, &
    cdd_cst_tree_t
  use cdd_cst_parser, only: cst_parse
  implicit none (type, external)
  private

  public :: cdd_field_annot_t
  public :: extract_udt_from_source, extract_udt_to_ir

  integer(i32), parameter :: MAX_ANNOTS = 64_i32 !< Max @@cdd-field comment annotations per file.

  !> @brief Parsed `@@cdd-field:` annotation entry cached from CST trivia.
  type :: cdd_field_annot_t
    type(cdd_ident_t) :: f_name     !< Annotated Fortran field identifier.
    type(cdd_ident_t) :: j_name     !< Original JSON property key.
    integer(i32) :: type_kind = 0   !< Annotated canonical IR_* type kind.
    integer(i32) :: char_len = 0    !< Annotated character length.
  end type cdd_field_annot_t

contains

  !> @brief Parse Fortran source text and extract its canonical IR schema bundle.
  !> @param[in]  source  Input Fortran source code.
  !> @param[in]  options Dialect and target-UDT options.
  !> @param[out] bundle  Extracted canonical IR bundle.
  !> @param[out] err     Structured error status.
  pure subroutine extract_udt_from_source(source, options, bundle, err)
    character(len=*, kind=c_k), intent(in) :: source
    type(cdd_options_t), intent(in) :: options
    type(cdd_ir_schema_bundle_t), intent(out) :: bundle
    type(cdd_error_t), intent(out) :: err
    type(cdd_cst_tree_t) :: tree

    call ir_bundle_init(bundle)
    call cst_parse(source, options, tree, err)
    if (err%code /= 0_i32) return
    call extract_udt_to_ir(tree, options, bundle, err)
  end subroutine extract_udt_from_source

  !> @brief Extract canonical cdd_ir_schema_bundle_t from an already-parsed cdd_cst_tree_t.
  !> @param[in]  tree    Parsed lossless CST tree.
  !> @param[in]  options SDK options (including optional target_udt_name).
  !> @param[out] bundle  Extracted canonical IR bundle.
  !> @param[out] err     Structured error status (CDD_ERR_UDT_NOT_FOUND if absent).
  pure subroutine extract_udt_to_ir(tree, options, bundle, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    type(cdd_options_t), intent(in) :: options
    type(cdd_ir_schema_bundle_t), intent(out) :: bundle
    type(cdd_error_t), intent(out) :: err
    type(cdd_field_annot_t) :: annots(MAX_ANNOTS)
    integer(i32) :: annot_cnt, i, target_idx

    call ir_bundle_init(bundle)
    call cdd_error_clear(err)
    call scan_all_field_annots(tree, annots, annot_cnt)

    do i = 1_i32, tree%node_count
      if (tree%nodes(i)%kind == CST_MODULE) then
        bundle%module_name = tree%nodes(i)%name
      else if (tree%nodes(i)%kind == CST_ENUMERATION_TYPE .or. &
               tree%nodes(i)%kind == CST_ENUM_BIND_C) then
        call extract_enum_from_cst(tree, i, bundle, err)
      end if
    end do

    do i = 1_i32, tree%node_count
      if (tree%nodes(i)%kind == CST_DERIVED_TYPE .or. &
          tree%nodes(i)%kind == CST_DEC_STRUCTURE) then
        call extract_structured_udt(tree, i, annots, annot_cnt, bundle, err)
      end if
    end do

    if (bundle%udt_count == 0_i32) then
      call extract_legacy_flat_udt(tree, annots, annot_cnt, bundle, err)
    end if

    if (bundle%udt_count == 0_i32) then
      call cdd_error_set_simple(err, CDD_ERR_UDT_NOT_FOUND, "No UDT, STRUCTURE, or COMMON found")
      return
    end if

    if (options%target_udt_name%len > 0_i32) then
      target_idx = ir_bundle_find_udt(bundle, &
        options%target_udt_name%raw(1:options%target_udt_name%len))
      if (target_idx == 0_i32) then
        call cdd_error_set_simple(err, CDD_ERR_UDT_NOT_FOUND, "Requested target UDT not found")
        return
      end if
      bundle%main_udt_idx = target_idx
    else
      bundle%main_udt_idx = bundle%udt_count
    end if
  end subroutine extract_udt_to_ir

  !> @brief Scan all trivia slices in tree%stream for `@@cdd-field:` annotations.
  !> @param[in]  tree      CST tree.
  !> @param[out] annots    Populated annotation array.
  !> @param[out] annot_cnt Active count in annots(1:annot_cnt).
  pure subroutine scan_all_field_annots(tree, annots, annot_cnt)
    type(cdd_cst_tree_t), intent(in) :: tree
    type(cdd_field_annot_t), intent(out) :: annots(MAX_ANNOTS)
    integer(i32), intent(out) :: annot_cnt
    integer(i32) :: k, off, slen, tk, clen
    type(cdd_ident_t) :: fn, jn
    logical(lk) :: matched

    annot_cnt = 0_i32
    do k = 1_i32, tree%stream%trivia_count
      if (tree%stream%trivia(k)%kind == TRIVIA_LINE_COMMENT .or. &
          tree%stream%trivia(k)%kind == TRIVIA_DOXYGEN_COMMENT) then
        off = tree%stream%trivia(k)%src_off
        slen = tree%stream%trivia(k)%src_len
        call parse_cdd_field_comment(tree%stream%source_buf%raw(off:off + slen - 1_i32), &
          fn, jn, tk, clen, matched)
        if (matched .and. annot_cnt < MAX_ANNOTS) then
          annot_cnt = annot_cnt + 1_i32
          annots(annot_cnt)%f_name = fn
          annots(annot_cnt)%j_name = jn
          annots(annot_cnt)%type_kind = tk
          annots(annot_cnt)%char_len = clen
        end if
      end if
    end do
  end subroutine scan_all_field_annots

  !> @brief Extract a CST_ENUMERATION_TYPE or CST_ENUM_BIND_C node into bundle%enums.
  !> @param[in]    tree   CST tree.
  !> @param[in]    enode  CST node index of enum block.
  !> @param[inout] bundle Target IR bundle.
  !> @param[inout] err    Error status.
  pure subroutine extract_enum_from_cst(tree, enode, bundle, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: enode
    type(cdd_ir_schema_bundle_t), intent(inout) :: bundle
    type(cdd_error_t), intent(inout) :: err
    character(len=64, kind=c_k) :: ename, inline_doc
    integer(i32) :: elen, eidx, c, vc, ilen

    call find_lead_tag_value(tree, tree%nodes(enode)%first_tok, "@cdd-enum:", ename, elen)
    if (elen == 0_i32 .and. tree%nodes(enode)%name%len > 0_i32) then
      call strip_type_suffix(tree%nodes(enode)%name, ename, elen)
    else if (elen == 0_i32) then
      ename(1:8) = "EnumType"
      elen = 8_i32
    end if

    call ir_bundle_add_enum(bundle, ename(1:elen), eidx, err)
    if (tree%nodes(enode)%name%len > 0_i32) then
      bundle%enums(eidx)%fortran_name = tree%nodes(enode)%name
    else
      call ident_set(bundle%enums(eidx)%fortran_name, ename(1:elen), err)
    end if
    call extract_lead_doxygen_brief(tree, tree%nodes(enode)%first_tok, &
      bundle%enums(eidx)%description)

    do c = 1_i32, tree%node_count
      if (tree%nodes(c)%parent == enode .and. tree%nodes(c)%kind == CST_FIELD_DECL) then
        bundle%enums(eidx)%val_count = bundle%enums(eidx)%val_count + 1_i32
        vc = bundle%enums(eidx)%val_count
        bundle%enums(eidx)%f_vals(vc) = tree%nodes(c)%name
        if (tree%nodes(c)%last_tok > 0_i32) then
          call extract_trail_doxygen(tree, tree%nodes(c)%last_tok, inline_doc, ilen)
        else
          ilen = 0_i32
        end if
        if (ilen > 0_i32) then
          call ident_set(bundle%enums(eidx)%json_vals(vc), inline_doc(1:ilen), err)
        else
          bundle%enums(eidx)%json_vals(vc) = tree%nodes(c)%name
        end if
      end if
    end do
  end subroutine extract_enum_from_cst

  !> @brief Extract a CST_DERIVED_TYPE or CST_DEC_STRUCTURE node into bundle%udts.
  !> @param[in]    tree      CST tree.
  !> @param[in]    unode     CST node index.
  !> @param[in]    annots    Parsed `@@cdd-field:` table.
  !> @param[in]    annot_cnt Active annotation count.
  !> @param[inout] bundle    Target IR bundle.
  !> @param[inout] err       Error status.
  pure subroutine extract_structured_udt(tree, unode, annots, annot_cnt, bundle, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: unode
    type(cdd_field_annot_t), intent(in) :: annots(MAX_ANNOTS)
    integer(i32), intent(in) :: annot_cnt
    type(cdd_ir_schema_bundle_t), intent(inout) :: bundle
    type(cdd_error_t), intent(inout) :: err
    character(len=64, kind=c_k) :: jname, doc_txt
    integer(i32) :: jlen, uidx, c, f_slot, dlen
    type(cdd_ir_field_t) :: fld

    call find_lead_tag_value(tree, tree%nodes(unode)%first_tok, "@cdd-udt:", jname, jlen)
    if (jlen == 0_i32) then
      call strip_type_suffix(tree%nodes(unode)%name, jname, jlen)
    end if
    if (jlen == 0_i32) return

    call ir_bundle_add_udt(bundle, jname(1:jlen), uidx, err)
    bundle%udts(uidx)%fortran_name = tree%nodes(unode)%name
    bundle%udts(uidx)%has_extends = tree%nodes(unode)%has_extends
    bundle%udts(uidx)%extends_name = tree%nodes(unode)%aux_name
    bundle%udts(uidx)%is_bind_c = tree%nodes(unode)%is_bind_c
    call extract_lead_doxygen_brief(tree, tree%nodes(unode)%first_tok, &
      bundle%udts(uidx)%description)

    do c = 1_i32, tree%node_count
      if (tree%nodes(c)%parent == unode .and. tree%nodes(c)%kind == CST_FIELD_DECL) then
        call cst_decl_to_ir_field(tree, c, annots, annot_cnt, bundle, fld, err)
        if (tree%nodes(c)%last_tok > 0_i32) then
          call extract_trail_doxygen(tree, tree%nodes(c)%last_tok, doc_txt, dlen)
          if (dlen > 0_i32) call str_set(fld%description, doc_txt(1:dlen), err)
        end if
        call ir_udt_add_field(bundle%udts(uidx), fld, f_slot, err)
      end if
    end do
  end subroutine extract_structured_udt

  !> @brief Extract legacy FORTRAN I..77 UDT from COMMON, DIMENSION, EQUIVALENCE, DATA, and @@cdd-field.
  !> @param[in]    tree      CST tree.
  !> @param[in]    annots    Parsed `@@cdd-field:` table.
  !> @param[in]    annot_cnt Active annotation count.
  !> @param[inout] bundle    Target IR bundle.
  !> @param[inout] err       Error status.
  pure subroutine extract_legacy_flat_udt(tree, annots, annot_cnt, bundle, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    type(cdd_field_annot_t), intent(in) :: annots(MAX_ANNOTS)
    integer(i32), intent(in) :: annot_cnt
    type(cdd_ir_schema_bundle_t), intent(inout) :: bundle
    type(cdd_error_t), intent(inout) :: err
    character(len=64, kind=c_k) :: udt_jname
    integer(i32) :: uj_len, uidx, i, f_idx, f_slot, flen
    type(cdd_ir_field_t) :: fld

    call find_any_udt_tag(tree, udt_jname, uj_len)
    if (uj_len == 0_i32) then
      call find_first_common_name(tree, udt_jname, uj_len)
    end if
    if (uj_len == 0_i32) then
      udt_jname(1:6) = "Record"
      uj_len = 6_i32
    end if

    call ir_bundle_add_udt(bundle, udt_jname(1:uj_len), uidx, err)
    call ident_set(bundle%udts(uidx)%fortran_name, udt_jname(1:min(6_i32, uj_len)), err)
    call extract_lead_doxygen_brief(tree, 1_i32, bundle%udts(uidx)%description)

    ! 1. Seed fields from @@cdd-field annotations when present (preserves exact order & types!)
    do i = 1_i32, annot_cnt
      call ir_field_init(fld)
      fld%fortran_name = annots(i)%f_name
      fld%json_name = annots(i)%j_name
      fld%type_kind = annots(i)%type_kind
      fld%char_len = max(1_i32, annots(i)%char_len)
      call ir_udt_add_field(bundle%udts(uidx), fld, f_slot, err)
    end do

    ! 2. Merge explicit CST_FIELD_DECL nodes (INTEGER, REAL, DOUBLE PRECISION, CHARACTER*N, etc.)
    do i = 1_i32, tree%node_count
      if (tree%nodes(i)%kind == CST_FIELD_DECL) then
        flen = tree%nodes(i)%name%len
        if (.not. is_synthetic_backing_name(tree%nodes(i)%name%raw(1:flen))) then
          f_idx = ir_udt_find_field(bundle%udts(uidx), tree%nodes(i)%name%raw(1:flen))
          if (f_idx == 0_i32) then
            call cst_decl_to_ir_field(tree, i, annots, annot_cnt, bundle, fld, err)
            call ir_udt_add_field(bundle%udts(uidx), fld, f_slot, err)
          else
            if (tree%nodes(i)%rank > 0_i32 .and. &
                bundle%udts(uidx)%fields(f_idx)%type_kind /= IR_STRING_FIXED) then
              bundle%udts(uidx)%fields(f_idx)%rank = tree%nodes(i)%rank
              bundle%udts(uidx)%fields(f_idx)%dims(1:4) = tree%nodes(i)%dims
            end if
          end if
        end if
      end if
    end do

    ! 3. Merge standalone DIMENSION and COMMON member variables (hand-written FORTRAN I/II/III!)
    do i = 1_i32, tree%node_count
      if (tree%nodes(i)%kind == CST_DIMENSION_STMT) then
        call merge_dimension_stmt(tree, i, bundle%udts(uidx), err)
      else if (tree%nodes(i)%kind == CST_COMMON_BLOCK) then
        call merge_common_members(tree, i, bundle%udts(uidx), err)
      else if (tree%nodes(i)%kind == CST_DATA_STMT) then
        call merge_data_stmt_defaults(tree, i, bundle%udts(uidx))
      end if
    end do

    if (bundle%udts(uidx)%field_count == 0_i32) then
      bundle%udt_count = 0_i32
    end if
  end subroutine extract_legacy_flat_udt

  !> @brief Convert a CST_FIELD_DECL node into a canonical cdd_ir_field_t.
  !> @param[in]  tree      CST tree.
  !> @param[in]  cnode     CST_FIELD_DECL node index.
  !> @param[in]  annots    Parsed `@@cdd-field:` table.
  !> @param[in]  annot_cnt Active annotation count.
  !> @param[in]  bundle    Enclosing IR bundle (for resolving enum/UDT names).
  !> @param[out] fld       Populated IR field.
  !> @param[inout] err     Error status.
  pure subroutine cst_decl_to_ir_field(tree, cnode, annots, annot_cnt, bundle, fld, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: cnode
    type(cdd_field_annot_t), intent(in) :: annots(MAX_ANNOTS)
    integer(i32), intent(in) :: annot_cnt
    type(cdd_ir_schema_bundle_t), intent(in) :: bundle
    type(cdd_ir_field_t), intent(out) :: fld
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: aidx, e_idx, k

    call ir_field_init(fld)
    fld%fortran_name = tree%nodes(cnode)%name
    fld%json_name = tree%nodes(cnode)%name
    fld%type_kind = tree%nodes(cnode)%type_kind
    fld%char_len = max(1_i32, tree%nodes(cnode)%char_len)
    fld%rank = tree%nodes(cnode)%rank
    do k = 1_i32, min(4_i32, fld%rank)
      fld%dims(k) = tree%nodes(cnode)%dims(k)
    end do
    fld%is_allocatable = tree%nodes(cnode)%is_allocatable
    fld%is_pointer = tree%nodes(cnode)%is_pointer

    if (fld%type_kind == IR_NESTED_UDT .and. tree%nodes(cnode)%aux_name%len > 0_i32) then
      e_idx = ir_bundle_find_enum(bundle, &
        tree%nodes(cnode)%aux_name%raw(1:tree%nodes(cnode)%aux_name%len))
      if (e_idx > 0_i32) then
        fld%type_kind = IR_ENUM
        fld%enum_name = bundle%enums(e_idx)%json_name
      else
        fld%ref_udt_name = tree%nodes(cnode)%aux_name
      end if
    end if

    aidx = find_annot(annots, annot_cnt, fld%fortran_name%raw(1:fld%fortran_name%len))
    if (aidx > 0_i32) then
      fld%json_name = annots(aidx)%j_name
      fld%type_kind = annots(aidx)%type_kind
      if (annots(aidx)%char_len > 0_i32) fld%char_len = annots(aidx)%char_len
      if (fld%type_kind == IR_ENUM .and. fld%enum_name%len == 0_i32 .and. &
          bundle%enum_count > 0_i32) then
        fld%enum_name = bundle%enums(1)%json_name
      end if
    end if

    if (tree%nodes(cnode)%has_default) then
      call normalize_fortran_default(tree%nodes(cnode)%default_val, fld, err)
    end if
  end subroutine cst_decl_to_ir_field

  !> @brief Normalize a Fortran initializer (`.true.`, `'abc'`, `null`, `42`) into IR default.
  !> @param[in]    raw_def Raw initializer identifier/slice.
  !> @param[inout] fld     Target IR field.
  !> @param[inout] err     Error status.
  pure subroutine normalize_fortran_default(raw_def, fld, err)
    type(cdd_ident_t), intent(in) :: raw_def
    type(cdd_ir_field_t), intent(inout) :: fld
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: n

    n = raw_def%len
    if (str_eq_nocase(raw_def%raw(1:n), "null")) then
      fld%is_nullable = .true._lk
      fld%is_required = .false._lk
      return
    end if
    fld%has_default = .true._lk
    if (starts_with(raw_def%raw(1:n), ".true") .or. starts_with(raw_def%raw(1:n), ".TRUE")) then
      call ident_set(fld%default_lexeme, "true", err)
    else if (starts_with(raw_def%raw(1:n), ".false") .or. starts_with(raw_def%raw(1:n), ".FALSE")) then
      call ident_set(fld%default_lexeme, "false", err)
    else if (n >= 2_i32 .and. (raw_def%raw(1:1) == '"' .or. raw_def%raw(1:1) == "'")) then
      call ident_set(fld%default_lexeme, raw_def%raw(2:n - 1_i32), err)
    else
      fld%default_lexeme = raw_def
    end if
  end subroutine normalize_fortran_default

  !> @brief Merge standalone `DIMENSION var(n)` declarations into udt (supporting FORTRAN I..66).
  !> @param[in]    tree  CST tree.
  !> @param[in]    dnode CST_DIMENSION_STMT node index.
  !> @param[inout] udt   Target UDT.
  !> @param[inout] err   Error status.
  pure subroutine merge_dimension_stmt(tree, dnode, udt, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: dnode
    type(cdd_ir_udt_t), intent(inout) :: udt
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: t, t_end, f_idx, f_slot, roff, rlen, r
    integer(i64) :: bound_v
    type(cdd_ir_field_t) :: fld
    type(cdd_error_t) :: tmp_err

    t = tree%nodes(dnode)%first_tok
    t_end = tree%nodes(dnode)%last_tok
    do while (t <= t_end)
      if (tree%stream%tokens(t)%kind == FTOK_IDENT .and. &
          .not. ftok_norm_eq(tree%stream, t, "DIMENSION")) then
        roff = tree%stream%tokens(t)%raw_off
        rlen = tree%stream%tokens(t)%raw_len
        if (.not. is_synthetic_backing_name(tree%stream%source_buf%raw(roff:roff + rlen - 1_i32))) then
          f_idx = ir_udt_find_field(udt, tree%stream%source_buf%raw(roff:roff + rlen - 1_i32))
          if (f_idx == 0_i32) then
            call ir_field_init(fld)
            call ident_set(fld%fortran_name, tree%stream%source_buf%raw(roff:roff + rlen - 1_i32), err)
            fld%json_name = fld%fortran_name
            if (is_implicit_int_letter(fld%fortran_name%raw(1:1))) then
              fld%type_kind = IR_INT32
            else
              fld%type_kind = IR_REAL32
            end if
            call ir_udt_add_field(udt, fld, f_idx, err)
          end if
          if (t + 1_i32 <= t_end .and. tree%stream%tokens(t + 1_i32)%kind == FTOK_LPAREN) then
            t = t + 2_i32
            r = 0_i32
            do while (t <= t_end .and. tree%stream%tokens(t)%kind /= FTOK_RPAREN)
              if (tree%stream%tokens(t)%kind == FTOK_INT_LIT .and. r < 4_i32) then
                r = r + 1_i32
                call parse_i64_strict(tree%stream%source_buf%raw( &
                  tree%stream%tokens(t)%raw_off: &
                  tree%stream%tokens(t)%raw_off + tree%stream%tokens(t)%raw_len - 1_i32), &
                  bound_v, tmp_err)
                if (udt%fields(f_idx)%type_kind /= IR_STRING_FIXED) then
                  udt%fields(f_idx)%rank = r
                  udt%fields(f_idx)%dims(r)%kind = DIM_FIXED
                  udt%fields(f_idx)%dims(r)%lower_bound = 1_i32
                  udt%fields(f_idx)%dims(r)%upper_bound = max(1_i32, int(bound_v, i32))
                end if
              end if
              t = t + 1_i32
            end do
          end if
          f_slot = f_idx
        else
          do while (t <= t_end .and. tree%stream%tokens(t)%kind /= FTOK_RPAREN)
            t = t + 1_i32
          end do
        end if
      end if
      t = t + 1_i32
    end do
    f_slot = 0_i32
    r = f_slot
  end subroutine merge_dimension_stmt

  !> @brief Merge variables listed in blank or named `COMMON` into udt (using I-N implicit fallback).
  !> @param[in]    tree  CST tree.
  !> @param[in]    cnode CST_COMMON_BLOCK node index.
  !> @param[inout] udt   Target UDT.
  !> @param[inout] err   Error status.
  pure subroutine merge_common_members(tree, cnode, udt, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: cnode
    type(cdd_ir_udt_t), intent(inout) :: udt
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: t, t_end, roff, rlen, f_slot
    logical(lk) :: in_slash
    type(cdd_ir_field_t) :: fld

    in_slash = .false._lk
    t = tree%nodes(cnode)%first_tok
    t_end = tree%nodes(cnode)%last_tok
    do while (t <= t_end)
      if (tree%stream%tokens(t)%kind == FTOK_SLASH) then
        in_slash = .not. in_slash
      else if (.not. in_slash .and. tree%stream%tokens(t)%kind == FTOK_IDENT .and. &
               .not. ftok_norm_eq(tree%stream, t, "COMMON")) then
        roff = tree%stream%tokens(t)%raw_off
        rlen = tree%stream%tokens(t)%raw_len
        if (ir_udt_find_field(udt, tree%stream%source_buf%raw(roff:roff + rlen - 1_i32)) == 0_i32) then
          call ir_field_init(fld)
          call ident_set(fld%fortran_name, tree%stream%source_buf%raw(roff:roff + rlen - 1_i32), err)
          fld%json_name = fld%fortran_name
          if (is_implicit_int_letter(fld%fortran_name%raw(1:1))) then
            fld%type_kind = IR_INT32
          else
            fld%type_kind = IR_REAL32
          end if
          call ir_udt_add_field(udt, fld, f_slot, err)
        end if
      end if
      t = t + 1_i32
    end do
  end subroutine merge_common_members

  !> @brief Extract `DATA var /val/` defaults (including `nH...` Hollerith strings) onto udt%fields.
  !> @param[in]    tree  CST tree.
  !> @param[in]    dnode CST_DATA_STMT node index.
  !> @param[inout] udt   Target UDT.
  pure subroutine merge_data_stmt_defaults(tree, dnode, udt)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: dnode
    type(cdd_ir_udt_t), intent(inout) :: udt
    integer(i32) :: t, t_end, f_idx, roff, rlen, noff, nlen, h_idx
    type(cdd_error_t) :: tmp_err

    t = tree%nodes(dnode)%first_tok + 1_i32
    t_end = tree%nodes(dnode)%last_tok
    do while (t + 2_i32 <= t_end)
      if (tree%stream%tokens(t)%kind == FTOK_IDENT .and. &
          tree%stream%tokens(t + 1_i32)%kind == FTOK_SLASH) then
        roff = tree%stream%tokens(t)%raw_off
        rlen = tree%stream%tokens(t)%raw_len
        f_idx = ir_udt_find_field(udt, tree%stream%source_buf%raw(roff:roff + rlen - 1_i32))
        if (f_idx > 0_i32) then
          udt%fields(f_idx)%has_default = .true._lk
          if (tree%stream%tokens(t + 2_i32)%kind == FTOK_HOLLERITH) then
            noff = tree%stream%tokens(t + 2_i32)%norm_off
            nlen = tree%stream%tokens(t + 2_i32)%norm_len
            h_idx = noff
            do while (tree%stream%norm_pool%raw(h_idx:h_idx) /= "H")
              h_idx = h_idx + 1_i32
            end do
            call ident_set(udt%fields(f_idx)%default_lexeme, &
              tree%stream%norm_pool%raw(h_idx + 1_i32:noff + nlen - 1_i32), tmp_err)
          else
            roff = tree%stream%tokens(t + 2_i32)%raw_off
            rlen = tree%stream%tokens(t + 2_i32)%raw_len
            call ident_set(udt%fields(f_idx)%default_lexeme, &
              tree%stream%source_buf%raw(roff:roff + min(63_i32, rlen) - 1_i32), tmp_err)
            call normalize_fortran_default(udt%fields(f_idx)%default_lexeme, &
              udt%fields(f_idx), tmp_err)
          end if
        end if
        t = t + 4_i32
      else
        t = t + 1_i32
      end if
    end do
  end subroutine merge_data_stmt_defaults

  !> @brief Look up Fortran identifier `fname` in annots(1:annot_cnt) (0 if absent).
  !> @param[in] annots    Annotation table.
  !> @param[in] annot_cnt Active count.
  !> @param[in] fname     Fortran identifier.
  !> @return 1-based index or 0.
  pure function find_annot(annots, annot_cnt, fname) result(idx)
    type(cdd_field_annot_t), intent(in) :: annots(MAX_ANNOTS)
    integer(i32), intent(in) :: annot_cnt
    character(len=*, kind=c_k), intent(in) :: fname
    integer(i32) :: idx
    integer(i32) :: k

    idx = 0_i32
    do k = 1_i32, annot_cnt
      if (str_eq_nocase(annots(k)%f_name%raw(1:annots(k)%f_name%len), fname)) then
        idx = k
        return
      end if
    end do
  end function find_annot

  !> @brief Test whether name is an internal FORTRAN I EQUIVALENCE backing array (`IUDTBK`/`XUDTBK`).
  !> @param[in] nm Identifier slice.
  !> @return True if synthetic backing symbol.
  pure function is_synthetic_backing_name(nm) result(is_syn)
    character(len=*, kind=c_k), intent(in) :: nm
    logical(lk) :: is_syn

    is_syn = str_eq_nocase(nm, "IUDTBK") .or. str_eq_nocase(nm, "XUDTBK")
  end function is_synthetic_backing_name

  !> @brief Strip trailing `_t` or `_e` suffix from a Fortran identifier when deriving JSON title.
  !> @param[in]  id       Source Fortran identifier.
  !> @param[out] out_name Output name buffer.
  !> @param[out] out_len  Active length.
  pure subroutine strip_type_suffix(id, out_name, out_len)
    type(cdd_ident_t), intent(in) :: id
    character(len=64, kind=c_k), intent(out) :: out_name
    integer(i32), intent(out) :: out_len
    integer(i32) :: n, k

    n = id%len
    if (n > 2_i32 .and. (ends_with(id%raw(1:n), "_t") .or. ends_with(id%raw(1:n), "_T") .or. &
                         ends_with(id%raw(1:n), "_e") .or. ends_with(id%raw(1:n), "_E"))) then
      n = n - 2_i32
    end if
    out_len = n
    do k = 1_i32, n
      out_name(k:k) = id%raw(k:k)
    end do
  end subroutine strip_type_suffix

  !> @brief Scan tok_idx's leading trivia for `tag_prefix <value>` (e.g. `@@cdd-udt:`).
  !> @param[in]  tree       CST tree.
  !> @param[in]  tok_idx    Token index.
  !> @param[in]  tag_prefix Prefix tag.
  !> @param[out] out_val    Extracted word.
  !> @param[out] out_len    Length of out_val (0 if absent).
  pure subroutine find_lead_tag_value(tree, tok_idx, tag_prefix, out_val, out_len)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: tok_idx
    character(len=*, kind=c_k), intent(in) :: tag_prefix
    character(len=64, kind=c_k), intent(out) :: out_val
    integer(i32), intent(out) :: out_len
    integer(i32) :: k, tidx

    out_len = 0_i32
    do k = 0_i32, tree%stream%tokens(tok_idx)%lead_count - 1_i32
      tidx = tree%stream%tokens(tok_idx)%lead_first + k
      call scan_piece_for_tag(tree, tidx, tag_prefix, out_val, out_len)
      if (out_len > 0_i32) return
    end do
  end subroutine find_lead_tag_value

  !> @brief Scan all trivia pieces in tree for the first `@@cdd-udt:` tag.
  !> @param[in]  tree    CST tree.
  !> @param[out] out_val Extracted UDT name.
  !> @param[out] out_len Length of out_val.
  pure subroutine find_any_udt_tag(tree, out_val, out_len)
    type(cdd_cst_tree_t), intent(in) :: tree
    character(len=64, kind=c_k), intent(out) :: out_val
    integer(i32), intent(out) :: out_len
    integer(i32) :: tidx

    out_len = 0_i32
    do tidx = 1_i32, tree%stream%trivia_count
      call scan_piece_for_tag(tree, tidx, "@cdd-udt:", out_val, out_len)
      if (out_len > 0_i32) return
    end do
  end subroutine find_any_udt_tag

  !> @brief Inspect a single trivia piece `tidx` for `tag_prefix <word>`.
  !> @param[in]  tree       CST tree.
  !> @param[in]  tidx       Trivia piece index.
  !> @param[in]  tag_prefix Tag prefix string.
  !> @param[out] out_val    Extracted identifier.
  !> @param[out] out_len    Active length.
  pure subroutine scan_piece_for_tag(tree, tidx, tag_prefix, out_val, out_len)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: tidx
    character(len=*, kind=c_k), intent(in) :: tag_prefix
    character(len=64, kind=c_k), intent(out) :: out_val
    integer(i32), intent(out) :: out_len
    integer(i32) :: off, slen, tlen, p, q

    out_len = 0_i32
    off = tree%stream%trivia(tidx)%src_off
    slen = tree%stream%trivia(tidx)%src_len
    tlen = len(tag_prefix, kind=i32)
    do p = off, off + slen - tlen
      if (starts_with(tree%stream%source_buf%raw(p:off + slen - 1_i32), tag_prefix)) then
        q = p + tlen
        do while (q <= off + slen - 1_i32)
          if (tree%stream%source_buf%raw(q:q) /= " ") exit
          q = q + 1_i32
        end do
        do while (q <= off + slen - 1_i32 .and. out_len < 64_i32)
          if (tree%stream%source_buf%raw(q:q) == " ") exit
          out_len = out_len + 1_i32
          out_val(out_len:out_len) = tree%stream%source_buf%raw(q:q)
          q = q + 1_i32
        end do
        return
      end if
    end do
  end subroutine scan_piece_for_tag

  !> @brief Find first named COMMON block label (stripping paired F77 N/C suffix if paired).
  !> @param[in]  tree    CST tree.
  !> @param[out] out_val COMMON block base name.
  !> @param[out] out_len Active length.
  pure subroutine find_first_common_name(tree, out_val, out_len)
    type(cdd_cst_tree_t), intent(in) :: tree
    character(len=64, kind=c_k), intent(out) :: out_val
    integer(i32), intent(out) :: out_len
    integer(i32) :: i, j, n1, n2, k

    out_len = 0_i32
    do i = 1_i32, tree%node_count
      if (tree%nodes(i)%kind == CST_COMMON_BLOCK .and. tree%nodes(i)%name%len > 0_i32) then
        n1 = tree%nodes(i)%name%len
        out_len = n1
        do k = 1_i32, n1
          out_val(k:k) = tree%nodes(i)%name%raw(k:k)
        end do
        if (n1 >= 2_i32 .and. out_val(n1:n1) == "N") then
          do j = i + 1_i32, tree%node_count
            if (tree%nodes(j)%kind == CST_COMMON_BLOCK) then
              n2 = tree%nodes(j)%name%len
              if (n2 == n1 .and. tree%nodes(j)%name%raw(n2:n2) == "C" .and. &
                  str_eq_nocase(tree%nodes(j)%name%raw(1:n1 - 1_i32), out_val(1:n1 - 1_i32))) then
                out_len = n1 - 1_i32
                return
              end if
            end if
          end do
        end if
        return
      end if
    end do
  end subroutine find_first_common_name

  !> @brief Extract leading Doxygen `!> @@brief <desc>` or `C> <desc>` comment into dst_desc.
  !> @param[in]  tree     CST tree.
  !> @param[in]  tok_idx  Token index.
  !> @param[out] dst_desc Destination string.
  pure subroutine extract_lead_doxygen_brief(tree, tok_idx, dst_desc)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: tok_idx
    type(cdd_string_t), intent(out) :: dst_desc
    integer(i32) :: k, tidx, off, slen, p, lo, hi
    type(cdd_error_t) :: tmp_err

    dst_desc%len = 0_i32
    do k = 0_i32, tree%stream%tokens(tok_idx)%lead_count - 1_i32
      tidx = tree%stream%tokens(tok_idx)%lead_first + k
      if (tree%stream%trivia(tidx)%kind == TRIVIA_DOXYGEN_COMMENT) then
        off = tree%stream%trivia(tidx)%src_off
        slen = tree%stream%trivia(tidx)%src_len
        p = off + 2_i32
        call trim_bounds(tree%stream%source_buf%raw(p:off + slen - 1_i32), lo, hi)
        if (lo <= hi) then
          p = p + lo - 1_i32
          if (starts_with(tree%stream%source_buf%raw(p:off + slen - 1_i32), "@brief ")) then
            p = p + 7_i32
          end if
          call trim_bounds(tree%stream%source_buf%raw(p:off + slen - 1_i32), lo, hi)
          if (lo <= hi) then
            call str_set(dst_desc, &
              tree%stream%source_buf%raw(p + lo - 1_i32:p + hi - 1_i32), tmp_err)
          end if
        end if
      end if
    end do
  end subroutine extract_lead_doxygen_brief

  !> @brief Extract inline trailing `!< <doc>` comment from tok_idx's trailing trivia.
  !> @param[in]  tree    CST tree.
  !> @param[in]  tok_idx Token index.
  !> @param[out] out_doc Extracted trimmed text.
  !> @param[out] out_len Active length.
  pure subroutine extract_trail_doxygen(tree, tok_idx, out_doc, out_len)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: tok_idx
    character(len=64, kind=c_k), intent(out) :: out_doc
    integer(i32), intent(out) :: out_len
    integer(i32) :: k, tidx, off, slen, lo, hi, m

    out_len = 0_i32
    do k = 0_i32, tree%stream%tokens(tok_idx)%trail_count - 1_i32
      tidx = tree%stream%tokens(tok_idx)%trail_first + k
      if (tree%stream%trivia(tidx)%kind == TRIVIA_DOXYGEN_COMMENT) then
        off = tree%stream%trivia(tidx)%src_off
        slen = tree%stream%trivia(tidx)%src_len
        if (slen > 2_i32) then
          call trim_bounds(tree%stream%source_buf%raw(off + 2_i32:off + slen - 1_i32), lo, hi)
          if (lo <= hi) then
            out_len = min(64_i32, hi - lo + 1_i32)
            do m = 1_i32, out_len
              out_doc(m:m) = tree%stream%source_buf%raw(off + 1_i32 + lo + m - 1_i32: &
                                                        off + 1_i32 + lo + m - 1_i32)
            end do
            return
          end if
        end if
      end if
    end do
  end subroutine extract_trail_doxygen

end module cdd_extract_udt
