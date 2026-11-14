!> @file cdd_cst_parser.f90
!> @brief Multi-standard lossless Fortran CST parser (FORTRAN I through Fortran 2023).

!> @brief Parses modules, UDTs, structures, enums, commons, fields, and procedures into cdd_cst_tree_t.
module cdd_cst_parser
  use cdd_kinds, only: i32, i64, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set, CDD_ERR_FORTRAN_PARSE
  use cdd_string, only: parse_i64_strict, starts_with
  use cdd_standard, only: cdd_options_t
  use cdd_ir_types, only: IR_INT8, IR_INT16, IR_INT32, IR_INT64, IR_REAL32, IR_REAL64, &
    IR_REAL128, IR_COMPLEX64, IR_COMPLEX128, IR_BOOL, IR_STRING_FIXED, IR_STRING_DEFERRED, &
    IR_ENUM, IR_NESTED_UDT, DIM_FIXED, DIM_DEFERRED, DIM_ASSUMED, cdd_ir_dim_t, &
    ident_set, ir_dim_init
  use cdd_cst_tokens, only: FTOK_EOF, FTOK_IDENT, FTOK_INT_LIT, FTOK_LABEL, FTOK_LPAREN, &
    FTOK_RPAREN, FTOK_COMMA, FTOK_COLON, FTOK_DCOLON, FTOK_EQ, FTOK_ARROW, FTOK_SLASH, &
    FTOK_STAR, ftok_norm_eq
  use cdd_cst_lexer, only: cst_lex
  use cdd_cst_nodes, only: CDD_MAX_CST_NODES, CDD_NODE_DIMS_CACHE, CST_FILE, CST_MODULE, &
    CST_SUBMODULE, &
    CST_PROGRAM, CST_BLOCK_DATA, CST_DERIVED_TYPE, CST_DEC_STRUCTURE, CST_DEC_RECORD, &
    CST_ENUM_BIND_C, CST_ENUMERATION_TYPE, CST_COMMON_BLOCK, CST_EQUIVALENCE_STMT, &
    CST_DIMENSION_STMT, CST_DATA_STMT, CST_SAVE_STMT, CST_FIELD_DECL, CST_SUBROUTINE, &
    CST_FUNCTION, CST_INTERFACE, CST_TYPE_BOUND_PROC, CST_ASSIGNED_GOTO_BLOCK, &
    CST_CONTAINS_STMT, CST_END_STMT, CST_OPAQUE_STMT, CST_INTENT_NONE, CST_INTENT_IN, &
    CST_INTENT_OUT, CST_INTENT_INOUT, cdd_cst_tree_t, cst_tree_init, cst_add_node
  implicit none (type, external)
  private

  public :: cst_parse, cst_parse_bounded

  integer(i32), parameter :: MAX_BLOCK_STACK = 32_i32 !< Max nested Fortran construct depth.

contains

  !> @brief Lex and parse Fortran source into a lossless Red-Green cdd_cst_tree_t.
  !> @param[in]    source  Input Fortran source text.
  !> @param[in]    options Dialect and source-form options.
  !> @param[inout] tree    Populated lossless CST tree.
  !> @param[out]   err     Structured error status on lexical or block-nesting error.
  pure subroutine cst_parse(source, options, tree, err)
    character(len=*, kind=c_k), intent(in) :: source
    type(cdd_options_t), intent(in) :: options
    type(cdd_cst_tree_t), intent(inout) :: tree
    type(cdd_error_t), intent(out) :: err

    call cst_parse_bounded(source, options, CDD_MAX_CST_NODES, tree, err)
  end subroutine cst_parse

  !> @brief Lex and parse Fortran source with a configurable CST node quota for testing.
  !> @param[in]    source        Input Fortran source text.
  !> @param[in]    options       Dialect and source-form options.
  !> @param[in]    max_cst_nodes Maximum allowed CST nodes (1..CDD_MAX_CST_NODES).
  !> @param[inout] tree          Populated lossless CST tree.
  !> @param[out]   err           Structured error status.
  pure subroutine cst_parse_bounded(source, options, max_cst_nodes, tree, err)
    character(len=*, kind=c_k), intent(in) :: source
    type(cdd_options_t), intent(in) :: options
    integer(i32), intent(in) :: max_cst_nodes
    type(cdd_cst_tree_t), intent(inout) :: tree
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: stack(MAX_BLOCK_STACK), depth, cur_parent, root_idx
    integer(i32) :: tok_pos, s_beg, s_end, head_tok, n_toks
    character(len=64, kind=c_k) :: rname
    integer(i32) :: rlen, blk_node

    call cst_tree_init(tree)
    tree%max_nodes = max_cst_nodes
    call cst_lex(source, options, tree%stream, err)
    if (err%code /= 0_i32) return

    n_toks = tree%stream%token_count
    call cst_add_node(tree, CST_FILE, 0_i32, 1_i32, n_toks, root_idx, err)
    if (err%code /= 0_i32) return
    tree%root = root_idx

    depth = 1_i32
    stack(1) = root_idx
    tok_pos = 1_i32

    do while (tok_pos < n_toks)
      s_beg = tok_pos
      s_end = s_beg
      do while (s_end < n_toks - 1_i32)
        if (tree%stream%tokens(s_end)%stmt_end) exit
        s_end = s_end + 1_i32
      end do
      tok_pos = s_end + 1_i32

      cur_parent = stack(depth)

      ! Check FORTRAN I @cdd-begin-routine / @cdd-end-routine trivia markers
      call check_routine_marker(tree, s_beg, "@cdd-begin-routine:", rname, rlen)
      if (rlen > 0_i32 .and. depth < MAX_BLOCK_STACK) then
        call cst_add_node(tree, CST_ASSIGNED_GOTO_BLOCK, cur_parent, s_beg, s_end, blk_node, err)
        if (err%code /= 0_i32) return
        call ident_set(tree%nodes(blk_node)%name, rname(1:rlen), err)
        depth = depth + 1_i32
        stack(depth) = blk_node
        cur_parent = blk_node
      end if

      head_tok = s_beg
      if (tree%stream%tokens(head_tok)%kind == FTOK_LABEL .and. head_tok < s_end) then
        head_tok = head_tok + 1_i32
      end if

      call classify_and_emit_stmt(tree, stack, depth, s_beg, head_tok, s_end, err)
      if (err%code /= 0_i32) return

      if (tree%nodes(stack(depth))%kind == CST_ASSIGNED_GOTO_BLOCK) then
        call check_routine_marker_end(tree, s_beg, s_end, rlen)
        if (rlen > 0_i32) then
          tree%nodes(stack(depth))%last_tok = s_end
          depth = depth - 1_i32
        end if
      end if
    end do

    if (depth > 1_i32) then
      call cdd_error_set(err, CDD_ERR_FORTRAN_PARSE, &
        "Unclosed Fortran block construct at end of file", &
        tree%stream%tokens(tree%nodes(stack(depth))%first_tok)%line, 1_i32, 1_i32, 1_i32, "")
    end if
  end subroutine cst_parse_bounded

  !> @brief Classify a single logical statement [s_beg..s_end] and update CST tree & block stack.
  !> @param[inout] tree     Target CST tree.
  !> @param[inout] stack    Open container stack.
  !> @param[inout] depth    Active stack depth (>= 1).
  !> @param[in]    s_beg    First token of statement (may be FTOK_LABEL).
  !> @param[in]    head_tok First non-label token of statement.
  !> @param[in]    s_end    Last token of statement.
  !> @param[out]   err      Error status.
  pure subroutine classify_and_emit_stmt(tree, stack, depth, s_beg, head_tok, s_end, err)
    type(cdd_cst_tree_t), intent(inout) :: tree
    integer(i32), intent(inout) :: stack(MAX_BLOCK_STACK)
    integer(i32), intent(inout) :: depth
    integer(i32), intent(in) :: s_beg
    integer(i32), intent(in) :: head_tok
    integer(i32), intent(in) :: s_end
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: cur_parent, nidx, proc_kw_tok, proc_kind

    call cdd_error_clear(err)
    cur_parent = stack(depth)

    if (tree%stream%tokens(head_tok)%kind /= FTOK_IDENT) then
      call cst_add_node(tree, CST_OPAQUE_STMT, cur_parent, s_beg, s_end, nidx, err)
      return
    end if

    ! 1. Closing END statements for containers (excluding END IF/DO/SELECT/ASSOCIATE/WHERE/FORALL)
    if (is_container_end_stmt(tree, head_tok, s_end)) then
      if (depth <= 1_i32) then
        if (s_end == head_tok) then
          call cst_add_node(tree, CST_END_STMT, cur_parent, s_beg, s_end, nidx, err)
          return
        end if
        call cdd_error_set(err, CDD_ERR_FORTRAN_PARSE, &
          "Unmatched END statement at top level", &
          tree%stream%tokens(head_tok)%line, tree%stream%tokens(head_tok)%column, 1_i32, 1_i32, "")
        return
      end if
      call cst_add_node(tree, CST_END_STMT, cur_parent, s_beg, s_end, nidx, err)
      tree%nodes(cur_parent)%last_tok = s_end
      tree%nodes(cur_parent)%end_stmt_node = nidx
      depth = depth - 1_i32
      return
    end if

    ! 2. CONTAINS separator
    if (head_tok == s_end .and. ftok_norm_eq(tree%stream, head_tok, "CONTAINS")) then
      call cst_add_node(tree, CST_CONTAINS_STMT, cur_parent, s_beg, s_end, nidx, err)
      return
    end if

    ! 3. MODULE (excluding MODULE PROCEDURE / MODULE SUBROUTINE / MODULE FUNCTION)
    if (ftok_norm_eq(tree%stream, head_tok, "MODULE") .and. head_tok + 1_i32 == s_end .and. &
        tree%stream%tokens(s_end)%kind == FTOK_IDENT .and. &
        .not. ftok_norm_eq(tree%stream, s_end, "PROCEDURE")) then
      call open_block(tree, stack, depth, CST_MODULE, s_beg, s_end, nidx, err)
      call set_tok_ident(tree, s_end, tree%nodes(nidx)%name)
      return
    end if

    ! 4. SUBMODULE (ancestor) name
    if (ftok_norm_eq(tree%stream, head_tok, "SUBMODULE") .and. s_end > head_tok) then
      call open_block(tree, stack, depth, CST_SUBMODULE, s_beg, s_end, nidx, err)
      call set_tok_ident(tree, s_end, tree%nodes(nidx)%name)
      return
    end if

    ! 5. PROGRAM name
    if (ftok_norm_eq(tree%stream, head_tok, "PROGRAM") .and. head_tok < s_end) then
      call open_block(tree, stack, depth, CST_PROGRAM, s_beg, s_end, nidx, err)
      call set_tok_ident(tree, head_tok + 1_i32, tree%nodes(nidx)%name)
      return
    end if

    ! 6. BLOCK DATA [name]
    if (ftok_norm_eq(tree%stream, head_tok, "BLOCKDATA") .or. &
        (ftok_norm_eq(tree%stream, head_tok, "BLOCK") .and. head_tok < s_end .and. &
         ftok_norm_eq(tree%stream, head_tok + 1_i32, "DATA"))) then
      call open_block(tree, stack, depth, CST_BLOCK_DATA, s_beg, s_end, nidx, err)
      if (.not. ftok_norm_eq(tree%stream, s_end, "DATA") .and. &
          .not. ftok_norm_eq(tree%stream, s_end, "BLOCKDATA")) then
        call set_tok_ident(tree, s_end, tree%nodes(nidx)%name)
      end if
      return
    end if

    ! 7. Fortran 2023 ENUMERATION TYPE [::] name
    if (ftok_norm_eq(tree%stream, head_tok, "ENUMERATION") .and. head_tok + 1_i32 <= s_end) then
      if (ftok_norm_eq(tree%stream, head_tok + 1_i32, "TYPE")) then
        call open_block(tree, stack, depth, CST_ENUMERATION_TYPE, s_beg, s_end, nidx, err)
        call set_tok_ident(tree, s_end, tree%nodes(nidx)%name)
        return
      end if
    end if

    ! 8. Fortran 2003 bind-c enum block
    if (ftok_norm_eq(tree%stream, head_tok, "E" // "NUM")) then
      call open_block(tree, stack, depth, CST_ENUM_BIND_C, s_beg, s_end, nidx, err)
      tree%nodes(nidx)%is_bind_c = .true._lk
      return
    end if

    ! 9. DEC STRUCTURE /name/
    if (ftok_norm_eq(tree%stream, head_tok, "STRUCTURE") .and. head_tok + 2_i32 <= s_end) then
      if (tree%stream%tokens(head_tok + 1_i32)%kind == FTOK_SLASH) then
        call open_block(tree, stack, depth, CST_DEC_STRUCTURE, s_beg, s_end, nidx, err)
        call set_tok_ident(tree, head_tok + 2_i32, tree%nodes(nidx)%name)
        return
      end if
    end if

    ! 10. DEC RECORD /struct_name/ var_name
    if (ftok_norm_eq(tree%stream, head_tok, "RECORD") .and. head_tok + 4_i32 <= s_end) then
      if (tree%stream%tokens(head_tok + 1_i32)%kind == FTOK_SLASH) then
        call cst_add_node(tree, CST_DEC_RECORD, cur_parent, s_beg, s_end, nidx, err)
        call set_tok_ident(tree, head_tok + 2_i32, tree%nodes(nidx)%aux_name)
        call set_tok_ident(tree, head_tok + 4_i32, tree%nodes(nidx)%name)
        return
      end if
    end if

    ! 11. Derived TYPE definition (TYPE [, attrs] [::] name — NOT TYPE(...) field or TYPE IS)
    if (is_derived_type_header(tree, head_tok, s_end)) then
      call open_block(tree, stack, depth, CST_DERIVED_TYPE, s_beg, s_end, nidx, err)
      call parse_derived_type_header(tree, head_tok, s_end, nidx)
      return
    end if

    ! 12. [ABSTRACT] INTERFACE
    if (ftok_norm_eq(tree%stream, head_tok, "INTERFACE") .or. &
        (ftok_norm_eq(tree%stream, head_tok, "ABSTRACT") .and. head_tok < s_end .and. &
         ftok_norm_eq(tree%stream, head_tok + 1_i32, "INTERFACE"))) then
      call open_block(tree, stack, depth, CST_INTERFACE, s_beg, s_end, nidx, err)
      return
    end if

    ! 13. SUBROUTINE / FUNCTION header (with optional PURE/ELEMENTAL/RECURSIVE/type prefixes)
    call detect_procedure_header(tree, head_tok, s_end, proc_kind, proc_kw_tok)
    if (proc_kind /= 0_i32) then
      call open_block(tree, stack, depth, proc_kind, s_beg, s_end, nidx, err)
      call parse_procedure_header(tree, proc_kw_tok, s_end, nidx)
      return
    end if

    ! 14. Type-bound PROCEDURE inside CST_DERIVED_TYPE or CST_INTERFACE
    if ((tree%nodes(cur_parent)%kind == CST_DERIVED_TYPE .or. &
         tree%nodes(cur_parent)%kind == CST_INTERFACE) .and. &
        (ftok_norm_eq(tree%stream, head_tok, "PROCEDURE") .or. &
         (ftok_norm_eq(tree%stream, head_tok, "MODULE") .and. head_tok < s_end .and. &
          ftok_norm_eq(tree%stream, head_tok + 1_i32, "PROCEDURE")))) then
      call cst_add_node(tree, CST_TYPE_BOUND_PROC, cur_parent, s_beg, s_end, nidx, err)
      call parse_tbp_stmt(tree, head_tok, s_end, nidx)
      return
    end if

    ! 15. COMMON, EQUIVALENCE, DIMENSION, DATA, SAVE statements
    if (ftok_norm_eq(tree%stream, head_tok, "COMMON")) then
      call cst_add_node(tree, CST_COMMON_BLOCK, cur_parent, s_beg, s_end, nidx, err)
      call parse_common_header(tree, head_tok, s_end, nidx)
      return
    else if (ftok_norm_eq(tree%stream, head_tok, "EQUIVALENCE")) then
      call cst_add_node(tree, CST_EQUIVALENCE_STMT, cur_parent, s_beg, s_end, nidx, err)
      return
    else if (ftok_norm_eq(tree%stream, head_tok, "DIMENSION")) then
      call cst_add_node(tree, CST_DIMENSION_STMT, cur_parent, s_beg, s_end, nidx, err)
      return
    else if (ftok_norm_eq(tree%stream, head_tok, "DATA")) then
      call cst_add_node(tree, CST_DATA_STMT, cur_parent, s_beg, s_end, nidx, err)
      return
    else if (ftok_norm_eq(tree%stream, head_tok, "SAVE")) then
      call cst_add_node(tree, CST_SAVE_STMT, cur_parent, s_beg, s_end, nidx, err)
      return
    end if

    ! 16. Type/Component/Enumerator declaration (CST_FIELD_DECL)
    if (is_field_decl_stmt(tree, head_tok, s_end)) then
      call parse_field_decl_stmt(tree, cur_parent, s_beg, head_tok, s_end, err)
      return
    end if

    ! 17. Fallback: preserve arbitrary user statement verbatim as CST_OPAQUE_STMT
    call cst_add_node(tree, CST_OPAQUE_STMT, cur_parent, s_beg, s_end, nidx, err)
  end subroutine classify_and_emit_stmt

  !> @brief Allocate a block container node and push it onto stack.
  !> @param[inout] tree   CST tree.
  !> @param[inout] stack  Container stack.
  !> @param[inout] depth  Active stack depth.
  !> @param[in]    kind   Container CST_* kind.
  !> @param[in]    s_beg  Opening statement start token.
  !> @param[in]    s_end  Opening statement end token.
  !> @param[out]   nidx   Allocated container node index.
  !> @param[inout] err    Error status.
  pure subroutine open_block(tree, stack, depth, kind, s_beg, s_end, nidx, err)
    type(cdd_cst_tree_t), intent(inout) :: tree
    integer(i32), intent(inout) :: stack(MAX_BLOCK_STACK)
    integer(i32), intent(inout) :: depth
    integer(i32), intent(in) :: kind
    integer(i32), intent(in) :: s_beg
    integer(i32), intent(in) :: s_end
    integer(i32), intent(out) :: nidx
    type(cdd_error_t), intent(inout) :: err

    if (depth >= MAX_BLOCK_STACK) then
      call cdd_error_set(err, CDD_ERR_FORTRAN_PARSE, "Maximum Fortran block depth exceeded", &
        tree%stream%tokens(s_beg)%line, 1_i32, 1_i32, 1_i32, "")
      nidx = 1_i32
      return
    end if
    call cst_add_node(tree, kind, stack(depth), s_beg, s_end, nidx, err)
    tree%nodes(nidx)%header_end_tok = s_end
    depth = depth + 1_i32
    stack(depth) = nidx
  end subroutine open_block

  !> @brief Test whether [head_tok..s_end] is a block-closing END statement (not END IF/DO/etc.).
  !> @param[in] tree     CST tree.
  !> @param[in] head_tok First non-label token.
  !> @param[in] s_end    Statement end token.
  !> @return True if statement closes a CST container block.
  pure function is_container_end_stmt(tree, head_tok, s_end) result(is_end)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: head_tok
    integer(i32), intent(in) :: s_end
    logical(lk) :: is_end
    integer(i32) :: t2

    is_end = .false._lk
    if (ftok_norm_eq(tree%stream, head_tok, "ENDSTRUCTURE") .or. &
        ftok_norm_eq(tree%stream, head_tok, "ENDTYPE") .or. &
        ftok_norm_eq(tree%stream, head_tok, "ENDMODULE") .or. &
        ftok_norm_eq(tree%stream, head_tok, "ENDSUBMODULE") .or. &
        ftok_norm_eq(tree%stream, head_tok, "ENDSUBROUTINE") .or. &
        ftok_norm_eq(tree%stream, head_tok, "ENDFUNCTION") .or. &
        ftok_norm_eq(tree%stream, head_tok, "ENDPROGRAM") .or. &
        ftok_norm_eq(tree%stream, head_tok, "ENDENUM") .or. &
        ftok_norm_eq(tree%stream, head_tok, "ENDINTERFACE")) then
      is_end = .true._lk
      return
    end if

    if (.not. ftok_norm_eq(tree%stream, head_tok, "END")) return
    if (head_tok == s_end) then
      is_end = .true._lk
      return
    end if

    t2 = head_tok + 1_i32
    if (tree%stream%tokens(t2)%kind /= FTOK_IDENT) return

    if (ftok_norm_eq(tree%stream, t2, "MODULE") .or. &
        ftok_norm_eq(tree%stream, t2, "SUBMODULE") .or. &
        ftok_norm_eq(tree%stream, t2, "PROGRAM") .or. &
        ftok_norm_eq(tree%stream, t2, "SUBROUTINE") .or. &
        ftok_norm_eq(tree%stream, t2, "FUNCTION") .or. &
        ftok_norm_eq(tree%stream, t2, "TYPE") .or. &
        ftok_norm_eq(tree%stream, t2, "STRUCTURE") .or. &
        ftok_norm_eq(tree%stream, t2, "E" // "NUM") .or. &
        ftok_norm_eq(tree%stream, t2, "ENUMERATION") .or. &
        ftok_norm_eq(tree%stream, t2, "INTERFACE") .or. &
        ftok_norm_eq(tree%stream, t2, "BLOCKDATA")) then
      is_end = .true._lk
    else if (ftok_norm_eq(tree%stream, t2, "BLOCK") .and. t2 < s_end) then
      is_end = ftok_norm_eq(tree%stream, t2 + 1_i32, "DATA")
    end if
  end function is_container_end_stmt

  !> @brief Test whether [head_tok..s_end] begins a TYPE derived-type definition block.
  !> @param[in] tree     CST tree.
  !> @param[in] head_tok First non-label token.
  !> @param[in] s_end    End token.
  !> @return True if derived type block header.
  pure function is_derived_type_header(tree, head_tok, s_end) result(ok)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: head_tok
    integer(i32), intent(in) :: s_end
    logical(lk) :: ok
    integer(i32) :: t2

    ok = .false._lk
    if (.not. ftok_norm_eq(tree%stream, head_tok, "TYPE")) return
    if (head_tok >= s_end) return
    t2 = head_tok + 1_i32
    if (tree%stream%tokens(t2)%kind == FTOK_LPAREN .or. &
        tree%stream%tokens(t2)%kind == FTOK_EQ) return
    if (tree%stream%tokens(t2)%kind == FTOK_IDENT .and. &
        ftok_norm_eq(tree%stream, t2, "IS")) return
    ok = .true._lk
  end function is_derived_type_header

  !> @brief Extract name, EXTENDS(parent), BIND(C), and '::' metadata from a TYPE header.
  !> @param[inout] tree     CST tree.
  !> @param[in]    head_tok 'TYPE' token index.
  !> @param[in]    s_end    Header end token.
  !> @param[in]    nidx     Allocated CST_DERIVED_TYPE node index.
  pure subroutine parse_derived_type_header(tree, head_tok, s_end, nidx)
    type(cdd_cst_tree_t), intent(inout) :: tree
    integer(i32), intent(in) :: head_tok
    integer(i32), intent(in) :: s_end
    integer(i32), intent(in) :: nidx
    integer(i32) :: t, dc_tok, name_tok

    dc_tok = 0_i32
    do t = head_tok + 1_i32, s_end
      if (tree%stream%tokens(t)%kind == FTOK_DCOLON) then
        dc_tok = t
        tree%nodes(nidx)%has_double_colon = .true._lk
        exit
      else if (ftok_norm_eq(tree%stream, t, "EXTENDS") .and. t + 2_i32 <= s_end) then
        tree%nodes(nidx)%has_extends = .true._lk
        call set_tok_ident(tree, t + 2_i32, tree%nodes(nidx)%aux_name)
      else if (ftok_norm_eq(tree%stream, t, "BIND")) then
        tree%nodes(nidx)%is_bind_c = .true._lk
      end if
    end do

    if (dc_tok > 0_i32 .and. dc_tok < s_end) then
      name_tok = dc_tok + 1_i32
    else
      name_tok = head_tok + 1_i32
    end if
    if (tree%stream%tokens(name_tok)%kind == FTOK_IDENT) then
      call set_tok_ident(tree, name_tok, tree%nodes(nidx)%name)
    end if
  end subroutine parse_derived_type_header

  !> @brief Detect whether [head_tok..s_end] is a SUBROUTINE or FUNCTION opening header.
  !> @param[in]  tree        CST tree.
  !> @param[in]  head_tok    First non-label token.
  !> @param[in]  s_end       Statement end token.
  !> @param[out] proc_kind   CST_SUBROUTINE, CST_FUNCTION, or 0.
  !> @param[out] proc_kw_tok Token index of SUBROUTINE or FUNCTION keyword.
  pure subroutine detect_procedure_header(tree, head_tok, s_end, proc_kind, proc_kw_tok)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: head_tok
    integer(i32), intent(in) :: s_end
    integer(i32), intent(out) :: proc_kind
    integer(i32), intent(out) :: proc_kw_tok
    integer(i32) :: t

    proc_kind = 0_i32
    proc_kw_tok = 0_i32
    do t = head_tok, s_end - 1_i32
      if (tree%stream%tokens(t)%kind == FTOK_EQ .or. &
          tree%stream%tokens(t)%kind == FTOK_DCOLON) return
      if (tree%stream%tokens(t)%kind == FTOK_IDENT) then
        if (ftok_norm_eq(tree%stream, t, "SUBROUTINE")) then
          proc_kind = CST_SUBROUTINE
          proc_kw_tok = t
          return
        else if (ftok_norm_eq(tree%stream, t, "FUNCTION")) then
          proc_kind = CST_FUNCTION
          proc_kw_tok = t
          return
        end if
      end if
    end do
  end subroutine detect_procedure_header

  !> @brief Extract procedure name and optional RESULT(res) name from SUBROUTINE/FUNCTION header.
  !> @param[inout] tree        CST tree.
  !> @param[in]    proc_kw_tok Index of SUBROUTINE or FUNCTION token.
  !> @param[in]    s_end       Header end token.
  !> @param[in]    nidx        Allocated procedure node index.
  pure subroutine parse_procedure_header(tree, proc_kw_tok, s_end, nidx)
    type(cdd_cst_tree_t), intent(inout) :: tree
    integer(i32), intent(in) :: proc_kw_tok
    integer(i32), intent(in) :: s_end
    integer(i32), intent(in) :: nidx
    integer(i32) :: t

    call set_tok_ident(tree, proc_kw_tok + 1_i32, tree%nodes(nidx)%name)
    do t = proc_kw_tok + 2_i32, s_end - 2_i32
      if (ftok_norm_eq(tree%stream, t, "RESULT") .and. &
          tree%stream%tokens(t + 1_i32)%kind == FTOK_LPAREN) then
        call set_tok_ident(tree, t + 2_i32, tree%nodes(nidx)%aux_name)
        exit
      end if
    end do
  end subroutine parse_procedure_header

  !> @brief Parse a type-bound PROCEDURE statement (`procedure :: a => b`).
  !> @param[inout] tree     CST tree.
  !> @param[in]    head_tok First token.
  !> @param[in]    s_end    End token.
  !> @param[in]    nidx     Allocated CST_TYPE_BOUND_PROC node index.
  pure subroutine parse_tbp_stmt(tree, head_tok, s_end, nidx)
    type(cdd_cst_tree_t), intent(inout) :: tree
    integer(i32), intent(in) :: head_tok
    integer(i32), intent(in) :: s_end
    integer(i32), intent(in) :: nidx
    integer(i32) :: t, arrow_tok

    arrow_tok = 0_i32
    do t = head_tok + 1_i32, s_end
      if (tree%stream%tokens(t)%kind == FTOK_ARROW) then
        arrow_tok = t
        exit
      end if
    end do
    if (arrow_tok > head_tok + 1_i32 .and. arrow_tok < s_end) then
      call set_tok_ident(tree, arrow_tok - 1_i32, tree%nodes(nidx)%name)
      call set_tok_ident(tree, arrow_tok + 1_i32, tree%nodes(nidx)%aux_name)
    else if (tree%stream%tokens(s_end)%kind == FTOK_IDENT) then
      call set_tok_ident(tree, s_end, tree%nodes(nidx)%name)
    end if
  end subroutine parse_tbp_stmt

  !> @brief Parse optional named COMMON block `/name/` onto CST_COMMON_BLOCK node.
  !> @param[inout] tree     CST tree.
  !> @param[in]    head_tok 'COMMON' token index.
  !> @param[in]    s_end    End token.
  !> @param[in]    nidx     Allocated CST_COMMON_BLOCK node index.
  pure subroutine parse_common_header(tree, head_tok, s_end, nidx)
    type(cdd_cst_tree_t), intent(inout) :: tree
    integer(i32), intent(in) :: head_tok
    integer(i32), intent(in) :: s_end
    integer(i32), intent(in) :: nidx

    if (head_tok + 2_i32 <= s_end) then
      if (tree%stream%tokens(head_tok + 1_i32)%kind == FTOK_SLASH .and. &
          tree%stream%tokens(head_tok + 2_i32)%kind == FTOK_IDENT) then
        call set_tok_ident(tree, head_tok + 2_i32, tree%nodes(nidx)%name)
      end if
    end if
  end subroutine parse_common_header

  !> @brief Test whether [head_tok..s_end] is a type/component/enumerator declaration statement.
  !> @param[in] tree     CST tree.
  !> @param[in] head_tok First non-label token.
  !> @param[in] s_end    End token.
  !> @return True if statement declares typed variables, components, or enumerators.
  pure function is_field_decl_stmt(tree, head_tok, s_end) result(ok)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: head_tok
    integer(i32), intent(in) :: s_end
    logical(lk) :: ok

    ok = .false._lk
    if (head_tok >= s_end) return
    if (tree%stream%tokens(head_tok + 1_i32)%kind == FTOK_EQ) return

    if (ftok_norm_eq(tree%stream, head_tok, "INTEGER") .or. &
        ftok_norm_eq(tree%stream, head_tok, "REAL") .or. &
        ftok_norm_eq(tree%stream, head_tok, "DOUBLEPRECISION") .or. &
        ftok_norm_eq(tree%stream, head_tok, "COMPLEX") .or. &
        ftok_norm_eq(tree%stream, head_tok, "LOGICAL") .or. &
        ftok_norm_eq(tree%stream, head_tok, "CHARACTER") .or. &
        ftok_norm_eq(tree%stream, head_tok, "ENUMERATOR")) then
      ok = .true._lk
    else if (ftok_norm_eq(tree%stream, head_tok, "DOUBLE") .and. &
             ftok_norm_eq(tree%stream, head_tok + 1_i32, "PRECISION")) then
      ok = .true._lk
    else if ((ftok_norm_eq(tree%stream, head_tok, "TYPE") .or. &
              ftok_norm_eq(tree%stream, head_tok, "CLASS")) .and. &
             tree%stream%tokens(head_tok + 1_i32)%kind == FTOK_LPAREN) then
      ok = .true._lk
    end if
  end function is_field_decl_stmt

  !> @brief Parse a Fortran declaration statement into one primary CST_FIELD_DECL (+ extra decls).
  !> @param[inout] tree       CST tree.
  !> @param[in]    parent_idx Enclosing container node.
  !> @param[in]    s_beg      First statement token.
  !> @param[in]    head_tok   Type-spec token.
  !> @param[in]    s_end      Last statement token.
  !> @param[out]   err        Error status.
  pure subroutine parse_field_decl_stmt(tree, parent_idx, s_beg, head_tok, s_end, err)
    type(cdd_cst_tree_t), intent(inout) :: tree
    integer(i32), intent(in) :: parent_idx
    integer(i32), intent(in) :: s_beg
    integer(i32), intent(in) :: head_tok
    integer(i32), intent(in) :: s_end
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: base_kind, char_len, attr_intent, base_rank, dc_tok, decl_start
    integer(i32) :: p, depth, item_beg, item_end, nidx, decl_cnt, k
    logical(lk) :: is_alloc, is_ptr, is_param, has_dc
    type(cdd_ir_dim_t) :: base_dims(CDD_NODE_DIMS_CACHE)
    character(len=64, kind=c_k) :: ref_name
    integer(i32) :: ref_len

    call cdd_error_clear(err)
    do k = 1_i32, CDD_NODE_DIMS_CACHE
      call ir_dim_init(base_dims(k))
    end do

    call parse_type_and_attrs(tree, head_tok, s_end, base_kind, char_len, ref_name, ref_len, &
      is_alloc, is_ptr, is_param, attr_intent, base_rank, base_dims, dc_tok, decl_start)
    has_dc = (dc_tok > 0_i32)

    if (is_param) then
      call cst_add_node(tree, CST_OPAQUE_STMT, parent_idx, s_beg, s_end, nidx, err)
      return
    end if

    decl_cnt = 0_i32
    p = decl_start
    do while (p <= s_end)
      item_beg = p
      depth = 0_i32
      do while (p <= s_end)
        if (tree%stream%tokens(p)%kind == FTOK_LPAREN) then
          depth = depth + 1_i32
        else if (tree%stream%tokens(p)%kind == FTOK_RPAREN) then
          depth = max(0_i32, depth - 1_i32)
        else if (depth == 0_i32 .and. tree%stream%tokens(p)%kind == FTOK_COMMA) then
          exit
        end if
        p = p + 1_i32
      end do
      item_end = p - 1_i32
      p = p + 1_i32

      if (item_end >= item_beg .and. tree%stream%tokens(item_beg)%kind == FTOK_IDENT) then
        decl_cnt = decl_cnt + 1_i32
        if (decl_cnt == 1_i32) then
          call cst_add_node(tree, CST_FIELD_DECL, parent_idx, s_beg, s_end, nidx, err)
        else
          ! Secondary declarator on a multi-variable statement: metadata-only (last_tok = 0)
          call cst_add_node(tree, CST_FIELD_DECL, parent_idx, item_beg, 0_i32, nidx, err)
        end if
        if (err%code /= 0_i32) return

        call set_tok_ident(tree, item_beg, tree%nodes(nidx)%name)
        if (ref_len > 0_i32) then
          call ident_set(tree%nodes(nidx)%aux_name, ref_name(1:ref_len), err)
        end if
        tree%nodes(nidx)%type_kind = base_kind
        tree%nodes(nidx)%char_len = char_len
        tree%nodes(nidx)%is_allocatable = is_alloc
        tree%nodes(nidx)%is_pointer = is_ptr
        tree%nodes(nidx)%intent_kind = attr_intent
        tree%nodes(nidx)%has_double_colon = has_dc
        tree%nodes(nidx)%rank = base_rank
        tree%nodes(nidx)%dims = base_dims

        call parse_declarator_tail(tree, item_beg + 1_i32, item_end, nidx)
      end if
    end do

    if (decl_cnt == 0_i32) then
      call cst_add_node(tree, CST_OPAQUE_STMT, parent_idx, s_beg, s_end, nidx, err)
    end if
  end subroutine parse_field_decl_stmt

  !> @brief Parse base type-spec, kind/len selector, and attributes up to '::' or first declarator.
  !> @param[in]  tree        CST tree.
  !> @param[in]  head_tok    Type-spec start token.
  !> @param[in]  s_end       Statement end token.
  !> @param[out] base_kind   Canonical IR_* type kind.
  !> @param[out] char_len    Character length (0 if deferred or non-char).
  !> @param[out] ref_name    Referenced UDT/enum name for TYPE(...)/CLASS(...).
  !> @param[out] ref_len     Length of ref_name.
  !> @param[out] is_alloc    ALLOCATABLE flag.
  !> @param[out] is_ptr      POINTER flag.
  !> @param[out] is_param    PARAMETER flag.
  !> @param[out]   attr_intent CST_INTENT_* constant.
  !> @param[out]   base_rank   Rank from DIMENSION(...) attribute.
  !> @param[inout] base_dims   Dimensions from DIMENSION(...) attribute.
  !> @param[out]   dc_tok      Token index of '::' (0 if absent).
  !> @param[out] decl_start  Token index of first declarator.
  pure subroutine parse_type_and_attrs(tree, head_tok, s_end, base_kind, char_len, &
      ref_name, ref_len, is_alloc, is_ptr, is_param, attr_intent, base_rank, base_dims, &
      dc_tok, decl_start)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: head_tok
    integer(i32), intent(in) :: s_end
    integer(i32), intent(out) :: base_kind
    integer(i32), intent(out) :: char_len
    character(len=64, kind=c_k), intent(out) :: ref_name
    integer(i32), intent(out) :: ref_len
    logical(lk), intent(out) :: is_alloc
    logical(lk), intent(out) :: is_ptr
    logical(lk), intent(out) :: is_param
    integer(i32), intent(out) :: attr_intent
    integer(i32), intent(out) :: base_rank
    type(cdd_ir_dim_t), intent(inout) :: base_dims(CDD_NODE_DIMS_CACHE)
    integer(i32), intent(out) :: dc_tok
    integer(i32), intent(out) :: decl_start
    integer(i32) :: p, close_p, k
    integer(i64) :: parsed_n
    type(cdd_error_t) :: tmp_err

    base_kind = IR_INT32
    char_len = 0_i32
    ref_len = 0_i32
    is_alloc = .false._lk
    is_ptr = .false._lk
    is_param = .false._lk
    attr_intent = CST_INTENT_NONE
    base_rank = 0_i32
    dc_tok = 0_i32

    p = head_tok
    if (ftok_norm_eq(tree%stream, p, "ENUMERATOR")) then
      base_kind = IR_ENUM
      p = p + 1_i32
    else if (ftok_norm_eq(tree%stream, p, "DOUBLE") .and. p < s_end .and. &
             ftok_norm_eq(tree%stream, p + 1_i32, "PRECISION")) then
      base_kind = IR_REAL64
      p = p + 2_i32
    else if (ftok_norm_eq(tree%stream, p, "DOUBLEPRECISION")) then
      base_kind = IR_REAL64
      p = p + 1_i32
    else if (ftok_norm_eq(tree%stream, p, "INTEGER")) then
      base_kind = IR_INT32
      p = p + 1_i32
      if (p <= s_end .and. tree%stream%tokens(p)%kind == FTOK_LPAREN) then
        close_p = find_matching_rparen(tree, p, s_end)
        call refine_int_kind(tree, p + 1_i32, close_p - 1_i32, base_kind)
        p = close_p + 1_i32
      else if (p + 1_i32 <= s_end .and. tree%stream%tokens(p)%kind == FTOK_STAR) then
        call refine_int_kind(tree, p + 1_i32, p + 1_i32, base_kind)
        p = p + 2_i32
      end if
    else if (ftok_norm_eq(tree%stream, p, "REAL")) then
      base_kind = IR_REAL32
      p = p + 1_i32
      if (p <= s_end .and. tree%stream%tokens(p)%kind == FTOK_LPAREN) then
        close_p = find_matching_rparen(tree, p, s_end)
        call refine_real_kind(tree, p + 1_i32, close_p - 1_i32, base_kind)
        p = close_p + 1_i32
      else if (p + 1_i32 <= s_end .and. tree%stream%tokens(p)%kind == FTOK_STAR) then
        call refine_real_kind(tree, p + 1_i32, p + 1_i32, base_kind)
        p = p + 2_i32
      end if
    else if (ftok_norm_eq(tree%stream, p, "COMPLEX")) then
      base_kind = IR_COMPLEX64
      p = p + 1_i32
      if (p <= s_end .and. tree%stream%tokens(p)%kind == FTOK_LPAREN) then
        close_p = find_matching_rparen(tree, p, s_end)
        p = close_p + 1_i32
      end if
    else if (ftok_norm_eq(tree%stream, p, "LOGICAL")) then
      base_kind = IR_BOOL
      p = p + 1_i32
      if (p <= s_end .and. tree%stream%tokens(p)%kind == FTOK_LPAREN) then
        close_p = find_matching_rparen(tree, p, s_end)
        p = close_p + 1_i32
      end if
    else if (ftok_norm_eq(tree%stream, p, "CHARACTER")) then
      base_kind = IR_STRING_FIXED
      char_len = 1_i32
      p = p + 1_i32
      if (p + 1_i32 <= s_end .and. tree%stream%tokens(p)%kind == FTOK_STAR) then
        p = p + 1_i32
        if (tree%stream%tokens(p)%kind == FTOK_LPAREN) then
          close_p = find_matching_rparen(tree, p, s_end)
          char_len = 64_i32
          p = close_p + 1_i32
        else if (tree%stream%tokens(p)%kind == FTOK_INT_LIT) then
          call parse_tok_int(tree, p, parsed_n, tmp_err)
          char_len = max(1_i32, int(parsed_n, i32))
          p = p + 1_i32
        end if
      else if (p <= s_end .and. tree%stream%tokens(p)%kind == FTOK_LPAREN) then
        close_p = find_matching_rparen(tree, p, s_end)
        call parse_char_selector(tree, p + 1_i32, close_p - 1_i32, base_kind, char_len)
        p = close_p + 1_i32
      end if
    else
      base_kind = IR_NESTED_UDT
      p = p + 1_i32
      close_p = find_matching_rparen(tree, p, s_end)
      if (p + 1_i32 < close_p) then
        ref_len = min(64_i32, tree%stream%tokens(p + 1_i32)%raw_len)
        do k = 1_i32, ref_len
          ref_name(k:k) = tree%stream%source_buf%raw( &
            tree%stream%tokens(p + 1_i32)%raw_off + k - 1_i32: &
            tree%stream%tokens(p + 1_i32)%raw_off + k - 1_i32)
        end do
      end if
      p = close_p + 1_i32
    end if

    do k = p, s_end
      if (tree%stream%tokens(k)%kind == FTOK_DCOLON) then
        dc_tok = k
        exit
      end if
    end do

    if (dc_tok > 0_i32) then
      call parse_attr_tokens(tree, p, dc_tok - 1_i32, is_alloc, is_ptr, is_param, &
        attr_intent, base_rank, base_dims)
      decl_start = dc_tok + 1_i32
    else
      decl_start = p
    end if
  end subroutine parse_type_and_attrs

  !> @brief Scan comma-separated attribute specifications prior to '::'.
  !> @param[in]    tree        CST tree.
  !> @param[in]    beg_tok     Start token.
  !> @param[in]    end_tok     End token before '::'.
  !> @param[inout] is_alloc    ALLOCATABLE flag.
  !> @param[inout] is_ptr      POINTER flag.
  !> @param[inout] is_param    PARAMETER flag.
  !> @param[inout] attr_intent CST_INTENT_* constant.
  !> @param[inout] base_rank   Parsed rank from DIMENSION(...).
  !> @param[inout] base_dims   Parsed bounds from DIMENSION(...).
  pure subroutine parse_attr_tokens(tree, beg_tok, end_tok, is_alloc, is_ptr, is_param, &
      attr_intent, base_rank, base_dims)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: beg_tok
    integer(i32), intent(in) :: end_tok
    logical(lk), intent(inout) :: is_alloc
    logical(lk), intent(inout) :: is_ptr
    logical(lk), intent(inout) :: is_param
    integer(i32), intent(inout) :: attr_intent
    integer(i32), intent(inout) :: base_rank
    type(cdd_ir_dim_t), intent(inout) :: base_dims(CDD_NODE_DIMS_CACHE)
    integer(i32) :: t, close_p

    t = beg_tok
    do while (t <= end_tok)
      if (ftok_norm_eq(tree%stream, t, "ALLOCATABLE")) then
        is_alloc = .true._lk
      else if (ftok_norm_eq(tree%stream, t, "POINTER")) then
        is_ptr = .true._lk
      else if (ftok_norm_eq(tree%stream, t, "PARAMETER")) then
        is_param = .true._lk
      else if (ftok_norm_eq(tree%stream, t, "INTENT") .and. t + 2_i32 <= end_tok) then
        close_p = find_matching_rparen(tree, t + 1_i32, end_tok)
        if (ftok_norm_eq(tree%stream, t + 2_i32, "INOUT")) then
          attr_intent = CST_INTENT_INOUT
        else if (ftok_norm_eq(tree%stream, t + 2_i32, "OUT")) then
          attr_intent = CST_INTENT_OUT
        else if (ftok_norm_eq(tree%stream, t + 2_i32, "IN")) then
          if (t + 3_i32 < close_p .and. ftok_norm_eq(tree%stream, t + 3_i32, "OUT")) then
            attr_intent = CST_INTENT_INOUT
          else
            attr_intent = CST_INTENT_IN
          end if
        end if
        t = close_p
      else if (ftok_norm_eq(tree%stream, t, "DIMENSION") .and. t + 1_i32 <= end_tok) then
        if (tree%stream%tokens(t + 1_i32)%kind == FTOK_LPAREN) then
          close_p = find_matching_rparen(tree, t + 1_i32, end_tok)
          call parse_parens_dims(tree, t + 2_i32, close_p - 1_i32, base_rank, base_dims)
          t = close_p
        end if
      end if
      t = t + 1_i32
    end do
  end subroutine parse_attr_tokens

  !> @brief Parse per-declarator array spec `(dims)`, `*char_len`, and `= default` / `=> null()`.
  !> @param[inout] tree     CST tree.
  !> @param[in]    beg_tok  First token after declarator identifier.
  !> @param[in]    end_tok  Last token of declarator item.
  !> @param[in]    nidx     Target CST_FIELD_DECL node index.
  pure subroutine parse_declarator_tail(tree, beg_tok, end_tok, nidx)
    type(cdd_cst_tree_t), intent(inout) :: tree
    integer(i32), intent(in) :: beg_tok
    integer(i32), intent(in) :: end_tok
    integer(i32), intent(in) :: nidx
    integer(i32) :: p, close_p, v_off, v_end
    integer(i64) :: clen
    type(cdd_error_t) :: tmp_err

    p = beg_tok
    if (p <= end_tok) then
      if (tree%stream%tokens(p)%kind == FTOK_LPAREN) then
        close_p = find_matching_rparen(tree, p, end_tok)
        call parse_parens_dims(tree, p + 1_i32, close_p - 1_i32, &
          tree%nodes(nidx)%rank, tree%nodes(nidx)%dims)
        p = close_p + 1_i32
      end if
    end if

    if (p + 1_i32 <= end_tok) then
      if (tree%stream%tokens(p)%kind == FTOK_STAR .and. &
          tree%stream%tokens(p + 1_i32)%kind == FTOK_INT_LIT) then
        call parse_tok_int(tree, p + 1_i32, clen, tmp_err)
        tree%nodes(nidx)%char_len = max(1_i32, int(clen, i32))
        p = p + 2_i32
      end if
    end if

    if (p < end_tok) then
      if (tree%stream%tokens(p)%kind == FTOK_EQ) then
        tree%nodes(nidx)%has_default = .true._lk
        v_off = tree%stream%tokens(p + 1_i32)%raw_off
        v_end = tree%stream%tokens(end_tok)%raw_off + tree%stream%tokens(end_tok)%raw_len - 1_i32
        call ident_set(tree%nodes(nidx)%default_val, &
          tree%stream%source_buf%raw(v_off:min(v_end, v_off + 63_i32)), tmp_err)
      else if (tree%stream%tokens(p)%kind == FTOK_ARROW) then
        tree%nodes(nidx)%has_default = .true._lk
        call ident_set(tree%nodes(nidx)%default_val, "null", tmp_err)
      end if
    end if
  end subroutine parse_declarator_tail

  !> @brief Parse dimension list inside `(...)` into rank and dims(1:4).
  !> @param[in]    tree CST tree.
  !> @param[in]    b_t  First token inside parens.
  !> @param[in]    e_t  Last token inside parens.
  !> @param[out]   rank Parsed dimension count.
  !> @param[inout] dims Cached dimensions array.
  pure subroutine parse_parens_dims(tree, b_t, e_t, rank, dims)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: b_t
    integer(i32), intent(in) :: e_t
    integer(i32), intent(out) :: rank
    type(cdd_ir_dim_t), intent(inout) :: dims(CDD_NODE_DIMS_CACHE)
    integer(i32) :: p, seg_b, seg_e, colon_t
    integer(i64) :: lo_v, hi_v
    type(cdd_error_t) :: tmp_err

    rank = 0_i32
    if (b_t > e_t) return
    p = b_t
    do while (p <= e_t)
      seg_b = p
      do while (p <= e_t)
        if (tree%stream%tokens(p)%kind == FTOK_COMMA) exit
        p = p + 1_i32
      end do
      seg_e = p - 1_i32
      p = p + 1_i32

      rank = rank + 1_i32
      if (rank <= CDD_NODE_DIMS_CACHE .and. seg_e >= seg_b) then
        if (seg_b == seg_e .and. tree%stream%tokens(seg_b)%kind == FTOK_COLON) then
          dims(rank)%kind = DIM_DEFERRED
        else if (seg_b == seg_e .and. tree%stream%tokens(seg_b)%kind == FTOK_STAR) then
          dims(rank)%kind = DIM_ASSUMED
        else
          call find_tok_kind(tree, seg_b, seg_e, FTOK_COLON, colon_t)
          dims(rank)%kind = DIM_FIXED
          if (colon_t > 0_i32) then
            call parse_signed_dim_int(tree, seg_b, colon_t - 1_i32, lo_v)
            call parse_signed_dim_int(tree, colon_t + 1_i32, seg_e, hi_v)
            dims(rank)%lower_bound = int(lo_v, i32)
            dims(rank)%upper_bound = int(hi_v, i32)
          else
            dims(rank)%lower_bound = 1_i32
            call parse_tok_int(tree, seg_b, hi_v, tmp_err)
            if (tmp_err%code == 0_i32) then
              dims(rank)%upper_bound = max(1_i32, int(hi_v, i32))
            else
              dims(rank)%upper_bound = 8_i32
            end if
          end if
        end if
      end if
    end do
  end subroutine parse_parens_dims

  !> @brief Refine INTEGER kind selector (int8/1, int16/2, int32/4, int64/8).
  !> @param[in]    tree      CST tree.
  !> @param[in]    b_t       Start token of selector.
  !> @param[in]    e_t       End token of selector.
  !> @param[inout] base_kind Updated IR_INT* constant.
  pure subroutine refine_int_kind(tree, b_t, e_t, base_kind)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: b_t
    integer(i32), intent(in) :: e_t
    integer(i32), intent(inout) :: base_kind
    integer(i32) :: t

    do t = b_t, e_t
      if (ftok_norm_eq(tree%stream, t, "INT8") .or. ftok_norm_eq(tree%stream, t, "I8") .or. &
          ftok_norm_eq(tree%stream, t, "1")) then
        base_kind = IR_INT8
      else if (ftok_norm_eq(tree%stream, t, "INT16") .or. ftok_norm_eq(tree%stream, t, "I16") .or. &
               ftok_norm_eq(tree%stream, t, "2")) then
        base_kind = IR_INT16
      else if (ftok_norm_eq(tree%stream, t, "INT64") .or. ftok_norm_eq(tree%stream, t, "I64") .or. &
               ftok_norm_eq(tree%stream, t, "K_I64") .or. ftok_norm_eq(tree%stream, t, "8")) then
        base_kind = IR_INT64
      end if
    end do
  end subroutine refine_int_kind

  !> @brief Refine REAL kind selector (real32/4, real64/8/dp, real128/16/qp).
  !> @param[in]    tree      CST tree.
  !> @param[in]    b_t       Start token of selector.
  !> @param[in]    e_t       End token of selector.
  !> @param[inout] base_kind Updated IR_REAL* constant.
  pure subroutine refine_real_kind(tree, b_t, e_t, base_kind)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: b_t
    integer(i32), intent(in) :: e_t
    integer(i32), intent(inout) :: base_kind
    integer(i32) :: t

    do t = b_t, e_t
      if (ftok_norm_eq(tree%stream, t, "REAL64") .or. ftok_norm_eq(tree%stream, t, "R64") .or. &
          ftok_norm_eq(tree%stream, t, "K_R64") .or. ftok_norm_eq(tree%stream, t, "DP") .or. &
          ftok_norm_eq(tree%stream, t, "8")) then
        base_kind = IR_REAL64
      else if (ftok_norm_eq(tree%stream, t, "REAL128") .or. ftok_norm_eq(tree%stream, t, "R128") .or. &
               ftok_norm_eq(tree%stream, t, "K_R128") .or. ftok_norm_eq(tree%stream, t, "16")) then
        base_kind = IR_REAL128
      end if
    end do
  end subroutine refine_real_kind

  !> @brief Parse CHARACTER(...) selector (`len=:`, `len=32`, `32`).
  !> @param[in]    tree      CST tree.
  !> @param[in]    b_t       First token inside parens.
  !> @param[in]    e_t       Last token inside parens.
  !> @param[inout] base_kind IR_STRING_FIXED or IR_STRING_DEFERRED.
  !> @param[inout] char_len  Parsed length when fixed.
  pure subroutine parse_char_selector(tree, b_t, e_t, base_kind, char_len)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: b_t
    integer(i32), intent(in) :: e_t
    integer(i32), intent(inout) :: base_kind
    integer(i32), intent(inout) :: char_len
    integer(i32) :: t
    integer(i64) :: nval
    type(cdd_error_t) :: tmp_err

    do t = b_t, e_t
      if (tree%stream%tokens(t)%kind == FTOK_COLON) then
        base_kind = IR_STRING_DEFERRED
        char_len = 0_i32
        return
      else if (tree%stream%tokens(t)%kind == FTOK_STAR) then
        base_kind = IR_STRING_FIXED
        char_len = 64_i32
        return
      else if (tree%stream%tokens(t)%kind == FTOK_INT_LIT) then
        call parse_tok_int(tree, t, nval, tmp_err)
        char_len = max(1_i32, int(nval, i32))
        return
      end if
    end do
  end subroutine parse_char_selector

  !> @brief Find matching ')' token starting from '(' at open_t within [open_t..limit_t].
  !> @param[in] tree    CST tree.
  !> @param[in] open_t  Index of FTOK_LPAREN.
  !> @param[in] limit_t Upper bound token index.
  !> @return Matching FTOK_RPAREN token index (or limit_t if malformed).
  pure function find_matching_rparen(tree, open_t, limit_t) result(close_t)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: open_t
    integer(i32), intent(in) :: limit_t
    integer(i32) :: close_t
    integer(i32) :: depth, t

    depth = 0_i32
    do t = open_t, limit_t
      if (tree%stream%tokens(t)%kind == FTOK_LPAREN) then
        depth = depth + 1_i32
      else if (tree%stream%tokens(t)%kind == FTOK_RPAREN) then
        depth = depth - 1_i32
        if (depth == 0_i32) then
          close_t = t
          return
        end if
      end if
    end do
    close_t = limit_t
  end function find_matching_rparen

  !> @brief Find first token of target_kind in [b_t..e_t] (0 if absent).
  !> @param[in]  tree        CST tree.
  !> @param[in]  b_t         Start token.
  !> @param[in]  e_t         End token.
  !> @param[in]  target_kind FTOK_* constant.
  !> @param[out] found_t     Token index or 0.
  pure subroutine find_tok_kind(tree, b_t, e_t, target_kind, found_t)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: b_t
    integer(i32), intent(in) :: e_t
    integer(i32), intent(in) :: target_kind
    integer(i32), intent(out) :: found_t
    integer(i32) :: t

    found_t = 0_i32
    do t = b_t, e_t
      if (tree%stream%tokens(t)%kind == target_kind) then
        found_t = t
        return
      end if
    end do
  end subroutine find_tok_kind

  !> @brief Parse a possibly signed integer bound in [b_t..e_t] (defaults to 1 if symbolic).
  !> @param[in]  tree CST tree.
  !> @param[in]  b_t  Start token.
  !> @param[in]  e_t  End token.
  !> @param[out] val  Parsed integer bound.
  pure subroutine parse_signed_dim_int(tree, b_t, e_t, val)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: b_t
    integer(i32), intent(in) :: e_t
    integer(i64), intent(out) :: val
    type(cdd_error_t) :: tmp_err

    val = 1_i64
    if (b_t > e_t) return
    if (ftok_norm_eq(tree%stream, b_t, "-") .and. b_t < e_t) then
      call parse_tok_int(tree, b_t + 1_i32, val, tmp_err)
      if (tmp_err%code == 0_i32) val = -val
    else
      call parse_tok_int(tree, b_t, val, tmp_err)
    end if
  end subroutine parse_signed_dim_int

  !> @brief Parse integer literal token tok_idx (stripping optional `_kind` suffix).
  !> @param[in]  tree    CST tree.
  !> @param[in]  tok_idx Token index.
  !> @param[out] val     Parsed 64-bit value.
  !> @param[out] err     Error status.
  pure subroutine parse_tok_int(tree, tok_idx, val, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: tok_idx
    integer(i64), intent(out) :: val
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: roff, rlen, u_pos, k

    roff = tree%stream%tokens(tok_idx)%raw_off
    rlen = tree%stream%tokens(tok_idx)%raw_len
    u_pos = rlen
    do k = 1_i32, rlen
      if (tree%stream%source_buf%raw(roff + k - 1_i32:roff + k - 1_i32) == "_") then
        u_pos = k - 1_i32
        exit
      end if
    end do
    call parse_i64_strict(tree%stream%source_buf%raw(roff:roff + u_pos - 1_i32), val, err)
  end subroutine parse_tok_int

  !> @brief Copy original-case identifier from tok_idx into dst_ident (or normalized if spaced).
  !> @param[in]  tree      CST tree.
  !> @param[in]  tok_idx   Token index.
  !> @param[out] dst_ident Destination cdd_ident_t.
  pure subroutine set_tok_ident(tree, tok_idx, dst_ident)
    use cdd_ir_types, only: cdd_ident_t
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: tok_idx
    type(cdd_ident_t), intent(out) :: dst_ident
    integer(i32) :: roff, rlen
    type(cdd_error_t) :: tmp_err

    roff = tree%stream%tokens(tok_idx)%raw_off
    rlen = min(64_i32, tree%stream%tokens(tok_idx)%raw_len)
    call ident_set(dst_ident, tree%stream%source_buf%raw(roff:roff + rlen - 1_i32), tmp_err)
  end subroutine set_tok_ident

  !> @brief Scan tok_idx's leading trivia for a `@@cdd-begin-routine:` / `@@cdd-end-routine:` tag.
  !> @param[in]  tree     CST tree.
  !> @param[in]  tok_idx  Token index whose leading trivia is inspected.
  !> @param[in]  marker   Tag prefix string.
  !> @param[out] out_name Extracted routine identifier.
  !> @param[out] out_len  Length of out_name (0 if absent).
  pure subroutine check_routine_marker(tree, tok_idx, marker, out_name, out_len)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: tok_idx
    character(len=*, kind=c_k), intent(in) :: marker
    character(len=64, kind=c_k), intent(out) :: out_name
    integer(i32), intent(out) :: out_len
    integer(i32) :: k, tidx, off, slen, mlen, p, q

    out_len = 0_i32
    mlen = len(marker, kind=i32)
    do k = 0_i32, tree%stream%tokens(tok_idx)%lead_count - 1_i32
      tidx = tree%stream%tokens(tok_idx)%lead_first + k
      off = tree%stream%trivia(tidx)%src_off
      slen = tree%stream%trivia(tidx)%src_len
      do p = off, off + slen - mlen
        if (starts_with(tree%stream%source_buf%raw(p:off + slen - 1_i32), marker)) then
          q = p + mlen
          do while (q <= off + slen - 1_i32)
            if (tree%stream%source_buf%raw(q:q) /= " ") exit
            q = q + 1_i32
          end do
          do while (q <= off + slen - 1_i32 .and. out_len < 64_i32)
            if (tree%stream%source_buf%raw(q:q) == " ") exit
            out_len = out_len + 1_i32
            out_name(out_len:out_len) = tree%stream%source_buf%raw(q:q)
            q = q + 1_i32
          end do
          if (out_len > 0_i32) return
        end if
      end do
    end do
  end subroutine check_routine_marker

  !> @brief Check if statement [s_beg..s_end] has `@@cdd-end-routine:` in next token leading trivia.
  !> @param[in]  tree    CST tree.
  !> @param[in]  s_beg   First token of statement.
  !> @param[in]  s_end   Last token of statement.
  !> @param[out] hit_len > 0 if end-routine marker was found.
  pure subroutine check_routine_marker_end(tree, s_beg, s_end, hit_len)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: s_beg
    integer(i32), intent(in) :: s_end
    integer(i32), intent(out) :: hit_len
    character(len=64, kind=c_k) :: rname

    associate (unused_beg => s_beg)
    end associate
    call check_routine_marker(tree, s_end + 1_i32, "@cdd-end-routine:", rname, hit_len)
  end subroutine check_routine_marker_end

end module cdd_cst_parser
