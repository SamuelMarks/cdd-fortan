!> @file cdd_cst_nodes.f90
!> @brief Red-Green arena CST node definitions for multi-standard Fortran UDTs and procedures.

!> @brief Strongly-typed CST tree, node categories, and surgical patch-attachment primitives.
module cdd_cst_nodes
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set_simple, CDD_ERR_BUFFER_OVERFLOW
  use cdd_string, only: cdd_buffer_t, buf_init, buf_append_str
  use cdd_ir_types, only: IR_INT32, cdd_ident_t, cdd_ir_dim_t, ident_clear, ir_dim_init
  use cdd_cst_tokens, only: cdd_cst_token_stream_t, fstream_init
  implicit none (type, external)
  private

  public :: CDD_MAX_CST_NODES, CDD_NODE_DIMS_CACHE
  public :: CST_FILE, CST_MODULE, CST_SUBMODULE, CST_PROGRAM, CST_BLOCK_DATA
  public :: CST_DERIVED_TYPE, CST_DEC_STRUCTURE, CST_DEC_RECORD
  public :: CST_ENUM_BIND_C, CST_ENUMERATION_TYPE
  public :: CST_COMMON_BLOCK, CST_EQUIVALENCE_STMT, CST_DIMENSION_STMT
  public :: CST_DATA_STMT, CST_SAVE_STMT, CST_FIELD_DECL
  public :: CST_SUBROUTINE, CST_FUNCTION, CST_INTERFACE, CST_TYPE_BOUND_PROC
  public :: CST_ASSIGNED_GOTO_BLOCK, CST_CONTAINS_STMT, CST_END_STMT, CST_OPAQUE_STMT
  public :: CST_INTENT_NONE, CST_INTENT_IN, CST_INTENT_OUT, CST_INTENT_INOUT
  public :: cdd_cst_node_t, cdd_cst_tree_t
  public :: cst_node_init, cst_tree_init, cst_add_node
  public :: cst_mark_deleted, cst_replace_node, cst_insert_before, cst_insert_after

  integer(i32), parameter :: CDD_MAX_CST_NODES = 3584_i32  !< Max CST nodes per parsed Fortran file.
  integer(i32), parameter :: CDD_NODE_DIMS_CACHE = 4_i32   !< Inline cached dimensions on CST node.

  integer(i32), parameter :: CST_FILE = 1_i32                !< Root file compilation unit node.
  integer(i32), parameter :: CST_MODULE = 2_i32              !< MODULE ... END MODULE block.
  integer(i32), parameter :: CST_SUBMODULE = 3_i32           !< SUBMODULE (...) ... END SUBMODULE.
  integer(i32), parameter :: CST_PROGRAM = 4_i32             !< PROGRAM ... END PROGRAM block.
  integer(i32), parameter :: CST_BLOCK_DATA = 5_i32          !< BLOCK DATA ... END block.
  integer(i32), parameter :: CST_DERIVED_TYPE = 6_i32        !< TYPE ... END TYPE block.
  integer(i32), parameter :: CST_DEC_STRUCTURE = 7_i32       !< DEC STRUCTURE /name/ ... END STRUCTURE.
  integer(i32), parameter :: CST_DEC_RECORD = 8_i32          !< DEC RECORD /name/ var statement.
  integer(i32), parameter :: CST_ENUM_BIND_C = 9_i32         !< ENUM, BIND(C) ... END ENUM block.
  integer(i32), parameter :: CST_ENUMERATION_TYPE = 10_i32   !< F2023 ENUMERATION TYPE ... END block.
  integer(i32), parameter :: CST_COMMON_BLOCK = 11_i32       !< Blank or named COMMON statement.
  integer(i32), parameter :: CST_EQUIVALENCE_STMT = 12_i32   !< EQUIVALENCE (...) statement.
  integer(i32), parameter :: CST_DIMENSION_STMT = 13_i32     !< Standalone DIMENSION statement.
  integer(i32), parameter :: CST_DATA_STMT = 14_i32          !< DATA initialization statement.
  integer(i32), parameter :: CST_SAVE_STMT = 15_i32          !< SAVE statement.
  integer(i32), parameter :: CST_FIELD_DECL = 16_i32         !< Type/component/enumerator declaration.
  integer(i32), parameter :: CST_SUBROUTINE = 17_i32         !< SUBROUTINE ... END SUBROUTINE block.
  integer(i32), parameter :: CST_FUNCTION = 18_i32           !< FUNCTION ... END FUNCTION block.
  integer(i32), parameter :: CST_INTERFACE = 19_i32          !< [ABSTRACT] INTERFACE ... END INTERFACE.
  integer(i32), parameter :: CST_TYPE_BOUND_PROC = 20_i32    !< Type-bound PROCEDURE binding statement.
  integer(i32), parameter :: CST_ASSIGNED_GOTO_BLOCK = 21_i32 !< FORTRAN I labeled routine block.
  integer(i32), parameter :: CST_CONTAINS_STMT = 22_i32      !< CONTAINS separator statement.
  integer(i32), parameter :: CST_END_STMT = 23_i32           !< Closing END [...] statement of a block.
  integer(i32), parameter :: CST_OPAQUE_STMT = 24_i32        !< Arbitrary user statement kept verbatim.

  integer(i32), parameter :: CST_INTENT_NONE = 0_i32         !< No explicit INTENT attribute.
  integer(i32), parameter :: CST_INTENT_IN = 1_i32           !< INTENT(IN) attribute.
  integer(i32), parameter :: CST_INTENT_OUT = 2_i32          !< INTENT(OUT) attribute.
  integer(i32), parameter :: CST_INTENT_INOUT = 3_i32        !< INTENT(INOUT) attribute.

  !> @brief Single Red-Green CST node referencing an exact token span and optional patch slices.
  type :: cdd_cst_node_t
    integer(i32) :: kind = CST_OPAQUE_STMT               !< Node category constant (CST_*).
    integer(i32) :: parent = 0_i32                       !< Enclosing parent node index (0 for root).
    integer(i32) :: first_tok = 1_i32                    !< 1-based start token index in tree%stream.
    integer(i32) :: last_tok = 0_i32                     !< 1-based inclusive end token index.
    integer(i32) :: header_end_tok = 0_i32               !< End token of opening header statement.
    integer(i32) :: end_stmt_node = 0_i32                !< Child CST_END_STMT node index for blocks.
    type(cdd_ident_t) :: name                            !< Primary construct / declarator name.
    type(cdd_ident_t) :: aux_name                        !< Parent EXTENDS, RESULT, or TYPE(ref) name.
    integer(i32) :: type_kind = IR_INT32                 !< Parsed canonical IR_* type kind.
    integer(i32) :: char_len = 0_i32                     !< Fixed string length (0 if N/A).
    integer(i32) :: rank = 0_i32                         !< Array rank (0 = scalar).
    type(cdd_ir_dim_t) :: dims(CDD_NODE_DIMS_CACHE)      !< Cached leading array bounds.
    integer(i32) :: intent_kind = CST_INTENT_NONE        !< Parsed INTENT attribute for dummy args.
    logical(lk) :: is_allocatable = .false._lk           !< ALLOCATABLE attribute present.
    logical(lk) :: is_pointer = .false._lk               !< POINTER attribute present.
    logical(lk) :: has_extends = .false._lk              !< EXTENDS(...) attribute present on TYPE.
    logical(lk) :: is_bind_c = .false._lk                !< BIND(C) attribute present.
    logical(lk) :: has_double_colon = .false._lk         !< True when '::' separator is present.
    logical(lk) :: has_default = .false._lk              !< True when '= init' / '=> null()' present.
    type(cdd_ident_t) :: default_val                     !< Extracted default initializer lexeme.
    logical(lk) :: is_deleted = .false._lk               !< Surgical patch flag: omit node tokens.
    logical(lk) :: is_replaced = .false._lk              !< Surgical patch flag: emit rep slice.
    logical(lk) :: keep_lead_trivia = .false._lk         !< Preserve first_tok leading trivia on rep.
    logical(lk) :: keep_trail_trivia = .false._lk        !< Preserve last_tok trailing trivia on rep.
    integer(i32) :: rep_off = 1_i32                      !< Replacement start offset in patch_pool.
    integer(i32) :: rep_len = 0_i32                      !< Replacement byte length in patch_pool.
    integer(i32) :: before_off = 1_i32                   !< Inserted prefix start in patch_pool.
    integer(i32) :: before_len = 0_i32                   !< Inserted prefix length in patch_pool.
    integer(i32) :: after_off = 1_i32                    !< Inserted suffix start in patch_pool.
    integer(i32) :: after_len = 0_i32                    !< Inserted suffix length in patch_pool.
  end type cdd_cst_node_t

  !> @brief Complete lossless Concrete Syntax Tree arena for a Fortran compilation unit.
  type :: cdd_cst_tree_t
    type(cdd_cst_token_stream_t) :: stream               !< Underlying lossless token & trivia arena.
    type(cdd_buffer_t) :: patch_pool                     !< Synthesized code pool for surgical edits.
    integer(i32) :: root = 0_i32                         !< Root CST_FILE node index (1 when parsed).
    integer(i32) :: node_count = 0_i32                   !< Active CST nodes in nodes(1:node_count).
    integer(i32) :: max_nodes = CDD_MAX_CST_NODES        !< Configurable CST node limit for testing.
    type(cdd_cst_node_t) :: nodes(CDD_MAX_CST_NODES)     !< Flat CST node arena.
  end type cdd_cst_tree_t

contains

  !> @brief Initialize a single cdd_cst_node_t to clean defaults.
  !> @param[out] node CST node to reset.
  pure subroutine cst_node_init(node)
    type(cdd_cst_node_t), intent(out) :: node
    integer(i32) :: k

    node%kind = CST_OPAQUE_STMT
    node%parent = 0_i32
    node%first_tok = 1_i32
    node%last_tok = 0_i32
    node%header_end_tok = 0_i32
    node%end_stmt_node = 0_i32
    call ident_clear(node%name)
    call ident_clear(node%aux_name)
    node%type_kind = IR_INT32
    node%char_len = 0_i32
    node%rank = 0_i32
    do k = 1_i32, CDD_NODE_DIMS_CACHE
      call ir_dim_init(node%dims(k))
    end do
    node%intent_kind = CST_INTENT_NONE
    node%is_allocatable = .false._lk
    node%is_pointer = .false._lk
    node%has_extends = .false._lk
    node%is_bind_c = .false._lk
    node%has_double_colon = .false._lk
    node%has_default = .false._lk
    call ident_clear(node%default_val)
    node%is_deleted = .false._lk
    node%is_replaced = .false._lk
    node%keep_lead_trivia = .false._lk
    node%keep_trail_trivia = .false._lk
    node%rep_off = 1_i32
    node%rep_len = 0_i32
    node%before_off = 1_i32
    node%before_len = 0_i32
    node%after_off = 1_i32
    node%after_len = 0_i32
  end subroutine cst_node_init

  !> @brief Initialize an empty cdd_cst_tree_t arena.
  !> @param[out] tree CST tree to reset.
  pure subroutine cst_tree_init(tree)
    type(cdd_cst_tree_t), intent(out) :: tree

    call fstream_init(tree%stream)
    call buf_init(tree%patch_pool)
    tree%root = 0_i32
    tree%node_count = 0_i32
    tree%max_nodes = CDD_MAX_CST_NODES
  end subroutine cst_tree_init

  !> @brief Allocate and initialize a new CST node in tree%nodes.
  !> @param[inout] tree      Target CST tree.
  !> @param[in]    kind      Node category (CST_*).
  !> @param[in]    parent    Parent node index.
  !> @param[in]    first_tok 1-based start token index.
  !> @param[in]    last_tok  1-based end token index.
  !> @param[out]   node_idx  Allocated 1-based node index.
  !> @param[inout] err       Error status record.
  pure subroutine cst_add_node(tree, kind, parent, first_tok, last_tok, node_idx, err)
    type(cdd_cst_tree_t), intent(inout) :: tree
    integer(i32), intent(in) :: kind
    integer(i32), intent(in) :: parent
    integer(i32), intent(in) :: first_tok
    integer(i32), intent(in) :: last_tok
    integer(i32), intent(out) :: node_idx
    type(cdd_error_t), intent(inout) :: err

    node_idx = 0_i32
    if (err%code /= 0_i32) return
    if (tree%node_count >= tree%max_nodes) then
      call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "Fortran CST node arena full")
      return
    end if
    tree%node_count = tree%node_count + 1_i32
    node_idx = tree%node_count
    call cst_node_init(tree%nodes(node_idx))
    tree%nodes(node_idx)%kind = kind
    tree%nodes(node_idx)%parent = parent
    tree%nodes(node_idx)%first_tok = first_tok
    tree%nodes(node_idx)%last_tok = last_tok
  end subroutine cst_add_node

  !> @brief Mark a CST node for surgical deletion during printing.
  !> @param[inout] tree     Target CST tree.
  !> @param[in]    node_idx 1-based node index to excise.
  pure subroutine cst_mark_deleted(tree, node_idx)
    type(cdd_cst_tree_t), intent(inout) :: tree
    integer(i32), intent(in) :: node_idx

    tree%nodes(node_idx)%is_deleted = .true._lk
  end subroutine cst_mark_deleted

  !> @brief Surgically replace node_idx's body with new_text while optionally keeping outer trivia.
  !> @param[inout] tree       Target CST tree.
  !> @param[in]    node_idx   1-based node index to replace.
  !> @param[in]    new_text   Replacement Fortran fragment.
  !> @param[in]    keep_lead  True to retain first_tok's leading comments/blank lines/indent.
  !> @param[in]    keep_trail True to retain last_tok's inline comment/card ID/newline.
  !> @param[inout] err        Error status record.
  pure subroutine cst_replace_node(tree, node_idx, new_text, keep_lead, keep_trail, err)
    type(cdd_cst_tree_t), intent(inout) :: tree
    integer(i32), intent(in) :: node_idx
    character(len=*, kind=c_k), intent(in) :: new_text
    logical(lk), intent(in) :: keep_lead
    logical(lk), intent(in) :: keep_trail
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: off, slen

    off = tree%patch_pool%len + 1_i32
    slen = len(new_text, kind=i32)
    call buf_append_str(tree%patch_pool, new_text, err)
    if (err%code /= 0_i32) return
    tree%nodes(node_idx)%is_replaced = .true._lk
    tree%nodes(node_idx)%keep_lead_trivia = keep_lead
    tree%nodes(node_idx)%keep_trail_trivia = keep_trail
    tree%nodes(node_idx)%rep_off = off
    tree%nodes(node_idx)%rep_len = slen
  end subroutine cst_replace_node

  !> @brief Splice text into patch_pool to be emitted immediately BEFORE node_idx.
  !> @param[inout] tree     Target CST tree.
  !> @param[in]    node_idx 1-based anchor node index.
  !> @param[in]    ins_text Text to insert before node_idx (cumulative if called multiple times).
  !> @param[inout] err      Error status record.
  pure subroutine cst_insert_before(tree, node_idx, ins_text, err)
    type(cdd_cst_tree_t), intent(inout) :: tree
    integer(i32), intent(in) :: node_idx
    character(len=*, kind=c_k), intent(in) :: ins_text
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: old_off, old_len, new_off
    type(cdd_buffer_t) :: combined

    call buf_init(combined)
    old_off = tree%nodes(node_idx)%before_off
    old_len = tree%nodes(node_idx)%before_len
    if (old_len > 0_i32) then
      call buf_append_str(combined, tree%patch_pool%raw(old_off:old_off + old_len - 1_i32), err)
    end if
    call buf_append_str(combined, ins_text, err)
    new_off = tree%patch_pool%len + 1_i32
    call buf_append_str(tree%patch_pool, combined%raw(1:combined%len), err)
    if (err%code /= 0_i32) return
    tree%nodes(node_idx)%before_off = new_off
    tree%nodes(node_idx)%before_len = combined%len
  end subroutine cst_insert_before

  !> @brief Splice text into patch_pool to be emitted immediately AFTER node_idx.
  !> @param[inout] tree     Target CST tree.
  !> @param[in]    node_idx 1-based anchor node index.
  !> @param[in]    ins_text Text to insert after node_idx (cumulative if called multiple times).
  !> @param[inout] err      Error status record.
  pure subroutine cst_insert_after(tree, node_idx, ins_text, err)
    type(cdd_cst_tree_t), intent(inout) :: tree
    integer(i32), intent(in) :: node_idx
    character(len=*, kind=c_k), intent(in) :: ins_text
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: old_off, old_len, new_off
    type(cdd_buffer_t) :: combined

    call buf_init(combined)
    old_off = tree%nodes(node_idx)%after_off
    old_len = tree%nodes(node_idx)%after_len
    if (old_len > 0_i32) then
      call buf_append_str(combined, tree%patch_pool%raw(old_off:old_off + old_len - 1_i32), err)
    end if
    call buf_append_str(combined, ins_text, err)
    new_off = tree%patch_pool%len + 1_i32
    call buf_append_str(tree%patch_pool, combined%raw(1:combined%len), err)
    if (err%code /= 0_i32) return
    tree%nodes(node_idx)%after_off = new_off
    tree%nodes(node_idx)%after_len = combined%len
  end subroutine cst_insert_after

end module cdd_cst_nodes
