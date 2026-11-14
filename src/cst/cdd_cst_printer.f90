!> @file cdd_cst_printer.f90
!> @brief Lossless byte-exact CST round-trip and surgical-patch printer.

!> @brief Emits exact original bytes + trivia for untouched CST nodes and splices patched nodes.
module cdd_cst_printer
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear
  use cdd_string, only: cdd_buffer_t, buf_init, buf_append_str
  use cdd_cst_nodes, only: cdd_cst_tree_t
  implicit none (type, external)
  private

  public :: cst_print, cst_print_to_buf

contains

  !> @brief Serialize a cdd_cst_tree_t into a freshly initialized out_buf.
  !> @param[in]  tree    Parsed (and optionally surgically patched) CST tree.
  !> @param[out] out_buf Reconstructed Fortran source buffer.
  !> @param[out] err     Structured error status on buffer overflow.
  pure subroutine cst_print(tree, out_buf, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(out) :: err

    call buf_init(out_buf)
    call cst_print_to_buf(tree, out_buf, err)
  end subroutine cst_print

  !> @brief Append serialized CST output into an existing cdd_buffer_t (respecting out_buf%cap).
  !> @param[in]    tree    Parsed (and optionally surgically patched) CST tree.
  !> @param[inout] out_buf Destination Fortran source buffer.
  !> @param[out]   err     Structured error status on buffer overflow.
  pure subroutine cst_print_to_buf(tree, out_buf, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: t, n_toks, patch_node, end_t

    call cdd_error_clear(err)
    n_toks = tree%stream%token_count
    t = 1_i32

    do while (t <= n_toks)
      call emit_before_insertions(tree, t, out_buf, err)
      if (err%code /= 0_i32) return

      patch_node = find_patched_span_at(tree, t)
      if (patch_node > 0_i32) then
        end_t = tree%nodes(patch_node)%last_tok
        if (tree%nodes(patch_node)%is_replaced) then
          if (tree%nodes(patch_node)%keep_lead_trivia) then
            call emit_token_lead_trivia(tree, t, out_buf, err)
          end if
          call buf_append_str(out_buf, tree%patch_pool%raw( &
            tree%nodes(patch_node)%rep_off: &
            tree%nodes(patch_node)%rep_off + tree%nodes(patch_node)%rep_len - 1_i32), err)
          if (tree%nodes(patch_node)%keep_trail_trivia) then
            call emit_token_trail_trivia(tree, end_t, out_buf, err)
          end if
        end if
        call emit_after_insertions_for_node(tree, patch_node, out_buf, err)
        t = end_t + 1_i32
      else
        call emit_token_verbatim(tree, t, out_buf, err)
        call emit_after_insertions_at_end(tree, t, out_buf, err)
        t = t + 1_i32
      end if
      if (err%code /= 0_i32) return
    end do
  end subroutine cst_print_to_buf

  !> @brief Find outermost deleted or replaced node starting at token tok_idx (0 if none).
  !> @param[in] tree    CST tree.
  !> @param[in] tok_idx 1-based token index.
  !> @return Patched node index or 0.
  pure function find_patched_span_at(tree, tok_idx) result(nidx)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: tok_idx
    integer(i32) :: nidx
    integer(i32) :: i

    nidx = 0_i32
    do i = 2_i32, tree%node_count
      if (tree%nodes(i)%first_tok == tok_idx .and. tree%nodes(i)%last_tok >= tok_idx) then
        if (tree%nodes(i)%is_deleted .or. tree%nodes(i)%is_replaced) then
          nidx = i
          return
        end if
      end if
    end do
  end function find_patched_span_at

  !> @brief Emit any `before` patch slices anchored to nodes whose first_tok == tok_idx.
  !> @param[in]    tree    CST tree.
  !> @param[in]    tok_idx Current token index.
  !> @param[inout] out_buf Output buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_before_insertions(tree, tok_idx, out_buf, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: tok_idx
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: i, off, slen

    do i = 1_i32, tree%node_count
      if (tree%nodes(i)%first_tok == tok_idx .and. tree%nodes(i)%before_len > 0_i32) then
        off = tree%nodes(i)%before_off
        slen = tree%nodes(i)%before_len
        call buf_append_str(out_buf, tree%patch_pool%raw(off:off + slen - 1_i32), err)
      end if
    end do
  end subroutine emit_before_insertions

  !> @brief Emit `after` patch slice attached directly to a deleted/replaced node.
  !> @param[in]    tree     CST tree.
  !> @param[in]    node_idx Patched node index.
  !> @param[inout] out_buf  Output buffer.
  !> @param[inout] err      Error status.
  pure subroutine emit_after_insertions_for_node(tree, node_idx, out_buf, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: node_idx
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: off, slen

    if (tree%nodes(node_idx)%after_len > 0_i32) then
      off = tree%nodes(node_idx)%after_off
      slen = tree%nodes(node_idx)%after_len
      call buf_append_str(out_buf, tree%patch_pool%raw(off:off + slen - 1_i32), err)
    end if
  end subroutine emit_after_insertions_for_node

  !> @brief Emit any `after` patch slices anchored to untouched nodes ending at tok_idx.
  !> @param[in]    tree    CST tree.
  !> @param[in]    tok_idx Current token index.
  !> @param[inout] out_buf Output buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_after_insertions_at_end(tree, tok_idx, out_buf, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: tok_idx
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: i, off, slen

    do i = 1_i32, tree%node_count
      if (.not. tree%nodes(i)%is_deleted .and. .not. tree%nodes(i)%is_replaced) then
        if (tree%nodes(i)%last_tok == tok_idx .and. tree%nodes(i)%after_len > 0_i32) then
          off = tree%nodes(i)%after_off
          slen = tree%nodes(i)%after_len
          call buf_append_str(out_buf, tree%patch_pool%raw(off:off + slen - 1_i32), err)
        end if
      end if
    end do
  end subroutine emit_after_insertions_at_end

  !> @brief Emit leading_trivia + raw_lexeme + trailing_trivia of tok_idx verbatim.
  !> @param[in]    tree    CST tree.
  !> @param[in]    tok_idx Token index.
  !> @param[inout] out_buf Output buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_token_verbatim(tree, tok_idx, out_buf, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: tok_idx
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: off, slen

    call emit_token_lead_trivia(tree, tok_idx, out_buf, err)
    off = tree%stream%tokens(tok_idx)%raw_off
    slen = tree%stream%tokens(tok_idx)%raw_len
    if (slen > 0_i32) then
      call buf_append_str(out_buf, tree%stream%source_buf%raw(off:off + slen - 1_i32), err)
    end if
    call emit_token_trail_trivia(tree, tok_idx, out_buf, err)
  end subroutine emit_token_verbatim

  !> @brief Emit all leading trivia pieces of tok_idx.
  !> @param[in]    tree    CST tree.
  !> @param[in]    tok_idx Token index.
  !> @param[inout] out_buf Output buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_token_lead_trivia(tree, tok_idx, out_buf, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: tok_idx
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: k, idx, off, slen

    do k = 0_i32, tree%stream%tokens(tok_idx)%lead_count - 1_i32
      idx = tree%stream%tokens(tok_idx)%lead_first + k
      off = tree%stream%trivia(idx)%src_off
      slen = tree%stream%trivia(idx)%src_len
      call buf_append_str(out_buf, tree%stream%source_buf%raw(off:off + slen - 1_i32), err)
    end do
  end subroutine emit_token_lead_trivia

  !> @brief Emit all trailing trivia pieces of tok_idx.
  !> @param[in]    tree    CST tree.
  !> @param[in]    tok_idx Token index.
  !> @param[inout] out_buf Output buffer.
  !> @param[inout] err     Error status.
  pure subroutine emit_token_trail_trivia(tree, tok_idx, out_buf, err)
    type(cdd_cst_tree_t), intent(in) :: tree
    integer(i32), intent(in) :: tok_idx
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: k, idx, off, slen

    do k = 0_i32, tree%stream%tokens(tok_idx)%trail_count - 1_i32
      idx = tree%stream%tokens(tok_idx)%trail_first + k
      off = tree%stream%trivia(idx)%src_off
      slen = tree%stream%trivia(idx)%src_len
      call buf_append_str(out_buf, tree%stream%source_buf%raw(off:off + slen - 1_i32), err)
    end do
  end subroutine emit_token_trail_trivia

end module cdd_cst_printer
