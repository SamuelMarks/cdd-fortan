!> @file cdd_json_writer.f90
!> @brief Deterministic RFC 8259 JSON serializer supporting pretty-printed and compact modes.

!> @brief Pure JSON DOM-to-buffer serializer preserving object insertion order.
module cdd_json_writer
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set_simple, CDD_ERR_INVALID_OPTION
  use cdd_string, only: cdd_buffer_t, buf_init, buf_append_char, buf_append_str, &
    buf_append_i64, escape_json_to_buf, format_r64_to_str
  use cdd_json_dom, only: JVAL_NULL, JVAL_BOOL, JVAL_INT, JVAL_FLOAT, JVAL_STRING, &
    JVAL_ARRAY, JVAL_OBJECT, cdd_json_doc_t, jdoc_node_str_bounds, jdoc_array_len, &
    jdoc_array_item, jdoc_object_len, jdoc_object_entry
  implicit none (type, external)
  private

  public :: json_write, json_write_to_buf

contains

  !> @brief Serialize doc%root into a newly initialized cdd_buffer_t.
  !> @param[in]  doc           JSON document arena to serialize.
  !> @param[in]  indent_spaces Spaces per indentation level when compact is false (>= 0).
  !> @param[in]  compact       True for single-line compact JSON, false for pretty multi-line JSON.
  !> @param[out] out_buf       Destination buffer receiving RFC 8259 JSON text.
  !> @param[out] err           Error status if doc%root is invalid or buffer overflows.
  pure subroutine json_write(doc, indent_spaces, compact, out_buf, err)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: indent_spaces
    logical(lk), intent(in) :: compact
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(out) :: err

    call buf_init(out_buf)
    call json_write_to_buf(doc, indent_spaces, compact, out_buf, err)
  end subroutine json_write

  !> @brief Append serialized JSON for doc%root into an existing cdd_buffer_t.
  !> @param[in]    doc           JSON document arena.
  !> @param[in]    indent_spaces Spaces per indentation level (>= 0).
  !> @param[in]    compact       Compact mode toggle.
  !> @param[inout] out_buf       Target buffer (retains existing cap).
  !> @param[out]   err           Error status record.
  pure subroutine json_write_to_buf(doc, indent_spaces, compact, out_buf, err)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: indent_spaces
    logical(lk), intent(in) :: compact
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(out) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call cdd_error_clear(err)
    if (doc%root < 1_i32 .or. doc%root > doc%node_count .or. indent_spaces < 0_i32) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Invalid JSON document root or indent")
      return
    end if
    call write_node(doc, doc%root, 0_i32, indent_spaces, compact, out_buf, err)
    if (.not. compact) then
      call buf_append_char(out_buf, LF, err)
    end if
  end subroutine json_write_to_buf

  !> @brief Recursively serialize a single JSON DOM node at depth.
  !> @param[in]    doc           JSON document arena.
  !> @param[in]    node_idx      1-based node index.
  !> @param[in]    depth         0-based indentation depth.
  !> @param[in]    indent_spaces Spaces per level.
  !> @param[in]    compact       Compact mode flag.
  !> @param[inout] out_buf       Destination buffer.
  !> @param[inout] err           Error status record.
  recursive pure subroutine write_node(doc, node_idx, depth, indent_spaces, compact, out_buf, err)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: node_idx
    integer(i32), intent(in) :: depth
    integer(i32), intent(in) :: indent_spaces
    logical(lk), intent(in) :: compact
    type(cdd_buffer_t), intent(inout) :: out_buf
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: lo, hi, count, idx, child, klo, khi
    character(len=32, kind=c_k) :: fbuf
    integer(i32) :: flen
    character(len=1, kind=c_k), parameter :: QT = achar(34, kind=c_k)
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    if (err%code /= 0_i32) return

    select case (doc%nodes(node_idx)%kind)
    case (JVAL_NULL)
      call buf_append_str(out_buf, "null", err)
    case (JVAL_BOOL)
      if (doc%nodes(node_idx)%bool_val) then
        call buf_append_str(out_buf, "true", err)
      else
        call buf_append_str(out_buf, "false", err)
      end if
    case (JVAL_INT)
      call buf_append_i64(out_buf, doc%nodes(node_idx)%int_val, err)
    case (JVAL_FLOAT)
      call jdoc_node_str_bounds(doc, node_idx, lo, hi)
      if (hi >= lo) then
        call buf_append_str(out_buf, doc%str_pool%raw(lo:hi), err)
      else
        call format_r64_to_str(doc%nodes(node_idx)%float_val, fbuf, flen)
        call buf_append_str(out_buf, fbuf(1:flen), err)
      end if
    case (JVAL_STRING)
      call jdoc_node_str_bounds(doc, node_idx, lo, hi)
      call buf_append_char(out_buf, QT, err)
      if (hi >= lo) then
        call escape_json_to_buf(doc%str_pool%raw(lo:hi), out_buf, err)
      end if
      call buf_append_char(out_buf, QT, err)
    case (JVAL_ARRAY)
      count = jdoc_array_len(doc, node_idx)
      if (count == 0_i32) then
        call buf_append_str(out_buf, "[]", err)
        return
      end if
      call buf_append_char(out_buf, "[", err)
      do idx = 1_i32, count
        if (idx > 1_i32) then
          call buf_append_char(out_buf, ",", err)
        end if
        if (.not. compact) then
          call buf_append_char(out_buf, LF, err)
          call emit_spaces(out_buf, (depth + 1_i32) * indent_spaces, err)
        end if
        child = jdoc_array_item(doc, node_idx, idx)
        call write_node(doc, child, depth + 1_i32, indent_spaces, compact, out_buf, err)
      end do
      if (.not. compact) then
        call buf_append_char(out_buf, LF, err)
        call emit_spaces(out_buf, depth * indent_spaces, err)
      end if
      call buf_append_char(out_buf, "]", err)
    case default
      ! JVAL_OBJECT
      count = jdoc_object_len(doc, node_idx)
      if (count == 0_i32) then
        call buf_append_str(out_buf, "{}", err)
        return
      end if
      call buf_append_char(out_buf, "{", err)
      do idx = 1_i32, count
        if (idx > 1_i32) then
          call buf_append_char(out_buf, ",", err)
        end if
        if (.not. compact) then
          call buf_append_char(out_buf, LF, err)
          call emit_spaces(out_buf, (depth + 1_i32) * indent_spaces, err)
        end if
        call jdoc_object_entry(doc, node_idx, idx, klo, khi, child)
        call buf_append_char(out_buf, QT, err)
        if (khi >= klo) then
          call escape_json_to_buf(doc%str_pool%raw(klo:khi), out_buf, err)
        end if
        if (compact) then
          call buf_append_str(out_buf, '":', err)
        else
          call buf_append_str(out_buf, '": ', err)
        end if
        call write_node(doc, child, depth + 1_i32, indent_spaces, compact, out_buf, err)
      end do
      if (.not. compact) then
        call buf_append_char(out_buf, LF, err)
        call emit_spaces(out_buf, depth * indent_spaces, err)
      end if
      call buf_append_char(out_buf, "}", err)
    end select
  end subroutine write_node

  !> @brief Append count ASCII space characters to out_buf.
  !> @param[inout] out_buf Target buffer.
  !> @param[in]    count   Number of spaces (>= 0).
  !> @param[inout] err     Error status record.
  pure subroutine emit_spaces(out_buf, count, err)
    type(cdd_buffer_t), intent(inout) :: out_buf
    integer(i32), intent(in) :: count
    type(cdd_error_t), intent(inout) :: err
    integer(i32) :: k

    do k = 1_i32, count
      call buf_append_char(out_buf, " ", err)
    end do
  end subroutine emit_spaces

end module cdd_json_writer
