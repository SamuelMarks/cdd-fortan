!> @file cdd_json_parser.f90
!> @brief Recursive-descent RFC 8259 JSON parser populating cdd_json_doc_t.

!> @brief Strongly-typed JSON DOM parser with configurable nesting limit and duplicate-key check.
module cdd_json_parser
  use cdd_kinds, only: i32, i64, r64, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set, CDD_ERR_JSON_PARSE
  use cdd_string, only: cdd_buffer_t, buf_init, unescape_json_to_buf, &
    parse_i64_strict, parse_r64_strict
  use cdd_json_tokens, only: JTOK_EOF, JTOK_LBRACE, JTOK_RBRACE, JTOK_LBRACKET, JTOK_RBRACKET, &
    JTOK_COLON, JTOK_COMMA, JTOK_STRING, JTOK_INTEGER, JTOK_FLOAT, JTOK_TRUE, JTOK_FALSE, &
    JTOK_NULL, cdd_json_token_t, cdd_json_token_stream_t
  use cdd_json_lexer, only: json_lex
  use cdd_json_dom, only: cdd_json_doc_t, jdoc_init, jdoc_add_null, jdoc_add_bool, &
    jdoc_add_int, jdoc_add_float, jdoc_add_string, jdoc_add_array, jdoc_add_object, &
    jdoc_array_append, jdoc_object_set
  implicit none (type, external)
  private

  public :: CDD_DEFAULT_JSON_MAX_DEPTH
  public :: json_parse, json_parse_opts, json_parse_stream

  integer(i32), parameter :: CDD_DEFAULT_JSON_MAX_DEPTH = 128_i32 !< Default max container depth.

contains

  !> @brief Parse an RFC 8259 JSON string into a fresh cdd_json_doc_t with default settings.
  !> @param[in]  source Input JSON text.
  !> @param[out] doc    Populated JSON DOM document.
  !> @param[out] err    Structured error status.
  pure subroutine json_parse(source, doc, err)
    character(len=*, kind=c_k), intent(in) :: source
    type(cdd_json_doc_t), intent(out) :: doc
    type(cdd_error_t), intent(out) :: err

    call json_parse_opts(source, CDD_DEFAULT_JSON_MAX_DEPTH, .true._lk, doc, err)
  end subroutine json_parse

  !> @brief Parse an RFC 8259 JSON string with configurable depth limit and duplicate-key policy.
  !> @param[in]  source          Input JSON text.
  !> @param[in]  max_depth       Maximum allowed nesting depth (>= 1).
  !> @param[in]  reject_dup_keys True to reject duplicate object keys with CDD_ERR_JSON_PARSE.
  !> @param[out] doc             Populated JSON DOM document.
  !> @param[out] err             Structured error status.
  pure subroutine json_parse_opts(source, max_depth, reject_dup_keys, doc, err)
    character(len=*, kind=c_k), intent(in) :: source
    integer(i32), intent(in) :: max_depth
    logical(lk), intent(in) :: reject_dup_keys
    type(cdd_json_doc_t), intent(out) :: doc
    type(cdd_error_t), intent(out) :: err
    type(cdd_json_token_stream_t) :: stream

    call jdoc_init(doc)
    call json_lex(source, stream, err)
    if (err%code /= 0_i32) return
    call json_parse_stream(source, stream, max_depth, reject_dup_keys, doc, err)
  end subroutine json_parse_opts

  !> @brief Parse an already-tokenized JSON stream into an existing/initialized cdd_json_doc_t.
  !> @param[in]    source          Original JSON source text referenced by token spans.
  !> @param[in]    stream          Token stream ending with JTOK_EOF.
  !> @param[in]    max_depth       Maximum allowed nesting depth.
  !> @param[in]    reject_dup_keys True to reject duplicate keys in objects.
  !> @param[inout] doc             Target JSON DOM arena (allows pre-bounded arenas in tests).
  !> @param[out]   err             Structured error status.
  pure subroutine json_parse_stream(source, stream, max_depth, reject_dup_keys, doc, err)
    character(len=*, kind=c_k), intent(in) :: source
    type(cdd_json_token_stream_t), intent(in) :: stream
    integer(i32), intent(in) :: max_depth
    logical(lk), intent(in) :: reject_dup_keys
    type(cdd_json_doc_t), intent(inout) :: doc
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: cursor, root_idx
    type(cdd_json_token_t) :: trailing_tok

    call cdd_error_clear(err)
    cursor = 1_i32
    call parse_value(source, stream, cursor, 1_i32, max_depth, reject_dup_keys, doc, root_idx, err)
    if (err%code /= 0_i32) return

    trailing_tok = stream%tokens(cursor)
    if (trailing_tok%kind /= JTOK_EOF) then
      call set_tok_err(err, trailing_tok, "Unexpected trailing token after JSON root value")
      return
    end if
    doc%root = root_idx
  end subroutine json_parse_stream

  !> @brief Recursively parse a single JSON value at stream%tokens(cursor).
  !> @param[in]    source          Original JSON text.
  !> @param[in]    stream          Token stream.
  !> @param[inout] cursor          1-based token index (advanced past parsed value).
  !> @param[in]    depth           Current 1-based container depth.
  !> @param[in]    max_depth       Maximum permitted nesting depth.
  !> @param[in]    reject_dup_keys Duplicate key policy.
  !> @param[inout] doc             Target JSON DOM arena.
  !> @param[out]   node_idx        Allocated node index.
  !> @param[out]   err             Error status record.
  recursive pure subroutine parse_value(source, stream, cursor, depth, max_depth, &
      reject_dup_keys, doc, node_idx, err)
    character(len=*, kind=c_k), intent(in) :: source
    type(cdd_json_token_stream_t), intent(in) :: stream
    integer(i32), intent(inout) :: cursor
    integer(i32), intent(in) :: depth
    integer(i32), intent(in) :: max_depth
    logical(lk), intent(in) :: reject_dup_keys
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(out) :: node_idx
    type(cdd_error_t), intent(out) :: err
    type(cdd_json_token_t) :: tok
    type(cdd_buffer_t) :: decoded
    integer(i64) :: ival
    real(r64) :: rval

    call cdd_error_clear(err)
    node_idx = 0_i32
    tok = stream%tokens(cursor)

    select case (tok%kind)
    case (JTOK_NULL)
      cursor = cursor + 1_i32
      call jdoc_add_null(doc, node_idx, err)
    case (JTOK_TRUE)
      cursor = cursor + 1_i32
      call jdoc_add_bool(doc, .true._lk, node_idx, err)
    case (JTOK_FALSE)
      cursor = cursor + 1_i32
      call jdoc_add_bool(doc, .false._lk, node_idx, err)
    case (JTOK_STRING)
      cursor = cursor + 1_i32
      call decode_string_tok(source, tok, decoded, err)
      call jdoc_add_string(doc, decoded%raw(1:decoded%len), node_idx, err)
    case (JTOK_INTEGER)
      cursor = cursor + 1_i32
      call parse_i64_strict(source(tok%span_start:tok%span_end), ival, err)
      if (err%code /= 0_i32) then
        call parse_r64_strict(source(tok%span_start:tok%span_end), rval, err)
        call jdoc_add_float(doc, rval, source(tok%span_start:tok%span_end), node_idx, err)
      else
        call jdoc_add_int(doc, ival, node_idx, err)
      end if
    case (JTOK_FLOAT)
      cursor = cursor + 1_i32
      call parse_r64_strict(source(tok%span_start:tok%span_end), rval, err)
      call jdoc_add_float(doc, rval, source(tok%span_start:tok%span_end), node_idx, err)
    case (JTOK_LBRACKET)
      if (depth > max_depth) then
        call set_tok_err(err, tok, "JSON nesting depth limit exceeded")
        return
      end if
      call parse_array(source, stream, cursor, depth, max_depth, reject_dup_keys, doc, node_idx, err)
    case (JTOK_LBRACE)
      if (depth > max_depth) then
        call set_tok_err(err, tok, "JSON nesting depth limit exceeded")
        return
      end if
      call parse_object(source, stream, cursor, depth, max_depth, reject_dup_keys, doc, node_idx, err)
    case default
      call set_tok_err(err, tok, "Expected JSON value")
    end select
  end subroutine parse_value

  !> @brief Parse a JSON array '[ ... ]' starting at cursor.
  !> @param[in]    source          Original JSON text.
  !> @param[in]    stream          Token stream.
  !> @param[inout] cursor          Token cursor.
  !> @param[in]    depth           Current depth.
  !> @param[in]    max_depth       Max depth.
  !> @param[in]    reject_dup_keys Duplicate key policy.
  !> @param[inout] doc             Target JSON DOM arena.
  !> @param[out]   arr_idx         Allocated array node index.
  !> @param[out]   err             Error status record.
  recursive pure subroutine parse_array(source, stream, cursor, depth, max_depth, &
      reject_dup_keys, doc, arr_idx, err)
    character(len=*, kind=c_k), intent(in) :: source
    type(cdd_json_token_stream_t), intent(in) :: stream
    integer(i32), intent(inout) :: cursor
    integer(i32), intent(in) :: depth
    integer(i32), intent(in) :: max_depth
    logical(lk), intent(in) :: reject_dup_keys
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(out) :: arr_idx
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: item_node
    type(cdd_json_token_t) :: tok

    cursor = cursor + 1_i32
    call jdoc_add_array(doc, arr_idx, err)
    if (err%code /= 0_i32) return

    if (stream%tokens(cursor)%kind == JTOK_RBRACKET) then
      cursor = cursor + 1_i32
      return
    end if

    do
      call parse_value(source, stream, cursor, depth + 1_i32, max_depth, &
        reject_dup_keys, doc, item_node, err)
      if (err%code /= 0_i32) return
      call jdoc_array_append(doc, arr_idx, item_node, err)
      if (err%code /= 0_i32) return

      tok = stream%tokens(cursor)
      if (tok%kind == JTOK_RBRACKET) then
        cursor = cursor + 1_i32
        return
      else if (tok%kind == JTOK_COMMA) then
        cursor = cursor + 1_i32
        if (stream%tokens(cursor)%kind == JTOK_RBRACKET) then
          call set_tok_err(err, stream%tokens(cursor), "Trailing comma in JSON array")
          return
        end if
      else
        call set_tok_err(err, tok, "Expected ',' or ']' in JSON array")
        return
      end if
    end do
  end subroutine parse_array

  !> @brief Parse a JSON object '{ ... }' starting at cursor.
  !> @param[in]    source          Original JSON text.
  !> @param[in]    stream          Token stream.
  !> @param[inout] cursor          Token cursor.
  !> @param[in]    depth           Current depth.
  !> @param[in]    max_depth       Max depth.
  !> @param[in]    reject_dup_keys Duplicate key policy.
  !> @param[inout] doc             Target JSON DOM arena.
  !> @param[out]   obj_idx         Allocated object node index.
  !> @param[out]   err             Error status record.
  recursive pure subroutine parse_object(source, stream, cursor, depth, max_depth, &
      reject_dup_keys, doc, obj_idx, err)
    character(len=*, kind=c_k), intent(in) :: source
    type(cdd_json_token_stream_t), intent(in) :: stream
    integer(i32), intent(inout) :: cursor
    integer(i32), intent(in) :: depth
    integer(i32), intent(in) :: max_depth
    logical(lk), intent(in) :: reject_dup_keys
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(out) :: obj_idx
    type(cdd_error_t), intent(out) :: err
    type(cdd_json_token_t) :: key_tok, sep_tok
    type(cdd_buffer_t) :: key_buf
    integer(i32) :: val_node

    cursor = cursor + 1_i32
    call jdoc_add_object(doc, obj_idx, err)
    if (err%code /= 0_i32) return

    if (stream%tokens(cursor)%kind == JTOK_RBRACE) then
      cursor = cursor + 1_i32
      return
    end if

    do
      key_tok = stream%tokens(cursor)
      if (key_tok%kind /= JTOK_STRING) then
        call set_tok_err(err, key_tok, "Expected double-quoted property key in JSON object")
        return
      end if
      cursor = cursor + 1_i32
      call decode_string_tok(source, key_tok, key_buf, err)

      sep_tok = stream%tokens(cursor)
      if (sep_tok%kind /= JTOK_COLON) then
        call set_tok_err(err, sep_tok, "Expected ':' after property key in JSON object")
        return
      end if
      cursor = cursor + 1_i32

      call parse_value(source, stream, cursor, depth + 1_i32, max_depth, &
        reject_dup_keys, doc, val_node, err)
      if (err%code /= 0_i32) return

      call jdoc_object_set(doc, obj_idx, key_buf%raw(1:key_buf%len), val_node, &
        reject_dup_keys, err)
      if (err%code /= 0_i32) then
        err%line = key_tok%line
        err%column = key_tok%column
        return
      end if

      sep_tok = stream%tokens(cursor)
      if (sep_tok%kind == JTOK_RBRACE) then
        cursor = cursor + 1_i32
        return
      else if (sep_tok%kind == JTOK_COMMA) then
        cursor = cursor + 1_i32
        if (stream%tokens(cursor)%kind == JTOK_RBRACE) then
          call set_tok_err(err, stream%tokens(cursor), "Trailing comma in JSON object")
          return
        end if
      else
        call set_tok_err(err, sep_tok, "Expected ',' or '}' in JSON object")
        return
      end if
    end do
  end subroutine parse_object

  !> @brief Decode an already-validated JTOK_STRING token interior into out_buf.
  !> @param[in]  source  Original JSON text.
  !> @param[in]  tok     Validated JTOK_STRING token.
  !> @param[out] out_buf Decoded string buffer.
  !> @param[out] err     Error status record.
  pure subroutine decode_string_tok(source, tok, out_buf, err)
    character(len=*, kind=c_k), intent(in) :: source
    type(cdd_json_token_t), intent(in) :: tok
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(out) :: err

    call buf_init(out_buf)
    call cdd_error_clear(err)
    if (tok%span_end >= tok%span_start) then
      call unescape_json_to_buf(source(tok%span_start:tok%span_end), out_buf, err)
    end if
  end subroutine decode_string_tok

  !> @brief Populate CDD_ERR_JSON_PARSE with exact token span metadata.
  !> @param[out] err Error record to populate.
  !> @param[in]  tok Offending token.
  !> @param[in]  msg Human-readable syntax message.
  pure subroutine set_tok_err(err, tok, msg)
    type(cdd_error_t), intent(out) :: err
    type(cdd_json_token_t), intent(in) :: tok
    character(len=*, kind=c_k), intent(in) :: msg

    call cdd_error_set(err, CDD_ERR_JSON_PARSE, msg, tok%line, tok%column, &
      tok%span_start, tok%span_end, "")
  end subroutine set_tok_err

end module cdd_json_parser
