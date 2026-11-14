!> @file test_json_suite.f90
!> @brief Comprehensive unit tests for JSON lexer, DOM, parser, writer, schema mapper, and inferencer.

!> @brief Execute all Milestone 3 and Milestone 4.3-4.4 JSON engine and schema/infer unit tests.
!> @return Program exit status (0 on pass).
program test_json_suite
  use cdd_kinds, only: i32, i64, r64, lk, c_k
  use cdd_errors, only: CDD_OK, CDD_ERR_JSON_LEX, CDD_ERR_JSON_PARSE, CDD_ERR_JSON_SCHEMA, &
    CDD_ERR_JSON_INFER, CDD_ERR_DIALECT_CONSTRAINT, CDD_ERR_BUFFER_OVERFLOW, &
    CDD_ERR_INVALID_OPTION, cdd_error_t, cdd_error_clear
  use cdd_string, only: cdd_buffer_t, buf_init_bounded, buf_equals, str_set
  use cdd_standard, only: STD_FORTRAN_I, STD_FORTRAN_66, STD_FORTRAN_90, STD_FORTRAN_2003, &
    cdd_options_t, cdd_default_options
  use cdd_json_tokens, only: CDD_MAX_JTOKENS, cdd_json_token_t, cdd_json_token_stream_t, &
    jstream_init
  use cdd_json_lexer, only: json_lex, json_lex_bounded
  use cdd_json_dom, only: CDD_MAX_JNODES, CDD_MAX_JPROPS, CDD_MAX_JITEMS, JVAL_INT, &
    cdd_json_node_t, cdd_json_prop_t, cdd_json_item_t, cdd_json_doc_t, jdoc_init, &
    jdoc_init_bounded, jdoc_add_null, jdoc_add_bool, jdoc_add_int, jdoc_add_float, &
    jdoc_add_string, jdoc_add_array, jdoc_add_object, jdoc_array_append, jdoc_object_set, &
    jdoc_get_key, jdoc_has_key, jdoc_node_str_bounds, jdoc_array_len, jdoc_array_item, &
    jdoc_object_len, jdoc_object_entry
  use cdd_json_parser, only: json_parse, json_parse_opts, json_parse_stream
  use cdd_json_writer, only: json_write, json_write_to_buf
  use cdd_ir_types, only: IR_INT64, IR_REAL64, IR_ENUM, IR_NESTED_UDT, &
    cdd_ir_field_t, cdd_ir_schema_bundle_t, ident_set, ir_field_init, ir_bundle_init, &
    ir_bundle_add_udt, ir_udt_add_field
  use cdd_json_schema, only: is_json_schema_doc, json_schema_to_ir, ir_to_json_schema
  use cdd_json_infer, only: json_sample_to_ir, json_sample_to_ir_into, ir_to_json_sample
  use test_harness, only: test_state_t, th_init, th_check, th_eq_i32, th_eq_str, &
    th_touch_vtable, th_finish
  implicit none (type, external)

  type(test_state_t) :: st

  call th_init(st)
  call run_lexer_and_dom_tests(st)
  call run_parser_and_writer_tests(st)
  call run_schema_mapper_tests(st)
  call run_sample_infer_tests(st)
  call th_finish(st, "test_json_suite")

contains

  !> @brief Exercise every RFC 8259 lexer rule, error condition, and DOM arena boundary.
  !> @param[inout] st Test state accumulator.
  subroutine run_lexer_and_dom_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_json_token_t) :: dummy_tok
    type(cdd_json_token_stream_t) :: stream
    type(cdd_json_node_t) :: dummy_n
    type(cdd_json_prop_t) :: dummy_p
    type(cdd_json_item_t) :: dummy_i
    type(cdd_json_doc_t) :: doc
    type(cdd_error_t) :: err
    integer(i32) :: n1, n2, n3, lo, hi, vn
    character(len=1, kind=c_k), parameter :: CR = achar(13, kind=c_k)
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=1, kind=c_k), parameter :: TAB = achar(9, kind=c_k)
    character(len=1, kind=c_k), parameter :: BS = achar(92, kind=c_k)

    call th_touch_vtable(dummy_tok)
    call th_touch_vtable(stream)
    call th_touch_vtable(dummy_n)
    call th_touch_vtable(dummy_p)
    call th_touch_vtable(dummy_i)
    call th_touch_vtable(doc)

    call jstream_init(stream)
    call json_lex_bounded("{}", 0_i32, stream, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "lex max_toks < 1")
    call json_lex_bounded("{}", CDD_MAX_JTOKENS + 1_i32, stream, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "lex max_toks > max")
    call json_lex_bounded('{"a":[1,true,false,null]}', 2_i32, stream, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "lex quota overflow")

    ! CRLF, bare CR at EOF, standalone LF, TAB, 0 at EOF, 1e2 unsigned exp, escapes
    call json_lex("{" // CR // LF // TAB // '"k": [0, 1e2, -12.5E-2, "a' // BS // 'nb", "", true, false, null]' // LF // "}" // CR, &
      stream, err)
    call th_eq_i32(st, err%code, CDD_OK, "lex valid rich json")
    call json_lex("0", stream, err)
    call th_eq_i32(st, err%code, CDD_OK, "lex standalone 0")
    call json_lex("{" // CR // "}", stream, err)
    call th_eq_i32(st, err%code, CDD_OK, "lex bare CR before RBRACE")
    call json_lex("{" // CR // "}", stream, err)
    call th_eq_i32(st, err%code, CDD_OK, "lex CR without LF")

    ! Lexer invalid cases
    call check_lex_err(st, '{"a' // LF // '"}', "unescaped LF in string")
    call check_lex_err(st, '{"a' // BS // 'x"}', "invalid escape in string")
    call check_lex_err(st, '"abc' // BS, "trailing backslash at EOF")
    call check_lex_err(st, '"unterminated', "unterminated string")
    call check_lex_err(st, "-", "bare minus")
    call check_lex_err(st, "-x", "minus non-digit")
    call check_lex_err(st, "01", "leading zero")
    call check_lex_err(st, "1.", "trailing dot")
    call check_lex_err(st, "1.x", "non-digit after dot")
    call check_lex_err(st, "1e", "empty exp")
    call check_lex_err(st, "1e+", "sign-only exp")
    call check_lex_err(st, "12abc", "trailing letters on number")
    call check_lex_err(st, "tr", "short true")
    call check_lex_err(st, "truex", "true without delim")
    call check_lex_err(st, "fa", "short false")
    call check_lex_err(st, "falsehood", "false without delim")
    call check_lex_err(st, "nu", "short null")
    call check_lex_err(st, "nullable", "null without delim")
    call check_lex_err(st, "@", "unexpected symbol")

    ! DOM bounds & defensive query branches
    call jdoc_init_bounded(doc, 0_i32, 2_i32, 2_i32, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "dom bad mn")
    call jdoc_init_bounded(doc, 2_i32, 0_i32, 2_i32, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "dom bad mp")
    call jdoc_init_bounded(doc, 2_i32, 2_i32, 0_i32, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "dom bad mi")
    call jdoc_init_bounded(doc, CDD_MAX_JNODES + 1_i32, 2_i32, 2_i32, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "dom mn > max")
    call jdoc_init_bounded(doc, 2_i32, CDD_MAX_JPROPS + 1_i32, 2_i32, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "dom mp > max")
    call jdoc_init_bounded(doc, 2_i32, 2_i32, CDD_MAX_JITEMS + 1_i32, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "dom mi > max")

    call jdoc_init_bounded(doc, 4_i32, 1_i32, 1_i32, err)
    call jdoc_add_object(doc, n1, err)
    call jdoc_add_array(doc, n2, err)
    call jdoc_add_int(doc, 7_i64, n3, err)

    ! Wrong-kind / out-of-range array & object operations
    call jdoc_array_append(doc, 0_i32, n3, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "arr append idx 0")
    call jdoc_array_append(doc, n1, n3, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "arr append on obj")
    call jdoc_array_append(doc, n2, n3, err)
    call jdoc_array_append(doc, n2, n3, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "arr item arena full")

    call jdoc_object_set(doc, 0_i32, "k", n3, .false._lk, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "obj set idx 0")
    call jdoc_object_set(doc, n2, "k", n3, .false._lk, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "obj set on arr")
    call jdoc_object_set(doc, n1, "k1", n3, .false._lk, err)
    ! Update existing key when reject_dup = .false.
    call jdoc_object_set(doc, n1, "k1", n2, .false._lk, err)
    call th_eq_i32(st, jdoc_get_key(doc, n1, "k1"), n2, "obj overwrite key")
    call jdoc_object_set(doc, n1, "k2", n3, .false._lk, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "obj prop arena full")

    call jdoc_add_null(doc, vn, err)
    call jdoc_add_null(doc, vn, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "node arena full")
    call jdoc_add_bool(doc, .true._lk, vn, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "jdoc_add_bool full")
    call jdoc_add_float(doc, 1.0_r64, "1.0", vn, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "jdoc_add_float full")
    call jdoc_add_string(doc, "s", vn, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "jdoc_add_string full")

    ! str_pool overflow on jdoc_add_float, jdoc_add_string, and jdoc_object_set
    call jdoc_init(doc)
    call buf_init_bounded(doc%str_pool, 1_i32, err)
    call jdoc_add_float(doc, 1.5_r64, "1.5", vn, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "jdoc_add_float pool full")
    call jdoc_add_string(doc, "ab", vn, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "jdoc_add_string pool full")
    call jdoc_add_object(doc, n1, err)
    call jdoc_object_set(doc, n1, "ab", n1, .false._lk, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "jdoc_object_set pool full")

    ! Restore n1=obj, n2=arr for query checks
    call jdoc_init(doc)
    call jdoc_add_object(doc, n1, err)
    call jdoc_add_array(doc, n2, err)
    call jdoc_add_int(doc, 7_i64, n3, err)
    call jdoc_object_set(doc, n1, "k1", n2, .false._lk, err)
    call jdoc_object_set(doc, n1, "k2", n3, .false._lk, err)
    call jdoc_array_append(doc, n2, n3, err)
    call jdoc_array_append(doc, n2, n1, err)
    call th_eq_i32(st, jdoc_array_item(doc, n2, 2_i32), n1, "arr_item 2")

    call th_eq_i32(st, jdoc_get_key(doc, 0_i32, "k"), 0_i32, "get_key 0")
    call th_eq_i32(st, jdoc_get_key(doc, n2, "k"), 0_i32, "get_key non-obj")
    call th_check(st, jdoc_has_key(doc, n1, "k1") .and. .not. jdoc_has_key(doc, n1, "zz"), "has_key")
    call jdoc_node_str_bounds(doc, 0_i32, lo, hi)
    call th_check(st, lo > hi, "str_bounds invalid")
    call th_eq_i32(st, jdoc_array_len(doc, 0_i32), 0_i32, "arr_len 0")
    call th_eq_i32(st, jdoc_array_len(doc, n1), 0_i32, "arr_len non-arr")
    call th_eq_i32(st, jdoc_array_item(doc, n2, 0_i32), 0_i32, "arr_item 0")
    call th_eq_i32(st, jdoc_object_len(doc, 0_i32), 0_i32, "obj_len 0")
    call th_eq_i32(st, jdoc_object_len(doc, n2), 0_i32, "obj_len non-obj")
    call jdoc_object_entry(doc, n1, 99_i32, lo, hi, vn)
    call th_eq_i32(st, vn, 0_i32, "obj_entry out of range")
  end subroutine run_lexer_and_dom_tests

  !> @brief Assert that json_lex returns CDD_ERR_JSON_LEX on bad_src.
  !> @param[inout] st      Test state accumulator.
  !> @param[in]    bad_src Malformed JSON input.
  !> @param[in]    label   Assertion label.
  subroutine check_lex_err(st, bad_src, label)
    type(test_state_t), intent(inout) :: st
    character(len=*, kind=c_k), intent(in) :: bad_src
    character(len=*, kind=c_k), intent(in) :: label
    type(cdd_json_token_stream_t) :: s
    type(cdd_error_t) :: err

    call json_lex(bad_src, s, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_LEX, label)
  end subroutine check_lex_err

  !> @brief Exercise JSON parser valid/error paths and pretty/compact writer serialization.
  !> @param[inout] st Test state accumulator.
  subroutine run_parser_and_writer_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_json_doc_t) :: doc
    type(cdd_json_token_stream_t) :: stream
    type(cdd_buffer_t) :: out_pretty, out_compact, tiny_out
    type(cdd_error_t) :: err
    integer(i32) :: fnode

    ! Parse rich document including >64-bit integer fallback to float, empty {}, empty []
    call json_parse( &
      '{"nil":null,"t":true,"f":false,"s":"hi","e":"","i":42,' // &
      '"big":999999999999999999999,"fl":1.25,"ea":[],"eo":{},"arr":[1,2]}', &
      doc, err)
    call th_eq_i32(st, err%code, CDD_OK, "parse rich doc")
    call th_eq_i32(st, doc%nodes(jdoc_get_key(doc, doc%root, "i"))%kind, JVAL_INT, "int kind")

    call json_write(doc, 2_i32, .false._lk, out_pretty, err)
    call th_eq_i32(st, err%code, CDD_OK, "json_write pretty")
    call json_write(doc, 0_i32, .true._lk, out_compact, err)
    call th_eq_i32(st, err%code, CDD_OK, "json_write compact")

    ! Also write synthesized float node (empty lexeme) & empty object key
    call jdoc_init(doc)
    call jdoc_add_object(doc, doc%root, err)
    call jdoc_add_float(doc, 3.5_r64, "", fnode, err)
    call jdoc_object_set(doc, doc%root, "", fnode, .false._lk, err)
    call json_write(doc, 2_i32, .true._lk, out_compact, err)
    call th_check(st, buf_equals(out_compact, '{"":3.5}'), "write empty-lexeme float")

    call json_write(doc, -1_i32, .true._lk, out_compact, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "write negative indent")
    call buf_init_bounded(tiny_out, 2_i32, err)
    call json_write_to_buf(doc, 2_i32, .true._lk, tiny_out, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "write buffer overflow")

    ! Parser syntax error branches
    call check_parse_err(st, "", "empty input")
    call check_parse_err(st, "@", "lex error through json_parse")
    call check_parse_err(st, "true false", "trailing token")
    call check_parse_err(st, "[1,]", "array trailing comma")
    call check_parse_err(st, "[1 2]", "array missing comma")
    call check_parse_err(st, '{"a":1,}', "object trailing comma")
    call check_parse_err(st, '{1:2}', "object non-string key")
    call check_parse_err(st, '{"a" 1}', "object missing colon")
    call check_parse_err(st, '{"a":1 "b":2}', "object missing comma")
    call check_parse_err(st, '{"a":1,"a":2}', "duplicate key rejected")

    ! Depth limits on array and object
    call json_parse_opts("[[1]]", 1_i32, .true._lk, doc, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_PARSE, "array max_depth")
    call json_parse_opts('{"a":{"b":1}}', 1_i32, .true._lk, doc, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_PARSE, "object max_depth")

    ! Allow duplicate keys when reject_dup_keys = .false.
    call json_parse_opts('{"a":1,"a":2}', 8_i32, .false._lk, doc, err)
    call th_eq_i32(st, err%code, CDD_OK, "duplicate key allowed")

    ! Bounded DOM arena overflow inside json_parse_stream
    call json_lex("[1, 2]", stream, err)
    call jdoc_init_bounded(doc, 1_i32, 1_i32, 1_i32, err)
    call json_parse_stream("[1, 2]", stream, 8_i32, .true._lk, doc, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "parse array elem overflow")

    call jdoc_init_bounded(doc, 4_i32, 1_i32, 1_i32, err)
    call json_parse_stream("[1, 2]", stream, 8_i32, .true._lk, doc, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "parse array item link overflow")

    call json_lex("[[]]", stream, err)
    call jdoc_init_bounded(doc, 1_i32, 1_i32, 1_i32, err)
    call json_parse_stream("[[]]", stream, 8_i32, .true._lk, doc, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "parse nested array alloc overflow")

    call json_lex('{"a":{}}', stream, err)
    call jdoc_init_bounded(doc, 1_i32, 1_i32, 1_i32, err)
    call json_parse_stream('{"a":{}}', stream, 8_i32, .true._lk, doc, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "parse nested object alloc overflow")
  end subroutine run_parser_and_writer_tests

  !> @brief Assert that json_parse fails on bad_json.
  !> @param[inout] st       Test state accumulator.
  !> @param[in]    bad_json Invalid JSON input.
  !> @param[in]    label    Assertion label.
  subroutine check_parse_err(st, bad_json, label)
    type(test_state_t), intent(inout) :: st
    character(len=*, kind=c_k), intent(in) :: bad_json
    character(len=*, kind=c_k), intent(in) :: label
    type(cdd_json_doc_t) :: d
    type(cdd_error_t) :: err

    call json_parse(bad_json, d, err)
    call th_check(st, err%code /= CDD_OK, label)
  end subroutine check_parse_err

  !> @brief Exercise JSON Schema Draft 2020-12 & OpenAPI <-> IR conversion across standards.
  !> @param[inout] st Test state accumulator.
  subroutine run_schema_mapper_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_json_doc_t) :: doc, out_doc
    type(cdd_options_t) :: opts
    type(cdd_ir_schema_bundle_t) :: bundle
    type(cdd_buffer_t) :: jout
    type(cdd_error_t) :: err
    character(len=*), parameter :: RICH_SCHEMA = &
      '{"$schema":"https://json-schema.org/draft/2020-12/schema",' // &
      '"title":"Telemetry","description":"Telemetry packet","type":"object",' // &
      '"allOf":[{"$ref":"#/$defs/Header"},{"properties":{"extra":{"type":"integer","format":"int8","default":3}}}],' // &
      '"required":["id","mode"],' // &
      '"properties":{' // &
      '"id":{"type":"integer","format":"int64","description":"Packet ID","default":100},' // &
      '"code16":{"type":"integer","format":"int16"},' // &
      '"temp":{"type":"number","format":"float","default":20.5},' // &
      '"prec":{"type":"number","format":"quad"},' // &
      '"val64":{"type":"number"},' // &
      '"ok":{"type":"boolean","default":true},' // &
      '"flag2":{"type":"boolean","default":false},' // &
      '"tag":{"type":"string","maxLength":12,"default":"NOM"},' // &
      '"note":{"type":["string","null"]},' // &
      '"opt_num":{"anyOf":[{"type":"null"},{"type":"integer"}]},' // &
      '"mode":{"$ref":"#/$defs/Mode"},' // &
      '"inline_st":{"type":"string","enum":["A",""]},' // &
      '"mat":{"type":"array","maxItems":3,"items":{"type":"array","items":{"type":"number"}}},' // &
      '"sub":{"type":"object","properties":{"x":{"type":"integer"}}},' // &
      '"self_link":{"$ref":"#/$defs/Node"}' // &
      '},' // &
      '"$defs":{' // &
      '"Mode":{"type":"string","description":"Op mode","enum":["IDLE","RUN"]},' // &
      '"Header":{"type":"object","properties":{"seq":{"type":"integer"}}},' // &
      '"Node":{"type":"object","properties":{"next":{"$ref":"#/$defs/Node"}}}' // &
      '}}'

    call json_parse(RICH_SCHEMA, doc, err)
    call th_eq_i32(st, err%code, CDD_OK, "parse rich schema")
    call th_check(st, is_json_schema_doc(doc), "is_json_schema_doc true")

    ! Convert for Fortran 2023 (OOP, allocatable, deferred char)
    call cdd_default_options(opts)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "schema -> IR F2023")

    ! Reverse IR -> JSON Schema -> string, AND IR -> JSON Sample (covering enums, defaults, extends, recursive UDT!)
    call ir_to_json_schema(bundle, opts, out_doc, err)
    call th_eq_i32(st, err%code, CDD_OK, "IR -> schema F2023")
    call json_write(out_doc, 2_i32, .false._lk, jout, err)
    call th_eq_i32(st, err%code, CDD_OK, "write emitted schema")
    call ir_to_json_sample(bundle, opts, out_doc, err)
    call th_eq_i32(st, err%code, CDD_OK, "IR -> sample on rich schema")

    ! Forward $ref inside $defs (Child defined BEFORE Parent and Status), omitted "type", default "" and [1]
    call json_parse( &
      '{"type":"object","properties":{"c":{"$ref":"#/$defs/Child"},' // &
      '"implicit_obj":{"properties":{"k":{"type":"string","default":""}}},' // &
      '"arr_def":{"type":"array","items":{"type":"integer"},"default":[1]}},' // &
      '"$defs":{' // &
      '"Child":{"allOf":[{"$ref":"#/$defs/Parent"}],' // &
      '"properties":{"st":{"$ref":"#/$defs/Status"},"p":{"$ref":"#/$defs/Leaf"}}},' // &
      '"Parent":{"type":"object","description":123,"properties":{"id":{"type":"integer"}}},' // &
      '"Leaf":{"type":"object","properties":{"v":{"type":"boolean"}}},' // &
      '"Status":{"enum":["OK","ERR"]}}}', &
      doc, err)
    call th_check(st, is_json_schema_doc(doc), "is_json_schema_doc type=object+properties")
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "forward $ref in $defs")

    call json_parse('{"type":"object","properties":{"a":{"type":"integer"}}}', doc, err)
    call th_check(st, is_json_schema_doc(doc), "type=object without $defs/$schema is schema")
    call str_set(opts%target_udt_name, repeat("T", 70), err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "overlong target_udt_name clamped")
    call str_set(opts%target_udt_name, "ShortName", err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "short target_udt_name")
    call cdd_default_options(opts)

    call json_parse('{"type":"string","properties":{}}', doc, err)
    call th_check(st, .not. is_json_schema_doc(doc), "type=string (len 6) with properties not obj")
    call json_parse('{"type":"object","properties":{"a":{"type":"integer"}}}', doc, err)
    call th_check(st, is_json_schema_doc(doc), "pure type=object+properties is schema")
    call str_set(opts%target_udt_name, repeat("T", 68), err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "overlong target_udt_name clamped on root schema")
    call cdd_default_options(opts)

    call json_parse('{"type":"string","properties":{}}', doc, err)
    call th_check(st, .not. is_json_schema_doc(doc), "type=string with 6-char len not obj schema")
    call json_parse('{"type":"array","properties":{}}', doc, err)
    call th_check(st, .not. is_json_schema_doc(doc), "type=array with properties not obj schema")
    call json_parse('{"plain_key":1}', doc, err)
    call th_check(st, .not. is_json_schema_doc(doc), "plain sample obj without properties not schema")

    ! Empty object UDT referenced from properties + non-object element in anyOf + nullable $ref on F66
    call json_parse( &
      '{"type":"object","properties":{"e":{"$ref":"#/$defs/EmptyUdt"},"u":{"anyOf":[123,{"type":"integer"}]}},' // &
      '"$defs":{"EmptyUdt":{"type":"object"}}}', doc, err)
    opts%standard%id = STD_FORTRAN_66
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "empty UDT ref and non-obj anyOf")
    call json_parse( &
      '{"title":"EmptyUdt","type":"object","properties":{"e":{"nullable":true,"$ref":"#/$defs/EmptyUdt"}},' // &
      '"$defs":{"EmptyUdt":{"type":"object"}}}', doc, err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_DIALECT_CONSTRAINT, "recursive EmptyUdt on F66 via root title")
    call json_parse( &
      '{"type":"object","properties":{"e":{"nullable":true,"$ref":"#/$defs/EmptyUdt"}},' // &
      '"$defs":{"EmptyUdt":{"type":"object"}}}', doc, err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "nullable non-recursive ref on F66")
    call cdd_default_options(opts)

    ! Cover oneOf, nullable=false, type array with non-string/empty/extra strings, >64 title/desc, $defs junk
    call json_parse( &
      '{"title":"' // repeat("Q", 68) // '","description":"",' // &
      '"allOf":[123],' // &
      '"required":[123,"","k1"],' // &
      '"properties":{' // &
      '"": {"type":"integer"},' // &
      '"k1":{"oneOf":[{"type":"null"},{"type":"string","nullable":false,"format":"' // repeat("f", 68) // '"}]},' // &
      '"k2":{"type":[123,"","null","string","integer"]},' // &
      '"k3":{"type":"integer","nullable":"not_bool"}},' // &
      '"$defs":{"": {"type":"object"}, "junk": 42, ' // &
      '"DupEnum":{"enum":["A"]}, "Ref1":{"properties":{"e":{"$ref":"#/$defs/DupEnum"}}}}}', &
      doc, err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "edge-case schema attributes")
    ! Single-UDT no-defs ir_to_json_schema branch
    call json_parse('{"properties":{"only_opt":{"type":"integer"}}}', doc, err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call ir_to_json_schema(bundle, opts, out_doc, err)
    call th_eq_i32(st, err%code, CDD_OK, "single UDT no required no defs schema")

    ! Convert non-recursive subset for Fortran 90 (POINTER arrays/nullables) and FORTRAN 66 (flattened allOf)
    call json_parse( &
      '{"type":"object","allOf":[{"$ref":"#/definitions/Base"}],' // &
      '"properties":{"arr":{"type":"array","items":{"type":"string"}},' // &
      '"n":{"type":"string","nullable":true,"default":null}},' // &
      '"definitions":{"Base":{"type":"object","properties":{"b":{"type":"integer"}}}}}', &
      doc, err)
    call th_check(st, is_json_schema_doc(doc), "is_json_schema_doc definitions")
    opts%standard%id = STD_FORTRAN_90
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "schema -> IR F90")

    opts%standard%id = STD_FORTRAN_66
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "schema -> IR F66 flattened allOf")

    ! OpenAPI components/schemas root with and without target_udt_name
    call json_parse( &
      '{"components":{"schemas":{"Status":{"type":"string","enum":["UP"]},' // &
      '"Packet":{"type":"object","properties":{"s":{"$ref":"#/components/schemas/Status"}}}}}}', &
      doc, err)
    call cdd_default_options(opts)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "OpenAPI auto main schema")
    call str_set(opts%target_udt_name, "Packet", err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "OpenAPI explicit Packet")
    call str_set(opts%target_udt_name, "Missing", err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_SCHEMA, "OpenAPI missing target")

    ! Recursive UDT on FORTRAN 66 -> CDD_ERR_DIALECT_CONSTRAINT; on Fortran 90 -> POINTER
    call json_parse( &
      '{"$defs":{"Node":{"type":"object","properties":{"next":{"$ref":"#/$defs/Node"}}}}}', &
      doc, err)
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_90
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "recursive UDT F90 pointer")
    opts%standard%id = STD_FORTRAN_66
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_DIALECT_CONSTRAINT, "recursive UDT F66 error")

    ! Schema error branches
    call jdoc_init(doc)
    call th_check(st, .not. is_json_schema_doc(doc), "empty doc not schema")
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_SCHEMA, "empty doc schema err")

    call json_parse("[1,2]", doc, err)
    call th_check(st, .not. is_json_schema_doc(doc), "array doc not schema")
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_SCHEMA, "array root schema err")

    call check_schema_err(st, '{"properties":123}', "non-obj properties")
    call check_schema_err(st, '{"properties":{"x":123}}', "non-obj prop schema")
    call check_schema_err(st, '{"properties":{"x":{"type":"unknown"}}}', "unknown type")
    call check_schema_err(st, '{"properties":{"x":{"type":123}}}', "numeric type")
    call check_schema_err(st, '{"properties":{"x":{"type":"array"}}}', "array no items")
    call check_schema_err(st, '{"properties":{"x":{"$ref":123}}}', "non-string ref")
    call check_schema_err(st, '{"properties":{"x":{"$ref":""}}}', "empty ref")
    call check_schema_err(st, '{"properties":{"x":{"$ref":"http://ext/a"}}}', "external ref")
    call check_schema_err(st, '{"properties":{"x":{"$ref":"#/$defs/"}}}', "empty ref target")
    call check_schema_err(st, '{"properties":{"x":{"$ref":"#/$defs/Nope"}},"$defs":{}}', "unresolved ref with defs")
    call check_schema_err(st, '{"allOf":[{"$ref":"#/$defs/Nope"}],"$defs":{}}', "unresolved allOf ref with defs")
    call check_schema_err(st, '{"$defs":{"A":{"allOf":[{"$ref":"#/$defs/B"}]},"B":{"properties":123}}}', "fwd allOf err")
    call check_schema_err(st, '{"$defs":{"A":{"properties":{"e":{"$ref":"#/$defs/B"}}},"B":{"enum":[]}}}', "fwd enum err")
    call check_schema_err(st, '{"$defs":{"A":{"properties":{"u":{"$ref":"#/$defs/B"}}},"B":{"properties":123}}}', "fwd udt err")
    call check_schema_err(st, '{"properties":{"x":{"type":"array","items":123}}}', "non-obj array items")
    call check_schema_err(st, '{"properties":{"x":{"enum":[]}}}', "empty enum")
    call check_schema_err(st, '{"properties":{"x":{"enum":[1]}}}', "non-string enum")
    call check_schema_err(st, '{"properties":{"x":{"type":""}}}', "empty type string")
    call check_schema_err(st, '{"properties":{"x":{"type":["null"]}}}', "only null in type array")
    call check_schema_err(st, &
      '{"properties":{"x":{"type":"array","items":{"type":"array","items":{"type":"array",' // &
      '"items":{"type":"array","items":{"type":"array","items":{"type":"array",' // &
      '"items":{"type":"array","items":{"type":"array","items":{"type":"array",' // &
      '"items":{"type":"array","items":{"type":"array","items":{"type":"array",' // &
      '"items":{"type":"array","items":{"type":"array","items":{"type":"array",' // &
      '"items":{"type":"array","items":{"type":"integer"}}}}}}}}}}}}}}}}}}}', &
      "16-rank schema array rejected")
    call check_schema_err(st, '{"$defs":{"OnlyEnum":{"enum":["A"]}}}', "no object schemas")

    ! Overlong property key & overlong enum value (> 64 chars)
    call json_parse('{"properties":{"' // repeat("k", 66) // '":{"type":"integer"}}}', doc, err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "overlong schema prop key")

    call json_parse('{"properties":{"e":{"enum":["' // repeat("E", 66) // '"]}}}', doc, err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "overlong enum literal")

    call json_parse('{"allOf":[{"properties":{"' // repeat("k", 66) // '":{"type":"integer"}}}]}', doc, err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "allOf inline prop error")

    call json_parse('{"allOf":[{"$ref":"#/$defs/BadP"}],"$defs":{"BadP":{"properties":123}}}', doc, err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_SCHEMA, "allOf forward ref parse error")

    call json_parse('{"allOf":[{"$ref":123}]}', doc, err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_SCHEMA, "allOf non-string ref")

    call json_parse('{"properties":{"p":{"$ref":"#/$defs/BadE"}},"$defs":{"BadE":{"enum":[]}}}', doc, err)
    ! Test forward $ref error by putting Root properties ref without pre-pass via components:
    call json_parse('{"properties":{"p":{"type":"object","properties":123}}}', doc, err)
    call json_schema_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_SCHEMA, "inline child object parse error")

    ! Standalone synthetic IR bundle with external extends_name / enum_name / ref_udt_name
    call test_unresolved_ir_refs(st)

    call ir_bundle_init(bundle)
    call ir_to_json_schema(bundle, opts, out_doc, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_SCHEMA, "empty bundle ir_to_json_schema")
  end subroutine run_schema_mapper_tests

  !> @brief Exercise ir_to_json_schema and ir_to_json_sample when extends/enum/udt names are external.
  !> @param[inout] st Test state accumulator.
  subroutine test_unresolved_ir_refs(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_ir_schema_bundle_t) :: b
    type(cdd_ir_field_t) :: f1, f2
    type(cdd_options_t) :: opts
    type(cdd_json_doc_t) :: d1, d2
    type(cdd_error_t) :: err
    integer(i32) :: uidx, fidx

    call cdd_default_options(opts)
    call ir_bundle_init(b)
    call ir_bundle_add_udt(b, "Orphan", uidx, err)
    b%main_udt_idx = uidx
    b%udts(uidx)%has_extends = .true._lk
    call ident_set(b%udts(uidx)%extends_name, "ExternalBase", err)

    call ir_field_init(f1)
    call ident_set(f1%json_name, "ext_e", err)
    f1%type_kind = IR_ENUM
    call ident_set(f1%enum_name, "ExternalEnum", err)
    call ir_udt_add_field(b%udts(uidx), f1, fidx, err)

    call ir_field_init(f2)
    call ident_set(f2%json_name, "ext_u", err)
    f2%type_kind = IR_NESTED_UDT
    call ident_set(f2%ref_udt_name, "ExternalChild", err)
    call ir_udt_add_field(b%udts(uidx), f2, fidx, err)

    call ir_to_json_schema(b, opts, d1, err)
    call th_eq_i32(st, err%code, CDD_OK, "ir_to_json_schema external refs")
    call ir_to_json_sample(b, opts, d2, err)
    call th_eq_i32(st, err%code, CDD_OK, "ir_to_json_sample external refs")
  end subroutine test_unresolved_ir_refs

  !> @brief Assert json_schema_to_ir fails on bad_schema.
  !> @param[inout] st         Test state accumulator.
  !> @param[in]    bad_schema Invalid JSON Schema text.
  !> @param[in]    label      Assertion label.
  subroutine check_schema_err(st, bad_schema, label)
    type(test_state_t), intent(inout) :: st
    character(len=*, kind=c_k), intent(in) :: bad_schema
    character(len=*, kind=c_k), intent(in) :: label
    type(cdd_json_doc_t) :: d
    type(cdd_options_t) :: o
    type(cdd_ir_schema_bundle_t) :: b
    type(cdd_error_t) :: err

    call cdd_default_options(o)
    call json_parse(bad_schema, d, err)
    call json_schema_to_ir(d, o, b, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_SCHEMA, label)
  end subroutine check_schema_err

  !> @brief Exercise raw JSON sample inference and sample emission across dialects.
  !> @param[inout] st Test state accumulator.
  subroutine run_sample_infer_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_json_doc_t) :: doc, sdoc
    type(cdd_options_t) :: opts
    type(cdd_ir_schema_bundle_t) :: bundle
    type(cdd_error_t) :: err

    ! Multi-object sample array testing type promotion (int32->int64, int->float, null->string, missing->optional, required bool)
    call json_parse( &
      '[{"":0,"a":1,"b":2,"c":null,"d":[1,2.5],"e":[],"obj":{"k":"v"},"flag":true,"opt":1},' // &
      ' {"a":5000000000,"b":3.5,"c":"filled","d":[3.0],"e":[1.0],"obj":{"k":"long_val"},"flag":false}]', &
      doc, err)
    call th_check(st, .not. is_json_schema_doc(doc), "sample array not schema")

    call cdd_default_options(opts)
    call str_set(opts%target_udt_name, "Measurement", err)
    call json_sample_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "infer sample array F2023")
    call th_eq_i32(st, bundle%udts(bundle%main_udt_idx)%fields(1)%type_kind, IR_INT64, "promoted i64")
    call th_eq_i32(st, bundle%udts(bundle%main_udt_idx)%fields(2)%type_kind, IR_REAL64, "promoted r64")

    call ir_to_json_sample(bundle, opts, sdoc, err)
    call th_eq_i32(st, err%code, CDD_OK, "ir_to_json_sample ok")

    ! Also infer single object on FORTRAN I (including string!) and Fortran 90
    call json_parse('{"s":"legacy","n":10,"arr":[[1,2],[3,4]],"kids":[{"x":1},{"x":null}],"nil":null}', doc, err)
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_I
    call json_sample_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "infer sample F57")

    opts%standard%id = STD_FORTRAN_90
    call json_sample_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "infer sample F90")

    opts%standard%id = STD_FORTRAN_2003
    call json_sample_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "infer sample F2003")

    ! Reverse promotion order: int64 then int32, float then int32
    call json_parse('[{"a":5000000000,"b":1.5},{"a":2,"b":3}]', doc, err)
    call json_sample_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_OK, "infer reverse numeric promotion")

    ! 16-dimensional array exceeds max rank 15
    call json_parse('{"r":[[[[[[[[[[[[[[[[1]]]]]]]]]]]]]]]]}', doc, err)
    call json_sample_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_INFER, "infer rank > 15 rejected")

    ! Sample inference error branches
    call jdoc_init(doc)
    call json_sample_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_INFER, "infer empty doc")

    call json_parse("42", doc, err)
    call json_sample_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_INFER, "infer scalar root")

    call json_parse("[]", doc, err)
    call json_sample_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_INFER, "infer empty root array")

    call json_parse("[1]", doc, err)
    call json_sample_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_INFER, "infer non-obj root array")

    call json_parse('{"bad":[1,"str"]}', doc, err)
    call json_sample_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_INFER, "infer heterogeneous array")

    call json_parse('[{"a":1},{"a":"conflict"}]', doc, err)
    call json_sample_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_INFER, "infer root array object conflict")

    call json_parse('{"sub":{"bad":[1,"s"]}}', doc, err)
    call json_sample_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_INFER, "infer nested object error")

    call json_parse('{"kids":[{"x":1},{"x":"s"}]}', doc, err)
    call json_sample_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_INFER, "infer nested object array conflict")

    call json_parse('{"' // repeat("z", 66) // '":1}', doc, err)
    call json_sample_to_ir(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "infer overlong sample key")

    call json_parse('{"sub":{"x":1}}', doc, err)
    call ir_bundle_init(bundle)
    bundle%max_udts = 1_i32
    call json_sample_to_ir_into(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "infer max_udts overflow")

    call json_parse('{"a":1,"b":2}', doc, err)
    call ir_bundle_init(bundle)
    call ir_bundle_add_udt(bundle, "Sample", bundle%main_udt_idx, err)
    bundle%udts(1)%max_fields = 1_i32
    call json_sample_to_ir_into(doc, opts, bundle, err)
    call th_eq_i32(st, err%code, CDD_ERR_BUFFER_OVERFLOW, "infer max_fields overflow")

    call ir_bundle_init(bundle)
    call ir_to_json_sample(bundle, opts, sdoc, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_INFER, "empty bundle ir_to_json_sample")
  end subroutine run_sample_infer_tests

end program test_json_suite
