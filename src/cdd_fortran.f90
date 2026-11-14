!> @file cdd_fortran.f90
!> @brief Public strongly-typed Fortran SDK facade for `cdd-fortran` (Operations 0, 1, 2, and 3).

!> @brief Top-level pure and filesystem-backed entry points for JSON <-> Fortran UDT conversion & sync.
module cdd_fortran
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: CDD_OK, cdd_error_t, cdd_error_clear, cdd_has_error, cdd_error_format
  use cdd_string, only: cdd_buffer_t, buf_init, buf_append_str, buf_equals, str_eq
  use cdd_fs, only: cdd_fs_t, fs_init_os, fs_init_mock, fs_read_text_file, fs_write_text_file, &
    fs_read_stdin, fs_write_stdout
  use cdd_standard, only: STD_FORTRAN_I, STD_FORTRAN_II, STD_FORTRAN_III, STD_FORTRAN_IV, &
    STD_FORTRAN_66, STD_FORTRAN_77, STD_FORTRAN_90, STD_FORTRAN_95, STD_FORTRAN_2003, &
    STD_FORTRAN_2008, STD_FORTRAN_2018, STD_FORTRAN_2023, FORM_AUTO, FORM_FIXED, FORM_FREE, &
    JSON_MODE_AUTO, JSON_MODE_SCHEMA, JSON_MODE_SAMPLE, cdd_standard_t, cdd_options_t, &
    cdd_default_options, parse_standard_name
  use cdd_json_dom, only: cdd_json_doc_t
  use cdd_json_parser, only: json_parse
  use cdd_json_writer, only: json_write
  use cdd_json_schema, only: is_json_schema_doc, json_schema_to_ir, ir_to_json_schema
  use cdd_json_infer, only: json_sample_to_ir, ir_to_json_sample
  use cdd_ir_types, only: cdd_ir_schema_bundle_t
  use cdd_emit_udt, only: emit_udt_fortran
  use cdd_extract_udt, only: extract_udt_from_source
  use cdd_emit_codec, only: emit_codec_fortran
  use cdd_patch_udt, only: patch_fortran_with_json
  implicit none (type, external)
  private

  ! Re-exported SDK types, constants, and helpers
  public :: CDD_OK, cdd_error_t, cdd_has_error, cdd_error_format
  public :: cdd_buffer_t, cdd_fs_t, fs_init_os, fs_init_mock
  public :: STD_FORTRAN_I, STD_FORTRAN_II, STD_FORTRAN_III, STD_FORTRAN_IV
  public :: STD_FORTRAN_66, STD_FORTRAN_77, STD_FORTRAN_90, STD_FORTRAN_95
  public :: STD_FORTRAN_2003, STD_FORTRAN_2008, STD_FORTRAN_2018, STD_FORTRAN_2023
  public :: FORM_AUTO, FORM_FIXED, FORM_FREE
  public :: JSON_MODE_AUTO, JSON_MODE_SCHEMA, JSON_MODE_SAMPLE
  public :: cdd_standard_t, cdd_options_t, cdd_default_options, parse_standard_name
  public :: CDD_FORTRAN_VERSION

  ! Core pure string/buffer SDK procedures (Ops 0..3)
  public :: json_to_fortran_udt
  public :: fortran_udt_to_json
  public :: generate_json_codecs
  public :: update_fortran_from_json

  ! Filesystem/stream-backed SDK procedures (`"-" ` = stdin/stdout)
  public :: json_to_fortran_udt_file
  public :: fortran_udt_to_json_file
  public :: generate_json_codecs_file
  public :: update_fortran_from_json_file

  character(len=5, kind=c_k), parameter :: CDD_FORTRAN_VERSION = "0.1.0" !< Semantic SDK/CLI version.

contains

  !> @brief Operation 0: Convert JSON Schema or raw JSON sample payload to Fortran UDT source.
  !> @param[in]  json_source  Input JSON Schema or sample JSON text.
  !> @param[in]  options      Dialect, source form, and codegen options.
  !> @param[out] fortran_code Generated Fortran UDT code buffer.
  !> @param[out] err          Structured error status.
  pure subroutine json_to_fortran_udt(json_source, options, fortran_code, err)
    character(len=*, kind=c_k), intent(in) :: json_source
    type(cdd_options_t), intent(in) :: options
    type(cdd_buffer_t), intent(out) :: fortran_code
    type(cdd_error_t), intent(out) :: err
    type(cdd_ir_schema_bundle_t) :: bundle

    call buf_init(fortran_code)
    call parse_json_into_ir(json_source, options, bundle, err)
    if (err%code /= 0_i32) return
    call emit_udt_fortran(bundle, options, fortran_code, err)
  end subroutine json_to_fortran_udt

  !> @brief Operation 1: Extract Fortran UDT (across FORTRAN I..2023) to JSON Schema or JSON sample.
  !> @param[in]  fortran_code Input Fortran source code.
  !> @param[in]  options      Dialect, form, target UDT, and `json_mode` (`SCHEMA` vs `SAMPLE`).
  !> @param[out] json_output  Serialized JSON Schema Draft 2020-12 or sample JSON buffer.
  !> @param[out] err          Structured error status.
  pure subroutine fortran_udt_to_json(fortran_code, options, json_output, err)
    character(len=*, kind=c_k), intent(in) :: fortran_code
    type(cdd_options_t), intent(in) :: options
    type(cdd_buffer_t), intent(out) :: json_output
    type(cdd_error_t), intent(out) :: err
    type(cdd_ir_schema_bundle_t) :: bundle
    type(cdd_json_doc_t) :: doc

    call buf_init(json_output)
    call extract_udt_from_source(fortran_code, options, bundle, err)
    if (err%code /= 0_i32) return

    if (options%json_mode == JSON_MODE_SAMPLE) then
      call ir_to_json_sample(bundle, options, doc, err)
    else
      call ir_to_json_schema(bundle, options, doc, err)
    end if

    call json_write(doc, max(0_i32, options%indent_width), .false._lk, json_output, err)
  end subroutine fortran_udt_to_json

  !> @brief Operation 2: Generate zero-dependency JSON <-> UDT codecs from JSON or Fortran input.
  !> @param[in]  spec_source Input JSON Schema, JSON sample, or Fortran UDT source code.
  !> @param[in]  options     Target Fortran standard and codec options.
  !> @param[out] codec_code  Generated dialect-pure Fortran codec code buffer.
  !> @param[out] err         Structured error status.
  pure subroutine generate_json_codecs(spec_source, options, codec_code, err)
    character(len=*, kind=c_k), intent(in) :: spec_source
    type(cdd_options_t), intent(in) :: options
    type(cdd_buffer_t), intent(out) :: codec_code
    type(cdd_error_t), intent(out) :: err
    type(cdd_ir_schema_bundle_t) :: bundle

    call buf_init(codec_code)
    if (looks_like_json(spec_source)) then
      call parse_json_into_ir(spec_source, options, bundle, err)
    else
      call extract_udt_from_source(spec_source, options, bundle, err)
    end if
    if (err%code /= 0_i32) return
    call emit_codec_fortran(bundle, options, codec_code, err)
  end subroutine generate_json_codecs

  !> @brief Operation 3: Surgically synchronize an existing Fortran source with JSON, keeping trivia.
  !> @param[in]  existing_fortran Existing Fortran source code.
  !> @param[in]  json_source      Target JSON Schema or sample JSON text.
  !> @param[in]  options          Dialect, form, target UDT, and codec-sync options.
  !> @param[out] updated_fortran  Patched Fortran source buffer.
  !> @param[out] err              Structured error status.
  pure subroutine update_fortran_from_json(existing_fortran, json_source, options, &
      updated_fortran, err)
    character(len=*, kind=c_k), intent(in) :: existing_fortran
    character(len=*, kind=c_k), intent(in) :: json_source
    type(cdd_options_t), intent(in) :: options
    type(cdd_buffer_t), intent(out) :: updated_fortran
    type(cdd_error_t), intent(out) :: err

    call patch_fortran_with_json(existing_fortran, json_source, options, updated_fortran, err)
  end subroutine update_fortran_from_json

  !> @brief File/stream wrapper for Operation 0 (`json2fortran`, optionally `--with-codecs`).
  !> @param[inout] fs          Filesystem provider (OS or mock).
  !> @param[in]    input_path  JSON file path or `"-"` for stdin.
  !> @param[in]    output_path Fortran output file path or `"-"` for stdout.
  !> @param[in]    with_codecs True to emit companion JSON <-> UDT codecs alongside UDT.
  !> @param[in]    options     Dialect and codegen options.
  !> @param[out]   err         Structured error status.
  subroutine json_to_fortran_udt_file(fs, input_path, output_path, with_codecs, options, err)
    type(cdd_fs_t), intent(inout) :: fs
    character(len=*, kind=c_k), intent(in) :: input_path
    character(len=*, kind=c_k), intent(in) :: output_path
    logical(lk), intent(in) :: with_codecs
    type(cdd_options_t), intent(in) :: options
    type(cdd_error_t), intent(out) :: err
    type(cdd_buffer_t) :: in_buf, udt_buf, cod_buf

    call read_path_or_stdin(fs, input_path, in_buf, err)
    if (err%code /= 0_i32) return

    if (with_codecs) then
      if (options%standard%id >= STD_FORTRAN_90 .and. .not. options%emit_submodules) then
        call generate_json_codecs(in_buf%raw(1:in_buf%len), options, udt_buf, err)
      else
        call json_to_fortran_udt(in_buf%raw(1:in_buf%len), options, udt_buf, err)
        if (err%code /= 0_i32) return
        call generate_json_codecs(in_buf%raw(1:in_buf%len), options, cod_buf, err)
        call buf_append_str(udt_buf, cod_buf%raw(1:cod_buf%len), err)
      end if
    else
      call json_to_fortran_udt(in_buf%raw(1:in_buf%len), options, udt_buf, err)
    end if
    if (err%code /= 0_i32) return

    call write_path_or_stdout(fs, output_path, udt_buf%raw(1:udt_buf%len), err)
  end subroutine json_to_fortran_udt_file

  !> @brief File/stream wrapper for Operation 1 (`fortran2json`).
  !> @param[inout] fs          Filesystem provider (OS or mock).
  !> @param[in]    input_path  Fortran file path or `"-"` for stdin.
  !> @param[in]    output_path JSON output file path or `"-"` for stdout.
  !> @param[in]    options     Dialect and JSON output mode options.
  !> @param[out]   err         Structured error status.
  subroutine fortran_udt_to_json_file(fs, input_path, output_path, options, err)
    type(cdd_fs_t), intent(inout) :: fs
    character(len=*, kind=c_k), intent(in) :: input_path
    character(len=*, kind=c_k), intent(in) :: output_path
    type(cdd_options_t), intent(in) :: options
    type(cdd_error_t), intent(out) :: err
    type(cdd_buffer_t) :: in_buf, out_buf

    call read_path_or_stdin(fs, input_path, in_buf, err)
    if (err%code /= 0_i32) return
    call fortran_udt_to_json(in_buf%raw(1:in_buf%len), options, out_buf, err)
    if (err%code /= 0_i32) return
    call write_path_or_stdout(fs, output_path, out_buf%raw(1:out_buf%len), err)
  end subroutine fortran_udt_to_json_file

  !> @brief File/stream wrapper for Operation 2 (`gen-codec`).
  !> @param[inout] fs          Filesystem provider (OS or mock).
  !> @param[in]    input_path  JSON or Fortran file path or `"-"` for stdin.
  !> @param[in]    output_path Fortran codec output path or `"-"` for stdout.
  !> @param[in]    options     Target standard and codec options.
  !> @param[out]   err         Structured error status.
  subroutine generate_json_codecs_file(fs, input_path, output_path, options, err)
    type(cdd_fs_t), intent(inout) :: fs
    character(len=*, kind=c_k), intent(in) :: input_path
    character(len=*, kind=c_k), intent(in) :: output_path
    type(cdd_options_t), intent(in) :: options
    type(cdd_error_t), intent(out) :: err
    type(cdd_buffer_t) :: in_buf, out_buf

    call read_path_or_stdin(fs, input_path, in_buf, err)
    if (err%code /= 0_i32) return
    call generate_json_codecs(in_buf%raw(1:in_buf%len), options, out_buf, err)
    if (err%code /= 0_i32) return
    call write_path_or_stdout(fs, output_path, out_buf%raw(1:out_buf%len), err)
  end subroutine generate_json_codecs_file

  !> @brief File/stream wrapper for Operation 3 (`sync` / `update`, supporting `--in-place` & `--check`).
  !> @param[inout] fs          Filesystem provider (OS or mock).
  !> @param[in]    target_path Existing target Fortran file path.
  !> @param[in]    json_path   JSON Schema/sample path or `"-"` for stdin.
  !> @param[in]    output_path Output Fortran path or `"-"` for stdout (ignored if `in_place`).
  !> @param[in]    in_place    True to overwrite `target_path`.
  !> @param[in]    check_only  True for dry-run check without writing output.
  !> @param[out]   out_of_sync Set true when `updated_fortran` differs from `existing_fortran`.
  !> @param[in]    options     Dialect and sync options.
  !> @param[out]   err         Structured error status.
  subroutine update_fortran_from_json_file(fs, target_path, json_path, output_path, &
      in_place, check_only, out_of_sync, options, err)
    type(cdd_fs_t), intent(inout) :: fs
    character(len=*, kind=c_k), intent(in) :: target_path
    character(len=*, kind=c_k), intent(in) :: json_path
    character(len=*, kind=c_k), intent(in) :: output_path
    logical(lk), intent(in) :: in_place
    logical(lk), intent(in) :: check_only
    logical(lk), intent(out) :: out_of_sync
    type(cdd_options_t), intent(in) :: options
    type(cdd_error_t), intent(out) :: err
    type(cdd_buffer_t) :: f_buf, j_buf, p_buf

    out_of_sync = .false._lk
    call read_path_or_stdin(fs, target_path, f_buf, err)
    if (err%code /= 0_i32) return
    call read_path_or_stdin(fs, json_path, j_buf, err)
    if (err%code /= 0_i32) return

    call update_fortran_from_json(f_buf%raw(1:f_buf%len), j_buf%raw(1:j_buf%len), &
      options, p_buf, err)
    if (err%code /= 0_i32) return

    out_of_sync = .not. buf_equals(p_buf, f_buf%raw(1:f_buf%len))
    if (check_only) return

    if (in_place) then
      call write_path_or_stdout(fs, target_path, p_buf%raw(1:p_buf%len), err)
    else
      call write_path_or_stdout(fs, output_path, p_buf%raw(1:p_buf%len), err)
    end if
  end subroutine update_fortran_from_json_file

  !> @brief Parse JSON source text into a canonical IR bundle per `options%json_mode`.
  !> @param[in]  json_source Input JSON string.
  !> @param[in]  options     SDK options.
  !> @param[out] bundle      Populated IR bundle.
  !> @param[out] err         Error status.
  pure subroutine parse_json_into_ir(json_source, options, bundle, err)
    character(len=*, kind=c_k), intent(in) :: json_source
    type(cdd_options_t), intent(in) :: options
    type(cdd_ir_schema_bundle_t), intent(out) :: bundle
    type(cdd_error_t), intent(out) :: err
    type(cdd_json_doc_t) :: doc

    call cdd_error_clear(err)
    call json_parse(json_source, doc, err)
    if (err%code /= 0_i32) return

    if (options%json_mode == JSON_MODE_SCHEMA) then
      call json_schema_to_ir(doc, options, bundle, err)
    else if (options%json_mode == JSON_MODE_SAMPLE) then
      call json_sample_to_ir(doc, options, bundle, err)
    else if (is_json_schema_doc(doc)) then
      call json_schema_to_ir(doc, options, bundle, err)
    else
      call json_sample_to_ir(doc, options, bundle, err)
    end if
  end subroutine parse_json_into_ir

  !> @brief Heuristic check whether `src` begins with JSON `{` or `[` after leading whitespace.
  !> @param[in] src Input text.
  !> @return True if first non-whitespace byte is `{` or `[`.
  pure function looks_like_json(src) result(is_j)
    character(len=*, kind=c_k), intent(in) :: src
    logical(lk) :: is_j
    integer(i32) :: k, code

    is_j = .false._lk
    do k = 1_i32, len(src, kind=i32)
      code = iachar(src(k:k), i32)
      if (code == 32_i32 .or. code == 9_i32 .or. code == 10_i32 .or. code == 13_i32) cycle
      is_j = (src(k:k) == "{" .or. src(k:k) == "[")
      return
    end do
  end function looks_like_json

  !> @brief Read from `stdin` when `path == "-"` or empty, otherwise from `fs_read_text_file`.
  !> @param[inout] fs   Filesystem handle.
  !> @param[in]    path Path or `"-"`.
  !> @param[out]   buf  Output text buffer.
  !> @param[out]   err  Error status.
  subroutine read_path_or_stdin(fs, path, buf, err)
    type(cdd_fs_t), intent(inout) :: fs
    character(len=*, kind=c_k), intent(in) :: path
    type(cdd_buffer_t), intent(out) :: buf
    type(cdd_error_t), intent(out) :: err

    if (len(path, kind=i32) == 0_i32 .or. str_eq(path, "-")) then
      call fs_read_stdin(fs, buf, err)
    else
      call fs_read_text_file(fs, path, buf, err)
    end if
  end subroutine read_path_or_stdin

  !> @brief Write to `stdout` when `path == "-"` or empty, otherwise to `fs_write_text_file`.
  !> @param[inout] fs      Filesystem handle.
  !> @param[in]    path    Path or `"-"`.
  !> @param[in]    content Text payload.
  !> @param[out]   err     Error status.
  subroutine write_path_or_stdout(fs, path, content, err)
    type(cdd_fs_t), intent(inout) :: fs
    character(len=*, kind=c_k), intent(in) :: path
    character(len=*, kind=c_k), intent(in) :: content
    type(cdd_error_t), intent(out) :: err

    if (len(path, kind=i32) == 0_i32 .or. str_eq(path, "-")) then
      call fs_write_stdout(fs, content, err)
    else
      call fs_write_text_file(fs, path, content, err)
    end if
  end subroutine write_path_or_stdout

end module cdd_fortran
