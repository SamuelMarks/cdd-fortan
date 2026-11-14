!> @file test_cli_suite.f90
!> @brief Comprehensive 100% branch-coverage unit & end-to-end tests for SDK facade and CLI.

!> @brief Execute Milestone 11 SDK (`cdd_fortran`), CLI (`cdd_cli_*`), and 12-standard binary tests.
!> @return Program exit status (0 on pass).
program test_cli_suite
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: CDD_OK, CDD_ERR_CLI_USAGE, CDD_ERR_INVALID_OPTION, CDD_ERR_JSON_LEX, &
    CDD_ERR_JSON_SCHEMA, CDD_ERR_FORTRAN_LEX, CDD_ERR_UDT_NOT_FOUND, cdd_error_t
  use cdd_string, only: cdd_string_t, cdd_buffer_t, str_set, buf_append_str
  use cdd_fs, only: cdd_fs_t, fs_init_mock, fs_init_os, fs_write_text_file, fs_read_text_file, &
    mock_fs_put, mock_fs_get
  use cdd_fortran, only: STD_FORTRAN_77, STD_FORTRAN_2008, &
    FORM_FREE, JSON_MODE_SCHEMA, JSON_MODE_SAMPLE, cdd_options_t, cdd_default_options, &
    json_to_fortran_udt, fortran_udt_to_json, generate_json_codecs, update_fortran_from_json, &
    json_to_fortran_udt_file, fortran_udt_to_json_file, generate_json_codecs_file, &
    update_fortran_from_json_file
  use cdd_cli_args, only: CDD_MAX_CLI_ARGS, cdd_cli_command_t, cli_command_init, parse_cli_args
  use cdd_cli_runner, only: run_cli
  use test_harness, only: test_state_t, th_init, th_check, th_eq_i32, th_touch_vtable, th_finish
  implicit none (type, external)

  type(test_state_t) :: st
  type(cdd_cli_command_t) :: vcmd

  call th_init(st)
  call cli_command_init(vcmd)
  call th_touch_vtable(vcmd)

  call test_sdk_pure_and_file_facade(st)
  call test_cli_arg_parser_branches(st)
  call test_cli_runner_mock_fs(st)
  call test_end_to_end_binary_12_standards(st)

  call th_finish(st, "test_cli_suite")

contains

  !> @brief Populate `args(1:n)` from a space-separated token list for concise CLI unit tests.
  !> @param[in]  spec Space-delimited CLI string.
  !> @param[out] args Token array.
  !> @param[out] n    Number of parsed tokens.
  pure subroutine make_args(spec, args, n)
    character(len=*, kind=c_k), intent(in) :: spec
    type(cdd_string_t), intent(out) :: args(CDD_MAX_CLI_ARGS)
    integer(i32), intent(out) :: n
    type(cdd_error_t) :: err
    integer(i32) :: p, tot, b

    n = 0_i32
    p = 1_i32
    tot = len(spec, kind=i32)
    do while (p <= tot)
      do while (p <= tot)
        if (spec(p:p) /= " ") exit
        p = p + 1_i32
      end do
      if (p > tot) exit
      b = p
      do while (p <= tot)
        if (spec(p:p) == " ") exit
        p = p + 1_i32
      end do
      n = n + 1_i32
      call str_set(args(n), spec(b:p - 1_i32), err)
    end do
  end subroutine make_args

  !> @brief Test all pure and file-based procedures in `src/cdd_fortran.f90` (Ops 0..3 + errors).
  !> @param[inout] st Test harness state.
  subroutine test_sdk_pure_and_file_facade(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_options_t) :: opts
    type(cdd_buffer_t) :: f_code, j_out, c_code, u_code
    type(cdd_fs_t) :: mfs
    type(cdd_error_t) :: err
    logical(lk) :: oos
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    ! Op 0: schema, sample, auto-schema, auto-sample, and errors
    call cdd_default_options(opts)
    call json_to_fortran_udt('{"id": 7, "ok": true}', opts, f_code, err)
    call th_eq_i32(st, err%code, CDD_OK, "Op 0 auto-sample")
    call json_to_fortran_udt('{"title":"T","type":"object","properties":{"id":{"type":"integer"}}}', &
      opts, f_code, err)
    call th_eq_i32(st, err%code, CDD_OK, "Op 0 auto-schema")

    opts%json_mode = JSON_MODE_SCHEMA
    call json_to_fortran_udt('{"title":"T","type":"object","properties":{"id":{"type":"integer"}}}', &
      opts, f_code, err)
    call th_eq_i32(st, err%code, CDD_OK, "Op 0 explicit schema")
    call json_to_fortran_udt("{bad", opts, f_code, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_LEX, "Op 0 JSON lex error")
    call json_to_fortran_udt("[1]", opts, f_code, err)
    call th_eq_i32(st, err%code, CDD_ERR_JSON_SCHEMA, "Op 0 JSON schema error")

    opts%json_mode = JSON_MODE_SAMPLE
    call json_to_fortran_udt('{"id": 7}', opts, f_code, err)
    call th_eq_i32(st, err%code, CDD_OK, "Op 0 explicit sample")

    ! Op 1: schema & sample & error branches
    call fortran_udt_to_json(f_code%raw(1:f_code%len), opts, j_out, err)
    call th_eq_i32(st, err%code, CDD_OK, "Op 1 sample output")
    opts%json_mode = JSON_MODE_SCHEMA
    call fortran_udt_to_json(f_code%raw(1:f_code%len), opts, j_out, err)
    call th_eq_i32(st, err%code, CDD_OK, "Op 1 schema output")
    call fortran_udt_to_json("@bad", opts, j_out, err)
    call th_eq_i32(st, err%code, CDD_ERR_FORTRAN_LEX, "Op 1 Fortran lex error")

    ! Op 2: JSON object, JSON array, leading-ws JSON, Fortran source, whitespace-only source
    call cdd_default_options(opts)
    call generate_json_codecs(" " // achar(9) // achar(10) // achar(13) // '{"id":1}', &
      opts, c_code, err)
    call th_eq_i32(st, err%code, CDD_OK, "Op 2 leading-ws JSON object")
    call generate_json_codecs('[{"id":1}]', opts, c_code, err)
    call th_eq_i32(st, err%code, CDD_OK, "Op 2 JSON array sample")
    call generate_json_codecs(f_code%raw(1:f_code%len), opts, c_code, err)
    call th_eq_i32(st, err%code, CDD_OK, "Op 2 Fortran UDT input")
    call generate_json_codecs("   ", opts, c_code, err)
    call th_eq_i32(st, err%code, CDD_ERR_UDT_NOT_FOUND, "Op 2 blank input error")

    ! Op 3: pure update_fortran_from_json
    call update_fortran_from_json(f_code%raw(1:f_code%len), '{"id":7,"extra":1}', opts, u_code, err)
    call th_eq_i32(st, err%code, CDD_OK, "Op 3 pure facade")

    ! File wrappers: empty path "" (stdin/stdout), F77 --with-codecs, F2008 --submodules --with-codecs,
    ! and fault injection on every read/write stage
    call fs_init_mock(mfs)
    call buf_append_str(mfs%mock%stdin_buf, '{"id": 1}', err)
    call json_to_fortran_udt_file(mfs, "", "", .false._lk, opts, err)
    call th_eq_i32(st, err%code, CDD_OK, "Op 0 empty-path stdin/stdout")

    opts%standard%id = STD_FORTRAN_77
    call mock_fs_put(mfs%mock, "in.json", '{"id": 1}', err)
    call json_to_fortran_udt_file(mfs, "in.json", "out77.f", .true._lk, opts, err)
    call th_eq_i32(st, err%code, CDD_OK, "Op 0 F77 --with-codecs")

    opts%standard%id = STD_FORTRAN_2008
    opts%emit_submodules = .true._lk
    call json_to_fortran_udt_file(mfs, "in.json", "out08.f90", .true._lk, opts, err)
    call th_eq_i32(st, err%code, CDD_OK, "Op 0 F2008 submodules --with-codecs")

    opts%source_form = FORM_FREE
    opts%standard%id = STD_FORTRAN_77
    call mock_fs_put(mfs%mock, "bad.json", "{bad", err)
    call json_to_fortran_udt_file(mfs, "bad.json", "x.f", .true._lk, opts, err)
    call th_check(st, err%code /= CDD_OK, "Op 0 file with_codecs parse error")
    call json_to_fortran_udt_file(mfs, "bad.json", "x.f", .false._lk, opts, err)
    call th_check(st, err%code /= CDD_OK, "Op 0 file without_codecs parse error")

    call cdd_default_options(opts)
    mfs%mock%fail_next_read = .true._lk
    call json_to_fortran_udt_file(mfs, "in.json", "x.f90", .false._lk, opts, err)
    call th_check(st, err%code /= CDD_OK, "Op 0 read fail")

    mfs%mock%fail_next_read = .true._lk
    call fortran_udt_to_json_file(mfs, "out08.f90", "x.json", opts, err)
    call th_check(st, err%code /= CDD_OK, "Op 1 read fail")
    call mock_fs_put(mfs%mock, "bad.f90", "@bad", err)
    call fortran_udt_to_json_file(mfs, "bad.f90", "x.json", opts, err)
    call th_check(st, err%code /= CDD_OK, "Op 1 extract fail")

    mfs%mock%fail_next_read = .true._lk
    call generate_json_codecs_file(mfs, "in.json", "c.f90", opts, err)
    call th_check(st, err%code /= CDD_OK, "Op 2 read fail")
    call generate_json_codecs_file(mfs, "bad.json", "c.f90", opts, err)
    call th_check(st, err%code /= CDD_OK, "Op 2 codegen fail")

    mfs%mock%fail_next_read = .true._lk
    call update_fortran_from_json_file(mfs, "out08.f90", "in.json", "-", .false._lk, &
      .false._lk, oos, opts, err)
    call th_check(st, err%code /= CDD_OK, "Op 3 target read fail")

    call mock_fs_put(mfs%mock, "t.f90", "module m" // LF // "end module m" // LF, err)
    call update_fortran_from_json_file(mfs, "t.f90", "missing.json", "-", .false._lk, &
      .false._lk, oos, opts, err)
    call th_check(st, err%code /= CDD_OK, "Op 3 json read fail")

    call update_fortran_from_json_file(mfs, "t.f90", "bad.json", "-", .false._lk, &
      .false._lk, oos, opts, err)
    call th_check(st, err%code /= CDD_OK, "Op 3 patch fail")
  end subroutine test_sdk_pure_and_file_facade

  !> @brief Exercise every subcommand, alias, flag, and error branch in `parse_cli_args`.
  !> @param[inout] st Test harness state.
  subroutine test_cli_arg_parser_branches(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_string_t) :: args(CDD_MAX_CLI_ARGS)
    type(cdd_cli_command_t) :: cmd
    type(cdd_error_t) :: err
    integer(i32) :: n

    call parse_cli_args(args, 0_i32, cmd, err)
    call th_eq_i32(st, err%code, CDD_ERR_CLI_USAGE, "0 args rejected")

    call make_args("-h", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_OK, "-h")

    call make_args("json2fortran --help", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_OK, "--help")

    call make_args("-V", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_OK, "-V")

    call make_args("--version", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_OK, "--version")

    call make_args("from-json --with-codecs --dec-structure --submodules -i a.json -o a.f90 " // &
      "-s f77 --form fixed --type-name Orbit --json-mode schema", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_OK, "from-json full flags")

    call make_args("to-json --input a.f90 --output a.json --std f2003 --form free " // &
      "--json-mode sample", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_OK, "to-json long flags")

    call make_args("update --target t.f90 --in-place --check --form auto --json-mode auto", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_OK, "update alias + auto modes")

    ! Error branches
    call make_args("bogus-cmd", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_ERR_CLI_USAGE, "unknown subcommand")

    call make_args("gen-codec --bogus-flag", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_ERR_CLI_USAGE, "unknown flag")

    call make_args("gen-codec -i", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_ERR_CLI_USAGE, "missing -i value")

    call make_args("gen-codec -o", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_ERR_CLI_USAGE, "missing -o value")

    call make_args("sync -t", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_ERR_CLI_USAGE, "missing -t value")

    call make_args("gen-codec -s", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_ERR_CLI_USAGE, "missing -s value")

    call make_args("gen-codec -s f9999", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "invalid -s standard")

    call make_args("gen-codec --form", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_ERR_CLI_USAGE, "missing --form value")

    call make_args("gen-codec --form punch", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_ERR_CLI_USAGE, "invalid --form value")

    call make_args("gen-codec --type-name", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_ERR_CLI_USAGE, "missing --type-name value")

    call make_args("gen-codec --json-mode", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_ERR_CLI_USAGE, "missing --json-mode value")

    call make_args("gen-codec --json-mode yaml", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_ERR_CLI_USAGE, "invalid --json-mode value")

    call make_args("sync -i a.json", args, n)
    call parse_cli_args(args, n, cmd, err)
    call th_eq_i32(st, err%code, CDD_ERR_CLI_USAGE, "sync missing -t")
  end subroutine test_cli_arg_parser_branches

  !> @brief Exercise `run_cli` across all subcommands, `--check`, `--in-place`, and mock I/O errors.
  !> @param[inout] st Test harness state.
  subroutine test_cli_runner_mock_fs(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_string_t) :: args(CDD_MAX_CLI_ARGS)
    type(cdd_fs_t) :: fs
    type(cdd_buffer_t) :: out_b
    type(cdd_error_t) :: err
    integer(i32) :: n, rc

    call fs_init_mock(fs)

    ! --help and --version and CLI usage failure
    call make_args("--help", args, n)
    call run_cli(args, n, fs, rc)
    call th_eq_i32(st, rc, 0_i32, "run_cli --help")

    call make_args("--version", args, n)
    call run_cli(args, n, fs, rc)
    call th_eq_i32(st, rc, 0_i32, "run_cli --version")

    call make_args("bad-sub", args, n)
    call run_cli(args, n, fs, rc)
    call th_eq_i32(st, rc, 1_i32, "run_cli usage error -> rc 1")

    ! json2fortran -> fortran2json -> gen-codec -> sync (--check out-of-sync -> --in-place -> --check in-sync)
    call mock_fs_put(fs%mock, "s.json", &
      '{"title":"Vec","type":"object","properties":{"x":{"type":"integer"}}}', err)
    call make_args("json2fortran -i s.json -o vec.f90 --with-codecs", args, n)
    call run_cli(args, n, fs, rc)
    call th_eq_i32(st, rc, 0_i32, "run_cli json2fortran --with-codecs")

    call make_args("fortran2json -i vec.f90 -o out.json --json-mode schema", args, n)
    call run_cli(args, n, fs, rc)
    call th_eq_i32(st, rc, 0_i32, "run_cli fortran2json")

    call make_args("gen-codec -i vec.f90 -o cod.f90", args, n)
    call run_cli(args, n, fs, rc)
    call th_eq_i32(st, rc, 0_i32, "run_cli gen-codec")

    call mock_fs_put(fs%mock, "s2.json", &
      '{"title":"Vec","type":"object","properties":{"x":{"type":"integer"},"y":{"type":"number"}}}', err)
    call make_args("sync -t vec.f90 -i s2.json --check", args, n)
    call run_cli(args, n, fs, rc)
    call th_eq_i32(st, rc, 1_i32, "run_cli sync --check out-of-sync returns 1")

    call make_args("sync -t vec.f90 -i s2.json -o synced.f90", args, n)
    call run_cli(args, n, fs, rc)
    call th_eq_i32(st, rc, 0_i32, "run_cli sync to -o file")

    call make_args("sync -t vec.f90 -i s2.json --in-place", args, n)
    call run_cli(args, n, fs, rc)
    call th_eq_i32(st, rc, 0_i32, "run_cli sync --in-place")
    call mock_fs_get(fs%mock, "vec.f90", out_b, err)
    call th_check(st, index(out_b%raw(1:out_b%len), "self%y") > 0, "vec.f90 updated in place")

    call make_args("sync -t vec.f90 -i s2.json --check", args, n)
    call run_cli(args, n, fs, rc)
    call th_eq_i32(st, rc, 0_i32, "run_cli sync --check in-sync returns 0")

    ! Write failure propagation inside run_cli -> exit_code = 1
    fs%mock%fail_next_write = .true._lk
    call make_args("--version", args, n)
    call run_cli(args, n, fs, rc)
    call th_eq_i32(st, rc, 1_i32, "run_cli write failure -> rc 1")
  end subroutine test_cli_runner_mock_fs

  !> @brief Execute real `./build/cdd-fortran` binary across all 12 standards + exit 0/1 paths.
  !> @param[inout] st Test harness state.
  subroutine test_end_to_end_binary_12_standards(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_fs_t) :: os_fs
    type(cdd_buffer_t) :: rt_json
    type(cdd_error_t) :: err
    integer(i32) :: k
    integer :: rc
    character(len=8, kind=c_k), parameter :: STDS(12) = [ &
      "f57     ", "f58     ", "f58-iii ", "f62     ", "f66     ", "f77     ", &
      "f90     ", "f95     ", "f2003   ", "f2008   ", "f2018   ", "f2023   " ]

    call fs_init_os(os_fs)
    call execute_command_line("mkdir -p build/cli_e2e", exitstat=rc)
    call fs_write_text_file(os_fs, "build/cli_e2e/in.json", &
      '{"$schema":"https://json-schema.org/draft/2020-12/schema",' // &
      '"title":"Probe","type":"object","required":["cnt"],' // &
      '"properties":{"cnt":{"type":"integer"},"mass":{"type":"number"},' // &
      '"ok":{"type":"boolean"},"tag":{"type":"string","maxLength":12}}}', err)

    ! 0-arg invocation (exercises 0-arg loop & exit_code /= 0 branch in app/main.f90!)
    call execute_command_line("./build/cdd-fortran > /dev/null 2>&1", exitstat=rc)
    call th_check(st, rc /= 0, "cdd-fortran 0-arg returns non-zero")

    ! --version invocation (exercises >=1-arg loop & exit_code == 0 branch in app/main.f90!)
    call execute_command_line("./build/cdd-fortran --version > /dev/null", exitstat=rc)
    call th_eq_i32(st, int(rc, i32), 0_i32, "cdd-fortran --version returns 0")

    ! Full CLI Op 0 -> Op 1 -> Op 2 -> Op 3 round-trip across ALL 12 Fortran standards!
    do k = 1_i32, 12_i32
      call execute_command_line( &
        "./build/cdd-fortran json2fortran -s " // trim(STDS(k)) // &
        " -i build/cli_e2e/in.json -o build/cli_e2e/udt.txt", exitstat=rc)
      call th_eq_i32(st, int(rc, i32), 0_i32, "CLI json2fortran " // trim(STDS(k)))

      call execute_command_line( &
        "./build/cdd-fortran fortran2json -s " // trim(STDS(k)) // &
        " -i build/cli_e2e/udt.txt -o build/cli_e2e/out.json", exitstat=rc)
      call th_eq_i32(st, int(rc, i32), 0_i32, "CLI fortran2json " // trim(STDS(k)))

      call fs_read_text_file(os_fs, "build/cli_e2e/out.json", rt_json, err)
      call th_check(st, index(rt_json%raw(1:rt_json%len), '"mass"') > 0, &
        "CLI round-trip preserved field mass in " // trim(STDS(k)))

      call execute_command_line( &
        "./build/cdd-fortran gen-codec -s " // trim(STDS(k)) // &
        " -i build/cli_e2e/in.json -o build/cli_e2e/codec.txt", exitstat=rc)
      call th_eq_i32(st, int(rc, i32), 0_i32, "CLI gen-codec " // trim(STDS(k)))

      call execute_command_line( &
        "./build/cdd-fortran sync -s " // trim(STDS(k)) // &
        " -t build/cli_e2e/udt.txt -i build/cli_e2e/in.json --in-place", exitstat=rc)
      call th_eq_i32(st, int(rc, i32), 0_i32, "CLI sync --in-place " // trim(STDS(k)))
    end do
  end subroutine test_end_to_end_binary_12_standards

end program test_cli_suite
