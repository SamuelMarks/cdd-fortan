!> @file cdd_cli_args.f90
!> @brief Pure, strongly-typed CLI argument parser for `cdd-fortran`.

!> @brief Defines `cdd_cli_command_t`, subcommand constants, help text, and `parse_cli_args`.
module cdd_cli_args
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: CDD_ERR_CLI_USAGE, cdd_error_t, cdd_error_clear, cdd_error_set_simple
  use cdd_string, only: cdd_string_t, cdd_buffer_t, str_set, str_eq, str_eq_nocase, &
    buf_init, buf_append_str
  use cdd_standard, only: FORM_AUTO, FORM_FIXED, FORM_FREE, JSON_MODE_AUTO, JSON_MODE_SCHEMA, &
    JSON_MODE_SAMPLE, cdd_options_t, cdd_default_options, parse_standard_name
  implicit none (type, external)
  private

  public :: CDD_MAX_CLI_ARGS
  public :: CLI_CMD_HELP, CLI_CMD_VERSION
  public :: CLI_CMD_JSON2FORTRAN, CLI_CMD_FORTRAN2JSON, CLI_CMD_GEN_CODEC, CLI_CMD_SYNC
  public :: cdd_cli_command_t, cli_command_init, parse_cli_args, format_cli_help

  integer(i32), parameter :: CDD_MAX_CLI_ARGS = 64_i32      !< Max CLI tokens parsed per invocation.
  integer(i32), parameter :: CLI_CMD_HELP = 1_i32           !< Display usage help (`-h|--help`).
  integer(i32), parameter :: CLI_CMD_VERSION = 2_i32        !< Display version (`-V|--version`).
  integer(i32), parameter :: CLI_CMD_JSON2FORTRAN = 3_i32   !< Op 0 (`json2fortran` / `from-json`).
  integer(i32), parameter :: CLI_CMD_FORTRAN2JSON = 4_i32   !< Op 1 (`fortran2json` / `to-json`).
  integer(i32), parameter :: CLI_CMD_GEN_CODEC = 5_i32      !< Op 2 (`gen-codec`).
  integer(i32), parameter :: CLI_CMD_SYNC = 6_i32           !< Op 3 (`sync` / `update`).

  !> @brief Parsed strongly-typed CLI command specification.
  type :: cdd_cli_command_t
    integer(i32) :: subcommand = CLI_CMD_HELP !< Selected CLI_CMD_* action.
    type(cdd_string_t) :: input_path          !< `-i|--input` path (`"-"` default).
    type(cdd_string_t) :: output_path         !< `-o|--output` path (`"-"` default).
    type(cdd_string_t) :: target_path         !< `-t|--target` existing Fortran file for `sync`.
    logical(lk) :: with_codecs = .false._lk   !< `--with-codecs` flag for `json2fortran`.
    logical(lk) :: in_place = .false._lk      !< `--in-place` flag for `sync`.
    logical(lk) :: check_only = .false._lk    !< `--check` dry-run flag for `sync`.
    type(cdd_options_t) :: options            !< Target Fortran standard, form, and codegen flags.
  end type cdd_cli_command_t

contains

  !> @brief Initialize `cdd_cli_command_t` with deterministic defaults (`"-"` stdin/stdout, F2023).
  !> @param[out] cmd Command structure to initialize.
  pure subroutine cli_command_init(cmd)
    type(cdd_cli_command_t), intent(out) :: cmd
    type(cdd_error_t) :: unused_err

    cmd%subcommand = CLI_CMD_HELP
    call str_set(cmd%input_path, "-", unused_err)
    call str_set(cmd%output_path, "-", unused_err)
    cmd%target_path%len = 0_i32
    cmd%with_codecs = .false._lk
    cmd%in_place = .false._lk
    cmd%check_only = .false._lk
    call cdd_default_options(cmd%options)
  end subroutine cli_command_init

  !> @brief Parse an array of CLI argument tokens into `cdd_cli_command_t`.
  !> @param[in]  args      CLI argument array `args(1:arg_count)`.
  !> @param[in]  arg_count Number of active arguments.
  !> @param[out] cmd       Parsed CLI command configuration.
  !> @param[out] err       Error status (`CDD_ERR_CLI_USAGE` on invalid flags/subcommands).
  pure subroutine parse_cli_args(args, arg_count, cmd, err)
    type(cdd_string_t), intent(in) :: args(CDD_MAX_CLI_ARGS)
    integer(i32), intent(in) :: arg_count
    type(cdd_cli_command_t), intent(out) :: cmd
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: i, alen, vlen

    call cli_command_init(cmd)
    call cdd_error_clear(err)

    if (arg_count <= 0_i32) then
      call cdd_error_set_simple(err, CDD_ERR_CLI_USAGE, "Missing subcommand (try --help)")
      return
    end if

    do i = 1_i32, arg_count
      alen = args(i)%len
      if (str_eq(args(i)%raw(1:alen), "-h") .or. str_eq(args(i)%raw(1:alen), "--help")) then
        cmd%subcommand = CLI_CMD_HELP
        return
      else if (str_eq(args(i)%raw(1:alen), "-V") .or. str_eq(args(i)%raw(1:alen), "--version")) then
        cmd%subcommand = CLI_CMD_VERSION
        return
      end if
    end do

    alen = args(1)%len
    if (str_eq(args(1)%raw(1:alen), "json2fortran") .or. &
        str_eq(args(1)%raw(1:alen), "from-json")) then
      cmd%subcommand = CLI_CMD_JSON2FORTRAN
    else if (str_eq(args(1)%raw(1:alen), "fortran2json") .or. &
             str_eq(args(1)%raw(1:alen), "to-json")) then
      cmd%subcommand = CLI_CMD_FORTRAN2JSON
    else if (str_eq(args(1)%raw(1:alen), "gen-codec")) then
      cmd%subcommand = CLI_CMD_GEN_CODEC
    else if (str_eq(args(1)%raw(1:alen), "sync") .or. &
             str_eq(args(1)%raw(1:alen), "update")) then
      cmd%subcommand = CLI_CMD_SYNC
    else
      call cdd_error_set_simple(err, CDD_ERR_CLI_USAGE, &
        "Unknown subcommand: " // args(1)%raw(1:alen))
      return
    end if

    i = 2_i32
    do while (i <= arg_count)
      alen = args(i)%len
      if (str_eq(args(i)%raw(1:alen), "--with-codecs")) then
        cmd%with_codecs = .true._lk
        i = i + 1_i32
      else if (str_eq(args(i)%raw(1:alen), "--dec-structure")) then
        cmd%options%allow_dec_structure = .true._lk
        i = i + 1_i32
      else if (str_eq(args(i)%raw(1:alen), "--submodules")) then
        cmd%options%emit_submodules = .true._lk
        i = i + 1_i32
      else if (str_eq(args(i)%raw(1:alen), "--in-place")) then
        cmd%in_place = .true._lk
        i = i + 1_i32
      else if (str_eq(args(i)%raw(1:alen), "--check")) then
        cmd%check_only = .true._lk
        i = i + 1_i32
      else if (str_eq(args(i)%raw(1:alen), "-i") .or. &
               str_eq(args(i)%raw(1:alen), "--input")) then
        if (i == arg_count) then
          call cdd_error_set_simple(err, CDD_ERR_CLI_USAGE, "Missing value for --input")
          return
        end if
        cmd%input_path = args(i + 1_i32)
        i = i + 2_i32
      else if (str_eq(args(i)%raw(1:alen), "-o") .or. &
               str_eq(args(i)%raw(1:alen), "--output")) then
        if (i == arg_count) then
          call cdd_error_set_simple(err, CDD_ERR_CLI_USAGE, "Missing value for --output")
          return
        end if
        cmd%output_path = args(i + 1_i32)
        i = i + 2_i32
      else if (str_eq(args(i)%raw(1:alen), "-t") .or. &
               str_eq(args(i)%raw(1:alen), "--target")) then
        if (i == arg_count) then
          call cdd_error_set_simple(err, CDD_ERR_CLI_USAGE, "Missing value for --target")
          return
        end if
        cmd%target_path = args(i + 1_i32)
        i = i + 2_i32
      else if (str_eq(args(i)%raw(1:alen), "-s") .or. &
               str_eq(args(i)%raw(1:alen), "--std")) then
        if (i == arg_count) then
          call cdd_error_set_simple(err, CDD_ERR_CLI_USAGE, "Missing value for --std")
          return
        end if
        vlen = args(i + 1_i32)%len
        call parse_standard_name(args(i + 1_i32)%raw(1:vlen), cmd%options%standard, err)
        if (err%code /= 0_i32) return
        i = i + 2_i32
      else if (str_eq(args(i)%raw(1:alen), "--form")) then
        if (i == arg_count) then
          call cdd_error_set_simple(err, CDD_ERR_CLI_USAGE, "Missing value for --form")
          return
        end if
        vlen = args(i + 1_i32)%len
        if (str_eq_nocase(args(i + 1_i32)%raw(1:vlen), "auto")) then
          cmd%options%source_form = FORM_AUTO
        else if (str_eq_nocase(args(i + 1_i32)%raw(1:vlen), "fixed")) then
          cmd%options%source_form = FORM_FIXED
        else if (str_eq_nocase(args(i + 1_i32)%raw(1:vlen), "free")) then
          cmd%options%source_form = FORM_FREE
        else
          call cdd_error_set_simple(err, CDD_ERR_CLI_USAGE, "Invalid --form (expected auto|fixed|free)")
          return
        end if
        i = i + 2_i32
      else if (str_eq(args(i)%raw(1:alen), "--type-name")) then
        if (i == arg_count) then
          call cdd_error_set_simple(err, CDD_ERR_CLI_USAGE, "Missing value for --type-name")
          return
        end if
        cmd%options%target_udt_name = args(i + 1_i32)
        i = i + 2_i32
      else if (str_eq(args(i)%raw(1:alen), "--json-mode")) then
        if (i == arg_count) then
          call cdd_error_set_simple(err, CDD_ERR_CLI_USAGE, "Missing value for --json-mode")
          return
        end if
        vlen = args(i + 1_i32)%len
        if (str_eq_nocase(args(i + 1_i32)%raw(1:vlen), "auto")) then
          cmd%options%json_mode = JSON_MODE_AUTO
        else if (str_eq_nocase(args(i + 1_i32)%raw(1:vlen), "schema")) then
          cmd%options%json_mode = JSON_MODE_SCHEMA
        else if (str_eq_nocase(args(i + 1_i32)%raw(1:vlen), "sample")) then
          cmd%options%json_mode = JSON_MODE_SAMPLE
        else
          call cdd_error_set_simple(err, CDD_ERR_CLI_USAGE, &
            "Invalid --json-mode (expected auto|schema|sample)")
          return
        end if
        i = i + 2_i32
      else
        call cdd_error_set_simple(err, CDD_ERR_CLI_USAGE, &
          "Unknown CLI option: " // args(i)%raw(1:alen))
        return
      end if
    end do

    if (cmd%subcommand == CLI_CMD_SYNC .and. cmd%target_path%len == 0_i32) then
      call cdd_error_set_simple(err, CDD_ERR_CLI_USAGE, &
        "Subcommand 'sync' requires -t|--target <fortran-file>")
    end if
  end subroutine parse_cli_args

  !> @brief Build CLI `--help` documentation text covering all subcommands, flags, and 12 standards.
  !> @param[out] help_buf Populated help message buffer.
  pure subroutine format_cli_help(help_buf)
    type(cdd_buffer_t), intent(out) :: help_buf
    type(cdd_error_t) :: err
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call buf_init(help_buf)
    call cdd_error_clear(err)
    call buf_append_str(help_buf, &
      "cdd-fortran - Bidirectional JSON <-> Fortran UDT SDK & CLI (FORTRAN I .. Fortran 2023)" // &
      LF // LF // &
      "Usage: cdd-fortran <subcommand> [options]" // LF // LF // &
      "Subcommands:" // LF // &
      "  json2fortran (from-json)  Convert JSON Schema or sample JSON to Fortran UDT" // LF // &
      "  fortran2json (to-json)    Extract Fortran UDT to JSON Schema or sample JSON" // LF // &
      "  gen-codec                 Generate zero-dependency JSON <-> UDT codecs" // LF // &
      "  sync (update)             Surgically patch existing Fortran UDT & codecs in place" // &
      LF // LF // &
      "Options:" // LF // &
      "  -i, --input <file|->      Input file path or '-' for stdin" // LF // &
      "  -o, --output <file|->     Output file path or '-' for stdout" // LF // &
      "  -t, --target <file>       Target Fortran file for 'sync'" // LF // &
      "  -s, --std <standard>      f57|f58|f58-iii|f62|f66|f77|f90|f95|f2003|f2008|f2018|f2023" // LF // &
      "  --form <auto|fixed|free>  Override source form" // LF // &
      "  --dec-structure           Enable DEC STRUCTURE / RECORD mode (F62/F77)" // LF // &
      "  --submodules              Emit F2008+ SUBMODULE codec split" // LF // &
      "  --type-name <name>        Target UDT name" // LF // &
      "  --json-mode <mode>        auto | schema | sample" // LF // &
      "  --with-codecs             Include JSON <-> UDT routines in json2fortran" // LF // &
      "  --in-place                Modify target Fortran file in place during sync" // LF // &
      "  --check                   Dry-run sync check (exit 1 if out of sync)" // LF // &
      "  -h, --help                Show this help text" // LF // &
      "  -V, --version             Show cdd-fortran version" // LF, err)
  end subroutine format_cli_help

end module cdd_cli_args
