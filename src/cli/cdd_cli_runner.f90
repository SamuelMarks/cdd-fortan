!> @file cdd_cli_runner.f90
!> @brief Mockable-FS CLI command dispatcher delegating exclusively to `cdd_fortran` SDK.

!> @brief Executes parsed CLI commands against `cdd_fs_t` without direct OS side effects.
module cdd_cli_runner
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_format
  use cdd_string, only: cdd_string_t, cdd_buffer_t
  use cdd_fs, only: cdd_fs_t, fs_write_stdout
  use cdd_fortran, only: CDD_FORTRAN_VERSION, json_to_fortran_udt_file, fortran_udt_to_json_file, &
    generate_json_codecs_file, update_fortran_from_json_file
  use cdd_cli_args, only: CDD_MAX_CLI_ARGS, CLI_CMD_HELP, CLI_CMD_VERSION, CLI_CMD_JSON2FORTRAN, &
    CLI_CMD_FORTRAN2JSON, CLI_CMD_GEN_CODEC, cdd_cli_command_t, parse_cli_args, format_cli_help
  implicit none (type, external)
  private

  public :: run_cli

contains

  !> @brief Parse CLI arguments and execute the requested subcommand over `fs`.
  !> @param[in]    args      CLI argument token array `args(1:arg_count)`.
  !> @param[in]    arg_count Number of active argument tokens.
  !> @param[inout] fs        Filesystem provider (`cdd_fs_t`, OS or in-memory mock).
  !> @param[out]   exit_code Process exit code (`0` on success, `1` on error or `--check` diff).
  subroutine run_cli(args, arg_count, fs, exit_code)
    type(cdd_string_t), intent(in) :: args(CDD_MAX_CLI_ARGS)
    integer(i32), intent(in) :: arg_count
    type(cdd_fs_t), intent(inout) :: fs
    integer(i32), intent(out) :: exit_code
    type(cdd_cli_command_t) :: cmd
    type(cdd_error_t) :: err, ign_err
    type(cdd_buffer_t) :: msg_buf
    character(len=512, kind=c_k) :: err_str
    integer(i32) :: err_len
    logical(lk) :: out_of_sync
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    exit_code = 0_i32
    call parse_cli_args(args, arg_count, cmd, err)
    if (err%code /= 0_i32) then
      call cdd_error_format(err, err_str, err_len)
      call fs_write_stdout(fs, err_str(1:err_len) // LF, ign_err)
      exit_code = 1_i32
      return
    end if

    select case (cmd%subcommand)
    case (CLI_CMD_HELP)
      call format_cli_help(msg_buf)
      call fs_write_stdout(fs, msg_buf%raw(1:msg_buf%len), err)
    case (CLI_CMD_VERSION)
      call fs_write_stdout(fs, "cdd-fortran " // CDD_FORTRAN_VERSION // LF, err)
    case (CLI_CMD_JSON2FORTRAN)
      call json_to_fortran_udt_file(fs, cmd%input_path%raw(1:cmd%input_path%len), &
        cmd%output_path%raw(1:cmd%output_path%len), cmd%with_codecs, cmd%options, err)
    case (CLI_CMD_FORTRAN2JSON)
      call fortran_udt_to_json_file(fs, cmd%input_path%raw(1:cmd%input_path%len), &
        cmd%output_path%raw(1:cmd%output_path%len), cmd%options, err)
    case (CLI_CMD_GEN_CODEC)
      call generate_json_codecs_file(fs, cmd%input_path%raw(1:cmd%input_path%len), &
        cmd%output_path%raw(1:cmd%output_path%len), cmd%options, err)
    case default
      call update_fortran_from_json_file(fs, cmd%target_path%raw(1:cmd%target_path%len), &
        cmd%input_path%raw(1:cmd%input_path%len), cmd%output_path%raw(1:cmd%output_path%len), &
        cmd%in_place, cmd%check_only, out_of_sync, cmd%options, err)
      if (err%code == 0_i32 .and. cmd%check_only .and. out_of_sync) then
        exit_code = 1_i32
        return
      end if
    end select

    if (err%code /= 0_i32) then
      call cdd_error_format(err, err_str, err_len)
      call fs_write_stdout(fs, err_str(1:err_len) // LF, ign_err)
      exit_code = 1_i32
    end if
  end subroutine run_cli

end module cdd_cli_runner
