!> @file main.f90
!> @brief Minimal OS wrapper entry point for the `cdd-fortran` command-line executable.

!> @brief Collect command-line arguments, initialize OS stream I/O, and invoke `run_cli`.
!> @return Process exit status (0 on success, 1 on CLI/SDK error or `--check` diff).
program main
  use cdd_kinds, only: i32, c_k
  use cdd_errors, only: cdd_error_t
  use cdd_string, only: cdd_string_t, str_set
  use cdd_fs, only: cdd_fs_t, fs_init_os
  use cdd_cli_args, only: CDD_MAX_CLI_ARGS
  use cdd_cli_runner, only: run_cli
  implicit none (type, external)

  type(cdd_string_t) :: args(CDD_MAX_CLI_ARGS)
  type(cdd_fs_t) :: fs
  type(cdd_error_t) :: err
  character(len=256, kind=c_k) :: raw_arg
  integer :: raw_cnt, raw_len, k
  integer(i32) :: n_args, exit_code

  raw_cnt = command_argument_count()
  n_args = min(int(raw_cnt, i32), CDD_MAX_CLI_ARGS)
  do k = 1, int(n_args)
    raw_arg = " "
    call get_command_argument(k, raw_arg, raw_len)
    call str_set(args(k), raw_arg(1:max(0, min(256, raw_len))), err)
  end do
  call fs_init_os(fs)
  call run_cli(args, n_args, fs, exit_code)
  if (exit_code /= 0_i32) stop 1, quiet=.true.
end program main
