!> @file cdd_kinds.f90
!> @brief Explicit numeric, logical, I/O unit, and character kind constants for cdd-fortran.

!> @brief Strongly-typed kind parameters re-exported from intrinsic iso_fortran_env.
module cdd_kinds
  use, intrinsic :: iso_fortran_env, only: &
    int8, int16, int32, int64, real32, real64, real128, &
    input_unit, output_unit, error_unit
  implicit none (type, external)
  private

  public :: i8, i16, i32, i64, r32, r64, r128, lk, c_k
  public :: std_in_unit, std_out_unit, std_err_unit

  integer(int32), parameter :: i8 = int8          !< 8-bit signed integer kind.
  integer(int32), parameter :: i16 = int16        !< 16-bit signed integer kind.
  integer(int32), parameter :: i32 = int32        !< 32-bit signed integer kind.
  integer(int32), parameter :: i64 = int64        !< 64-bit signed integer kind.
  integer(int32), parameter :: r32 = real32       !< IEEE 32-bit single-precision real kind.
  integer(int32), parameter :: r64 = real64       !< IEEE 64-bit double-precision real kind.
  integer(int32), parameter :: r128 = real128     !< IEEE 128-bit quadruple-precision real kind.
  integer(int32), parameter :: lk = kind(.true.)  !< Default logical kind parameter.
  integer(int32), parameter :: c_k = kind("a")    !< Default ASCII character kind parameter.
  integer(int32), parameter :: std_in_unit = input_unit   !< Standard input unit number.
  integer(int32), parameter :: std_out_unit = output_unit !< Standard output unit number.
  integer(int32), parameter :: std_err_unit = error_unit  !< Standard error unit number.

end module cdd_kinds
