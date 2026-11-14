!> @file test_harness.f90
!> @brief Zero-dependency unit-test assertion and polymorphic vtable exerciser module.

!> @brief Strongly-typed test assertion utilities for cdd-fortran test suites.
module test_harness
  use cdd_kinds, only: i32, i64, r64, lk, c_k, std_err_unit, std_out_unit
  use cdd_string, only: str_eq
  implicit none (type, external)
  private

  public :: test_state_t
  public :: th_init, th_check, th_eq_i32, th_eq_i64, th_near_r64, th_eq_str
  public :: th_touch_vtable, th_finish

  !> @brief Accumulator tracking assertion counts and failure status for a test binary.
  type :: test_state_t
    integer(i32) :: passed = 0_i32 !< Count of passing assertions.
    integer(i32) :: failed = 0_i32 !< Count of failing assertions.
  end type test_state_t

contains

  !> @brief Initialize a fresh test_state_t accumulator.
  !> @param[out] st Test state record.
  pure subroutine th_init(st)
    type(test_state_t), intent(out) :: st

    st%passed = 0_i32
    st%failed = 0_i32
  end subroutine th_init

  !> @brief Assert that cond is true, logging label to stderr if false.
  !> @param[inout] st    Test state accumulator.
  !> @param[in]    cond  Boolean condition expected to be true.
  !> @param[in]    label Diagnostic test description.
  subroutine th_check(st, cond, label)
    type(test_state_t), intent(inout) :: st
    logical(lk), intent(in) :: cond
    character(len=*, kind=c_k), intent(in) :: label

    if (cond) then
      st%passed = st%passed + 1_i32
    else
      st%failed = st%failed + 1_i32
      write(std_err_unit, "(A,A)") "ASSERT FAILED: ", label
    end if
  end subroutine th_check

  !> @brief Assert equality of two 32-bit signed integers.
  !> @param[inout] st       Test state accumulator.
  !> @param[in]    actual   Actual integer value.
  !> @param[in]    expected Expected integer value.
  !> @param[in]    label    Diagnostic test description.
  subroutine th_eq_i32(st, actual, expected, label)
    type(test_state_t), intent(inout) :: st
    integer(i32), intent(in) :: actual
    integer(i32), intent(in) :: expected
    character(len=*, kind=c_k), intent(in) :: label

    if (actual == expected) then
      st%passed = st%passed + 1_i32
    else
      st%failed = st%failed + 1_i32
      write(std_err_unit, "(A,A,A,I0,A,I0)") "ASSERT EQ_I32 FAILED: ", label, &
        " actual=", actual, " expected=", expected
    end if
  end subroutine th_eq_i32

  !> @brief Assert equality of two 64-bit signed integers.
  !> @param[inout] st       Test state accumulator.
  !> @param[in]    actual   Actual 64-bit integer value.
  !> @param[in]    expected Expected 64-bit integer value.
  !> @param[in]    label    Diagnostic test description.
  subroutine th_eq_i64(st, actual, expected, label)
    type(test_state_t), intent(inout) :: st
    integer(i64), intent(in) :: actual
    integer(i64), intent(in) :: expected
    character(len=*, kind=c_k), intent(in) :: label

    if (actual == expected) then
      st%passed = st%passed + 1_i32
    else
      st%failed = st%failed + 1_i32
      write(std_err_unit, "(A,A,A,I0,A,I0)") "ASSERT EQ_I64 FAILED: ", label, &
        " actual=", actual, " expected=", expected
    end if
  end subroutine th_eq_i64

  !> @brief Assert approximate equality of two 64-bit reals within tol.
  !> @param[inout] st       Test state accumulator.
  !> @param[in]    actual   Actual real(r64) value.
  !> @param[in]    expected Expected real(r64) value.
  !> @param[in]    tol      Maximum permitted absolute difference.
  !> @param[in]    label    Diagnostic test description.
  subroutine th_near_r64(st, actual, expected, tol, label)
    type(test_state_t), intent(inout) :: st
    real(r64), intent(in) :: actual
    real(r64), intent(in) :: expected
    real(r64), intent(in) :: tol
    character(len=*, kind=c_k), intent(in) :: label

    if (abs(actual - expected) <= tol) then
      st%passed = st%passed + 1_i32
    else
      st%failed = st%failed + 1_i32
      write(std_err_unit, "(A,A)") "ASSERT NEAR_R64 FAILED: ", label
    end if
  end subroutine th_near_r64

  !> @brief Assert exact byte-for-byte equality of two string slices.
  !> @param[inout] st       Test state accumulator.
  !> @param[in]    actual   Actual character slice.
  !> @param[in]    expected Expected character slice.
  !> @param[in]    label    Diagnostic test description.
  subroutine th_eq_str(st, actual, expected, label)
    type(test_state_t), intent(inout) :: st
    character(len=*, kind=c_k), intent(in) :: actual
    character(len=*, kind=c_k), intent(in) :: expected
    character(len=*, kind=c_k), intent(in) :: label

    if (str_eq(actual, expected)) then
      st%passed = st%passed + 1_i32
    else
      st%failed = st%failed + 1_i32
      write(std_err_unit, "(A,A,A,A,A,A,A)") "ASSERT EQ_STR FAILED: ", label, &
        " actual=[", actual, "] expected=[", expected, "]"
    end if
  end subroutine th_eq_str

  !> @brief Exercise gfortran's compiler-generated polymorphic __copy vtable thunk for a type.
  !> @param[in] sample Instance of any extensible derived type.
  subroutine th_touch_vtable(sample)
    class(*), intent(in) :: sample
    class(*), allocatable :: cloned

    allocate(cloned, source=sample)
    deallocate(cloned)
  end subroutine th_touch_vtable

  !> @brief Print suite summary and terminate non-zero if any assertion failed.
  !> @param[in] st         Test state accumulator.
  !> @param[in] suite_name Human-readable test suite name.
  subroutine th_finish(st, suite_name)
    type(test_state_t), intent(in) :: st
    character(len=*, kind=c_k), intent(in) :: suite_name

    write(std_out_unit, "(A,A,A,I0,A,I0,A)") "[", suite_name, "] passed=", &
      st%passed, " failed=", st%failed, ""
    if (st%failed > 0_i32) then
      stop 1, quiet=.true.
    end if
  end subroutine th_finish

end module test_harness
