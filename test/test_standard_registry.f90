!> @file test_standard_registry.f90
!> @brief Comprehensive 100% branch-coverage unit tests for the 12-standard dialect registry.

!> @brief Execute all Milestone 2 standard registry and capability tests.
!> @return Program exit status (0 on pass).
program test_standard_registry
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: CDD_OK, CDD_ERR_INVALID_OPTION, cdd_error_t
  use cdd_standard, only: STD_FORTRAN_I, STD_FORTRAN_II, STD_FORTRAN_III, STD_FORTRAN_IV, &
    STD_FORTRAN_66, STD_FORTRAN_77, STD_FORTRAN_90, STD_FORTRAN_95, STD_FORTRAN_2003, &
    STD_FORTRAN_2008, STD_FORTRAN_2018, STD_FORTRAN_2023, FORM_AUTO, FORM_FIXED, FORM_FREE, &
    JSON_MODE_AUTO, JSON_MODE_SCHEMA, JSON_MODE_SAMPLE, cdd_standard_t, cdd_dialect_caps_t, &
    cdd_options_t, parse_standard_name, standard_display_name, get_dialect_capabilities, &
    cdd_default_options, resolve_effective_form
  use test_harness, only: test_state_t, th_init, th_check, th_eq_i32, th_eq_str, &
    th_touch_vtable, th_finish
  implicit none (type, external)

  type(test_state_t) :: st

  call th_init(st)
  call run_parse_and_display_tests(st)
  call run_capability_and_form_tests(st)
  call th_finish(st, "test_standard_registry")

contains

  !> @brief Verify all 12 standards, all CLI/human aliases, display names, and invalid inputs.
  !> @param[inout] st Test state accumulator.
  subroutine run_parse_and_display_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_standard_t) :: std
    type(cdd_error_t) :: err
    character(len=32, kind=c_k) :: disp
    character(len=4, kind=c_k) :: short_disp
    integer(i32) :: dlen

    call th_touch_vtable(std)
    call th_eq_i32(st, JSON_MODE_AUTO + JSON_MODE_SCHEMA + JSON_MODE_SAMPLE, 3_i32, "json modes")

    call check_alias(st, "f57", STD_FORTRAN_I)
    call check_alias(st, "fortran1", STD_FORTRAN_I)
    call check_alias(st, "fortran-i", STD_FORTRAN_I)
    call check_alias(st, "FORTRAN I", STD_FORTRAN_I)
    call check_alias(st, "  FORTRAN (I)  ", STD_FORTRAN_I)

    call check_alias(st, "f58", STD_FORTRAN_II)
    call check_alias(st, "fortran2", STD_FORTRAN_II)
    call check_alias(st, "fortran-ii", STD_FORTRAN_II)
    call check_alias(st, "FORTRAN II", STD_FORTRAN_II)

    call check_alias(st, "f58-iii", STD_FORTRAN_III)
    call check_alias(st, "f58b", STD_FORTRAN_III)
    call check_alias(st, "fortran3", STD_FORTRAN_III)
    call check_alias(st, "fortran-iii", STD_FORTRAN_III)
    call check_alias(st, "FORTRAN III", STD_FORTRAN_III)

    call check_alias(st, "f62", STD_FORTRAN_IV)
    call check_alias(st, "fortran4", STD_FORTRAN_IV)
    call check_alias(st, "fortran-iv", STD_FORTRAN_IV)
    call check_alias(st, "FORTRAN IV", STD_FORTRAN_IV)

    call check_alias(st, "f66", STD_FORTRAN_66)
    call check_alias(st, "fortran66", STD_FORTRAN_66)
    call check_alias(st, "fortran-66", STD_FORTRAN_66)
    call check_alias(st, "FORTRAN 66", STD_FORTRAN_66)

    call check_alias(st, "f77", STD_FORTRAN_77)
    call check_alias(st, "fortran77", STD_FORTRAN_77)
    call check_alias(st, "fortran-77", STD_FORTRAN_77)
    call check_alias(st, "FORTRAN 77", STD_FORTRAN_77)

    call check_alias(st, "f90", STD_FORTRAN_90)
    call check_alias(st, "fortran90", STD_FORTRAN_90)
    call check_alias(st, "fortran-90", STD_FORTRAN_90)
    call check_alias(st, "Fortran 90", STD_FORTRAN_90)

    call check_alias(st, "f95", STD_FORTRAN_95)
    call check_alias(st, "fortran95", STD_FORTRAN_95)
    call check_alias(st, "fortran-95", STD_FORTRAN_95)
    call check_alias(st, "Fortran 95", STD_FORTRAN_95)

    call check_alias(st, "f2003", STD_FORTRAN_2003)
    call check_alias(st, "f03", STD_FORTRAN_2003)
    call check_alias(st, "fortran2003", STD_FORTRAN_2003)
    call check_alias(st, "fortran-2003", STD_FORTRAN_2003)
    call check_alias(st, "Fortran 2003", STD_FORTRAN_2003)

    call check_alias(st, "f2008", STD_FORTRAN_2008)
    call check_alias(st, "f08", STD_FORTRAN_2008)
    call check_alias(st, "fortran2008", STD_FORTRAN_2008)
    call check_alias(st, "fortran-2008", STD_FORTRAN_2008)
    call check_alias(st, "Fortran 2008", STD_FORTRAN_2008)

    call check_alias(st, "f2018", STD_FORTRAN_2018)
    call check_alias(st, "f18", STD_FORTRAN_2018)
    call check_alias(st, "fortran2018", STD_FORTRAN_2018)
    call check_alias(st, "fortran-2018", STD_FORTRAN_2018)
    call check_alias(st, "Fortran 2018", STD_FORTRAN_2018)

    call check_alias(st, "f2023", STD_FORTRAN_2023)
    call check_alias(st, "f23", STD_FORTRAN_2023)
    call check_alias(st, "fortran2023", STD_FORTRAN_2023)
    call check_alias(st, "fortran-2023", STD_FORTRAN_2023)
    call check_alias(st, "Fortran 2023", STD_FORTRAN_2023)

    call parse_standard_name("   ", std, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "empty standard string")
    call parse_standard_name("fortran-2099", std, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "unknown standard string")

    do dlen = STD_FORTRAN_I, STD_FORTRAN_2023
      std%id = dlen
      call standard_display_name(std, disp, err%code)
      call th_check(st, err%code >= 10_i32, "display_name len")
    end do
    std%id = STD_FORTRAN_I
    call standard_display_name(std, short_disp, dlen)
    call th_eq_i32(st, dlen, 4_i32, "display_name clamped")
  end subroutine run_parse_and_display_tests

  !> @brief Parse a single alias and assert it maps to expected_id.
  !> @param[inout] st          Test state accumulator.
  !> @param[in]    alias_str   Input standard alias.
  !> @param[in]    expected_id Expected standard ID constant.
  subroutine check_alias(st, alias_str, expected_id)
    type(test_state_t), intent(inout) :: st
    character(len=*, kind=c_k), intent(in) :: alias_str
    integer(i32), intent(in) :: expected_id
    type(cdd_standard_t) :: std
    type(cdd_error_t) :: err

    call parse_standard_name(alias_str, std, err)
    call th_eq_i32(st, err%code, CDD_OK, alias_str)
    call th_eq_i32(st, std%id, expected_id, alias_str)
  end subroutine check_alias

  !> @brief Exercise capability matrix flags and source-form validation across all 12 standards.
  !> @param[inout] st Test state accumulator.
  subroutine run_capability_and_form_tests(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_dialect_caps_t) :: caps
    type(cdd_options_t) :: opts
    type(cdd_standard_t) :: std
    type(cdd_error_t) :: err
    integer(i32) :: sid, form

    call cdd_default_options(opts)
    call th_touch_vtable(opts)
    call th_touch_vtable(caps)
    call th_eq_i32(st, opts%standard%id, STD_FORTRAN_2023, "default f2023")
    call th_eq_i32(st, opts%source_form, FORM_AUTO, "default form auto")

    do sid = STD_FORTRAN_I, STD_FORTRAN_2023
      std%id = sid
      caps = get_dialect_capabilities(std, .false._lk)
      if (sid == STD_FORTRAN_I) then
        call th_check(st, .not. caps%has_subprograms .and. caps%implicit_only_typing, "F57 caps")
      else if (sid == STD_FORTRAN_III) then
        call th_check(st, caps%has_boolean_word_ops .and. caps%has_subprograms, "F58-III caps")
      else if (sid == STD_FORTRAN_2023) then
        call th_check(st, caps%has_enumeration_type .and. caps%max_line_width == 10000_i32, "F2023 caps")
      end if
    end do

    ! DEC structure opt-in on IV, 77, and 90
    std%id = STD_FORTRAN_IV
    caps = get_dialect_capabilities(std, .true._lk)
    call th_check(st, caps%has_dec_structure .and. caps%max_ident_len == 31_i32, "DEC F62")
    std%id = STD_FORTRAN_77
    caps = get_dialect_capabilities(std, .true._lk)
    call th_check(st, caps%has_dec_structure .and. caps%allows_underscore, "DEC F77")
    std%id = STD_FORTRAN_66
    caps = get_dialect_capabilities(std, .true._lk)
    call th_check(st, .not. caps%has_dec_structure .and. caps%max_ident_len == 6_i32, "No DEC F66")

    ! resolve_effective_form branches
    call cdd_default_options(opts)
    call resolve_effective_form(opts, form, err)
    call th_eq_i32(st, form, FORM_FREE, "auto f2023 -> free")

    opts%source_form = FORM_FIXED
    call resolve_effective_form(opts, form, err)
    call th_eq_i32(st, form, FORM_FIXED, "explicit fixed on f2023")

    opts%source_form = FORM_FREE
    call resolve_effective_form(opts, form, err)
    call th_eq_i32(st, form, FORM_FREE, "explicit free on f2023")

    opts%standard%id = STD_FORTRAN_77
    call resolve_effective_form(opts, form, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "free form rejected on f77")

    opts%source_form = 99_i32
    call resolve_effective_form(opts, form, err)
    call th_eq_i32(st, err%code, CDD_ERR_INVALID_OPTION, "invalid form enum rejected")
  end subroutine run_capability_and_form_tests

end program test_standard_registry
