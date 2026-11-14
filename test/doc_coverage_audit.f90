!> @file doc_coverage_audit.f90
!> @brief Fortran CST-based 100% Doxygen documentation coverage auditor & self-test suite.

!> @brief Run auditor unit self-tests and enforce 100.0% doc coverage on `src/`, `app/`, and `test/`.
!> @return Program exit status (0 when 100.0% documented and all self-tests pass).
program doc_coverage_audit
  use cdd_kinds, only: i32, lk, c_k, std_err_unit
  use cdd_errors, only: CDD_OK, cdd_error_t
  use cdd_string, only: cdd_buffer_t, buf_init, buf_append_str, str_eq_nocase
  use cdd_fs, only: cdd_fs_t, fs_init_os, fs_read_text_file
  use cdd_standard, only: STD_FORTRAN_2023, cdd_options_t, cdd_default_options
  use cdd_cst_trivia, only: TRIVIA_NEWLINE, TRIVIA_DOXYGEN_COMMENT
  use cdd_cst_tokens, only: FTOK_IDENT, FTOK_LPAREN, FTOK_RPAREN, ftok_norm_eq
  use cdd_cst_nodes, only: CST_MODULE, CST_SUBMODULE, CST_PROGRAM, CST_DERIVED_TYPE, &
    CST_ENUM_BIND_C, CST_ENUMERATION_TYPE, CST_FIELD_DECL, CST_SUBROUTINE, CST_FUNCTION, &
    CST_INTERFACE, CST_TYPE_BOUND_PROC, CST_INTENT_NONE, CST_INTENT_IN, CST_INTENT_OUT, &
    CST_INTENT_INOUT, cdd_cst_tree_t
  use cdd_cst_parser, only: cst_parse
  use test_harness, only: test_state_t, th_init, th_check, th_eq_i32, th_finish
  implicit none (type, external)

  !> @brief Aggregated documentation coverage counters and violation tally.
  type :: audit_stats_t
    integer(i32) :: files_checked = 0_i32      !< Number of `.f90` files audited.
    integer(i32) :: containers_checked = 0_i32 !< Modules, submodules, and programs audited.
    integer(i32) :: types_checked = 0_i32      !< Derived types and enums audited.
    integer(i32) :: fields_checked = 0_i32     !< Derived-type/enum components audited.
    integer(i32) :: procs_checked = 0_i32      !< Functions, subroutines, interfaces, TBPs audited.
    integer(i32) :: args_checked = 0_i32       !< Procedure dummy arguments audited (+ intent match).
    integer(i32) :: violations = 0_i32         !< Total documentation coverage violations found.
  end type audit_stats_t

  type(cdd_cst_tree_t), allocatable :: g_tree
  type(cdd_buffer_t), allocatable :: g_src_buf, g_list_buf
  type(test_state_t) :: st

  allocate (g_tree, g_src_buf, g_list_buf)
  call th_init(st)
  call run_auditor_self_tests(st)
  call audit_all_repository_sources(st)
  deallocate (g_tree, g_src_buf, g_list_buf)
  call th_finish(st, "doc_coverage_audit")

contains

  !> @brief Unit-test the auditor itself on synthetic violations (missing @@file, @@brief, @@return, @@param, intent).
  !> @param[inout] st Test harness state.
  subroutine run_auditor_self_tests(st)
    type(test_state_t), intent(inout) :: st
    type(audit_stats_t) :: a
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    ! 1. Valid minimal module with type, field, function, @return, and matching @param intents
    call reset_stats(a)
    call audit_source_text("ok_mod.f90", &
      "!> @file ok_mod.f90" // LF // &
      "!> @brief Valid file brief." // LF // LF // &
      "!> @brief Valid module brief." // LF // &
      "module ok_mod" // LF // &
      "  implicit none" // LF // &
      "  !> @brief Valid type." // LF // &
      "  type :: box_t" // LF // &
      "    integer :: x !< Documented field." // LF // &
      "  end type box_t" // LF // &
      "contains" // LF // &
      "  !> @brief Compute sum." // LF // &
      "  !> @param[in]  a Input value." // LF // &
      "  !> @param[out] b Output value." // LF // &
      "  !> @return Computed integer." // LF // &
      "  pure integer function calc(a, b)" // LF // &
      "    integer, intent(in) :: a" // LF // &
      "    integer, intent(out) :: b" // LF // &
      "    b = a; calc = a" // LF // &
      "  end function calc" // LF // &
      "end module ok_mod" // LF, .false._lk, a)
    call th_eq_i32(st, a%violations, 0_i32, "self-test: valid fully documented module passes")

    ! 2. Missing @file tag
    call reset_stats(a)
    call audit_source_text("no_file.f90", &
      "!> @brief Missing at-file." // LF // LF // &
      "!> @brief Mod." // LF // "module m" // LF // "end module m" // LF, .false._lk, a)
    call th_check(st, a%violations > 0_i32, "self-test: detects missing @file")

    ! 3. Missing module @brief
    call reset_stats(a)
    call audit_source_text("no_mbrief.f90", &
      "!> @file no_mbrief.f90" // LF // "!> @brief File." // LF // LF // &
      "module m" // LF // "end module m" // LF, .false._lk, a)
    call th_check(st, a%violations > 0_i32, "self-test: detects missing module @brief")

    ! 4. Undocumented type component field
    call reset_stats(a)
    call audit_source_text("no_fdoc.f90", &
      "!> @file no_fdoc.f90" // LF // "!> @brief File." // LF // LF // &
      "!> @brief Mod." // LF // "module m" // LF // &
      "  !> @brief Type." // LF // "  type :: t" // LF // &
      "    integer :: undoc_fld" // LF // &
      "  end type t" // LF // "end module m" // LF, .false._lk, a)
    call th_check(st, a%violations > 0_i32, "self-test: detects undocumented type field")

    ! 5. Missing @return on function
    call reset_stats(a)
    call audit_source_text("no_ret.f90", &
      "!> @file no_ret.f90" // LF // "!> @brief File." // LF // LF // &
      "!> @brief Mod." // LF // "module m" // LF // "contains" // LF // &
      "  !> @brief Fn without return tag." // LF // &
      "  integer function f()" // LF // "    f = 1" // LF // "  end function f" // LF // &
      "end module m" // LF, .false._lk, a)
    call th_check(st, a%violations > 0_i32, "self-test: detects missing function @return")

    ! 6. Missing @param on dummy argument
    call reset_stats(a)
    call audit_source_text("no_prm.f90", &
      "!> @file no_prm.f90" // LF // "!> @brief File." // LF // LF // &
      "!> @brief Mod." // LF // "module m" // LF // "contains" // LF // &
      "  !> @brief Sub." // LF // &
      "  subroutine s(x)" // LF // "    integer, intent(in) :: x" // LF // &
      "  end subroutine s" // LF // "end module m" // LF, .false._lk, a)
    call th_check(st, a%violations > 0_i32, "self-test: detects missing @param")

    ! 7. Mismatched @param[in] vs intent(out)
    call reset_stats(a)
    call audit_source_text("bad_int.f90", &
      "!> @file bad_int.f90" // LF // "!> @brief File." // LF // LF // &
      "!> @brief Mod." // LF // "module m" // LF // "contains" // LF // &
      "  !> @brief Sub." // LF // &
      "  !> @param[in] x Wrongly marked in!" // LF // &
      "  subroutine s(x)" // LF // "    integer, intent(out) :: x" // LF // &
      "    x = 0" // LF // "  end subroutine s" // LF // "end module m" // LF, .false._lk, a)
    call th_check(st, a%violations > 0_i32, "self-test: detects @param[in] vs intent(out) mismatch")
  end subroutine run_auditor_self_tests

  !> @brief Discover and audit every `.f90` file in `src/`, `app/`, and `test/`.
  !> @param[inout] st Test harness state.
  subroutine audit_all_repository_sources(st)
    type(test_state_t), intent(inout) :: st
    type(cdd_fs_t) :: fs
    type(cdd_error_t) :: err
    type(audit_stats_t) :: stats
    integer :: rc
    integer(i32) :: pos, le, n
    character(len=256, kind=c_k) :: rel_path, base_nm
    integer(i32) :: rp_len, bn_len
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)

    call reset_stats(stats)
    call fs_init_os(fs)
    call execute_command_line( &
      "mkdir -p build && find src app test -name '*.f90' | sort > build/doc_audit_files.txt", &
      exitstat=rc)
    call th_eq_i32(st, int(rc, i32), 0_i32, "discover repository .f90 files")

    call fs_read_text_file(fs, "build/doc_audit_files.txt", g_list_buf, err)
    call th_eq_i32(st, err%code, CDD_OK, "read doc_audit_files.txt")

    n = g_list_buf%len
    pos = 1_i32
    do while (pos <= n)
      le = pos
      do while (le <= n .and. g_list_buf%raw(le:le) /= LF)
        le = le + 1_i32
      end do
      rp_len = le - pos
      if (rp_len > 0_i32) then
        rel_path = " "
        rel_path(1:rp_len) = g_list_buf%raw(pos:le - 1_i32)
        call extract_basename(rel_path(1:rp_len), base_nm, bn_len)
        call fs_read_text_file(fs, rel_path(1:rp_len), g_src_buf, err)
        call th_eq_i32(st, err%code, CDD_OK, "read " // rel_path(1:rp_len))
        call audit_source_text(base_nm(1:bn_len), g_src_buf%raw(1:g_src_buf%len), .true._lk, stats)
      end if
      pos = le + 1_i32
    end do

    write (*, '(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A)') &
      "[doc_coverage_audit] 100% check: files=", stats%files_checked, &
      " containers=", stats%containers_checked, &
      " types=", stats%types_checked, &
      " fields=", stats%fields_checked, &
      " procedures=", stats%procs_checked, &
      " args=", stats%args_checked, ""
    call th_check(st, stats%files_checked >= 32_i32, "audited >= 32 .f90 files across src/app/test")
    call th_eq_i32(st, stats%violations, 0_i32, &
      "100.0% Doxygen coverage across all files, modules, types, fields, procedures, and arguments")
  end subroutine audit_all_repository_sources

  !> @brief Reset all counters in `audit_stats_t` to zero.
  !> @param[out] stats Audit counter structure.
  pure subroutine reset_stats(stats)
    type(audit_stats_t), intent(out) :: stats

    stats%files_checked = 0_i32
    stats%containers_checked = 0_i32
    stats%types_checked = 0_i32
    stats%fields_checked = 0_i32
    stats%procs_checked = 0_i32
    stats%args_checked = 0_i32
    stats%violations = 0_i32
  end subroutine reset_stats

  !> @brief Extract filename component after the final `/` separator.
  !> @param[in]  path     Relative or full path.
  !> @param[out] base_nm  Extracted basename buffer.
  !> @param[out] base_len Length of basename.
  pure subroutine extract_basename(path, base_nm, base_len)
    character(len=*, kind=c_k), intent(in) :: path
    character(len=256, kind=c_k), intent(out) :: base_nm
    integer(i32), intent(out) :: base_len
    integer(i32) :: k, last_sl, n

    base_nm = " "
    n = len(path, kind=i32)
    last_sl = 0_i32
    do k = 1_i32, n
      if (path(k:k) == "/") last_sl = k
    end do
    base_len = n - last_sl
    if (base_len > 0_i32) base_nm(1:base_len) = path(last_sl + 1_i32:n)
  end subroutine extract_basename

  !> @brief Parse `source` into `g_tree` and audit file, containers, types, fields, procs, and args.
  !> @param[in]    file_name Expected `@file` basename.
  !> @param[in]    source    Fortran source code.
  !> @param[in]    verbose   True to print violations to `std_err_unit`.
  !> @param[inout] stats     Updated audit counters and violation tally.
  subroutine audit_source_text(file_name, source, verbose, stats)
    character(len=*, kind=c_k), intent(in) :: file_name
    character(len=*, kind=c_k), intent(in) :: source
    logical(lk), intent(in) :: verbose
    type(audit_stats_t), intent(inout) :: stats
    type(cdd_options_t) :: opts
    type(cdd_error_t) :: err
    type(cdd_buffer_t) :: top_doc, attached_doc
    integer(i32) :: i, p_kind

    stats%files_checked = stats%files_checked + 1_i32
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_2023
    call cst_parse(source, opts, g_tree, err)
    if (err%code /= CDD_OK) then
      call record_violation(file_name, "<parse>", "CST parse failed", verbose, stats)
      return
    end if

    ! 1. File header @file <file_name> + @brief
    call collect_first_doxygen_block(1_i32, top_doc)
    if (index(top_doc%raw(1:top_doc%len), "@file " // file_name) == 0) then
      call record_violation(file_name, file_name, "missing '!> @file " // file_name // "'", &
        verbose, stats)
    end if
    if (index(top_doc%raw(1:top_doc%len), "@brief ") == 0) then
      call record_violation(file_name, file_name, "missing file '!> @brief'", verbose, stats)
    end if

    ! 2. Walk every CST node
    do i = 2_i32, g_tree%node_count
      select case (g_tree%nodes(i)%kind)
      case (CST_MODULE, CST_SUBMODULE, CST_PROGRAM)
        stats%containers_checked = stats%containers_checked + 1_i32
        call collect_attached_lead_doxygen(g_tree%nodes(i)%first_tok, attached_doc)
        if (index(attached_doc%raw(1:attached_doc%len), "@brief ") == 0) then
          call record_violation(file_name, &
            g_tree%nodes(i)%name%raw(1:g_tree%nodes(i)%name%len), &
            "missing module/program '!> @brief'", verbose, stats)
        end if

      case (CST_DERIVED_TYPE, CST_ENUM_BIND_C, CST_ENUMERATION_TYPE)
        stats%types_checked = stats%types_checked + 1_i32
        call collect_attached_lead_doxygen(g_tree%nodes(i)%first_tok, attached_doc)
        if (attached_doc%len == 0_i32) then
          call record_violation(file_name, &
            g_tree%nodes(i)%name%raw(1:g_tree%nodes(i)%name%len), &
            "missing type/enum Doxygen docstring", verbose, stats)
        end if

      case (CST_FIELD_DECL)
        p_kind = g_tree%nodes(g_tree%nodes(i)%parent)%kind
        if (p_kind == CST_DERIVED_TYPE .or. p_kind == CST_ENUM_BIND_C .or. &
            p_kind == CST_ENUMERATION_TYPE) then
          stats%fields_checked = stats%fields_checked + 1_i32
          if (.not. field_has_doxygen(i)) then
            call record_violation(file_name, &
              g_tree%nodes(i)%name%raw(1:g_tree%nodes(i)%name%len), &
              "undocumented type/enum component field", verbose, stats)
          end if
        end if

      case (CST_INTERFACE, CST_TYPE_BOUND_PROC)
        stats%procs_checked = stats%procs_checked + 1_i32
        if (.not. field_has_doxygen(i)) then
          call record_violation(file_name, &
            g_tree%nodes(i)%name%raw(1:max(1_i32, g_tree%nodes(i)%name%len)), &
            "undocumented interface or type-bound procedure", verbose, stats)
        end if

      case (CST_SUBROUTINE, CST_FUNCTION)
        stats%procs_checked = stats%procs_checked + 1_i32
        call audit_procedure_node(file_name, i, verbose, stats)
      end select
    end do
  end subroutine audit_source_text

  !> @brief Audit `@brief`, `@return` (on functions), and every dummy `@param[intent]` on a procedure.
  !> @param[in]    file_name Source filename for diagnostics.
  !> @param[in]    proc_node CST_SUBROUTINE or CST_FUNCTION node index.
  !> @param[in]    verbose   True to log violations to `std_err_unit`.
  !> @param[inout] stats     Updated audit counters and violation tally.
  subroutine audit_procedure_node(file_name, proc_node, verbose, stats)
    character(len=*, kind=c_k), intent(in) :: file_name
    integer(i32), intent(in) :: proc_node
    logical(lk), intent(in) :: verbose
    type(audit_stats_t), intent(inout) :: stats
    type(cdd_buffer_t) :: doc_b
    character(len=64, kind=c_k) :: p_name, arg_nm
    integer(i32) :: pn_len, ft, he, t, lp_tok, rp_tok, a_len, actual_int, doc_int

    pn_len = g_tree%nodes(proc_node)%name%len
    p_name = " "
    if (pn_len > 0_i32) p_name(1:pn_len) = g_tree%nodes(proc_node)%name%raw(1:pn_len)

    call collect_attached_lead_doxygen(g_tree%nodes(proc_node)%first_tok, doc_b)
    if (index(doc_b%raw(1:doc_b%len), "@brief ") == 0) then
      call record_violation(file_name, p_name(1:max(1_i32, pn_len)), &
        "missing procedure '!> @brief'", verbose, stats)
    end if

    if (g_tree%nodes(proc_node)%kind == CST_FUNCTION) then
      if (index(doc_b%raw(1:doc_b%len), "@return ") == 0 .and. &
          index(doc_b%raw(1:doc_b%len), "@returns ") == 0) then
        call record_violation(file_name, p_name(1:max(1_i32, pn_len)), &
          "missing function '!> @return'", verbose, stats)
      end if
    end if

    ! Extract dummy argument identifiers from header signature
    ft = g_tree%nodes(proc_node)%first_tok
    he = g_tree%nodes(proc_node)%header_end_tok
    lp_tok = 0_i32
    rp_tok = 0_i32
    do t = ft, he
      if (ftok_norm_eq(g_tree%stream, t, "SUBROUTINE") .or. &
          ftok_norm_eq(g_tree%stream, t, "FUNCTION")) then
        if (t + 2_i32 <= he .and. g_tree%stream%tokens(t + 2_i32)%kind == FTOK_LPAREN) then
          lp_tok = t + 2_i32
        end if
        exit
      end if
    end do
    if (lp_tok == 0_i32) return

    do t = lp_tok + 1_i32, he
      if (g_tree%stream%tokens(t)%kind == FTOK_RPAREN) then
        rp_tok = t
        exit
      end if
    end do
    if (rp_tok <= lp_tok + 1_i32) return

    do t = lp_tok + 1_i32, rp_tok - 1_i32
      if (g_tree%stream%tokens(t)%kind == FTOK_IDENT) then
        stats%args_checked = stats%args_checked + 1_i32
        a_len = min(64_i32, g_tree%stream%tokens(t)%raw_len)
        arg_nm = " "
        arg_nm(1:a_len) = g_tree%stream%source_buf%raw( &
          g_tree%stream%tokens(t)%raw_off:g_tree%stream%tokens(t)%raw_off + a_len - 1_i32)
        actual_int = find_dummy_intent(proc_node, arg_nm(1:a_len))
        doc_int = find_doc_param_intent(doc_b%raw(1:doc_b%len), arg_nm(1:a_len))
        if (doc_int < 0_i32) then
          call record_violation(file_name, p_name(1:max(1_i32, pn_len)) // "(" // &
            arg_nm(1:a_len) // ")", "missing '@param[...] " // arg_nm(1:a_len) // "'", &
            verbose, stats)
        else if (actual_int /= CST_INTENT_NONE .and. doc_int /= CST_INTENT_NONE .and. &
                 actual_int /= doc_int) then
          call record_violation(file_name, p_name(1:max(1_i32, pn_len)) // "(" // &
            arg_nm(1:a_len) // ")", "@param[...] intent does not match Fortran intent(...)", &
            verbose, stats)
        end if
      end if
    end do
  end subroutine audit_procedure_node

  !> @brief Look up `CST_INTENT_*` parsed on a dummy argument inside `proc_node`.
  !> @param[in] proc_node Procedure CST node index.
  !> @param[in] arg_nm    Dummy argument identifier.
  !> @return `CST_INTENT_IN`, `CST_INTENT_OUT`, `CST_INTENT_INOUT`, or `CST_INTENT_NONE`.
  pure function find_dummy_intent(proc_node, arg_nm) result(ikind)
    integer(i32), intent(in) :: proc_node
    character(len=*, kind=c_k), intent(in) :: arg_nm
    integer(i32) :: ikind
    integer(i32) :: k

    ikind = CST_INTENT_NONE
    do k = proc_node + 1_i32, g_tree%node_count
      if (g_tree%nodes(k)%parent == proc_node .and. g_tree%nodes(k)%kind == CST_FIELD_DECL) then
        if (str_eq_nocase(g_tree%nodes(k)%name%raw(1:g_tree%nodes(k)%name%len), arg_nm)) then
          ikind = g_tree%nodes(k)%intent_kind
          return
        end if
      end if
    end do
  end function find_dummy_intent

  !> @brief Parse `@param[in|out|inout] <arg_nm>` from `doc_txt` (-1 if missing).
  !> @param[in] doc_txt Concatenated Doxygen block for the procedure.
  !> @param[in] arg_nm  Dummy argument name to match.
  !> @return `CST_INTENT_*` if documented, or `-1` if undocumented.
  pure function find_doc_param_intent(doc_txt, arg_nm) result(dkind)
    character(len=*, kind=c_k), intent(in) :: doc_txt
    character(len=*, kind=c_k), intent(in) :: arg_nm
    integer(i32) :: dkind
    integer(i32) :: n, p, tag_pos, id_s, id_e, cand_kind

    dkind = -1_i32
    n = len(doc_txt, kind=i32)
    p = 1_i32
    do while (p <= n)
      tag_pos = index(doc_txt(p:n), "@param")
      if (tag_pos == 0) exit
      p = p + tag_pos + 5_i32
      cand_kind = CST_INTENT_NONE
      if (p <= n .and. doc_txt(p:p) == "[") then
        if (p + 6_i32 <= n .and. str_eq_nocase(doc_txt(p:p + 6_i32), "[inout]")) then
          cand_kind = CST_INTENT_INOUT
          p = p + 7_i32
        else if (p + 7_i32 <= n .and. str_eq_nocase(doc_txt(p:p + 7_i32), "[in,out]")) then
          cand_kind = CST_INTENT_INOUT
          p = p + 8_i32
        else if (p + 3_i32 <= n .and. str_eq_nocase(doc_txt(p:p + 3_i32), "[in]")) then
          cand_kind = CST_INTENT_IN
          p = p + 4_i32
        else if (p + 4_i32 <= n .and. str_eq_nocase(doc_txt(p:p + 4_i32), "[out]")) then
          cand_kind = CST_INTENT_OUT
          p = p + 5_i32
        end if
      end if
      do while (p <= n .and. doc_txt(p:p) == " ")
        p = p + 1_i32
      end do
      id_s = p
      do while (p <= n .and. doc_txt(p:p) /= " " .and. doc_txt(p:p) /= achar(10, kind=c_k))
        p = p + 1_i32
      end do
      id_e = p - 1_i32
      if (id_e >= id_s) then
        if (str_eq_nocase(doc_txt(id_s:id_e), arg_nm)) then
          dkind = cand_kind
          return
        end if
      end if
    end do
  end function find_doc_param_intent

  !> @brief Collect the top-of-file `!>` Doxygen block from `tok_idx`'s leading trivia.
  !> @param[in]  tok_idx  First token index (1).
  !> @param[out] out_buf  Collected file-header Doxygen text.
  pure subroutine collect_first_doxygen_block(tok_idx, out_buf)
    integer(i32), intent(in) :: tok_idx
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t) :: err
    integer(i32) :: tf, tc, k, off, slen
    logical(lk) :: seen_doc, prev_nl

    call buf_init(out_buf)
    tf = g_tree%stream%tokens(tok_idx)%lead_first
    tc = g_tree%stream%tokens(tok_idx)%lead_count
    seen_doc = .false._lk
    prev_nl = .false._lk
    do k = tf, tf + tc - 1_i32
      if (g_tree%stream%trivia(k)%kind == TRIVIA_DOXYGEN_COMMENT) then
        seen_doc = .true._lk
        prev_nl = .false._lk
        off = g_tree%stream%trivia(k)%src_off
        slen = g_tree%stream%trivia(k)%src_len
        call buf_append_str(out_buf, &
          g_tree%stream%source_buf%raw(off:off + slen - 1_i32) // achar(10, kind=c_k), err)
      else if (g_tree%stream%trivia(k)%kind == TRIVIA_NEWLINE) then
        if (seen_doc .and. prev_nl) exit
        prev_nl = .true._lk
      end if
    end do
  end subroutine collect_first_doxygen_block

  !> @brief Collect the contiguous `!>` Doxygen block immediately preceding `tok_idx`.
  !> @param[in]  tok_idx  Construct's `first_tok`.
  !> @param[out] out_buf  Collected attached Doxygen block (excluding separated `@file` header).
  pure subroutine collect_attached_lead_doxygen(tok_idx, out_buf)
    integer(i32), intent(in) :: tok_idx
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t) :: err
    integer(i32) :: tf, tc, nl_idx(0:4096), nl_cnt, k, ln_s, ln_e, start_tr, p, off, slen
    logical(lk) :: has_dox

    call buf_init(out_buf)
    tf = g_tree%stream%tokens(tok_idx)%lead_first
    tc = g_tree%stream%tokens(tok_idx)%lead_count
    if (tc <= 0_i32) return

    nl_idx(0) = tf - 1_i32
    nl_cnt = 0_i32
    do k = tf, tf + tc - 1_i32
      if (g_tree%stream%trivia(k)%kind == TRIVIA_NEWLINE .and. nl_cnt < 4096_i32) then
        nl_cnt = nl_cnt + 1_i32
        nl_idx(nl_cnt) = k
      end if
    end do
    if (nl_cnt == 0_i32) return

    start_tr = nl_idx(nl_cnt) + 1_i32
    do k = nl_cnt, 1_i32, -1_i32
      ln_s = nl_idx(k - 1_i32) + 1_i32
      ln_e = nl_idx(k)
      has_dox = .false._lk
      do p = ln_s, ln_e - 1_i32
        if (g_tree%stream%trivia(p)%kind == TRIVIA_DOXYGEN_COMMENT) then
          off = g_tree%stream%trivia(p)%src_off
          slen = g_tree%stream%trivia(p)%src_len
          if (index(g_tree%stream%source_buf%raw(off:off + slen - 1_i32), "@file ") == 0) then
            has_dox = .true._lk
          end if
        end if
      end do
      if (has_dox) then
        start_tr = ln_s
      else
        exit
      end if
    end do

    do p = start_tr, tf + tc - 1_i32
      if (g_tree%stream%trivia(p)%kind == TRIVIA_DOXYGEN_COMMENT) then
        off = g_tree%stream%trivia(p)%src_off
        slen = g_tree%stream%trivia(p)%src_len
        call buf_append_str(out_buf, &
          g_tree%stream%source_buf%raw(off:off + slen - 1_i32) // achar(10, kind=c_k), err)
      end if
    end do
  end subroutine collect_attached_lead_doxygen

  !> @brief Check whether CST node `node_idx` has either leading `!>` or trailing `!<` Doxygen doc.
  !> @param[in] node_idx CST node index (`CST_FIELD_DECL`, `CST_INTERFACE`, or `CST_TYPE_BOUND_PROC`).
  !> @return True if documented.
  pure function field_has_doxygen(node_idx) result(ok)
    integer(i32), intent(in) :: node_idx
    logical(lk) :: ok
    type(cdd_buffer_t) :: lead_b
    integer(i32) :: s_beg, s_end, tr_f, tr_c, k

    ok = .false._lk
    s_beg = g_tree%nodes(node_idx)%first_tok
    do while (s_beg > 1_i32 .and. .not. g_tree%stream%tokens(s_beg - 1_i32)%stmt_end)
      s_beg = s_beg - 1_i32
    end do
    call collect_attached_lead_doxygen(s_beg, lead_b)
    if (lead_b%len > 0_i32) then
      ok = .true._lk
      return
    end if

    s_end = g_tree%nodes(node_idx)%last_tok
    do while (s_end < g_tree%stream%token_count .and. .not. g_tree%stream%tokens(s_end)%stmt_end)
      s_end = s_end + 1_i32
    end do
    tr_f = g_tree%stream%tokens(s_end)%trail_first
    tr_c = g_tree%stream%tokens(s_end)%trail_count
    do k = tr_f, tr_f + tr_c - 1_i32
      if (g_tree%stream%trivia(k)%kind == TRIVIA_DOXYGEN_COMMENT) then
        ok = .true._lk
        return
      end if
    end do
  end function field_has_doxygen

  !> @brief Increment `stats%violations` and optionally print a structured diagnostic to `std_err_unit`.
  !> @param[in]    file_name Source file name.
  !> @param[in]    symbol    Symbol or construct name.
  !> @param[in]    reason    Human-readable violation description.
  !> @param[in]    verbose   True to write diagnostic to `std_err_unit`.
  !> @param[inout] stats     Audit stats accumulator.
  subroutine record_violation(file_name, symbol, reason, verbose, stats)
    character(len=*, kind=c_k), intent(in) :: file_name
    character(len=*, kind=c_k), intent(in) :: symbol
    character(len=*, kind=c_k), intent(in) :: reason
    logical(lk), intent(in) :: verbose
    type(audit_stats_t), intent(inout) :: stats

    stats%violations = stats%violations + 1_i32
    if (verbose) then
      write (std_err_unit, '(A)') "DOC COVERAGE VIOLATION [" // file_name // " :: " // &
        symbol // "]: " // reason
    end if
  end subroutine record_violation

end program doc_coverage_audit
