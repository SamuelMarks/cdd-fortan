!> @file cdd_ir_namemap.f90
!> @brief Reversible Fortran identifier sanitizer, 6-char I-N implicit namer, and @@cdd-field codec.

!> @brief Deterministic naming and reversible comment-tag serialization for FORTRAN I..2023.
module cdd_ir_namemap
  use cdd_kinds, only: i32, i64, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set_simple, &
    CDD_ERR_IDENTIFIER_COLLISION, CDD_ERR_INVALID_OPTION
  use cdd_string, only: cdd_buffer_t, buf_init, buf_append_char, buf_append_str, buf_append_i64, &
    char_to_lower, char_to_upper, is_ascii_alpha, is_ascii_alnum, str_eq, str_eq_nocase, &
    trim_bounds, starts_with, parse_i64_strict
  use cdd_ir_types, only: CDD_IDENT_MAX, IR_INT8, IR_INT16, IR_INT32, IR_INT64, &
    IR_REAL32, IR_REAL64, IR_REAL128, IR_COMPLEX64, IR_COMPLEX128, IR_BOOL, &
    IR_STRING_FIXED, IR_ENUM, cdd_ident_t, cdd_ir_field_t, ident_clear, ident_set, &
    ir_type_to_tag, ir_tag_to_type
  implicit none (type, external)
  private

  public :: sanitize_modern_ident, generate_legacy_ident
  public :: assign_udt_field_names, is_reserved_fortran_word, is_implicit_int_letter
  public :: format_cdd_field_comment, parse_cdd_field_comment

contains

  !> @brief Sanitize an arbitrary JSON name into a valid modern Fortran identifier (31 or 63 chars).
  !> @param[in]  raw_name  Input JSON property or schema name.
  !> @param[in]  max_len   Maximum allowed identifier length (1..CDD_IDENT_MAX, e.g. 31 or 63).
  !> @param[in]  uppercase True to emit UPPERCASE, false for snake_case lowercase.
  !> @param[out] out_ident Resulting Fortran identifier.
  !> @param[out] err       Error status if max_len is invalid.
  pure subroutine sanitize_modern_ident(raw_name, max_len, uppercase, out_ident, err)
    character(len=*, kind=c_k), intent(in) :: raw_name
    integer(i32), intent(in) :: max_len
    logical(lk), intent(in) :: uppercase
    type(cdd_ident_t), intent(out) :: out_ident
    type(cdd_error_t), intent(out) :: err
    character(len=CDD_IDENT_MAX, kind=c_k) :: work
    integer(i32) :: n, wlen, i
    character(len=1, kind=c_k) :: ch, mapped

    call ident_clear(out_ident)
    call cdd_error_clear(err)
    if (max_len < 4_i32 .or. max_len > CDD_IDENT_MAX) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Invalid modern max_len")
      return
    end if

    n = len(raw_name, kind=i32)
    wlen = 0_i32
    do i = 1_i32, n
      if (wlen >= max_len) exit
      ch = raw_name(i:i)
      if (is_ascii_alnum(ch)) then
        if (uppercase) then
          mapped = char_to_upper(ch)
        else
          mapped = char_to_lower(ch)
        end if
        wlen = wlen + 1_i32
        work(wlen:wlen) = mapped
      else if (wlen > 0_i32) then
        if (work(wlen:wlen) /= "_") then
          wlen = wlen + 1_i32
          work(wlen:wlen) = "_"
        end if
      end if
    end do

    do while (wlen > 0_i32)
      if (work(wlen:wlen) /= "_") exit
      wlen = wlen - 1_i32
    end do

    if (wlen == 0_i32) then
      if (uppercase) then
        work(1:5) = "FIELD"
      else
        work(1:5) = "field"
      end if
      wlen = 5_i32
    else if (.not. is_ascii_alpha(work(1:1))) then
      if (wlen + 2_i32 > max_len) then
        wlen = max_len - 2_i32
      end if
      do i = wlen, 1_i32, -1_i32
        work(i + 2_i32:i + 2_i32) = work(i:i)
      end do
      if (uppercase) then
        work(1:2) = "F_"
      else
        work(1:2) = "f_"
      end if
      wlen = wlen + 2_i32
    end if

    if (is_reserved_fortran_word(work(1:wlen))) then
      if (wlen + 2_i32 > max_len) then
        wlen = max_len - 2_i32
      end if
      if (uppercase) then
        work(wlen + 1_i32:wlen + 2_i32) = "_V"
      else
        work(wlen + 1_i32:wlen + 2_i32) = "_v"
      end if
      wlen = wlen + 2_i32
    end if

    call ident_set(out_ident, work(1:wlen), err)
  end subroutine sanitize_modern_ident

  !> @brief Generate a collision-free 6-char uppercase identifier obeying I-N implicit rules.
  !> @param[in]    raw_name       Original JSON property name.
  !> @param[in]    type_kind      Canonical IR_* field type.
  !> @param[in]    implicit_rule  True when FORTRAN I-III I-N / A-H,O-Z rule is mandatory.
  !> @param[in]    max_attempts   Max base-36 collision attempts (>= 1, e.g. 1296).
  !> @param[in]    used_cap       Capacity of used_names array (>= 1).
  !> @param[inout] used_names     Table of already-assigned identifiers in current scope.
  !> @param[inout] used_count     Active count in used_names(1:used_count).
  !> @param[out]   out_ident      Generated unique <=6-character uppercase identifier.
  !> @param[out]   err            Error status (CDD_ERR_IDENTIFIER_COLLISION on exhaustion).
  pure subroutine generate_legacy_ident(raw_name, type_kind, implicit_rule, max_attempts, &
      used_cap, used_names, used_count, out_ident, err)
    character(len=*, kind=c_k), intent(in) :: raw_name
    integer(i32), intent(in) :: type_kind
    logical(lk), intent(in) :: implicit_rule
    integer(i32), intent(in) :: max_attempts
    integer(i32), intent(in) :: used_cap
    type(cdd_ident_t), intent(inout) :: used_names(used_cap)
    integer(i32), intent(inout) :: used_count
    type(cdd_ident_t), intent(out) :: out_ident
    type(cdd_error_t), intent(out) :: err
    character(len=6, kind=c_k) :: base, cand
    integer(i32) :: n, blen, clen, i, attempt, hi_d, lo_d
    logical(lk) :: needs_int_prefix
    character(len=36, kind=c_k), parameter :: B36 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    call ident_clear(out_ident)
    call cdd_error_clear(err)

    n = len(raw_name, kind=i32)
    blen = 0_i32
    do i = 1_i32, n
      if (blen >= 6_i32) exit
      if (is_ascii_alnum(raw_name(i:i))) then
        if (blen == 0_i32 .and. .not. is_ascii_alpha(raw_name(i:i))) cycle
        blen = blen + 1_i32
        base(blen:blen) = char_to_upper(raw_name(i:i))
      end if
    end do

    needs_int_prefix = (type_kind == IR_INT8 .or. type_kind == IR_INT16 .or. &
                        type_kind == IR_INT32 .or. type_kind == IR_INT64 .or. &
                        type_kind == IR_BOOL .or. type_kind == IR_STRING_FIXED .or. &
                        type_kind == IR_ENUM)

    if (blen == 0_i32) then
      if (needs_int_prefix) then
        base(1:4) = "IFLD"
      else
        base(1:4) = "XFLD"
      end if
      blen = 4_i32
    end if

    if (implicit_rule) then
      if (needs_int_prefix .and. .not. is_implicit_int_letter(base(1:1))) then
        if (blen > 5_i32) blen = 5_i32
        do i = blen, 1_i32, -1_i32
          base(i + 1_i32:i + 1_i32) = base(i:i)
        end do
        base(1:1) = "I"
        blen = blen + 1_i32
      else if ((.not. needs_int_prefix) .and. &
               (type_kind == IR_REAL32 .or. type_kind == IR_REAL64 .or. &
                type_kind == IR_REAL128 .or. type_kind == IR_COMPLEX64 .or. &
                type_kind == IR_COMPLEX128) .and. &
               is_implicit_int_letter(base(1:1))) then
        if (blen > 5_i32) blen = 5_i32
        do i = blen, 1_i32, -1_i32
          base(i + 1_i32:i + 1_i32) = base(i:i)
        end do
        base(1:1) = "X"
        blen = blen + 1_i32
      end if
    end if

    do attempt = 0_i32, max_attempts - 1_i32
      if (attempt == 0_i32) then
        clen = blen
        do i = 1_i32, clen
          cand(i:i) = base(i:i)
        end do
      else if (attempt <= 36_i32) then
        clen = blen
        if (clen > 5_i32) clen = 5_i32
        do i = 1_i32, clen
          cand(i:i) = base(i:i)
        end do
        clen = clen + 1_i32
        cand(clen:clen) = B36(attempt:attempt)
      else
        clen = blen
        if (clen > 4_i32) clen = 4_i32
        do i = 1_i32, clen
          cand(i:i) = base(i:i)
        end do
        hi_d = mod((attempt - 37_i32) / 36_i32, 36_i32) + 1_i32
        lo_d = mod(attempt - 37_i32, 36_i32) + 1_i32
        cand(clen + 1_i32:clen + 1_i32) = B36(hi_d:hi_d)
        cand(clen + 2_i32:clen + 2_i32) = B36(lo_d:lo_d)
        clen = clen + 2_i32
      end if

      if (.not. is_reserved_fortran_word(cand(1:clen)) .and. &
          .not. is_name_used(cand(1:clen), used_cap, used_names, used_count)) then
        if (used_count >= used_cap) then
          call cdd_error_set_simple(err, CDD_ERR_IDENTIFIER_COLLISION, "Used-name table full")
          return
        end if
        call ident_set(out_ident, cand(1:clen), err)
        used_count = used_count + 1_i32
        used_names(used_count) = out_ident
        return
      end if
    end do

    call cdd_error_set_simple(err, CDD_ERR_IDENTIFIER_COLLISION, &
      "Exhausted legacy 6-character identifier collision candidates")
  end subroutine generate_legacy_ident

  !> @brief Populate unique dialect-compliant fortran_name for every field of a UDT.
  !> @param[in]    max_ident_len  Max identifier length for target dialect (6, 31, or 63).
  !> @param[in]    implicit_rule  True when FORTRAN I-III implicit I-N rule applies.
  !> @param[in]    uppercase      True when uppercase identifiers are requested.
  !> @param[in]    field_cap      Array capacity of fields.
  !> @param[in]    field_count    Active field count in fields(1:field_count).
  !> @param[inout] fields         Array of IR fields whose fortran_name will be populated.
  !> @param[out]   err            Error status record.
  pure subroutine assign_udt_field_names(max_ident_len, implicit_rule, uppercase, &
      field_cap, field_count, fields, err)
    integer(i32), intent(in) :: max_ident_len
    logical(lk), intent(in) :: implicit_rule
    logical(lk), intent(in) :: uppercase
    integer(i32), intent(in) :: field_cap
    integer(i32), intent(in) :: field_count
    type(cdd_ir_field_t), intent(inout) :: fields(field_cap)
    type(cdd_error_t), intent(out) :: err
    type(cdd_ident_t) :: used(64), base_id
    integer(i32) :: used_count, i, jlen, suffix, slen
    character(len=8, kind=c_k) :: sbuf
    character(len=CDD_IDENT_MAX, kind=c_k) :: cand
    integer(i32) :: keep_len, k

    call cdd_error_clear(err)
    used_count = 0_i32

    do i = 1_i32, field_count
      jlen = fields(i)%json_name%len
      if (max_ident_len <= 6_i32) then
        call generate_legacy_ident(fields(i)%json_name%raw(1:jlen), fields(i)%type_kind, &
          implicit_rule, 200_i32, 64_i32, used, used_count, fields(i)%fortran_name, err)
      else
        call sanitize_modern_ident(fields(i)%json_name%raw(1:jlen), max_ident_len, &
          uppercase, base_id, err)
        if (err%code /= 0_i32) return
        if (.not. is_name_used(base_id%raw(1:base_id%len), 64_i32, used, used_count)) then
          fields(i)%fortran_name = base_id
          used_count = used_count + 1_i32
          used(used_count) = base_id
        else
          suffix = 2_i32
          do
            sbuf(1:1) = "_"
            slen = 2_i32
            sbuf(2:2) = achar(48_i32 + mod(suffix, 10_i32), kind=c_k)
            keep_len = base_id%len
            if (keep_len + slen > max_ident_len) then
              keep_len = max_ident_len - slen
            end if
            do k = 1_i32, keep_len
              cand(k:k) = base_id%raw(k:k)
            end do
            do k = 1_i32, slen
              cand(keep_len + k:keep_len + k) = sbuf(k:k)
            end do
            if (.not. is_name_used(cand(1:keep_len + slen), 64_i32, used, used_count)) then
              call ident_set(fields(i)%fortran_name, cand(1:keep_len + slen), err)
              used_count = used_count + 1_i32
              used(used_count) = fields(i)%fortran_name
              exit
            end if
            suffix = suffix + 1_i32
          end do
        end if
      end if
    end do
  end subroutine assign_udt_field_names

  !> @brief Test whether letter is in the classical Fortran implicit INTEGER range 'I'..'N'.
  !> @param[in] ch Single character.
  !> @return True if uppercase(ch) is I, J, K, L, M, or N.
  pure function is_implicit_int_letter(ch) result(ok)
    character(len=1, kind=c_k), intent(in) :: ch
    logical(lk) :: ok
    integer(i32) :: code

    code = iachar(char_to_upper(ch), kind=i32)
    ok = (code >= 73_i32 .and. code <= 78_i32)
  end function is_implicit_int_letter

  !> @brief Test whether an identifier collides with a reserved Fortran keyword.
  !> @param[in] word Candidate identifier.
  !> @return True if word matches a Fortran statement/declaration keyword.
  pure function is_reserved_fortran_word(word) result(reserved)
    character(len=*, kind=c_k), intent(in) :: word
    logical(lk) :: reserved

    reserved = &
      str_eq_nocase(word, "type") .or. str_eq_nocase(word, "class") .or. &
      str_eq_nocase(word, "integer") .or. str_eq_nocase(word, "real") .or. &
      str_eq_nocase(word, "logical") .or. str_eq_nocase(word, "character") .or. &
      str_eq_nocase(word, "complex") .or. str_eq_nocase(word, "double") .or. &
      str_eq_nocase(word, "common") .or. str_eq_nocase(word, "dimension") .or. &
      str_eq_nocase(word, "equivalence") .or. str_eq_nocase(word, "module") .or. &
      str_eq_nocase(word, "submodule") .or. str_eq_nocase(word, "program") .or. &
      str_eq_nocase(word, "subroutine") .or. str_eq_nocase(word, "function") .or. &
      str_eq_nocase(word, "contains") .or. str_eq_nocase(word, "end") .or. &
      str_eq_nocase(word, "if") .or. str_eq_nocase(word, "else") .or. &
      str_eq_nocase(word, "do") .or. str_eq_nocase(word, "return") .or. &
      str_eq_nocase(word, "stop") .or. str_eq_nocase(word, "assign") .or. &
      str_eq_nocase(word, "format") .or. str_eq_nocase(word, "data") .or. &
      str_eq_nocase(word, "save") .or. str_eq_nocase(word, "pointer") .or. &
      str_eq_nocase(word, "allocatable") .or. str_eq_nocase(word, "enum") .or. &
      str_eq_nocase(word, "enumeration") .or. str_eq_nocase(word, "structure") .or. &
      str_eq_nocase(word, "record") .or. str_eq_nocase(word, "call")
  end function is_reserved_fortran_word

  !> @brief Format a reversible '@@cdd-field:' comment line preserving original JSON metadata.
  !> @param[in]  comment_lead Comment prefix (e.g. "C" for fixed-form or "!" for free-form).
  !> @param[in]  field        IR field descriptor.
  !> @param[out] out_buf      Formatted comment line (without trailing newline).
  !> @param[out] err          Error status record.
  pure subroutine format_cdd_field_comment(comment_lead, field, out_buf, err)
    character(len=*, kind=c_k), intent(in) :: comment_lead
    type(cdd_ir_field_t), intent(in) :: field
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(out) :: err
    character(len=20, kind=c_k) :: tbuf
    integer(i32) :: tlen

    call buf_init(out_buf)
    call cdd_error_clear(err)
    call ir_type_to_tag(field%type_kind, tbuf, tlen)
    call buf_append_str(out_buf, comment_lead, err)
    call buf_append_str(out_buf, " @cdd-field: ", err)
    call buf_append_str(out_buf, field%fortran_name%raw(1:field%fortran_name%len), err)
    call buf_append_str(out_buf, ' json="', err)
    call buf_append_str(out_buf, field%json_name%raw(1:field%json_name%len), err)
    call buf_append_str(out_buf, '" type=', err)
    call buf_append_str(out_buf, tbuf(1:tlen), err)
    call buf_append_str(out_buf, " len=", err)
    call buf_append_i64(out_buf, int(field%char_len, kind=i64), err)
  end subroutine format_cdd_field_comment

  !> @brief Parse a '@@cdd-field:' comment line if present, recovering original JSON metadata.
  !> @param[in]  line_text Raw comment line text.
  !> @param[out] f_name    Extracted Fortran identifier.
  !> @param[out] j_name    Extracted original JSON property key.
  !> @param[out] type_kind Extracted canonical IR_* type kind.
  !> @param[out] char_len  Extracted string length metadata.
  !> @param[out] matched   True if line_text was a valid @@cdd-field annotation.
  pure subroutine parse_cdd_field_comment(line_text, f_name, j_name, type_kind, char_len, matched)
    character(len=*, kind=c_k), intent(in) :: line_text
    type(cdd_ident_t), intent(out) :: f_name
    type(cdd_ident_t), intent(out) :: j_name
    integer(i32), intent(out) :: type_kind
    integer(i32), intent(out) :: char_len
    logical(lk), intent(out) :: matched
    integer(i32) :: n, p, marker_pos, f_end, q1, q2, t_pos, t_end, l_pos, lo, hi
    integer(i64) :: parsed_len
    type(cdd_error_t) :: tmp_err

    call ident_clear(f_name)
    call ident_clear(j_name)
    type_kind = IR_INT32
    char_len = 0_i32
    matched = .false._lk

    n = len(line_text, kind=i32)
    marker_pos = find_substr(line_text, "@cdd-field:")
    if (marker_pos == 0_i32) return

    p = marker_pos + 11_i32
    call skip_spaces(line_text, n, p)
    if (p > n) return
    f_end = p
    do while (f_end <= n)
      if (line_text(f_end:f_end) == " ") exit
      f_end = f_end + 1_i32
    end do
    call ident_set(f_name, line_text(p:f_end - 1_i32), tmp_err)
    if (tmp_err%code /= 0_i32) return

    q1 = find_substr(line_text(f_end:n), 'json="')
    if (q1 == 0_i32) return
    q1 = f_end + q1 + 5_i32
    q2 = q1
    do while (q2 <= n)
      if (line_text(q2:q2) == '"') exit
      q2 = q2 + 1_i32
    end do
    if (q2 > n) return
    call ident_set(j_name, line_text(q1:q2 - 1_i32), tmp_err)
    if (tmp_err%code /= 0_i32) return

    t_pos = find_substr(line_text(q2:n), "type=")
    if (t_pos == 0_i32) return
    t_pos = q2 + t_pos + 4_i32
    t_end = t_pos
    do while (t_end <= n)
      if (line_text(t_end:t_end) == " ") exit
      t_end = t_end + 1_i32
    end do
    call ir_tag_to_type(line_text(t_pos:t_end - 1_i32), type_kind, tmp_err)
    if (tmp_err%code /= 0_i32) return

    l_pos = find_substr(line_text(t_end:n), "len=")
    if (l_pos > 0_i32) then
      l_pos = t_end + l_pos + 3_i32
      call trim_bounds(line_text(l_pos:n), lo, hi)
      if (lo <= hi) then
        call parse_i64_strict(line_text(l_pos + lo - 1_i32:l_pos + hi - 1_i32), parsed_len, tmp_err)
        if (tmp_err%code == 0_i32) then
          char_len = int(parsed_len, kind=i32)
        end if
      end if
    end if

    matched = .true._lk
  end subroutine parse_cdd_field_comment

  !> @brief Check if candidate matches any entry in used_names(1:used_count) case-insensitively.
  !> @param[in] cand       Candidate identifier.
  !> @param[in] used_cap   Capacity of used_names.
  !> @param[in] used_names Populated identifier table.
  !> @param[in] used_count Active count in used_names.
  !> @return True if already present.
  pure function is_name_used(cand, used_cap, used_names, used_count) result(found)
    character(len=*, kind=c_k), intent(in) :: cand
    integer(i32), intent(in) :: used_cap
    type(cdd_ident_t), intent(in) :: used_names(used_cap)
    integer(i32), intent(in) :: used_count
    logical(lk) :: found
    integer(i32) :: k, ulen

    found = .false._lk
    do k = 1_i32, used_count
      ulen = used_names(k)%len
      if (str_eq_nocase(used_names(k)%raw(1:ulen), cand)) then
        found = .true._lk
        return
      end if
    end do
  end function is_name_used

  !> @brief Advance pos past ASCII spaces in s(1:n).
  !> @param[in]    s   Input string.
  !> @param[in]    n   Length of s.
  !> @param[inout] pos 1-based cursor.
  pure subroutine skip_spaces(s, n, pos)
    character(len=*, kind=c_k), intent(in) :: s
    integer(i32), intent(in) :: n
    integer(i32), intent(inout) :: pos

    do while (pos <= n)
      if (s(pos:pos) /= " ") exit
      pos = pos + 1_i32
    end do
  end subroutine skip_spaces

  !> @brief Find 1-based first occurrence of needle in haystack (0 if absent).
  !> @param[in] haystack Target string.
  !> @param[in] needle   Substring to find.
  !> @return 1-based start offset or 0.
  pure function find_substr(haystack, needle) result(idx)
    character(len=*, kind=c_k), intent(in) :: haystack
    character(len=*, kind=c_k), intent(in) :: needle
    integer(i32) :: idx
    integer(i32) :: nh, nn, k

    idx = 0_i32
    nh = len(haystack, kind=i32)
    nn = len(needle, kind=i32)
    if (nn == 0_i32 .or. nn > nh) return
    do k = 1_i32, nh - nn + 1_i32
      if (starts_with(haystack(k:nh), needle)) then
        idx = k
        return
      end if
    end do
  end function find_substr

end module cdd_ir_namemap
