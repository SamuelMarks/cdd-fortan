!> @file test_codec_runtime_compile.f90
!> @brief Integration tests compiling & executing generated codecs with `gfortran` across 12 standards.

!> @brief Compile and run round-trip `JSON -> UDT -> JSON` binaries for FORTRAN I..Fortran 2023.
!> @return Program exit status (0 on pass).
program test_codec_runtime_compile
  use cdd_kinds, only: i32, lk, c_k
  use cdd_errors, only: CDD_OK, cdd_error_t
  use cdd_string, only: cdd_buffer_t
  use cdd_fs, only: cdd_fs_t, fs_init_os, fs_write_text_file, fs_read_text_file
  use cdd_standard, only: STD_FORTRAN_I, STD_FORTRAN_II, STD_FORTRAN_III, STD_FORTRAN_IV, &
    STD_FORTRAN_66, STD_FORTRAN_77, STD_FORTRAN_90, STD_FORTRAN_95, STD_FORTRAN_2003, &
    STD_FORTRAN_2008, STD_FORTRAN_2018, STD_FORTRAN_2023, cdd_options_t, cdd_default_options
  use cdd_json_dom, only: cdd_json_doc_t, jdoc_get_key, jdoc_node_str_bounds
  use cdd_json_parser, only: json_parse
  use cdd_json_schema, only: json_schema_to_ir
  use cdd_ir_types, only: cdd_ir_schema_bundle_t
  use cdd_emit_udt, only: emit_udt_fortran
  use cdd_emit_codec, only: emit_codec_fortran
  use test_harness, only: test_state_t, th_init, th_check, th_eq_i32, th_finish
  implicit none (type, external)

  type(test_state_t) :: st
  type(cdd_fs_t) :: fs
  integer :: mkdir_rc

  call th_init(st)
  call fs_init_os(fs)
  call execute_command_line("mkdir -p build/rt_codecs", exitstat=mkdir_rc)

  call test_rt_fortran_i(st, fs)
  call test_rt_fortran_ii_iii(st, fs)
  call test_rt_fortran_iv_66(st, fs)
  call test_rt_fortran_77_common_and_dec(st, fs)
  call test_rt_fortran_90_95(st, fs)
  call test_rt_fortran_2003_oop_enum(st, fs)
  call test_rt_fortran_2008_2018_2023(st, fs)

  call th_finish(st, "test_codec_runtime_compile")

contains

  !> @brief Compile and run a generated FORTRAN (I) assigned-GOTO JSON round-trip deck.
  !> @param[inout] st Test harness state.
  !> @param[inout] fs OS filesystem handle.
  subroutine test_rt_fortran_i(st, fs)
    type(test_state_t), intent(inout) :: st
    type(cdd_fs_t), intent(inout) :: fs
    type(cdd_json_doc_t) :: doc
    type(cdd_options_t) :: opts
    type(cdd_ir_schema_bundle_t) :: bundle
    type(cdd_buffer_t) :: udt_b, cod_b
    type(cdd_error_t) :: err
    integer :: rc
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=*), parameter :: SCH = &
      '{"title":"Card","type":"object","properties":{' // &
      '"cnt":{"type":"integer"},"mass":{"type":"number"},' // &
      '"ok":{"type":"boolean"},"tag":{"type":"string","maxLength":12}}}'

    call json_parse(SCH, doc, err)
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_I
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_udt_fortran(bundle, opts, udt_b, err)
    call emit_codec_fortran(bundle, opts, cod_b, err)
    call th_eq_i32(st, err%code, CDD_OK, "F57 emit")

    call fs_write_text_file(fs, "build/rt_codecs/f57.f", &
      udt_b%raw(1:udt_b%len) // &
      "      DIMENSION JBUF(512)" // LF // &
      "      ICNT = -42" // LF // &
      "      XMASS = 12.5" // LF // &
      "      IOK = 1" // LF // &
      "      ITAG(1) = 33 + 64 * 34" // LF // &
      "      ITAG(2) = 0" // LF // &
      "      ASSIGN 9001 TO IJSRET" // LF // &
      "      GO TO 8000" // LF // &
      " 9001 CONTINUE" // LF // &
      "      IF (IERR .NE. 0) STOP 1" // LF // &
      "      ICNT = 0" // LF // &
      "      XMASS = 0.0" // LF // &
      "      IOK = 0" // LF // &
      "      ITAG(1) = 0" // LF // &
      "      ASSIGN 9002 TO IJSRET" // LF // &
      "      GO TO 8500" // LF // &
      " 9002 CONTINUE" // LF // &
      "      IF (IERR .NE. 0) STOP 2" // LF // &
      "      IF (ICNT .NE. -42) STOP 3" // LF // &
      "      IF (ABS(XMASS - 12.5) .GT. 0.01) STOP 4" // LF // &
      "      IF (IOK .NE. 1) STOP 5" // LF // &
      "      IF (ITAG(1) .NE. 2209) STOP 6" // LF // &
      "      STOP" // LF // &
      cod_b%raw(1:cod_b%len) // &
      "      END" // LF, err)
    call execute_command_line( &
      "gfortran -ffixed-form -std=legacy -w build/rt_codecs/f57.f -o build/rt_codecs/f57_bin " // &
      "&& ./build/rt_codecs/f57_bin", exitstat=rc)
    call th_eq_i32(st, int(rc, i32), 0_i32, "FORTRAN (I) runtime round-trip")
  end subroutine test_rt_fortran_i

  !> @brief Compile and run generated FORTRAN II and FORTRAN III blank-COMMON codecs.
  !> @param[inout] st Test harness state.
  !> @param[inout] fs OS filesystem handle.
  subroutine test_rt_fortran_ii_iii(st, fs)
    type(test_state_t), intent(inout) :: st
    type(cdd_fs_t), intent(inout) :: fs
    type(cdd_json_doc_t) :: doc
    type(cdd_options_t) :: opts
    type(cdd_ir_schema_bundle_t) :: bundle
    type(cdd_buffer_t) :: udt_b, cod_b
    type(cdd_error_t) :: err
    integer(i32) :: s
    integer :: rc
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=*), parameter :: SCH = &
      '{"title":"Tel","type":"object","properties":{' // &
      '"seq":{"type":"integer"},"ok":{"type":"boolean"},' // &
      '"arr":{"type":"array","maxItems":2,"items":{"type":"integer"}}}}'

    call json_parse(SCH, doc, err)
    do s = STD_FORTRAN_II, STD_FORTRAN_III
      call cdd_default_options(opts)
      opts%standard%id = s
      call json_schema_to_ir(doc, opts, bundle, err)
      call emit_udt_fortran(bundle, opts, udt_b, err)
      call emit_codec_fortran(bundle, opts, cod_b, err)
      call fs_write_text_file(fs, "build/rt_codecs/f58.f", &
        udt_b%raw(1:udt_b%len) // &
        "      DIMENSION JBUF(256)" // LF // &
        "      ISEQ = 77" // LF // &
        "      IOK = 1" // LF // &
        "      IARR(1) = 10" // LF // &
        "      IARR(2) = -20" // LF // &
        "      CALL U2JTEL(JBUF, JLEN, IERR)" // LF // &
        "      IF (IERR .NE. 0) STOP 1" // LF // &
        "      ISEQ = 0" // LF // &
        "      IOK = 0" // LF // &
        "      IARR(1) = 0" // LF // &
        "      IARR(2) = 0" // LF // &
        "      CALL J2UTEL(JBUF, JLEN, IERR)" // LF // &
        "      IF (IERR .NE. 0) STOP 2" // LF // &
        "      IF (ISEQ .NE. 77 .OR. IOK .NE. 1) STOP 3" // LF // &
        "      IF (IARR(1) .NE. 10 .OR. IARR(2) .NE. -20) STOP 4" // LF // &
        "      END" // LF // &
        cod_b%raw(1:cod_b%len), err)
      call execute_command_line( &
        "gfortran -ffixed-form -std=legacy -w build/rt_codecs/f58.f -o build/rt_codecs/f58_bin " // &
        "&& ./build/rt_codecs/f58_bin", exitstat=rc)
      call th_eq_i32(st, int(rc, i32), 0_i32, "FORTRAN II/III runtime round-trip")
    end do
  end subroutine test_rt_fortran_ii_iii

  !> @brief Compile and run generated FORTRAN IV and FORTRAN 66 named-COMMON codecs.
  !> @param[inout] st Test harness state.
  !> @param[inout] fs OS filesystem handle.
  subroutine test_rt_fortran_iv_66(st, fs)
    type(test_state_t), intent(inout) :: st
    type(cdd_fs_t), intent(inout) :: fs
    type(cdd_json_doc_t) :: doc
    type(cdd_options_t) :: opts
    type(cdd_ir_schema_bundle_t) :: bundle
    type(cdd_buffer_t) :: udt_b, cod_b
    type(cdd_error_t) :: err
    integer(i32) :: s
    integer :: rc
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=*), parameter :: SCH = &
      '{"title":"Sens","type":"object","properties":{' // &
      '"cnt":{"type":"integer"},"val":{"type":"number","format":"float"},' // &
      '"act":{"type":"boolean"},"lbl":{"type":"string","maxLength":8}}}'

    call json_parse(SCH, doc, err)
    do s = STD_FORTRAN_IV, STD_FORTRAN_66
      call cdd_default_options(opts)
      opts%standard%id = s
      call json_schema_to_ir(doc, opts, bundle, err)
      call emit_udt_fortran(bundle, opts, udt_b, err)
      call emit_codec_fortran(bundle, opts, cod_b, err)
      call fs_write_text_file(fs, "build/rt_codecs/f66.f", &
        udt_b%raw(1:udt_b%len) // &
        "      INTEGER JBUF(512), JLEN, IERR" // LF // &
        "      CNT = 123" // LF // &
        "      VAL = -3.25" // LF // &
        "      ACT = .TRUE." // LF // &
        "      LBL(1) = 79 + 128 * 75" // LF // &
        "      LBL(2) = 0" // LF // &
        "      CALL U2JSEN(JBUF, 2, JLEN, IERR)" // LF // &
        "      IF (IERR .NE. 2) STOP 1" // LF // &
        "      CALL U2JSEN(JBUF, 512, JLEN, IERR)" // LF // &
        "      IF (IERR .NE. 0) STOP 2" // LF // &
        "      CNT = 0" // LF // &
        "      VAL = 0.0" // LF // &
        "      ACT = .FALSE." // LF // &
        "      LBL(1) = 0" // LF // &
        "      CALL J2USEN(JBUF, JLEN, IERR)" // LF // &
        "      IF (IERR .NE. 0) STOP 3" // LF // &
        "      IF (CNT .NE. 123 .OR. .NOT. ACT) STOP 4" // LF // &
        "      IF (ABS(VAL + 3.25) .GT. 0.01) STOP 5" // LF // &
        "      IF (LBL(1) .NE. 79 + 128 * 75) STOP 6" // LF // &
        "      END" // LF // &
        cod_b%raw(1:cod_b%len), err)
      call execute_command_line( &
        "gfortran -ffixed-form -std=legacy -w build/rt_codecs/f66.f -o build/rt_codecs/f66_bin " // &
        "&& ./build/rt_codecs/f66_bin", exitstat=rc)
      call th_eq_i32(st, int(rc, i32), 0_i32, "FORTRAN IV/66 runtime round-trip")
    end do
  end subroutine test_rt_fortran_iv_66

  !> @brief Compile and run FORTRAN 77 split-COMMON and DEC STRUCTURE/RECORD codecs.
  !> @param[inout] st Test harness state.
  !> @param[inout] fs OS filesystem handle.
  subroutine test_rt_fortran_77_common_and_dec(st, fs)
    type(test_state_t), intent(inout) :: st
    type(cdd_fs_t), intent(inout) :: fs
    type(cdd_json_doc_t) :: doc
    type(cdd_options_t) :: opts
    type(cdd_ir_schema_bundle_t) :: bundle
    type(cdd_buffer_t) :: udt_b, cod_b
    type(cdd_error_t) :: err
    integer :: rc
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=1, kind=c_k), parameter :: BS = achar(92, kind=c_k)
    character(len=*), parameter :: SCH = &
      '{"title":"Nav","type":"object","properties":{' // &
      '"seq":{"type":"integer"},"ok":{"type":"boolean"},' // &
      '"name":{"type":"string","maxLength":16}}}'
    character(len=*), parameter :: DEC_SCH = &
      '{"$defs":{"Pt":{"type":"object","properties":{"x":{"type":"integer"}}}},' // &
      '"title":"Pose","type":"object","properties":{' // &
      '"pt":{"$ref":"#/$defs/Pt"},' // &
      '"m":{"type":"array","maxItems":2,"items":{"type":"array","maxItems":2,"items":{"type":"integer"}}}}}'

    ! 1. Standard FORTRAN 77 split COMMON + out-of-order + unknown key + \u0041 escape
    call json_parse(SCH, doc, err)
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_77
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_udt_fortran(bundle, opts, udt_b, err)
    call emit_codec_fortran(bundle, opts, cod_b, err)
    call fs_write_text_file(fs, "build/rt_codecs/f77.f", &
      udt_b%raw(1:udt_b%len) // &
      "      CHARACTER*256 JBUF" // LF // &
      "      INTEGER JLEN, IERR" // LF // &
      "      JBUF = '{" // '"extra":[1,2],"name":"' // BS // &
      'u0041B","ok":true,"seq":99}' // "'" // LF // &
      "      CALL NAV_FROM_JSON(JBUF, IERR)" // LF // &
      "      IF (IERR .NE. 0) STOP 1" // LF // &
      "      IF (SEQ .NE. 99 .OR. .NOT. OK .OR. NAME(1:2) .NE. 'AB') STOP 2" // LF // &
      "      CALL NAV_TO_JSON(JBUF, JLEN, IERR)" // LF // &
      "      IF (IERR .NE. 0 .OR. JLEN .LE. 10) STOP 3" // LF // &
      "      SEQ = 0" // LF // &
      "      CALL NAV_FROM_JSON(JBUF(1:JLEN), IERR)" // LF // &
      "      IF (IERR .NE. 0 .OR. SEQ .NE. 99) STOP 4" // LF // &
      "      END" // LF // &
      cod_b%raw(1:cod_b%len), err)
    call execute_command_line( &
      "gfortran -ffixed-form -std=legacy -w build/rt_codecs/f77.f -o build/rt_codecs/f77_bin " // &
      "&& ./build/rt_codecs/f77_bin", exitstat=rc)
    call th_eq_i32(st, int(rc, i32), 0_i32, "FORTRAN 77 split COMMON runtime round-trip")

    ! 2. FORTRAN 77 DEC STRUCTURE / RECORD with nested RECORD & 2D array
    call json_parse(DEC_SCH, doc, err)
    opts%allow_dec_structure = .true._lk
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_udt_fortran(bundle, opts, udt_b, err)
    call emit_codec_fortran(bundle, opts, cod_b, err)
    call fs_write_text_file(fs, "build/rt_codecs/f77_dec.f", &
      "      PROGRAM TDEC" // LF // &
      udt_b%raw(1:udt_b%len) // &
      "      CHARACTER*256 JBUF" // LF // &
      "      INTEGER JLEN, IERR" // LF // &
      "      REC_VAL.PT.X = 55" // LF // &
      "      REC_VAL.M(1,1) = 1" // LF // &
      "      REC_VAL.M(1,2) = 2" // LF // &
      "      REC_VAL.M(2,1) = 3" // LF // &
      "      REC_VAL.M(2,2) = 4" // LF // &
      "      CALL POSE_T_TO_JSON(REC_VAL, JBUF, JLEN, IERR)" // LF // &
      "      IF (IERR .NE. 0) STOP 1" // LF // &
      "      REC_VAL.PT.X = 0" // LF // &
      "      REC_VAL.M(2,2) = 0" // LF // &
      "      CALL POSE_T_FROM_JSON(JBUF(1:JLEN), REC_VAL, IERR)" // LF // &
      "      IF (IERR .NE. 0) STOP 2" // LF // &
      "      IF (REC_VAL.PT.X .NE. 55 .OR. REC_VAL.M(2,2) .NE. 4) STOP 3" // LF // &
      "      END" // LF // &
      cod_b%raw(1:cod_b%len), err)
    call execute_command_line( &
      "gfortran -ffixed-form -fdec-structure -std=legacy -w build/rt_codecs/f77_dec.f " // &
      "-o build/rt_codecs/f77_dec_bin && ./build/rt_codecs/f77_dec_bin", exitstat=rc)
    call th_eq_i32(st, int(rc, i32), 0_i32, "FORTRAN 77 DEC STRUCTURE runtime round-trip")
  end subroutine test_rt_fortran_77_common_and_dec

  !> @brief Compile and run Fortran 90 & Fortran 95 codecs with strict `-std=f95 -Werror`.
  !> @param[inout] st Test harness state.
  !> @param[inout] fs OS filesystem handle.
  subroutine test_rt_fortran_90_95(st, fs)
    type(test_state_t), intent(inout) :: st
    type(cdd_fs_t), intent(inout) :: fs
    type(cdd_json_doc_t) :: doc
    type(cdd_options_t) :: opts
    type(cdd_ir_schema_bundle_t) :: bundle
    type(cdd_buffer_t) :: cod_b
    type(cdd_error_t) :: err
    integer(i32) :: s
    integer :: rc
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=*), parameter :: SCH = &
      '{"title":"Packet","type":"object","properties":{' // &
      '"id":{"type":"integer"},"ok":{"type":"boolean"},' // &
      '"scores":{"type":"array","items":{"type":"integer"}},' // &
      '"note":{"type":["string","null"],"maxLength":16}}}'

    call json_parse(SCH, doc, err)
    do s = STD_FORTRAN_90, STD_FORTRAN_95
      call cdd_default_options(opts)
      opts%standard%id = s
      call json_schema_to_ir(doc, opts, bundle, err)
      call emit_codec_fortran(bundle, opts, cod_b, err)
      call fs_write_text_file(fs, "build/rt_codecs/f95_test.f90", &
        cod_b%raw(1:cod_b%len) // &
        "p" // "rogram drv" // LF // &
        "  use packet_mod, only: int32, packet_t, packet_t_to_json, packet_t_from_json" // LF // &
        "  implicit none" // LF // &
        "  type(packet_t) :: p1, p2" // LF // &
        "  character(len=512) :: jbuf" // LF // &
        "  integer(int32) :: jlen, ierr" // LF // &
        "  call packet_t_from_json(p1, " // &
        "'{" // '"id":42,"ok":true,"scores":[7,8,9],"note":null' // "}', ierr)" // LF // &
        "  if (ierr /= 0) stop 1" // LF // &
        "  if (p1%id /= 42 .or. .not. p1%ok .or. size(p1%scores) /= 3) stop 2" // LF // &
        "  if (associated(p1%note)) stop 3" // LF // &
        "  call packet_t_to_json(p1, jbuf, jlen, ierr)" // LF // &
        "  if (ierr /= 0) stop 4" // LF // &
        "  call packet_t_from_json(p2, jbuf(1:jlen), ierr)" // LF // &
        "  if (ierr /= 0 .or. p2%scores(3) /= 9) stop 5" // LF // &
        "end p" // "rogram drv" // LF, err)
      call execute_command_line( &
        "gfortran -std=f95 -Wall -Wextra -Wpedantic -Werror -J build/rt_codecs " // &
        "build/rt_codecs/f95_test.f90 -o build/rt_codecs/f95_bin && ./build/rt_codecs/f95_bin", &
        exitstat=rc)
      call th_eq_i32(st, int(rc, i32), 0_i32, "Fortran 90/95 strict -std=f95 round-trip")
    end do
  end subroutine test_rt_fortran_90_95

  !> @brief Compile and run Fortran 2003 OOP `EXTENDS` + `ENUM (BIND(C))` + TBP codec (`-std=f2003`).
  !> @param[inout] st Test harness state.
  !> @param[inout] fs OS filesystem handle.
  subroutine test_rt_fortran_2003_oop_enum(st, fs)
    type(test_state_t), intent(inout) :: st
    type(cdd_fs_t), intent(inout) :: fs
    type(cdd_json_doc_t) :: doc
    type(cdd_options_t) :: opts
    type(cdd_ir_schema_bundle_t) :: bundle
    type(cdd_buffer_t) :: cod_b
    type(cdd_error_t) :: err
    integer :: rc
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=*), parameter :: SCH = &
      '{"$defs":{"Header":{"type":"object","properties":{"seq":{"type":"integer"}}}},' // &
      '"title":"Cmd","allOf":[{"$ref":"#/$defs/Header"}],' // &
      '"type":"object","properties":{' // &
      '"name":{"type":"string"},' // &
      '"mode":{"type":"string","e' // 'num":["IDLE","BURN"]},' // &
      '"grid":{"type":"array","items":{"type":"array","items":{"type":"integer"}}}}}'

    call json_parse(SCH, doc, err)
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_2003
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_codec_fortran(bundle, opts, cod_b, err)
    call fs_write_text_file(fs, "build/rt_codecs/f2003_test.f90", &
      cod_b%raw(1:cod_b%len) // &
      "p" // "rogram drv03" // LF // &
      "  use cmd_mod, only: int32, cmd_t, BURN, cmd_t_to_json_str" // LF // &
      "  implicit none" // LF // &
      "  type(cmd_t) :: c1, c2" // LF // &
      "  character(len=:), allocatable :: jstr, emsg" // LF // &
      "  integer(int32) :: ierr" // LF // &
      "  call c1%from_json(" // &
      "'{" // '"seq":11,"name":"orb","mode":"BURN","grid":[[1,2],[3,4]]' // "}', ierr, emsg)" // LF // &
      "  if (ierr /= 0) stop 1" // LF // &
      "  if (c1%seq /= 11 .or. c1%name /= 'orb' .or. c1%mode /= BURN) stop 2" // LF // &
      "  jstr = cmd_t_to_json_str(c1)" // LF // &
      "  call c2%from_json(jstr, ierr, emsg)" // LF // &
      "  if (ierr /= 0 .or. c2%seq /= 11 .or. c2%grid(2,2) /= 4) stop 3" // LF // &
      "end p" // "rogram drv03" // LF, err)
    call execute_command_line( &
      "gfortran -std=f2003 -Wall -Wextra -Wpedantic -Werror -J build/rt_codecs " // &
      "build/rt_codecs/f2003_test.f90 -o build/rt_codecs/f2003_bin && ./build/rt_codecs/f2003_bin", &
      exitstat=rc)
    call th_eq_i32(st, int(rc, i32), 0_i32, "Fortran 2003 OOP + ENUM -std=f2003 round-trip")
  end subroutine test_rt_fortran_2003_oop_enum

  !> @brief Compile & run F2008/F2018 SUBMODULE + recursive UDT and F2023 codecs, verifying JSON DOM.
  !> @param[inout] st Test harness state.
  !> @param[inout] fs OS filesystem handle.
  subroutine test_rt_fortran_2008_2018_2023(st, fs)
    type(test_state_t), intent(inout) :: st
    type(cdd_fs_t), intent(inout) :: fs
    type(cdd_json_doc_t) :: doc, out_doc
    type(cdd_options_t) :: opts
    type(cdd_ir_schema_bundle_t) :: bundle
    type(cdd_buffer_t) :: udt_b, cod_b, json_out_b
    type(cdd_error_t) :: err
    integer(i32) :: s, id_idx, ok_idx, tag_idx, so, slen
    integer :: rc
    character(len=32, kind=c_k) :: std_flag
    character(len=1, kind=c_k), parameter :: LF = achar(10, kind=c_k)
    character(len=*), parameter :: REC_SCH = &
      '{"$defs":{"Node":{"type":"object","properties":{' // &
      '"val":{"type":"integer"},"next":{"$ref":"#/$defs/Node"}}}},' // &
      '"$ref":"#/$defs/Node"}'
    character(len=*), parameter :: F23_SCH = &
      '{"title":"Sat","type":"object","properties":{' // &
      '"id":{"type":"integer"},"ok":{"type":"boolean"},"tag":{"type":"string"}}}'

    ! F2008 & F2018 with SUBMODULE split + recursive allocatable Node linked list
    call json_parse(REC_SCH, doc, err)
    do s = STD_FORTRAN_2008, STD_FORTRAN_2018
      call cdd_default_options(opts)
      opts%standard%id = s
      opts%emit_submodules = .true._lk
      if (s == STD_FORTRAN_2008) then
        std_flag = "-std=f2008"
      else
        std_flag = "-std=f2018"
      end if
      call json_schema_to_ir(doc, opts, bundle, err)
      call emit_udt_fortran(bundle, opts, udt_b, err)
      call emit_codec_fortran(bundle, opts, cod_b, err)
      call fs_write_text_file(fs, "build/rt_codecs/f08_smod.f90", &
        udt_b%raw(1:udt_b%len) // cod_b%raw(1:cod_b%len) // &
        "p" // "rogram drv08" // LF // &
        "  use node_mod, only: node_t" // LF // &
        "  use, intrinsic :: iso_fortran_env, only: int32" // LF // &
        "  implicit none" // LF // &
        "  type(node_t) :: n1, n2" // LF // &
        "  character(len=:), allocatable :: js" // LF // &
        "  integer(int32) :: ierr" // LF // &
        "  call n1%from_json('{" // '"val":10,"next":{"val":20,"next":null}' // "}', ierr)" // LF // &
        "  if (ierr /= 0 .or. n1%val /= 10 .or. .not. allocated(n1%next)) stop 1" // LF // &
        "  if (n1%next%val /= 20 .or. allocated(n1%next%next)) stop 2" // LF // &
        "  call n1%to_json(js, ierr)" // LF // &
        "  if (ierr /= 0) stop 3" // LF // &
        "  call n2%from_json(js, ierr)" // LF // &
        "  if (ierr /= 0 .or. n2%next%val /= 20) stop 4" // LF // &
        "end p" // "rogram drv08" // LF, err)
      call execute_command_line( &
        "gfortran " // trim(std_flag) // " -Wall -Wextra -Wpedantic -Werror -J build/rt_codecs " // &
        "build/rt_codecs/f08_smod.f90 -o build/rt_codecs/f08_bin && ./build/rt_codecs/f08_bin", &
        exitstat=rc)
      call th_eq_i32(st, int(rc, i32), 0_i32, "F2008/F2018 SUBMODULE + recursive UDT round-trip")
    end do

    ! F2023 (-std=f2023) writing serialized JSON to file & verifying with host json_parse!
    call json_parse(F23_SCH, doc, err)
    call cdd_default_options(opts)
    opts%standard%id = STD_FORTRAN_2023
    call json_schema_to_ir(doc, opts, bundle, err)
    call emit_codec_fortran(bundle, opts, cod_b, err)
    call fs_write_text_file(fs, "build/rt_codecs/f2023_test.f90", &
      cod_b%raw(1:cod_b%len) // &
      "p" // "rogram drv23" // LF // &
      "  use sat_mod, only: sat_t" // LF // &
      "  use, intrinsic :: iso_fortran_env, only: int32" // LF // &
      "  implicit none (type, external)" // LF // &
      "  type(sat_t) :: s" // LF // &
      "  character(len=:), allocatable :: js" // LF // &
      "  integer(int32) :: ierr" // LF // &
      "  integer :: u" // LF // &
      "  call s%from_json('{" // '"tag":"VOYAGER","ok":true,"id":2023' // "}', ierr)" // LF // &
      "  if (ierr /= 0) stop 1" // LF // &
      "  call s%to_json(js, ierr)" // LF // &
      "  if (ierr /= 0) stop 2" // LF // &
      "  open(newunit=u, file='build/rt_codecs/f2023_out.json', status='replace', action='write')" // LF // &
      "  write(u, '(A)') js" // LF // &
      "  close(u)" // LF // &
      "end p" // "rogram drv23" // LF, err)
    call execute_command_line( &
      "gfortran -std=f2023 -Wall -Wextra -Wpedantic -Werror -J build/rt_codecs " // &
      "build/rt_codecs/f2023_test.f90 -o build/rt_codecs/f2023_bin && ./build/rt_codecs/f2023_bin", &
      exitstat=rc)
    call th_eq_i32(st, int(rc, i32), 0_i32, "Fortran 2023 -std=f2023 runtime execution")

    call fs_read_text_file(fs, "build/rt_codecs/f2023_out.json", json_out_b, err)
    call th_eq_i32(st, err%code, CDD_OK, "read F2023 emitted JSON")
    call json_parse(json_out_b%raw(1:json_out_b%len), out_doc, err)
    call th_eq_i32(st, err%code, CDD_OK, "parse F2023 emitted JSON with RFC 8259 parser")
    id_idx = jdoc_get_key(out_doc, out_doc%root, "id")
    ok_idx = jdoc_get_key(out_doc, out_doc%root, "ok")
    tag_idx = jdoc_get_key(out_doc, out_doc%root, "tag")
    call th_eq_i32(st, int(out_doc%nodes(id_idx)%int_val, i32), 2023_i32, &
      "F2023 round-trip id == 2023")
    call th_check(st, out_doc%nodes(ok_idx)%bool_val, "F2023 round-trip ok == true")
    call jdoc_node_str_bounds(out_doc, tag_idx, so, slen)
    call th_check(st, out_doc%str_pool%raw(so:slen) == "VOYAGER", &
      "F2023 round-trip tag == VOYAGER")
  end subroutine test_rt_fortran_2008_2018_2023

end program test_codec_runtime_compile
