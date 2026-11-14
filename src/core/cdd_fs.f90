!> @file cdd_fs.f90
!> @brief Strongly-typed composition-based filesystem provider (OS stream + fault-injecting mock).

!> @brief Filesystem abstraction using explicit delegation for OS stream I/O and in-memory testing.
module cdd_fs
  use cdd_kinds, only: i32, lk, c_k, std_in_unit, std_out_unit
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set_simple, &
    CDD_ERR_IO_READ, CDD_ERR_IO_WRITE, CDD_ERR_BUFFER_OVERFLOW, CDD_ERR_INVALID_OPTION
  use cdd_string, only: CDD_BUF_MAX, cdd_buffer_t, cdd_string_t, buf_init, buf_append_char, &
    buf_append_str, str_set, str_eq
  implicit none (type, external)
  private

  public :: CDD_MOCK_MAX_FILES, FS_BACKEND_OS, FS_BACKEND_MOCK
  public :: cdd_os_fs_t, cdd_mock_entry_t, cdd_mock_fs_t, cdd_fs_t
  public :: fs_init_os, fs_init_mock
  public :: fs_read_text_file, fs_write_text_file, fs_file_exists, fs_read_stdin, fs_write_stdout
  public :: mock_fs_init, mock_fs_put, mock_fs_get

  integer(i32), parameter :: CDD_MOCK_MAX_FILES = 8_i32 !< Max files stored in in-memory mock FS.
  integer(i32), parameter :: FS_BACKEND_OS = 1_i32      !< Delegate I/O to OS Fortran streams.
  integer(i32), parameter :: FS_BACKEND_MOCK = 2_i32    !< Delegate I/O to in-memory mock table.

  !> @brief Operating-system stream configuration state.
  type :: cdd_os_fs_t
    integer(i32) :: stdin_unit = std_in_unit      !< Fortran unit for stdin (overridable in tests).
    integer(i32) :: stdout_unit = std_out_unit    !< Fortran unit for stdout (overridable in tests).
    integer(i32) :: max_read_bytes = CDD_BUF_MAX  !< Configurable byte cap for read_text_file.
  end type cdd_os_fs_t

  !> @brief Single virtual file slot inside cdd_mock_fs_t.
  type :: cdd_mock_entry_t
    logical(lk) :: in_use = .false._lk !< True when slot contains an active virtual file.
    type(cdd_string_t) :: path         !< Virtual file path string.
    type(cdd_buffer_t) :: data         !< Virtual file raw byte contents.
  end type cdd_mock_entry_t

  !> @brief In-memory mock filesystem state supporting deterministic fault injection.
  type :: cdd_mock_fs_t
    logical(lk) :: fail_next_read = .false._lk           !< Inject read error on next read call.
    logical(lk) :: fail_next_write = .false._lk          !< Inject write error on next write call.
    type(cdd_buffer_t) :: stdin_buf                      !< Preloaded standard input content.
    type(cdd_buffer_t) :: stdout_buf                     !< Captured standard output content.
    type(cdd_mock_entry_t) :: files(CDD_MOCK_MAX_FILES)  !< Fixed virtual file table.
  end type cdd_mock_fs_t

  !> @brief Unified filesystem handle delegating via explicit composition to OS or mock backend.
  type :: cdd_fs_t
    integer(i32) :: backend = FS_BACKEND_OS !< Active backend selector (FS_BACKEND_OS/MOCK).
    type(cdd_os_fs_t) :: os                 !< OS stream backend state.
    type(cdd_mock_fs_t) :: mock             !< In-memory mock backend state.
  end type cdd_fs_t

contains

  !> @brief Initialize a cdd_fs_t handle configured for real OS stream I/O.
  !> @param[out] fs Filesystem handle to initialize.
  pure subroutine fs_init_os(fs)
    type(cdd_fs_t), intent(out) :: fs

    fs%backend = FS_BACKEND_OS
    fs%os%stdin_unit = std_in_unit
    fs%os%stdout_unit = std_out_unit
    fs%os%max_read_bytes = CDD_BUF_MAX
    call mock_fs_init(fs%mock)
  end subroutine fs_init_os

  !> @brief Initialize a cdd_fs_t handle configured for in-memory mock I/O.
  !> @param[out] fs Filesystem handle to initialize.
  pure subroutine fs_init_mock(fs)
    type(cdd_fs_t), intent(out) :: fs

    fs%backend = FS_BACKEND_MOCK
    fs%os%stdin_unit = std_in_unit
    fs%os%stdout_unit = std_out_unit
    fs%os%max_read_bytes = 32768_i32
    call mock_fs_init(fs%mock)
  end subroutine fs_init_mock

  !> @brief Read entire file at path into out_buf without newline translation.
  !> @param[inout] fs      Filesystem handle.
  !> @param[in]    path    Source file path.
  !> @param[out]   out_buf Destination byte buffer.
  !> @param[out]   err     Error status record.
  subroutine fs_read_text_file(fs, path, out_buf, err)
    type(cdd_fs_t), intent(inout) :: fs
    character(len=*, kind=c_k), intent(in) :: path
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(out) :: err

    select case (fs%backend)
    case (FS_BACKEND_OS)
      call os_read_text_file(fs%os, path, out_buf, err)
    case (FS_BACKEND_MOCK)
      call mock_read_text_file(fs%mock, path, out_buf, err)
    case default
      call buf_init(out_buf)
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Invalid filesystem backend")
    end select
  end subroutine fs_read_text_file

  !> @brief Write raw bytes of content to path, creating or replacing the file.
  !> @param[inout] fs      Filesystem handle.
  !> @param[in]    path    Destination file path.
  !> @param[in]    content Raw character bytes to write.
  !> @param[out]   err     Error status record.
  subroutine fs_write_text_file(fs, path, content, err)
    type(cdd_fs_t), intent(inout) :: fs
    character(len=*, kind=c_k), intent(in) :: path
    character(len=*, kind=c_k), intent(in) :: content
    type(cdd_error_t), intent(out) :: err

    select case (fs%backend)
    case (FS_BACKEND_OS)
      call os_write_text_file(fs%os, path, content, err)
    case (FS_BACKEND_MOCK)
      call mock_write_text_file(fs%mock, path, content, err)
    case default
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Invalid filesystem backend")
    end select
  end subroutine fs_write_text_file

  !> @brief Check whether a file exists at path.
  !> @param[in] fs   Filesystem handle.
  !> @param[in] path File path to check.
  !> @return True if file exists in the active backend.
  function fs_file_exists(fs, path) result(exists_flag)
    type(cdd_fs_t), intent(in) :: fs
    character(len=*, kind=c_k), intent(in) :: path
    logical(lk) :: exists_flag

    select case (fs%backend)
    case (FS_BACKEND_OS)
      exists_flag = os_file_exists(path)
    case (FS_BACKEND_MOCK)
      exists_flag = (find_mock_slot(fs%mock, path) > 0_i32)
    case default
      exists_flag = .false._lk
    end select
  end function fs_file_exists

  !> @brief Read standard input stream into out_buf.
  !> @param[inout] fs      Filesystem handle.
  !> @param[out]   out_buf Destination text buffer.
  !> @param[out]   err     Error status record.
  subroutine fs_read_stdin(fs, out_buf, err)
    type(cdd_fs_t), intent(inout) :: fs
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(out) :: err

    select case (fs%backend)
    case (FS_BACKEND_OS)
      call os_read_stdin(fs%os, out_buf, err)
    case (FS_BACKEND_MOCK)
      call mock_read_stdin(fs%mock, out_buf, err)
    case default
      call buf_init(out_buf)
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Invalid filesystem backend")
    end select
  end subroutine fs_read_stdin

  !> @brief Write content string to standard output stream.
  !> @param[inout] fs      Filesystem handle.
  !> @param[in]    content Output text payload.
  !> @param[out]   err     Error status record.
  subroutine fs_write_stdout(fs, content, err)
    type(cdd_fs_t), intent(inout) :: fs
    character(len=*, kind=c_k), intent(in) :: content
    type(cdd_error_t), intent(out) :: err

    select case (fs%backend)
    case (FS_BACKEND_OS)
      call os_write_stdout(fs%os, content, err)
    case (FS_BACKEND_MOCK)
      call mock_write_stdout(fs%mock, content, err)
    case default
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Invalid filesystem backend")
    end select
  end subroutine fs_write_stdout

  !> @brief Read a disk file byte-for-byte via Fortran unformatted stream access.
  !> @param[in]  os      OS filesystem configuration.
  !> @param[in]  path    Target file path.
  !> @param[out] out_buf Destination buffer.
  !> @param[out] err     Error status record.
  subroutine os_read_text_file(os, path, out_buf, err)
    type(cdd_os_fs_t), intent(in) :: os
    character(len=*, kind=c_k), intent(in) :: path
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: unit_num, ios
    character(len=1, kind=c_k) :: byte_ch

    call buf_init(out_buf)
    call cdd_error_clear(err)

    open(newunit=unit_num, file=path, access="stream", form="unformatted", &
         status="old", action="read", iostat=ios)
    if (ios /= 0_i32) then
      call cdd_error_set_simple(err, CDD_ERR_IO_READ, "Cannot open file for reading")
      return
    end if

    do
      read(unit_num, iostat=ios) byte_ch
      if (ios /= 0_i32) exit
      if (out_buf%len >= os%max_read_bytes) then
        close(unit_num, iostat=ios)
        call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "File exceeds max_read_bytes")
        return
      end if
      out_buf%len = out_buf%len + 1_i32
      out_buf%raw(out_buf%len:out_buf%len) = byte_ch
    end do

    close(unit_num, iostat=ios)
  end subroutine os_read_text_file

  !> @brief Write raw bytes to disk via Fortran unformatted stream access.
  !> @param[in]  os      OS filesystem configuration.
  !> @param[in]  path    Target file path.
  !> @param[in]  content Raw character content.
  !> @param[out] err     Error status record.
  subroutine os_write_text_file(os, path, content, err)
    type(cdd_os_fs_t), intent(in) :: os
    character(len=*, kind=c_k), intent(in) :: path
    character(len=*, kind=c_k), intent(in) :: content
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: unit_num, ios, n

    call cdd_error_clear(err)
    if (os%max_read_bytes < 0_i32) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Negative max_read_bytes")
      return
    end if

    open(newunit=unit_num, file=path, access="stream", form="unformatted", &
         status="replace", action="write", iostat=ios)
    if (ios /= 0_i32) then
      call cdd_error_set_simple(err, CDD_ERR_IO_WRITE, "Cannot open file for writing")
      return
    end if

    n = len(content, kind=i32)
    if (n > 0_i32) then
      write(unit_num, iostat=ios) content
    end if
    close(unit_num, iostat=ios)
  end subroutine os_write_text_file

  !> @brief Check disk file existence using standard Fortran INQUIRE.
  !> @param[in] path File path to check.
  !> @return True if file exists on disk.
  function os_file_exists(path) result(exists_flag)
    character(len=*, kind=c_k), intent(in) :: path
    logical(lk) :: exists_flag
    integer(i32) :: ios

    exists_flag = .false._lk
    inquire(file=path, exist=exists_flag, iostat=ios)
  end function os_file_exists

  !> @brief Read lines from os%stdin_unit using non-advancing formatted I/O.
  !> @param[in]  os      OS filesystem configuration.
  !> @param[out] out_buf Destination text buffer.
  !> @param[out] err     Error status record.
  subroutine os_read_stdin(os, out_buf, err)
    type(cdd_os_fs_t), intent(in) :: os
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(out) :: err
    character(len=512, kind=c_k) :: chunk
    integer(i32) :: nread, ios
    character(len=1, kind=c_k), parameter :: CH_LF = achar(10, kind=c_k)

    call buf_init(out_buf)
    call cdd_error_clear(err)

    do
      nread = 0_i32
      read(os%stdin_unit, "(A)", advance="no", size=nread, iostat=ios) chunk
      if (ios > 0_i32) then
        call cdd_error_set_simple(err, CDD_ERR_IO_READ, "Failed reading standard input")
        return
      end if
      if (nread > 0_i32) then
        call buf_append_str(out_buf, chunk(1:nread), err)
      end if
      if (is_iostat_eor(ios)) then
        call buf_append_char(out_buf, CH_LF, err)
      else if (is_iostat_end(ios)) then
        exit
      end if
    end do
  end subroutine os_read_stdin

  !> @brief Write content to os%stdout_unit without extra leading blanks.
  !> @param[in]  os      OS filesystem configuration.
  !> @param[in]  content Text payload to emit.
  !> @param[out] err     Error status record.
  subroutine os_write_stdout(os, content, err)
    type(cdd_os_fs_t), intent(in) :: os
    character(len=*, kind=c_k), intent(in) :: content
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: ios

    call cdd_error_clear(err)
    write(os%stdout_unit, "(A)", advance="no", iostat=ios) content
    if (ios /= 0_i32) then
      call cdd_error_set_simple(err, CDD_ERR_IO_WRITE, "Failed writing standard output")
    end if
  end subroutine os_write_stdout

  !> @brief Initialize an empty in-memory mock filesystem.
  !> @param[out] mfs Mock filesystem instance to reset.
  pure subroutine mock_fs_init(mfs)
    type(cdd_mock_fs_t), intent(out) :: mfs
    integer(i32) :: i

    mfs%fail_next_read = .false._lk
    mfs%fail_next_write = .false._lk
    call buf_init(mfs%stdin_buf)
    call buf_init(mfs%stdout_buf)
    do i = 1_i32, CDD_MOCK_MAX_FILES
      mfs%files(i)%in_use = .false._lk
      call buf_init(mfs%files(i)%data)
    end do
  end subroutine mock_fs_init

  !> @brief Store or overwrite a virtual file in cdd_mock_fs_t.
  !> @param[inout] mfs     Mock filesystem instance.
  !> @param[in]    path    Virtual file path.
  !> @param[in]    content Virtual file text content.
  !> @param[out]   err     Error status if path/content/table overflows.
  pure subroutine mock_fs_put(mfs, path, content, err)
    type(cdd_mock_fs_t), intent(inout) :: mfs
    character(len=*, kind=c_k), intent(in) :: path
    character(len=*, kind=c_k), intent(in) :: content
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: slot, i

    call cdd_error_clear(err)
    slot = find_mock_slot(mfs, path)
    if (slot == 0_i32) then
      do i = 1_i32, CDD_MOCK_MAX_FILES
        if (.not. mfs%files(i)%in_use) then
          slot = i
          exit
        end if
      end do
    end if
    if (slot == 0_i32) then
      call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "Mock FS file table full")
      return
    end if

    call str_set(mfs%files(slot)%path, path, err)
    if (err%code /= 0_i32) return
    call buf_init(mfs%files(slot)%data)
    call buf_append_str(mfs%files(slot)%data, content, err)
    if (err%code /= 0_i32) return
    mfs%files(slot)%in_use = .true._lk
  end subroutine mock_fs_put

  !> @brief Retrieve a virtual file's content buffer from cdd_mock_fs_t.
  !> @param[in]  mfs     Mock filesystem instance.
  !> @param[in]  path    Virtual file path to inspect.
  !> @param[out] out_buf Copy of stored file buffer.
  !> @param[out] err     Error status (CDD_ERR_IO_READ if missing).
  pure subroutine mock_fs_get(mfs, path, out_buf, err)
    type(cdd_mock_fs_t), intent(in) :: mfs
    character(len=*, kind=c_k), intent(in) :: path
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: slot

    call buf_init(out_buf)
    call cdd_error_clear(err)
    slot = find_mock_slot(mfs, path)
    if (slot == 0_i32) then
      call cdd_error_set_simple(err, CDD_ERR_IO_READ, "Mock file not found")
      return
    end if
    call buf_append_str(out_buf, mfs%files(slot)%data%raw(1:mfs%files(slot)%data%len), err)
  end subroutine mock_fs_get

  !> @brief Read a virtual file from mock state with fault injection support.
  !> @param[inout] mfs     Mock filesystem state.
  !> @param[in]    path    Virtual file path.
  !> @param[out]   out_buf Destination buffer.
  !> @param[out]   err     Error status record.
  pure subroutine mock_read_text_file(mfs, path, out_buf, err)
    type(cdd_mock_fs_t), intent(inout) :: mfs
    character(len=*, kind=c_k), intent(in) :: path
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(out) :: err

    call buf_init(out_buf)
    if (mfs%fail_next_read) then
      mfs%fail_next_read = .false._lk
      call cdd_error_set_simple(err, CDD_ERR_IO_READ, "Injected mock read failure")
      return
    end if
    call mock_fs_get(mfs, path, out_buf, err)
  end subroutine mock_read_text_file

  !> @brief Write a virtual file into mock state with fault injection support.
  !> @param[inout] mfs     Mock filesystem state.
  !> @param[in]    path    Virtual file path.
  !> @param[in]    content Virtual file payload.
  !> @param[out]   err     Error status record.
  pure subroutine mock_write_text_file(mfs, path, content, err)
    type(cdd_mock_fs_t), intent(inout) :: mfs
    character(len=*, kind=c_k), intent(in) :: path
    character(len=*, kind=c_k), intent(in) :: content
    type(cdd_error_t), intent(out) :: err

    if (mfs%fail_next_write) then
      mfs%fail_next_write = .false._lk
      call cdd_error_set_simple(err, CDD_ERR_IO_WRITE, "Injected mock write failure")
      return
    end if
    call mock_fs_put(mfs, path, content, err)
  end subroutine mock_write_text_file

  !> @brief Read preloaded stdin buffer from mock state.
  !> @param[inout] mfs     Mock filesystem state.
  !> @param[out]   out_buf Destination buffer.
  !> @param[out]   err     Error status record.
  pure subroutine mock_read_stdin(mfs, out_buf, err)
    type(cdd_mock_fs_t), intent(inout) :: mfs
    type(cdd_buffer_t), intent(out) :: out_buf
    type(cdd_error_t), intent(out) :: err

    call buf_init(out_buf)
    call cdd_error_clear(err)
    if (mfs%fail_next_read) then
      mfs%fail_next_read = .false._lk
      call cdd_error_set_simple(err, CDD_ERR_IO_READ, "Injected mock stdin read failure")
      return
    end if
    call buf_append_str(out_buf, mfs%stdin_buf%raw(1:mfs%stdin_buf%len), err)
  end subroutine mock_read_stdin

  !> @brief Append output to captured stdout buffer in mock state.
  !> @param[inout] mfs     Mock filesystem state.
  !> @param[in]    content Output payload.
  !> @param[out]   err     Error status record.
  pure subroutine mock_write_stdout(mfs, content, err)
    type(cdd_mock_fs_t), intent(inout) :: mfs
    character(len=*, kind=c_k), intent(in) :: content
    type(cdd_error_t), intent(out) :: err

    call cdd_error_clear(err)
    if (mfs%fail_next_write) then
      mfs%fail_next_write = .false._lk
      call cdd_error_set_simple(err, CDD_ERR_IO_WRITE, "Injected mock stdout write failure")
      return
    end if
    call buf_append_str(mfs%stdout_buf, content, err)
  end subroutine mock_write_stdout

  !> @brief Locate an active slot index matching path in mfs%files (0 if absent).
  !> @param[in] mfs  Mock filesystem instance.
  !> @param[in] path Virtual path to search.
  !> @return 1-based slot index or 0 if not found.
  pure function find_mock_slot(mfs, path) result(slot)
    type(cdd_mock_fs_t), intent(in) :: mfs
    character(len=*, kind=c_k), intent(in) :: path
    integer(i32) :: slot
    integer(i32) :: i, plen

    slot = 0_i32
    do i = 1_i32, CDD_MOCK_MAX_FILES
      if (mfs%files(i)%in_use) then
        plen = mfs%files(i)%path%len
        if (str_eq(mfs%files(i)%path%raw(1:plen), path)) then
          slot = i
          return
        end if
      end if
    end do
  end function find_mock_slot

end module cdd_fs
