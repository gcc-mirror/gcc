// Filesystem operation utilities -*- C++ -*-

// Copyright (C) 2014-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#ifndef _GLIBCXX_OPS_COMMON_H
#define _GLIBCXX_OPS_COMMON_H 1

#include <chrono>
#include <bits/move.h> // std::__exchange

#ifdef _GLIBCXX_HAVE_UNISTD_H
# include <unistd.h>
# ifdef _GLIBCXX_HAVE_FCNTL_H
#  include <fcntl.h>  // AT_FDCWD, O_TRUNC etc.
# endif
# if defined(_GLIBCXX_HAVE_SYS_STAT_H) && defined(_GLIBCXX_HAVE_SYS_TYPES_H)
#  include <sys/types.h>
#  include <sys/stat.h>  // mkdir, chmod
# endif
#endif
#if !_GLIBCXX_USE_UTIMENSAT && _GLIBCXX_HAVE_UTIME_H
# include <utime.h> // utime
#endif

#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
# include <wchar.h>
#endif

#ifdef NEED_DO_COPY_FILE
# include <filesystem>
# include <ext/stdio_filebuf.h>
# ifdef _GLIBCXX_USE_COPY_FILE_RANGE
#  include <unistd.h> // copy_file_range
# endif
# ifdef _GLIBCXX_USE_SENDFILE
#  include <sys/sendfile.h> // sendfile
#  include <unistd.h> // lseek
# endif
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Get the last OS error (for POSIX this is just errno).
  inline error_code
  __last_system_error() noexcept
  {
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
    // N.B. use error_code::default_error_condition() to convert to generic.
    return {(int)::GetLastError(), std::system_category()};
#else
    return {errno, std::generic_category()};
#endif
  }

  // Get an error code indicating unsupported functionality.
  //
  // This should be used when a function is unable to behave as specified
  // due to an incomplete or partial implementation, e.g.
  // filesystem::equivalent(a, b) if is_other(a) && is_other(b) is true.
  //
  // Use errc::function_not_supported for functions that are entirely
  // unimplemented, e.g. create_symlink on Windows.
  //
  // Use errc::invalid_argument for requests to perform operations outside
  // the spec, e.g. trying to copy a directory using filesystem::copy_file.
  inline error_code
  __unsupported() noexcept
  {
#if defined __AVR__
    // avr-libc defines ENOTSUP and EOPNOTSUPP but with nonsense values.
    // ENOSYS is defined though, so use an error_code corresponding to that.
    // This contradicts the comment above, but we don't have much choice.
    return std::make_error_code(std::errc::function_not_supported);
#elif defined ENOTSUP
    return std::make_error_code(std::errc::not_supported);
#elif defined EOPNOTSUPP
    // This is supposed to be for socket operations
    return std::make_error_code(std::errc::operation_not_supported);
#else
    return std::make_error_code(std::errc::invalid_argument);
#endif
  }

namespace filesystem
{
namespace __gnu_posix
{
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
// Adapt the Windows _wxxx functions to look like POSIX xxx, but for wchar_t*.
  inline int open(const wchar_t* path, int flags)
  { return ::_wopen(path, flags); }

  inline int open(const wchar_t* path, int flags, int mode)
  { return ::_wopen(path, flags, mode); }

  inline int close(int fd)
  { return ::_close(fd); }

  using stat_type = struct ::__stat64;

  inline int stat(const wchar_t* path, stat_type* buffer)
  { return ::_wstat64(path, buffer); }

  inline int lstat(const wchar_t* path, stat_type* buffer)
  {
    // FIXME: symlinks not currently supported
    return stat(path, buffer);
  }

  using ::mode_t;

  inline int chmod(const wchar_t* path, mode_t mode)
  { return ::_wchmod(path, mode); }
#define _GLIBCXX_USE_CHMOD 1

  inline int mkdir(const wchar_t* path, mode_t)
  { return ::_wmkdir(path); }
#define _GLIBCXX_USE_MKDIR 1

  inline wchar_t* getcwd(wchar_t* buf, size_t size)
  { return ::_wgetcwd(buf, size > (size_t)INT_MAX ? INT_MAX : (int)size); }
#define _GLIBCXX_USE_GETCWD 1

  inline int chdir(const wchar_t* path)
  { return ::_wchdir(path); }
#define _GLIBCXX_USE_CHDIR 1

#if !_GLIBCXX_USE_UTIMENSAT && _GLIBCXX_HAVE_UTIME_H
  using utimbuf = _utimbuf;

  inline int utime(const wchar_t* path, utimbuf* times)
  { return ::_wutime(path, times); }
#endif

  inline int rename(const wchar_t* oldname, const wchar_t* newname)
  {
    if (MoveFileExW(oldname, newname,
		    MOVEFILE_REPLACE_EXISTING | MOVEFILE_COPY_ALLOWED))
      return 0;
    if (GetLastError() == ERROR_ACCESS_DENIED)
      errno = EACCES;
    else
      errno = EIO;
    return -1;
  }

  using off_t = _off64_t;
  inline int truncate(const wchar_t* path, _off64_t length)
  {
    const int fd = ::_wopen(path, _O_BINARY|_O_RDWR);
    if (fd == -1)
      return fd;
    const int ret = ::ftruncate64(fd, length);
    int err;
    ::_get_errno(&err);
    ::_close(fd);
    ::_set_errno(err);
    return ret;
  }
  using char_type = wchar_t;
#elif defined _GLIBCXX_HAVE_UNISTD_H && ! defined __AVR__
  using ::open;
  using ::close;
# ifdef _GLIBCXX_HAVE_SYS_STAT_H
  using stat_type = struct ::stat;
  using ::stat;
#  ifdef _GLIBCXX_USE_LSTAT
  using ::lstat;
#  else
  inline int lstat(const char* path, stat_type* buffer)
  { return stat(path, buffer); }
#  endif
# endif
  using ::mode_t;
# if _GLIBCXX_USE_CHMOD
  using ::chmod;
# endif
# if _GLIBCXX_USE_MKDIR
  using ::mkdir;
# endif
# if _GLIBCXX_USE_GETCWD
  using ::getcwd;
# endif
# if _GLIBCXX_USE_CHDIR
  using ::chdir;
# endif
# if !_GLIBCXX_USE_UTIMENSAT && _GLIBCXX_USE_UTIME
  using ::utimbuf;
  using ::utime;
# endif
  using ::rename;
  using ::off_t;
# ifdef _GLIBCXX_HAVE_TRUNCATE
  using ::truncate;
# else
  inline int truncate(const char* path, off_t length)
  {
    if (length == 0)
      {
	const int fd = ::open(path, O_WRONLY|O_TRUNC);
	if (fd == -1)
	  return fd;
	::close(fd);
	return 0;
      }
    errno = ENOTSUP;
    return -1;
  }
# endif
  using char_type = char;
#else // ! _GLIBCXX_FILESYSTEM_IS_WINDOWS && ! _GLIBCXX_HAVE_UNISTD_H
  inline int open(const char*, int, ...) { errno = ENOSYS; return -1; }
  inline int close(int) { errno = ENOSYS; return -1; }
  using mode_t = int;
  inline int chmod(const char*, mode_t) { errno = ENOSYS; return -1; }
  inline int mkdir(const char*, mode_t) { errno = ENOSYS; return -1; }
  inline char* getcwd(char*, size_t) { errno = ENOSYS; return nullptr; }
  inline int chdir(const char*) { errno = ENOSYS; return -1; }
  inline int rename(const char*, const char*) { errno = ENOSYS; return -1; }
  using off_t = long;
  inline int truncate(const char*, off_t) { errno = ENOSYS; return -1; }
  using char_type = char;
#endif // _GLIBCXX_FILESYSTEM_IS_WINDOWS
} // namespace __gnu_posix

  template<typename Bitmask>
    inline bool is_set(Bitmask obj, Bitmask bits)
    {
      return (obj & bits) != Bitmask::none;
    }

  inline bool
  is_not_found_errno(int err) noexcept
  {
    return err == ENOENT || err == ENOTDIR;
  }

#ifdef _GLIBCXX_HAVE_SYS_STAT_H
  using __gnu_posix::stat_type;

  inline std::chrono::system_clock::time_point
  file_time(const stat_type& st, std::error_code& ec) noexcept
  {
    using namespace std::chrono;
#ifdef _GLIBCXX_USE_ST_MTIM
    time_t s = st.st_mtim.tv_sec;
    nanoseconds ns{st.st_mtim.tv_nsec};
#else
    time_t s = st.st_mtime;
    nanoseconds ns{};
#endif

    // FIXME
    // There are possible timespec values which will overflow
    // chrono::system_clock::time_point but would not overflow
    // __file_clock::time_point, due to its different epoch.
    //
    // By checking for overflow of the intermediate system_clock::duration
    // type, we report an error for values which are actually representable
    // in the file_time_type result type.
    //
    // Howard Hinnant's solution for this problem is to use
    // duration<__int128>{s} + ns, which doesn't overflow.
    // An alternative would be to do the epoch correction on s before
    // the addition, and then go straight to file_time_type instead of
    // going via chrono::system_clock::time_point.
    //
    // (This only applies to the C++17 Filesystem library, because for the
    // Filesystem TS we don't have a distinct __file_clock, we just use the
    // system clock for file timestamps).
    if (seconds{s} >= floor<seconds>(system_clock::duration::max()))
      {
	ec = std::make_error_code(std::errc::value_too_large); // EOVERFLOW
	return system_clock::time_point::min();
      }
    ec.clear();
    return system_clock::time_point{seconds{s} + ns};
  }

  struct copy_options_existing_file
  {
    bool skip, update, overwrite;
  };
#endif // _GLIBCXX_HAVE_SYS_STAT_H

} // namespace filesystem

// BEGIN/END macros must be defined before including this file.
_GLIBCXX_BEGIN_NAMESPACE_FILESYSTEM

#ifdef _GLIBCXX_HAVE_SYS_STAT_H
  using std::filesystem::__gnu_posix::stat_type;
  using std::filesystem::__gnu_posix::char_type;

  bool
  do_copy_file(const char_type* from, const char_type* to,
	       std::filesystem::copy_options_existing_file options,
	       stat_type* from_st, stat_type* to_st,
	       std::error_code& ec) noexcept;

  void
  do_space(const char_type* pathname,
	   uintmax_t& capacity, uintmax_t& free, uintmax_t& available,
	   std::error_code&);


  // Test whether two files are the same file.
  bool
  equiv_files(const char_type*, const stat_type&,
	      const char_type*, const stat_type&,
	      error_code&);

  inline file_type
  make_file_type(const stat_type& st) noexcept
  {
#ifdef _GLIBCXX_HAVE_S_ISREG
    if (S_ISREG(st.st_mode))
      return file_type::regular;
    else if (S_ISDIR(st.st_mode))
      return file_type::directory;
    else if (S_ISCHR(st.st_mode))
      return file_type::character;
    else if (S_ISBLK(st.st_mode))
      return file_type::block;
    else if (S_ISFIFO(st.st_mode))
      return file_type::fifo;
#ifdef S_ISLNK // not present in mingw
    else if (S_ISLNK(st.st_mode))
      return file_type::symlink;
#endif
#ifdef S_ISSOCK // not present until POSIX:2001
    else if (S_ISSOCK(st.st_mode))
      return file_type::socket;
#endif
#endif
    return file_type::unknown;
  }

  inline file_status
  make_file_status(const stat_type& st) noexcept
  {
    return file_status{
	make_file_type(st),
	static_cast<perms>(st.st_mode) & perms::mask
    };
  }

  inline std::filesystem::copy_options_existing_file
  copy_file_options(copy_options opt)
  {
    using std::filesystem::is_set;
    return {
	is_set(opt, copy_options::skip_existing),
	is_set(opt, copy_options::update_existing),
	is_set(opt, copy_options::overwrite_existing)
    };
  }

#ifdef NEED_DO_COPY_FILE
#ifdef _GLIBCXX_USE_COPY_FILE_RANGE
  bool
  copy_file_copy_file_range(int fd_in, int fd_out, size_t length) noexcept
  {
    // a zero-length file is either empty, or not copyable by this syscall
    // return early to avoid the syscall cost
    if (length == 0)
      {
	errno = EINVAL;
	return false;
      }
    size_t bytes_left = length;
    loff_t off_in = 0, off_out = 0;
    ssize_t bytes_copied;
    do
      {
	bytes_copied = ::copy_file_range(fd_in, &off_in, fd_out, &off_out,
					 bytes_left, 0);
	bytes_left -= bytes_copied;
      }
    while (bytes_left > 0 && bytes_copied > 0);
    if (bytes_copied < 0)
      return false;
    return true;
  }
#endif

#if defined _GLIBCXX_USE_SENDFILE && ! defined _GLIBCXX_FILESYSTEM_IS_WINDOWS
  bool
  copy_file_sendfile(int fd_in, int fd_out, size_t length) noexcept
  {
    // a zero-length file is either empty, or not copyable by this syscall
    // return early to avoid the syscall cost
    if (length == 0)
      {
	errno = EINVAL;
	return false;
      }
    size_t bytes_left = length;
    off_t offset = 0;
    ssize_t bytes_copied;
    do
      {
	bytes_copied = ::sendfile(fd_out, fd_in, &offset, bytes_left);
	bytes_left -= bytes_copied;
      }
    while (bytes_left > 0 && bytes_copied > 0);
    if (bytes_copied < 0)
      {
	::lseek(fd_out, 0, SEEK_SET);
	return false;
      }
    return true;
  }
#endif

  bool
  do_copy_file(const char_type* from, const char_type* to,
	       std::filesystem::copy_options_existing_file options,
	       stat_type* from_st, stat_type* to_st,
	       std::error_code& ec) noexcept
  {
    namespace fs = std::filesystem;
    namespace posix = fs::__gnu_posix;

    stat_type st1, st2;
    file_status t, f;

    if (to_st == nullptr)
      {
	if (posix::stat(to, &st1))
	  {
	    const int err = errno;
	    if (!fs::is_not_found_errno(err))
	      {
		ec.assign(err, std::generic_category());
		return false;
	      }
	  }
	else
	  to_st = &st1;
      }
    else if (to_st == from_st)
      to_st = nullptr;

    if (to_st == nullptr)
      t = file_status{file_type::not_found};
    else
      t = make_file_status(*to_st);

    if (from_st == nullptr)
      {
	if (posix::stat(from, &st2))
	  {
	    ec.assign(errno, std::generic_category());
	    return false;
	  }
	else
	  from_st = &st2;
      }
    f = make_file_status(*from_st);
    // _GLIBCXX_RESOLVE_LIB_DEFECTS
    // 2712. copy_file() has a number of unspecified error conditions
    if (!is_regular_file(f))
      {
	ec = std::make_error_code(std::errc::invalid_argument);
	return false;
      }

    if (exists(t))
      {
	if (!is_regular_file(t))
	  {
	    ec = std::make_error_code(std::errc::invalid_argument);
	    return false;
	  }

	if (equiv_files(from, *from_st, to, *to_st, ec))
	  {
	    ec = std::make_error_code(std::errc::file_exists);
	    return false;
	  }

	if (options.skip)
	  {
	    ec.clear();
	    return false;
	  }
	else if (options.update)
	  {
	    const auto from_mtime = fs::file_time(*from_st, ec);
	    if (ec)
	      return false;
	    if ((from_mtime <= fs::file_time(*to_st, ec)) || ec)
	      return false;
	  }
	else if (!options.overwrite)
	  {
	    ec = std::make_error_code(std::errc::file_exists);
	    return false;
	  }
	else if (!is_regular_file(t))
	  {
	    ec = std::make_error_code(std::errc::invalid_argument);
	    return false;
	  }
      }

    struct CloseFD {
      ~CloseFD() { if (fd != -1) posix::close(fd); }
      bool close() { return posix::close(std::__exchange(fd, -1)) == 0; }
      int fd;
    };

    int common_flags = 0;
#ifdef O_CLOEXEC
    common_flags |= O_CLOEXEC;
#endif
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
    common_flags |= O_BINARY;
#endif

    const int iflag = O_RDONLY | common_flags;
    CloseFD in = { posix::open(from, iflag) };
    if (in.fd == -1)
      {
	ec.assign(errno, std::generic_category());
	return false;
      }
    int oflag = O_WRONLY | O_CREAT | common_flags;
    if (options.overwrite || options.update)
      oflag |= O_TRUNC;
    else
      oflag |= O_EXCL;
    CloseFD out = { posix::open(to, oflag, S_IWUSR) };
    if (out.fd == -1)
      {
	if (errno == EEXIST && options.skip)
	  ec.clear();
	else
	  ec.assign(errno, std::generic_category());
	return false;
      }

#if defined _GLIBCXX_USE_FCHMOD && ! defined _GLIBCXX_FILESYSTEM_IS_WINDOWS
    if (::fchmod(out.fd, from_st->st_mode))
#elif defined _GLIBCXX_USE_FCHMODAT && ! defined _GLIBCXX_FILESYSTEM_IS_WINDOWS
    if (::fchmodat(AT_FDCWD, to, from_st->st_mode, 0))
#elif defined _GLIBCXX_USE_CHMOD
    if (posix::chmod(to, from_st->st_mode))
#else
    if (false)
#endif
      {
	ec.assign(errno, std::generic_category());
	return false;
      }

    bool has_copied = false;

#ifdef _GLIBCXX_USE_COPY_FILE_RANGE
    if (!has_copied)
      has_copied = copy_file_copy_file_range(in.fd, out.fd, from_st->st_size);
    if (!has_copied)
      {
	// EINVAL: src and dst are the same file (this is not cheaply
	// detectable from userspace)
	// EINVAL: copy_file_range is unsupported for this file type by the
	// underlying filesystem
	// ENOTSUP: undocumented, can arise with old kernels and NFS
	// EOPNOTSUPP: filesystem does not implement copy_file_range
	// ETXTBSY: src or dst is an active swapfile (nonsensical, but allowed
	// with normal copying)
	// EXDEV: src and dst are on different filesystems that do not support
	// cross-fs copy_file_range
	// ENOENT: undocumented, can arise with CIFS
	// ENOSYS: unsupported by kernel or blocked by seccomp
	if (errno != EINVAL && errno != ENOTSUP && errno != EOPNOTSUPP
	      && errno != ETXTBSY && errno != EXDEV && errno != ENOENT
	      && errno != ENOSYS)
	  {
	    ec.assign(errno, std::generic_category());
	    return false;
	  }
      }
#endif

#if defined _GLIBCXX_USE_SENDFILE && ! defined _GLIBCXX_FILESYSTEM_IS_WINDOWS
    if (!has_copied)
      has_copied = copy_file_sendfile(in.fd, out.fd, from_st->st_size);
    if (!has_copied)
      {
	if (errno != ENOSYS && errno != EINVAL)
	  {
	    ec.assign(errno, std::generic_category());
	    return false;
	  }
      }
#endif

    if (has_copied)
      {
	if (!out.close() || !in.close())
	  {
	    ec.assign(errno, std::generic_category());
	    return false;
	  }
	ec.clear();
	return true;
      }

    using std::ios;
    __gnu_cxx::stdio_filebuf<char> sbin(in.fd, ios::in|ios::binary);
    __gnu_cxx::stdio_filebuf<char> sbout(out.fd, ios::out|ios::binary);

    if (sbin.is_open())
      in.fd = -1;
    if (sbout.is_open())
      out.fd = -1;

    // ostream::operator<<(streambuf*) fails if it extracts no characters,
    // so don't try to use it for empty files. But from_st->st_size == 0 for
    // some special files (e.g. procfs, see PR libstdc++/108178) so just try
    // to read a character to decide whether there is anything to copy or not.
    if (sbin.sgetc() != char_traits<char>::eof())
      if (!(std::ostream(&sbout) << &sbin))
	{
	  ec = std::make_error_code(std::errc::io_error);
	  return false;
	}

    if (!sbout.close() || !sbin.close())
      {
	ec.assign(errno, std::generic_category());
	return false;
      }
    ec.clear();
    return true;
  }
#endif // NEED_DO_COPY_FILE

#ifdef NEED_DO_SPACE
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
  void
  do_space(const char_type* pathname,
	   uintmax_t& capacity, uintmax_t& free, uintmax_t& available,
	   std::error_code& ec)
  {
#ifdef _GLIBCXX_HAVE_SYS_STATVFS_H
    struct ::statvfs f;
    if (::statvfs(pathname, &f))
	ec.assign(errno, std::generic_category());
    else
      {
	if (f.f_frsize != (unsigned long)-1)
	  {
	    const uintmax_t fragment_size = f.f_frsize;
	    const fsblkcnt_t unknown = -1;
	    if (f.f_blocks != unknown)
	      capacity = f.f_blocks * fragment_size;
	    if (f.f_bfree != unknown)
	      free = f.f_bfree * fragment_size;
	    if (f.f_bavail != unknown)
	      available = f.f_bavail * fragment_size;
	  }
	ec.clear();
      }
#elif _GLIBCXX_FILESYSTEM_IS_WINDOWS
    ULARGE_INTEGER bytes_avail = {}, bytes_total = {}, bytes_free = {};
    if (GetDiskFreeSpaceExW(pathname, &bytes_avail, &bytes_total, &bytes_free))
      {
	if (bytes_total.QuadPart != 0)
	  capacity = bytes_total.QuadPart;
	if (bytes_free.QuadPart != 0)
	  free = bytes_free.QuadPart;
	if (bytes_avail.QuadPart != 0)
	  available = bytes_avail.QuadPart;
	ec.clear();
      }
    else
      ec = std::__last_system_error();
#else
    ec = std::make_error_code(std::errc::function_not_supported);
#endif
  }
#pragma GCC diagnostic pop
#endif // NEED_DO_SPACE

#endif // _GLIBCXX_HAVE_SYS_STAT_H

  // Find OS-specific name of temporary directory from the environment,
  // Caller must check that the path is an accessible directory.
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  inline wstring
  get_temp_directory_from_env(error_code& ec)
  {
    unsigned len = 1024;
    std::wstring buf;
    do
      {
	buf.__resize_and_overwrite(len, [&len](wchar_t* p, unsigned n) {
	  len = GetTempPathW(n, p);
	  return len > n ? 0 : len;
	});
      }
    while (len > buf.size());

    if (len == 0)
      ec = __last_system_error();
    else
      ec.clear();

    return buf;
  }
#else
  inline const char*
  get_temp_directory_from_env(error_code& ec) noexcept
  {
    ec.clear();
    for (auto env : { "TMPDIR", "TMP", "TEMP", "TEMPDIR" })
      {
#if _GLIBCXX_HAVE_SECURE_GETENV
	auto tmpdir = ::secure_getenv(env);
#else
	auto tmpdir = ::getenv(env);
#endif
	if (tmpdir)
	  return tmpdir;
      }
    return "/tmp";
  }
#endif

_GLIBCXX_END_NAMESPACE_FILESYSTEM

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // _GLIBCXX_OPS_COMMON_H
