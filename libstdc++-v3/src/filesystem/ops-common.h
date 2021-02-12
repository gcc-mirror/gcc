// Filesystem operation utilities -*- C++ -*-

// Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

#ifdef _GLIBCXX_HAVE_UNISTD_H
# include <unistd.h>
# ifdef _GLIBCXX_HAVE_FCNTL_H
#  include <fcntl.h>  // AT_FDCWD, O_TRUNC etc.
# endif
# if defined(_GLIBCXX_HAVE_SYS_STAT_H) && defined(_GLIBCXX_HAVE_SYS_TYPES_H)
#  include <sys/types.h>
#  include <sys/stat.h>
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
# ifdef _GLIBCXX_USE_SENDFILE
#  include <sys/sendfile.h> // sendfile
# endif
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
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

  typedef struct ::__stat64 stat_type;

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

  inline int mkdir(const wchar_t* path, mode_t)
  { return ::_wmkdir(path); }

  inline wchar_t* getcwd(wchar_t* buf, size_t size)
  { return ::_wgetcwd(buf, size > (size_t)INT_MAX ? INT_MAX : (int)size); }

  inline int chdir(const wchar_t* path)
  { return ::_wchdir(path); }

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
#elif defined _GLIBCXX_HAVE_UNISTD_H
  using ::open;
  using ::close;
# ifdef _GLIBCXX_HAVE_SYS_STAT_H
  typedef struct ::stat stat_type;
  using ::stat;
#  ifdef _GLIBCXX_USE_LSTAT
  using ::lstat;
#  else
  inline int lstat(const char* path, stat_type* buffer)
  { return stat(path, buffer); }
#  endif
# endif
  using ::mode_t;
  using ::chmod;
  using ::mkdir;
  using ::getcwd;
  using ::chdir;
# if !_GLIBCXX_USE_UTIMENSAT && _GLIBCXX_USE_UTIME
  using ::utimbuf;
  using ::utime;
# endif
  using ::rename;
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
  inline int open(const char*, int, ...) { errno = ENOTSUP; return -1; }
  inline int close(int) { errno = ENOTSUP; return -1; }
  using mode_t = int;
  inline int chmod(const char*, mode_t) { errno = ENOTSUP; return -1; }
  inline int mkdir(const char*, mode_t) { errno = ENOTSUP; return -1; }
  inline char* getcwd(char*, size_t) { errno = ENOTSUP; return nullptr; }
  inline int chdir(const char*) { errno = ENOTSUP; return -1; }
  inline int rename(const char*, const char*) { errno = ENOTSUP; return -1; }
  inline int truncate(const char*, long) { errno = ENOTSUP; return -1; }
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
    if (s >= (nanoseconds::max().count() / 1e9))
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
	ec = std::make_error_code(std::errc::not_supported);
	return false;
      }

    if (exists(t))
      {
	if (!is_regular_file(t))
	  {
	    ec = std::make_error_code(std::errc::not_supported);
	    return false;
	  }

	if (to_st->st_dev == from_st->st_dev
	    && to_st->st_ino == from_st->st_ino)
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
	    ec = std::make_error_code(std::errc::not_supported);
	    return false;
	  }
      }

    struct CloseFD {
      ~CloseFD() { if (fd != -1) posix::close(fd); }
      bool close() { return posix::close(std::exchange(fd, -1)) == 0; }
      int fd;
    };

    int iflag = O_RDONLY;
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
    iflag |= O_BINARY;
#endif

    CloseFD in = { posix::open(from, iflag) };
    if (in.fd == -1)
      {
	ec.assign(errno, std::generic_category());
	return false;
      }
    int oflag = O_WRONLY|O_CREAT;
    if (options.overwrite || options.update)
      oflag |= O_TRUNC;
    else
      oflag |= O_EXCL;
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
    oflag |= O_BINARY;
#endif
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
#else
    if (posix::chmod(to, from_st->st_mode))
#endif
      {
	ec.assign(errno, std::generic_category());
	return false;
      }

    size_t count = from_st->st_size;
#if defined _GLIBCXX_USE_SENDFILE && ! defined _GLIBCXX_FILESYSTEM_IS_WINDOWS
    off_t offset = 0;
    ssize_t n = ::sendfile(out.fd, in.fd, &offset, count);
    if (n < 0 && errno != ENOSYS && errno != EINVAL)
      {
	ec.assign(errno, std::generic_category());
	return false;
      }
    if ((size_t)n == count)
      {
	if (!out.close() || !in.close())
	  {
	    ec.assign(errno, std::generic_category());
	    return false;
	  }
	ec.clear();
	return true;
      }
    else if (n > 0)
      count -= n;
#endif // _GLIBCXX_USE_SENDFILE

    using std::ios;
    __gnu_cxx::stdio_filebuf<char> sbin(in.fd, ios::in|ios::binary);
    __gnu_cxx::stdio_filebuf<char> sbout(out.fd, ios::out|ios::binary);

    if (sbin.is_open())
      in.fd = -1;
    if (sbout.is_open())
      out.fd = -1;

#ifdef _GLIBCXX_USE_SENDFILE
    if (n != 0)
      {
	if (n < 0)
	  n = 0;

	const auto p1 = sbin.pubseekoff(n, ios::beg, ios::in);
	const auto p2 = sbout.pubseekoff(n, ios::beg, ios::out);

	const std::streampos errpos(std::streamoff(-1));
	if (p1 == errpos || p2 == errpos)
	  {
	    ec = std::make_error_code(std::errc::io_error);
	    return false;
	  }
      }
#endif

    if (count && !(std::ostream(&sbout) << &sbin))
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
      ec.assign((int)GetLastError(), std::system_category());
#else
    ec = std::make_error_code(std::errc::not_supported);
#endif
  }
#pragma GCC diagnostic pop
#endif // NEED_DO_SPACE

#endif // _GLIBCXX_HAVE_SYS_STAT_H

_GLIBCXX_END_NAMESPACE_FILESYSTEM

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // _GLIBCXX_OPS_COMMON_H
