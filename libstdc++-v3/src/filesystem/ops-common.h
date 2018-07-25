// Filesystem operation utilities -*- C++ -*-

// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

  typedef struct ::_stat stat_type;

  inline int stat(const wchar_t* path, stat_type* buffer)
  { return ::_wstat(path, buffer); }

  inline lstat(const wchar_t* path, stat_type* buffer)
  {
    // TODO symlinks not currently supported
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
  { return _wrename(oldname, newname); }

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
#else // _GLIBCXX_FILESYSTEM_IS_WINDOWS
  using ::open;
  using ::close;
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
  typedef struct ::stat stat_type;
  using ::stat;
  using ::lstat;
#endif
  using ::mode_t;
  using ::chmod;
  using ::mkdir;
  using ::getcwd;
  using ::chdir;
#if !_GLIBCXX_USE_UTIMENSAT && _GLIBCXX_HAVE_UTIME_H
  using ::utimbuf;
  using ::utime;
#endif
  using ::rename;
  using ::truncate;
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

  bool
  do_copy_file(const __gnu_posix::char_type* from,
	       const __gnu_posix::char_type* to,
	       copy_options_existing_file options,
	       stat_type* from_st, stat_type* to_st,
	       std::error_code& ec) noexcept;

  void
  do_space(const __gnu_posix::char_type* pathname,
	   uintmax_t& capacity, uintmax_t& free, uintmax_t& available,
	   std::error_code&);

#endif // _GLIBCXX_HAVE_SYS_STAT_H

} // namespace filesystem

// BEGIN/END macros must be defined before including this file.
_GLIBCXX_BEGIN_NAMESPACE_FILESYSTEM

#ifdef _GLIBCXX_HAVE_SYS_STAT_H
  using std::filesystem::__gnu_posix::stat_type;

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
#endif // _GLIBCXX_HAVE_SYS_STAT_H

_GLIBCXX_END_NAMESPACE_FILESYSTEM

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // _GLIBCXX_OPS_COMMON_H
