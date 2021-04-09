// Filesystem directory iterator utilities -*- C++ -*-

// Copyright (C) 2014-2021 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_DIR_COMMON_H
#define _GLIBCXX_DIR_COMMON_H 1

#include <string.h>  // strcmp
#include <errno.h>
#if _GLIBCXX_FILESYSTEM_IS_WINDOWS
#include <wchar.h>  // wcscmp
#endif
#ifdef _GLIBCXX_HAVE_DIRENT_H
# ifdef _GLIBCXX_HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif
# include <dirent.h>
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace filesystem
{
namespace __gnu_posix
{
#if _GLIBCXX_FILESYSTEM_IS_WINDOWS
// Adapt the Windows _wxxx functions to look like POSIX xxx, but for wchar_t*.
using char_type = wchar_t;
using DIR = ::_WDIR;
using dirent = _wdirent;
inline DIR* opendir(const wchar_t* path) { return ::_wopendir(path); }
inline dirent* readdir(DIR* dir) { return ::_wreaddir(dir); }
inline int closedir(DIR* dir) { return ::_wclosedir(dir); }
#elif defined _GLIBCXX_HAVE_DIRENT_H
using char_type = char;
using DIR = ::DIR;
typedef struct ::dirent dirent;
using ::opendir;
using ::readdir;
using ::closedir;
#else
using char_type = char;
struct dirent { const char* d_name; };
struct DIR { };
inline DIR* opendir(const char*) { return nullptr; }
inline dirent* readdir(DIR*) { return nullptr; }
inline int closedir(DIR*) { return -1; }
#endif
} // namespace __gnu_posix

namespace posix = __gnu_posix;

struct _Dir_base
{
  _Dir_base(posix::DIR* dirp = nullptr) : dirp(dirp) { }

  // If no error occurs then dirp is non-null,
  // otherwise null (whether error ignored or not).
  _Dir_base(const posix::char_type* pathname, bool skip_permission_denied,
	    error_code& ec) noexcept
  : dirp(posix::opendir(pathname))
  {
    if (dirp)
      ec.clear();
    else
    {
      const int err = errno;
      if (err == EACCES && skip_permission_denied)
	ec.clear();
      else
	ec.assign(err, std::generic_category());
    }
  }

  _Dir_base(_Dir_base&& d) : dirp(std::exchange(d.dirp, nullptr)) { }

  _Dir_base& operator=(_Dir_base&&) = delete;

  ~_Dir_base() { if (dirp) posix::closedir(dirp); }

  const posix::dirent*
  advance(bool skip_permission_denied, error_code& ec) noexcept
  {
    ec.clear();

    int err = std::exchange(errno, 0);
    const posix::dirent* entp = posix::readdir(dirp);
    // std::swap cannot be used with Bionic's errno
    err = std::exchange(errno, err);

    if (entp)
      {
	// skip past dot and dot-dot
	if (is_dot_or_dotdot(entp->d_name))
	  return advance(skip_permission_denied, ec);
	return entp;
      }
    else if (err)
      {
	if (err == EACCES && skip_permission_denied)
	  return nullptr;
	ec.assign(err, std::generic_category());
	return nullptr;
      }
    else
      {
	// reached the end
	return nullptr;
      }
  }

  static bool is_dot_or_dotdot(const char* s) noexcept
  { return !strcmp(s, ".") || !strcmp(s, ".."); }

#if _GLIBCXX_FILESYSTEM_IS_WINDOWS
  static bool is_dot_or_dotdot(const wchar_t* s) noexcept
  { return !wcscmp(s, L".") || !wcscmp(s, L".."); }
#endif

  posix::DIR*	dirp;
};

inline bool
is_permission_denied_error(int e)
{
  if (e == EACCES)
    return true;
#ifdef __APPLE__
  if (e == EPERM) // See PR 99533
    return true;
#endif
  return false;
}

} // namespace filesystem

// BEGIN/END macros must be defined before including this file.
_GLIBCXX_BEGIN_NAMESPACE_FILESYSTEM

inline file_type
get_file_type(const std::filesystem::__gnu_posix::dirent& d [[gnu::unused]])
{
#ifdef _GLIBCXX_HAVE_STRUCT_DIRENT_D_TYPE
  switch (d.d_type)
  {
  case DT_BLK:
    return file_type::block;
  case DT_CHR:
    return file_type::character;
  case DT_DIR:
    return file_type::directory;
  case DT_FIFO:
    return file_type::fifo;
  case DT_LNK:
    return file_type::symlink;
  case DT_REG:
    return file_type::regular;
  case DT_SOCK:
    return file_type::socket;
  case DT_UNKNOWN:
    return file_type::unknown;
  default:
    return file_type::none;
  }
#else
  return file_type::none;
#endif
}
_GLIBCXX_END_NAMESPACE_FILESYSTEM

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // _GLIBCXX_DIR_COMMON_H
