// Filesystem directory iterator utilities -*- C++ -*-

// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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
#ifdef _GLIBCXX_HAVE_DIRENT_H
# ifdef _GLIBCXX_HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif
# include <dirent.h>
#else
# error "the <dirent.h> header is needed to build the Filesystem TS"
#endif

#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
# undef opendir
# define opendir _wopendir
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace filesystem
{

struct _Dir_base
{
  _Dir_base(DIR* dirp = nullptr) : dirp(dirp) { }

  // If no error occurs then dirp is non-null,
  // otherwise null (whether error ignored or not).
  _Dir_base(const char* p, bool skip_permission_denied,
	    error_code& ec) noexcept
  : dirp(::opendir(p))
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

  ~_Dir_base() { if (dirp) ::closedir(dirp); }

  const struct ::dirent*
  advance(bool skip_permission_denied, error_code& ec) noexcept
  {
    ec.clear();

    int err = std::exchange(errno, 0);
    const struct ::dirent* entp = readdir(dirp);
    // std::swap cannot be used with Bionic's errno
    err = std::exchange(errno, err);

    if (entp)
      {
	// skip past dot and dot-dot
	if (!strcmp(entp->d_name, ".") || !strcmp(entp->d_name, ".."))
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

  DIR*	dirp;
};

} // namespace filesystem

// BEGIN/END macros must be defined before including this file.
_GLIBCXX_BEGIN_NAMESPACE_FILESYSTEM
inline file_type
get_file_type(const ::dirent& d __attribute__((__unused__)))
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
