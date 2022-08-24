// Class filesystem::directory_entry etc. -*- C++ -*-

// Copyright (C) 2014-2022 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_USE_CXX11_ABI
# define _GLIBCXX_USE_CXX11_ABI 1
#endif
#ifndef _GNU_SOURCE
// Cygwin needs this for secure_getenv
# define _GNU_SOURCE 1
#endif

#include <bits/largefile-config.h>
#include <experimental/filesystem>

#ifndef _GLIBCXX_HAVE_DIRENT_H
# error "the <dirent.h> header is needed to build the Filesystem TS"
#endif

#include <utility>
#include <stack>
#include <string.h>
#include <errno.h>
#define _GLIBCXX_BEGIN_NAMESPACE_FILESYSTEM \
  namespace experimental { namespace filesystem {
#define _GLIBCXX_END_NAMESPACE_FILESYSTEM } }
#include "dir-common.h"

namespace fs = std::experimental::filesystem;
namespace posix = std::filesystem::__gnu_posix;

struct fs::_Dir : std::filesystem::_Dir_base
{
  _Dir(const fs::path& p, bool skip_permission_denied, bool nofollow,
       error_code& ec)
  : _Dir_base(p.c_str(), skip_permission_denied, nofollow, ec)
  {
    if (!ec)
      path = p;
  }

  _Dir(_Dir_base&& d, const path& p) : _Dir_base(std::move(d)), path(p) { }

  _Dir(_Dir&&) = default;

  // Returns false when the end of the directory entries is reached.
  // Reports errors by setting ec.
  bool advance(bool skip_permission_denied, error_code& ec) noexcept
  {
    if (const auto entp = _Dir_base::advance(skip_permission_denied, ec))
      {
	entry = fs::directory_entry{path / entp->d_name};
	type = get_file_type(*entp);
	return true;
      }
    else if (!ec)
      {
	// reached the end
	entry = {};
	type = file_type::none;
      }
    return false;
  }

  bool advance(error_code& ec) noexcept { return advance(false, ec); }

  // Returns false when the end of the directory entries is reached.
  // Reports errors by throwing.
  bool advance(bool skip_permission_denied = false)
  {
    error_code ec;
    const bool ok = advance(skip_permission_denied, ec);
    if (ec)
      _GLIBCXX_THROW_OR_ABORT(filesystem_error(
	      "directory iterator cannot advance", ec));
    return ok;
  }

  bool should_recurse(bool follow_symlink, error_code& ec) const
  {
    file_type type = this->type;
    if (type == file_type::none || type == file_type::unknown)
    {
      type = entry.symlink_status(ec).type();
      if (ec)
	return false;
    }

    if (type == file_type::directory)
      return true;
    if (type == file_type::symlink)
      return follow_symlink && is_directory(entry.status(ec));
    return false;
  }

  // Return a pathname for the current directory entry, as an _At_path.
  _Dir_base::_At_path
  current() const noexcept
  {
    const fs::path& p = entry.path();
#if _GLIBCXX_HAVE_DIRFD
    auto len = std::prev(p.end())->native().size();
    return {::dirfd(this->dirp), p.c_str(), p.native().size() - len};
#else
    return p.c_str();
#endif
  }

  // Create a new _Dir for the directory this->entry.path().
  _Dir
  open_subdir(bool skip_permission_denied, bool nofollow,
	      error_code& ec) noexcept
  {
    _Dir_base d(current(), skip_permission_denied, nofollow, ec);
    return _Dir(std::move(d), entry.path());
  }

  fs::path		path;
  directory_entry	entry;
  file_type		type = file_type::none;
};

namespace
{
  template<typename Bitmask>
    inline bool
    is_set(Bitmask obj, Bitmask bits)
    {
      return (obj & bits) != Bitmask::none;
    }
}

fs::directory_iterator::
directory_iterator(const path& p, directory_options options, error_code* ecptr)
{
  // Do not report an error for permission denied errors.
  const bool skip_permission_denied
    = is_set(options, directory_options::skip_permission_denied);

  error_code ec;
  _Dir dir(p, skip_permission_denied, /*nofollow*/false, ec);

  if (dir.dirp)
    {
      auto sp = std::make_shared<fs::_Dir>(std::move(dir));
      if (sp->advance(skip_permission_denied, ec))
	_M_dir.swap(sp);
    }
  if (ecptr)
    *ecptr = ec;
  else if (ec)
    _GLIBCXX_THROW_OR_ABORT(fs::filesystem_error(
	  "directory iterator cannot open directory", p, ec));
}

const fs::directory_entry&
fs::directory_iterator::operator*() const
{
  if (!_M_dir)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error(
	  "non-dereferenceable directory iterator",
	  std::make_error_code(errc::invalid_argument)));
  return _M_dir->entry;
}

fs::directory_iterator&
fs::directory_iterator::operator++()
{
  if (!_M_dir)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error(
	  "cannot advance non-dereferenceable directory iterator",
	  std::make_error_code(errc::invalid_argument)));
  if (!_M_dir->advance())
    _M_dir.reset();
  return *this;
}

fs::directory_iterator&
fs::directory_iterator::increment(error_code& ec) noexcept
{
  if (!_M_dir)
    {
      ec = std::make_error_code(errc::invalid_argument);
      return *this;
    }
  if (!_M_dir->advance(ec))
    _M_dir.reset();
  return *this;
}

struct fs::recursive_directory_iterator::_Dir_stack : std::stack<_Dir>
{
  _Dir_stack(_Dir&& dir)
  {
    this->push(std::move(dir));
  }

  void clear() { c.clear(); }
};

fs::recursive_directory_iterator::
recursive_directory_iterator(const path& p, directory_options options,
                             error_code* ecptr)
: _M_options(options), _M_pending(true)
{
  // Do not report an error for permission denied errors.
  const bool skip_permission_denied
    = is_set(options, directory_options::skip_permission_denied);

  error_code ec;
  _Dir dir(p, skip_permission_denied, /*nofollow*/false, ec);

  if (dir.dirp)
    {
      auto sp = std::__make_shared<_Dir_stack>(std::move(dir));
      if (ecptr ? sp->top().advance(skip_permission_denied, *ecptr)
		: sp->top().advance(skip_permission_denied))
	{
	  _M_dirs.swap(sp);
	}
    }
  else if (ecptr)
    *ecptr = ec;
  else if (ec)
    _GLIBCXX_THROW_OR_ABORT(fs::filesystem_error(
	  "recursive directory iterator cannot open directory", p, ec));
}

fs::recursive_directory_iterator::~recursive_directory_iterator() = default;

int
fs::recursive_directory_iterator::depth() const
{
  return int(_M_dirs->size()) - 1;
}

const fs::directory_entry&
fs::recursive_directory_iterator::operator*() const
{
  return _M_dirs->top().entry;
}

fs::recursive_directory_iterator&
fs::recursive_directory_iterator::
operator=(const recursive_directory_iterator& other) noexcept = default;

fs::recursive_directory_iterator&
fs::recursive_directory_iterator::
operator=(recursive_directory_iterator&& other) noexcept = default;

fs::recursive_directory_iterator&
fs::recursive_directory_iterator::operator++()
{
  error_code ec;
  increment(ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error(
	  "cannot increment recursive directory iterator", ec));
  return *this;
}

fs::recursive_directory_iterator&
fs::recursive_directory_iterator::increment(error_code& ec) noexcept
{
  if (!_M_dirs)
    {
      ec = std::make_error_code(errc::invalid_argument);
      return *this;
    }

  const bool follow
    = is_set(_M_options, directory_options::follow_directory_symlink);
  const bool skip_permission_denied
    = is_set(_M_options, directory_options::skip_permission_denied);

  auto& top = _M_dirs->top();

  if (std::exchange(_M_pending, true) && top.should_recurse(follow, ec))
    {
      _Dir dir = top.open_subdir(skip_permission_denied, !follow, ec);
      if (ec)
	{
	  _M_dirs.reset();
	  return *this;
	}
      if (dir.dirp)
	  _M_dirs->push(std::move(dir));
    }

  while (!_M_dirs->top().advance(skip_permission_denied, ec) && !ec)
    {
      _M_dirs->pop();
      if (_M_dirs->empty())
	{
	  _M_dirs.reset();
	  return *this;
	}
    }

  if (ec)
    _M_dirs.reset();

  return *this;
}

void
fs::recursive_directory_iterator::pop(error_code& ec)
{
  if (!_M_dirs)
    {
      ec = std::make_error_code(errc::invalid_argument);
      return;
    }

  const bool skip_permission_denied
    = is_set(_M_options, directory_options::skip_permission_denied);

  do {
    _M_dirs->pop();
    if (_M_dirs->empty())
      {
	_M_dirs.reset();
	ec.clear();
	return;
      }
  } while (!_M_dirs->top().advance(skip_permission_denied, ec) && !ec);

  if (ec)
    _M_dirs.reset();
}

void
fs::recursive_directory_iterator::pop()
{
  [[maybe_unused]] const bool dereferenceable = _M_dirs != nullptr;
  error_code ec;
  pop(ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error(dereferenceable
	  ? "recursive directory iterator cannot pop"
	  : "non-dereferenceable recursive directory iterator cannot pop",
	  ec));
}
