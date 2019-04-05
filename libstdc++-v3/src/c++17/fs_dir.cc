// Class filesystem::directory_entry etc. -*- C++ -*-

// Copyright (C) 2014-2019 Free Software Foundation, Inc.
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

#include <filesystem>
#include <utility>
#include <stack>
#include <string.h>
#include <errno.h>
#define _GLIBCXX_BEGIN_NAMESPACE_FILESYSTEM namespace filesystem {
#define _GLIBCXX_END_NAMESPACE_FILESYSTEM }
#include "../filesystem/dir-common.h"

namespace fs = std::filesystem;
namespace posix = std::filesystem::__gnu_posix;

template class std::__shared_ptr<fs::_Dir>;
template class std::__shared_ptr<fs::recursive_directory_iterator::_Dir_stack>;

struct fs::_Dir : _Dir_base
{
  _Dir(const fs::path& p, bool skip_permission_denied, error_code& ec)
  : _Dir_base(p.c_str(), skip_permission_denied, ec)
  {
    if (!ec)
      path = p;
  }

  _Dir(posix::DIR* dirp, const path& p) : _Dir_base(dirp), path(p) { }

  _Dir(_Dir&&) = default;

  // Returns false when the end of the directory entries is reached.
  // Reports errors by setting ec.
  bool advance(bool skip_permission_denied, error_code& ec) noexcept
  {
    if (const auto entp = _Dir_base::advance(skip_permission_denied, ec))
      {
	auto name = path;
	name /= entp->d_name;
	file_type type = file_type::none;
#ifdef _GLIBCXX_HAVE_STRUCT_DIRENT_D_TYPE
	// Even if the OS supports dirent::d_type the filesystem might not:
	if (entp->d_type != DT_UNKNOWN)
	  type = get_file_type(*entp);
#endif
	entry = fs::directory_entry{std::move(name), type};
	return true;
      }
    else if (!ec)
      {
	// reached the end
	entry = {};
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
    file_type type = entry._M_type;
    if (type == file_type::none)
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

  fs::path		path;
  directory_entry	entry;
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
  const bool skip_permission_denied
    = is_set(options, directory_options::skip_permission_denied);

  error_code ec;
  _Dir dir(p, skip_permission_denied, ec);

  if (dir.dirp)
    {
      auto sp = std::__make_shared<fs::_Dir>(std::move(dir));
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
fs::directory_iterator::increment(error_code& ec)
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
  _Dir_stack(directory_options opts, posix::DIR* dirp, const path& p)
  : options(opts), pending(true)
  {
    this->emplace(dirp, p);
  }

  const directory_options options;
  bool pending;

  void clear() { c.clear(); }
};

fs::recursive_directory_iterator::
recursive_directory_iterator(const path& p, directory_options options,
                             error_code* ecptr)
{
  if (posix::DIR* dirp = posix::opendir(p.c_str()))
    {
      if (ecptr)
	ecptr->clear();
      auto sp = std::__make_shared<_Dir_stack>(options, dirp, p);
      if (ecptr ? sp->top().advance(*ecptr) : sp->top().advance())
	_M_dirs.swap(sp);
    }
  else
    {
      const int err = errno;
      if (err == EACCES
	  && is_set(options, fs::directory_options::skip_permission_denied))
	{
	  if (ecptr)
	    ecptr->clear();
	  return;
	}

      if (!ecptr)
	_GLIBCXX_THROW_OR_ABORT(filesystem_error(
	      "recursive directory iterator cannot open directory", p,
	      std::error_code(err, std::generic_category())));

      ecptr->assign(err, std::generic_category());
    }
}

fs::recursive_directory_iterator::~recursive_directory_iterator() = default;

fs::directory_options
fs::recursive_directory_iterator::options() const noexcept
{
  return _M_dirs->options;
}

int
fs::recursive_directory_iterator::depth() const noexcept
{
  return int(_M_dirs->size()) - 1;
}

bool
fs::recursive_directory_iterator::recursion_pending() const noexcept
{
  return _M_dirs->pending;
}

const fs::directory_entry&
fs::recursive_directory_iterator::operator*() const noexcept
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
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error(
	  "cannot increment recursive directory iterator", ec));
  return *this;
}

fs::recursive_directory_iterator&
fs::recursive_directory_iterator::increment(error_code& ec)
{
  if (!_M_dirs)
    {
      ec = std::make_error_code(errc::invalid_argument);
      return *this;
    }

  const bool follow
    = is_set(_M_dirs->options, directory_options::follow_directory_symlink);
  const bool skip_permission_denied
    = is_set(_M_dirs->options, directory_options::skip_permission_denied);

  auto& top = _M_dirs->top();

  if (std::exchange(_M_dirs->pending, true) && top.should_recurse(follow, ec))
    {
      _Dir dir(top.entry.path(), skip_permission_denied, ec);
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
    = is_set(_M_dirs->options, directory_options::skip_permission_denied);

  do {
    _M_dirs->pop();
    if (_M_dirs->empty())
      {
	_M_dirs.reset();
	ec.clear();
	return;
      }
  } while (!_M_dirs->top().advance(skip_permission_denied, ec));
}

void
fs::recursive_directory_iterator::pop()
{
  error_code ec;
  pop(ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error(_M_dirs
	  ? "recursive directory iterator cannot pop"
	  : "non-dereferenceable recursive directory iterator cannot pop",
	  ec));
}

void
fs::recursive_directory_iterator::disable_recursion_pending() noexcept
{
  _M_dirs->pending = false;
}
