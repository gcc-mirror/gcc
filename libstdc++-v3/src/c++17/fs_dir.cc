// Class filesystem::directory_entry etc. -*- C++ -*-

// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

#include <bits/largefile-config.h>
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
  _Dir(const fs::path& p, bool skip_permission_denied, bool nofollow,
       [[maybe_unused]] bool filename_only, error_code& ec)
  : _Dir_base(p.c_str(), skip_permission_denied, nofollow, ec)
  {
#if _GLIBCXX_HAVE_DIRFD && _GLIBCXX_HAVE_OPENAT && _GLIBCXX_HAVE_UNLINKAT
    if (filename_only)
      return; // Do not store path p when we aren't going to use it.
#endif

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

  // Return a pathname for the current directory entry, as an _At_path.
  _Dir_base::_At_path
  current() const noexcept
  {
    const fs::path& p = entry.path();
#if _GLIBCXX_HAVE_DIRFD
    if (!p.empty()) [[__likely__]]
      {
	auto len = std::prev(p.end())->native().size();
	return {::dirfd(this->dirp), p.c_str(), p.native().size() - len};
      }
#endif
    return p.c_str();
  }

  // Create a new _Dir for the directory this->entry.path().
  _Dir
  open_subdir(bool skip_permission_denied, bool nofollow,
	      error_code& ec) const noexcept
  {
    _Dir_base d(current(), skip_permission_denied, nofollow, ec);
    // If this->path is empty, the new _Dir should have an empty path too.
    const fs::path& p = this->path.empty() ? this->path : this->entry.path();
    return _Dir(std::move(d), p);
  }

  bool
  do_unlink(bool is_directory, error_code& ec) const noexcept
  {
#if _GLIBCXX_HAVE_UNLINKAT
    const auto atp = current();
    if (::unlinkat(atp.dir(), atp.path_at_dir(),
		   is_directory ? AT_REMOVEDIR : 0) == -1)
      {
	ec.assign(errno, std::generic_category());
	return false;
      }
    else
      {
	ec.clear();
	return true;
      }
#else
    return fs::remove(entry.path(), ec);
#endif
  }

  // Remove the non-directory that this->entry refers to.
  bool
  unlink(error_code& ec) const noexcept
  { return do_unlink(/* is_directory*/ false, ec); }

  // Remove the directory that this->entry refers to.
  bool
  rmdir(error_code& ec) const noexcept
  { return do_unlink(/* is_directory*/ true, ec); }

  fs::path		path; // Empty if only using unlinkat with file descr.
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

// Non-standard directory option flags, currently only for internal use:
//
// Do not allow directory iterator to open a symlink.
// This might seem redundant given directory_options::follow_directory_symlink
// but that is only checked for recursing into sub-directories, and we need
// something that controls the initial opendir() call in the constructor.
constexpr fs::directory_options __directory_iterator_nofollow{64};
// Do not store full paths in std::filesystem::recursive_directory_iterator.
// When fs::remove_all uses recursive_directory_iterator::__erase and unlinkat
// is available in libc, we do not need the parent directory's path, only the
// filenames of the directory entries (and a file descriptor for the parent).
// This flag avoids allocating memory for full paths that won't be needed.
constexpr fs::directory_options __directory_iterator_filename_only{128};
}

fs::directory_iterator::
directory_iterator(const path& p, directory_options options, error_code* ecptr)
{
  // Do not report an error for permission denied errors.
  const bool skip_permission_denied
    = is_set(options, directory_options::skip_permission_denied);
  // Do not allow opening a symlink.
  const bool nofollow = is_set(options, __directory_iterator_nofollow);

  error_code ec;
  _Dir dir(p, skip_permission_denied, nofollow, /*filename only*/false, ec);

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
fs::directory_iterator::operator*() const noexcept
{
  return (*_M_dir).entry;
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
  _Dir_stack(directory_options opts, _Dir&& dir)
  : options(opts), pending(true)
  {
    this->push(std::move(dir));
  }

  path::string_type orig;
  const directory_options options;
  bool pending;

  void clear() { c.clear(); }

  path current_path() const
  {
    path p;
    if (top().path.empty())
      {
	// Reconstruct path that failed from dir stack.
	p = orig;
	for (auto& d : this->c)
	  p /= d.entry.path();
      }
    else
      p = top().entry.path();
    return p;
  }
};

fs::recursive_directory_iterator::
recursive_directory_iterator(const path& p, directory_options options,
                             error_code* ecptr)
{
  // Do not report an error for permission denied errors.
  const bool skip_permission_denied
    = is_set(options, directory_options::skip_permission_denied);
  // Do not allow opening a symlink as the starting directory.
  const bool nofollow = is_set(options, __directory_iterator_nofollow);
  // Prefer to store only filenames (not full paths) in directory_entry values.
  const bool filename_only
     = is_set(options, __directory_iterator_filename_only);

  error_code ec;
  _Dir dir(p, skip_permission_denied, nofollow, filename_only, ec);

  if (dir.dirp)
    {
      auto sp = std::__make_shared<_Dir_stack>(options, std::move(dir));
      if (ecptr ? sp->top().advance(skip_permission_denied, *ecptr)
		: sp->top().advance(skip_permission_denied))
	{
	  _M_dirs.swap(sp);
	  if (filename_only) // Need to save original path for error reporting.
	    _M_dirs->orig = p.native();
	}
    }
  else if (ecptr)
    *ecptr = ec;
  else if (ec)
    _GLIBCXX_THROW_OR_ABORT(fs::filesystem_error(
	  "recursive directory iterator cannot open directory", p, ec));
}

fs::recursive_directory_iterator::~recursive_directory_iterator() = default;

fs::directory_options
fs::recursive_directory_iterator::options() const noexcept
{
  return (*_M_dirs).options;
}

int
fs::recursive_directory_iterator::depth() const noexcept
{
  return int((*_M_dirs).size()) - 1;
}

bool
fs::recursive_directory_iterator::recursion_pending() const noexcept
{
  return (*_M_dirs).pending;
}

const fs::directory_entry&
fs::recursive_directory_iterator::operator*() const noexcept
{
  return (*_M_dirs).top().entry;
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
    = is_set(_M_dirs->options, directory_options::skip_permission_denied);

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

void
fs::recursive_directory_iterator::disable_recursion_pending() noexcept
{
  (*_M_dirs).pending = false;
}

// Used to implement filesystem::remove_all.
fs::recursive_directory_iterator&
fs::recursive_directory_iterator::__erase(error_code* ecptr)
{
  error_code ec;
  if (!_M_dirs)
    {
      ec = std::make_error_code(errc::invalid_argument);
      return *this;
    }

  // We never want to skip permission denied when removing files.
  const bool skip_permission_denied = false;
  // We never want to follow directory symlinks when removing files.
  const bool nofollow = true;

  // Loop until we find something we can remove.
  while (!ec)
    {
      auto& top = _M_dirs->top();

#if _GLIBCXX_FILESYSTEM_IS_WINDOWS
      // _Dir::unlink uses fs::remove which uses std::system_category() for
      // Windows errror codes, so we can't just check for EPERM and EISDIR.
      // Use directory_entry::refresh() here to check if we have a directory.
      // This can be a TOCTTOU race, but we don't have openat or unlinkat to
      // solve that on Windows, and generally don't support symlinks anyway.
      if (top.entry._M_type == file_type::none)
	top.entry.refresh();
#endif

      if (top.entry._M_type == file_type::directory)
	{
	  _Dir dir = top.open_subdir(skip_permission_denied, nofollow, ec);
	  if (!ec)
	    {
	      __glibcxx_assert(dir.dirp != nullptr);
	      if (dir.advance(skip_permission_denied, ec))
		{
		  // Non-empty directory, recurse into it.
		  _M_dirs->push(std::move(dir));
		  continue;
		}
	      if (!ec)
		{
		  // Directory is empty so we can remove it.
		  if (top.rmdir(ec))
		    break; // Success
		}
	    }
	}
      else if (top.unlink(ec))
	break; // Success
#if ! _GLIBCXX_FILESYSTEM_IS_WINDOWS
      else if (top.entry._M_type == file_type::none)
	{
	  // We did not have a cached type, so it's possible that top.entry
	  // is actually a directory, and that's why the unlink above failed.
#ifdef EPERM
	  // POSIX.1-2017 says unlink on a directory returns EPERM,
	  // but LSB allows EISDIR too. Some targets don't even define EPERM.
	  if (ec.value() == EPERM || ec.value() == EISDIR)
#else
	  if (ec.value() == EISDIR)
#endif
	    {
	      // Retry, treating it as a directory.
	      top.entry._M_type = file_type::directory;
	      ec.clear();
	      continue;
	    }
	}
#endif
    }

  if (!ec)
    {
      // We successfully removed the current entry, so advance to the next one.
      if (_M_dirs->top().advance(skip_permission_denied, ec))
	return *this;
      else if (!ec)
	{
	  // Reached the end of the current directory.
	  _M_dirs->pop();
	  if (_M_dirs->empty())
	    _M_dirs.reset();
	  return *this;
	}
    }

  // Reset _M_dirs to empty.
  auto dirs = std::move(_M_dirs);

  // Need to report an error
  if (ecptr)
    *ecptr = ec;
  else
    _GLIBCXX_THROW_OR_ABORT(fs::filesystem_error("cannot remove all",
						 dirs->orig,
						 dirs->current_path(),
						 ec));

  return *this;
}
