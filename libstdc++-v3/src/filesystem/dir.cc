// Class filesystem::directory_entry etc. -*- C++ -*-

// Copyright (C) 2014-2015 Free Software Foundation, Inc.
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

#include <experimental/filesystem>
#include <utility>
#include <stack>
#include <string.h>
#include <errno.h>
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

namespace fs = std::experimental::filesystem;

namespace
{
  struct ErrorCode
  {
    ErrorCode(std::error_code* p) : ec(p) { }

    ErrorCode(ErrorCode&& e) : ec(std::exchange(e.ec, nullptr)) { }

    ~ErrorCode() { if (ec) ec->clear(); }

    void assign(int err)
    {
      ec->assign(err, std::generic_category());
      ec = nullptr;
    }

    explicit operator bool() { return ec != nullptr; }

    std::error_code* ec;
  };
}

struct fs::_Dir
{
  _Dir() : dirp(nullptr) { }

  _Dir(DIR* dirp, const fs::path& path) : dirp(dirp), path(path) { }

  _Dir(_Dir&& d)
  : dirp(std::exchange(d.dirp, nullptr)), path(std::move(d.path)),
    entry(std::move(d.entry)), type(d.type)
  { }

  _Dir& operator=(_Dir&&) = delete;

  ~_Dir() { if (dirp) ::closedir(dirp); }

  bool advance(ErrorCode);

  DIR*			dirp;
  fs::path		path;
  directory_entry	entry;
  file_type		type = file_type::none;
};

namespace
{
  template<typename Bitmask>
    inline bool is_set(Bitmask obj, Bitmask bits)
    {
      return (obj & bits) != Bitmask::none;
    }

  fs::_Dir
  opendir(const fs::path& p, fs::directory_options options, ErrorCode ec)
  {
    if (DIR* dirp = ::opendir(p.c_str()))
      return {dirp, p};

    const int err = errno;
    if (err == EACCES
        && is_set(options, fs::directory_options::skip_permission_denied))
      return {};

    if (!ec)
      _GLIBCXX_THROW_OR_ABORT(fs::filesystem_error(
            "directory iterator cannot open directory", p,
            std::error_code(err, std::generic_category())));

    ec.assign(err);
    return {};
  }

  inline std::shared_ptr<fs::_Dir>
  make_shared_dir(fs::_Dir&& dir)
  {
    if (dir.dirp)
      return std::make_shared<fs::_Dir>(std::move(dir));
    return {};
  }

  inline fs::file_type
  get_file_type(const dirent& d __attribute__((__unused__)))
  {
#ifdef _GLIBCXX_HAVE_STRUCT_DIRENT_D_TYPE
    switch (d.d_type)
    {
    case DT_BLK:
      return fs::file_type::block;
    case DT_CHR:
      return fs::file_type::character;
    case DT_DIR:
      return fs::file_type::directory;
    case DT_FIFO:
      return fs::file_type::fifo;
    case DT_LNK:
      return fs::file_type::symlink;
    case DT_REG:
      return fs::file_type::regular;
    case DT_SOCK:
      return fs::file_type::socket;
    case DT_UNKNOWN:
      return fs::file_type::unknown;
    default:
      return fs::file_type::none;
    }
#else
    return fs::file_type::none;
#endif
  }

  int
  native_readdir(DIR* dirp, ::dirent*& entryp)
  {
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
    errno = 0;
    if ((entryp = ::readdir(dirp)))
      return 0;
    return errno;
#else
    return ::readdir_r(dirp, entryp, &entryp);
#endif
  }
}

bool
fs::_Dir::advance(ErrorCode ec)
{
  ::dirent ent;
  ::dirent* result = &ent;
  if (int err = native_readdir(dirp, result))
    {
      if (!ec)
	_GLIBCXX_THROW_OR_ABORT(filesystem_error(
	      "directory iterator cannot advance",
	      std::error_code(err, std::generic_category())));
      ec.assign(err);
      return true;
    }
  else if (result != nullptr)
    {
      // skip past dot and dot-dot
      if (!strcmp(ent.d_name, ".") || !strcmp(ent.d_name, ".."))
	return advance(std::move(ec));
      entry = fs::directory_entry{path / ent.d_name};
      type = get_file_type(ent);
      return true;
    }
  else
    {
      // reached the end
      entry = {};
      type = fs::file_type::none;
      return false;
    }
}

fs::directory_iterator::
directory_iterator(const path& p, directory_options options, error_code* ec)
: directory_iterator(make_shared_dir(opendir(p, options, ec)), ec)
{ }

fs::directory_iterator::
directory_iterator(std::shared_ptr<_Dir> dir, error_code* ec)
: _M_dir(std::move(dir))
{
  if (_M_dir && !_M_dir->advance(ec))
    _M_dir.reset();
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
  if (!_M_dir->advance(nullptr))
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
  if (!_M_dir->advance(&ec))
    _M_dir.reset();
  return *this;
}

using Dir_iter_pair = std::pair<fs::_Dir, fs::directory_iterator>;

struct fs::recursive_directory_iterator::_Dir_stack : std::stack<_Dir>
{
  void clear() { c.clear(); }
};

fs::recursive_directory_iterator::
recursive_directory_iterator(const path& p, directory_options options,
                                 error_code* ec)
: _M_options(options), _M_pending(true)
{
  if (DIR* dirp = ::opendir(p.c_str()))
    {
      _M_dirs = std::make_shared<_Dir_stack>();
      _M_dirs->push(_Dir{ dirp, p });
      if (!_M_dirs->top().advance(ec))
	_M_dirs.reset();
    }
  else
    {
      const int err = errno;
      if (err == EACCES
	  && is_set(options, fs::directory_options::skip_permission_denied))
	return;

      if (!ec)
	_GLIBCXX_THROW_OR_ABORT(filesystem_error(
	      "recursive directory iterator cannot open directory", p,
	      std::error_code(err, std::generic_category())));

      ec->assign(err, std::generic_category());
    }
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

namespace
{
  bool
  recurse(const fs::_Dir& d, fs::directory_options options, std::error_code& ec)
  {
    bool follow_symlink
      = is_set(options, fs::directory_options::follow_directory_symlink);
#ifdef _GLIBCXX_HAVE_STRUCT_DIRENT_D_TYPE
    if (d.type == fs::file_type::directory)
      return true;
    if (d.type == fs::file_type::symlink && follow_symlink)
      return d.entry.status().type() == fs::file_type::directory;
    if (d.type != fs::file_type::none && d.type != fs::file_type::unknown)
      return false;
#endif
    const fs::path& path = d.entry.path();
    auto type = fs::symlink_status(path, ec).type();
    if (ec.value())
      return false;
    if (type == fs::file_type::symlink)
      {
	if (!follow_symlink)
	  return false;
	type = fs::status(path, ec).type();
      }
    return type == fs::file_type::directory;
  }
}

fs::recursive_directory_iterator&
fs::recursive_directory_iterator::increment(error_code& ec) noexcept
{
  if (!_M_dirs)
    {
      ec = std::make_error_code(errc::invalid_argument);
      return *this;
    }

  auto& top = _M_dirs->top();

  if (std::exchange(_M_pending, true) && recurse(top, _M_options, ec))
    {
      _Dir dir = opendir(top.entry.path(), _M_options, &ec);
      if (ec.value())
	return *this;
      if (dir.dirp)
	{
	  _M_dirs->push(std::move(dir));
	  if (!_M_dirs->top().advance(&ec)) // dir is empty
	    pop();
	  return *this;
	}
      // else skip permission denied and continue in parent dir
    }

  ec.clear();
  while (!_M_dirs->top().advance(&ec) && !ec.value())
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
fs::recursive_directory_iterator::pop()
{
  if (!_M_dirs)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error(
	  "cannot pop non-dereferenceable recursive directory iterator",
	  std::make_error_code(errc::invalid_argument)));

  do {
    _M_dirs->pop();
    if (_M_dirs->empty())
      {
	_M_dirs.reset();
	return;
      }
  } while (!_M_dirs->top().advance(nullptr));
}
