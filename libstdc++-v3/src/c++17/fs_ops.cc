// Filesystem operations -*- C++ -*-

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
# define NEED_DO_COPY_FILE
# define NEED_DO_SPACE
#endif
#ifndef _GNU_SOURCE
// Cygwin needs this for secure_getenv
# define _GNU_SOURCE 1
#endif

#include <bits/largefile-config.h>
#include <filesystem>
#include <functional>
#include <ostream>
#include <stack>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <limits.h>  // PATH_MAX
#ifdef _GLIBCXX_HAVE_FCNTL_H
# include <fcntl.h>  // AT_FDCWD, AT_SYMLINK_NOFOLLOW
#endif
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
#  include <sys/stat.h>   // stat, utimensat, fchmodat
#endif
#ifdef _GLIBCXX_HAVE_SYS_STATVFS_H
# include <sys/statvfs.h> // statvfs
#endif
#if !_GLIBCXX_USE_UTIMENSAT && _GLIBCXX_HAVE_UTIME_H
# include <utime.h> // utime
#endif
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
# include <windows.h>
#endif

#define _GLIBCXX_BEGIN_NAMESPACE_FILESYSTEM namespace filesystem {
#define _GLIBCXX_END_NAMESPACE_FILESYSTEM }
#include "../filesystem/ops-common.h"

#pragma GCC diagnostic ignored "-Wunused-parameter"

namespace fs = std::filesystem;
namespace posix = std::filesystem::__gnu_posix;

fs::path
fs::absolute(const path& p)
{
  error_code ec;
  path ret = absolute(p, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot make absolute path", p,
					     ec));
  return ret;
}

fs::path
fs::absolute(const path& p, error_code& ec)
{
  path ret;
  if (p.empty())
    {
      ec = make_error_code(std::errc::invalid_argument);
      return ret;
    }
  ec.clear();
  if (p.is_absolute())
    {
      ret = p;
      return ret;
    }

#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  // s must remain null-terminated
  wstring_view s = p.native();

  if (p.has_root_directory()) // implies !p.has_root_name()
    {
      // GetFullPathNameW("//") gives unwanted result (PR 88884).
      // If there are multiple directory separators at the start,
      // skip all but the last of them.
      const auto pos = s.find_first_not_of(L"/\\");
      __glibcxx_assert(pos != 0);
      s.remove_prefix(std::min(s.length(), pos) - 1);
    }

  uint32_t len = 1024;
  wstring buf;
  do
    {
      buf.resize(len);
      len = GetFullPathNameW(s.data(), len, buf.data(), nullptr);
    }
  while (len > buf.size());

  if (len == 0)
    ec = __last_system_error();
  else
    {
      buf.resize(len);
      ret = std::move(buf);
    }
#else
  ret = current_path(ec);
  ret /= p;
#endif
  return ret;
}

namespace
{
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  inline bool is_dot(wchar_t c) { return c == L'.'; }
#else
  inline bool is_dot(char c) { return c == '.'; }
#endif

  inline bool is_dot(const fs::path& path)
  {
    const auto& filename = path.native();
    return filename.size() == 1 && is_dot(filename[0]);
  }

  inline bool is_dotdot(const fs::path& path)
  {
    const auto& filename = path.native();
    return filename.size() == 2 && is_dot(filename[0]) && is_dot(filename[1]);
  }

  struct free_as_in_malloc
  {
    void operator()(void* p) const { ::free(p); }
  };

  using char_ptr = std::unique_ptr<fs::path::value_type[], free_as_in_malloc>;
}

fs::path
fs::canonical(const path& p, error_code& ec)
{
  path result;
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  const path pa = absolute(p.lexically_normal(), ec);
#else
  const path pa = absolute(p, ec);
#endif
  if (ec)
    return result;

#ifdef _GLIBCXX_USE_REALPATH
  char_ptr buf{ nullptr };
# if _XOPEN_VERSION < 700
  // Not safe to call realpath(path, NULL)
  using char_type = fs::path::value_type;
  buf.reset( (char_type*)::malloc(PATH_MAX * sizeof(char_type)) );
# endif
  if (char* rp = ::realpath(pa.c_str(), buf.get()))
    {
      if (buf == nullptr)
	buf.reset(rp);
      result.assign(rp);
      ec.clear();
      return result;
    }
  if (errno != ENAMETOOLONG)
    {
      ec.assign(errno, std::generic_category());
      return result;
    }
#endif

  if (!exists(pa, ec))
    {
      if (!ec)
	ec = make_error_code(std::errc::no_such_file_or_directory);
      return result;
    }
  // else: we know there are (currently) no unresolvable symlink loops

  result = pa.root_path();

  deque<path> cmpts;
  for (auto& f : pa.relative_path())
    cmpts.push_back(f);

  int max_allowed_symlinks = 40;

  while (!cmpts.empty() && !ec)
    {
      path f = std::move(cmpts.front());
      cmpts.pop_front();

      if (f.empty())
	{
	  // ignore empty element
	}
      else if (is_dot(f))
	{
	  if (!is_directory(result, ec) && !ec)
	    ec.assign(ENOTDIR, std::generic_category());
	}
      else if (is_dotdot(f))
	{
	  auto parent = result.parent_path();
	  if (parent.empty())
	    result = pa.root_path();
	  else
	    result.swap(parent);
	}
      else
	{
	  result /= f;

	  if (is_symlink(result, ec))
	    {
	      path link = read_symlink(result, ec);
	      if (!ec)
		{
		  if (--max_allowed_symlinks == 0)
		    ec.assign(ELOOP, std::generic_category());
		  else
		    {
		      if (link.is_absolute())
			{
			  result = link.root_path();
			  link = link.relative_path();
			}
		      else
			result = result.parent_path();

		      cmpts.insert(cmpts.begin(), link.begin(), link.end());
		    }
		}
	    }
	}
    }

  if (ec || !exists(result, ec))
    result.clear();

  return result;
}

fs::path
fs::canonical(const path& p)
{
  error_code ec;
  path res = canonical(p, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot make canonical path",
					     p, ec));
  return res;
}

void
fs::copy(const path& from, const path& to, copy_options options)
{
  error_code ec;
  copy(from, to, options, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot copy", from, to, ec));
}

namespace std::filesystem
{
  // Need this as there's no 'perm_options::none' enumerator.
  static inline bool is_set(fs::perm_options obj, fs::perm_options bits)
  {
    return (obj & bits) != fs::perm_options{};
  }
}

namespace
{
  struct internal_file_clock : fs::__file_clock
  {
    using __file_clock::_S_to_sys;
    using __file_clock::_S_from_sys;

#ifdef _GLIBCXX_HAVE_SYS_STAT_H
    static fs::file_time_type
    from_stat(const fs::stat_type& st, std::error_code& ec) noexcept
    {
      const auto sys_time = fs::file_time(st, ec);
      if (sys_time == sys_time.min())
	return fs::file_time_type::min();
      return _S_from_sys(sys_time);
    }
#endif
  };
}

void
fs::copy(const path& from, const path& to, copy_options options,
	 error_code& ec)
{
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
  const bool skip_symlinks = is_set(options, copy_options::skip_symlinks);
  const bool create_symlinks = is_set(options, copy_options::create_symlinks);
  const bool copy_symlinks = is_set(options, copy_options::copy_symlinks);
  const bool use_lstat = create_symlinks || skip_symlinks;

  file_status f, t;
  stat_type from_st, to_st;
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 2681. filesystem::copy() cannot copy symlinks
  if (use_lstat || copy_symlinks
      ? posix::lstat(from.c_str(), &from_st)
      : posix::stat(from.c_str(), &from_st))
    {
      ec.assign(errno, std::generic_category());
      return;
    }
  if (use_lstat
      ? posix::lstat(to.c_str(), &to_st)
      : posix::stat(to.c_str(), &to_st))
    {
      if (!is_not_found_errno(errno))
	{
	  ec.assign(errno, std::generic_category());
	  return;
	}
      t = file_status{file_type::not_found};
    }
  else
    t = make_file_status(to_st);
  f = make_file_status(from_st);

  if (exists(t) && !is_other(t) && !is_other(f)
      && to_st.st_dev == from_st.st_dev && to_st.st_ino == from_st.st_ino)
    {
      ec = std::make_error_code(std::errc::file_exists);
      return;
    }
  if (is_other(f) || is_other(t))
    {
      ec = std::make_error_code(std::errc::invalid_argument);
      return;
    }
  if (is_directory(f) && is_regular_file(t))
    {
      ec = std::make_error_code(std::errc::is_a_directory);
      return;
    }

  if (is_symlink(f))
    {
      if (skip_symlinks)
	ec.clear();
      else if (!exists(t) && copy_symlinks)
	copy_symlink(from, to, ec);
      else
	// Not clear what should be done here.
	// "Otherwise report an error as specified in Error reporting (7)."
	ec = std::make_error_code(std::errc::invalid_argument);
    }
  else if (is_regular_file(f))
    {
      if (is_set(options, copy_options::directories_only))
	ec.clear();
      else if (create_symlinks)
	create_symlink(from, to, ec);
      else if (is_set(options, copy_options::create_hard_links))
	create_hard_link(from, to, ec);
      else if (is_directory(t))
	do_copy_file(from.c_str(), (to / from.filename()).c_str(),
		     copy_file_options(options), &from_st, nullptr, ec);
      else
	{
	  auto ptr = exists(t) ? &to_st : &from_st;
	  do_copy_file(from.c_str(), to.c_str(), copy_file_options(options),
		       &from_st, ptr, ec);
	}
    }
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 2682. filesystem::copy() won't create a symlink to a directory
  else if (is_directory(f) && create_symlinks)
    ec = std::make_error_code(errc::is_a_directory);
  else if (is_directory(f) && (is_set(options, copy_options::recursive)
			       || options == copy_options::none))
    {
      if (!exists(t))
	if (!create_directory(to, from, ec))
	  return;
      // set an unused bit in options to disable further recursion
      if (!is_set(options, copy_options::recursive))
	options |= static_cast<copy_options>(4096);
      for (const directory_entry& x : directory_iterator(from, ec))
	{
	  copy(x.path(), to/x.path().filename(), options, ec);
	  if (ec)
	    return;
	}
    }
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 2683. filesystem::copy() says "no effects"
  else
    ec.clear();
#else
  ec = std::make_error_code(std::errc::function_not_supported);
#endif
}

bool
fs::copy_file(const path& from, const path& to, copy_options option)
{
  error_code ec;
  bool result = copy_file(from, to, option, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot copy file", from, to,
					     ec));
  return result;
}

bool
fs::copy_file(const path& from, const path& to, copy_options options,
	      error_code& ec)
{
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
  return do_copy_file(from.c_str(), to.c_str(), copy_file_options(options),
		      nullptr, nullptr, ec);
#else
  ec = std::make_error_code(std::errc::function_not_supported);
  return false;
#endif
}


void
fs::copy_symlink(const path& existing_symlink, const path& new_symlink)
{
  error_code ec;
  copy_symlink(existing_symlink, new_symlink, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot copy symlink",
	  existing_symlink, new_symlink, ec));
}

void
fs::copy_symlink(const path& existing_symlink, const path& new_symlink,
		 error_code& ec) noexcept
{
  auto p = read_symlink(existing_symlink, ec);
  if (ec)
    return;
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  if (is_directory(p))
    {
      create_directory_symlink(p, new_symlink, ec);
      return;
    }
#endif
  create_symlink(p, new_symlink, ec);
}


bool
fs::create_directories(const path& p)
{
  error_code ec;
  bool result = create_directories(p, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot create directories", p,
					     ec));
  return result;
}

bool
fs::create_directories(const path& p, error_code& ec)
{
  if (p.empty())
    {
      ec = std::make_error_code(errc::invalid_argument);
      return false;
    }

  file_status st = status(p, ec);
  if (is_directory(st))
    return false;
  else if (ec && !status_known(st))
    return false;
  else if (exists(st))
    {
      if (!ec)
	ec = std::make_error_code(std::errc::not_a_directory);
      return false;
    }

  __glibcxx_assert(st.type() == file_type::not_found);
  // !exists(p) so there must be at least one non-existent component in p.

  std::stack<path> missing;
  path pp = p;

  // Strip any trailing slash
  if (pp.has_relative_path() && !pp.has_filename())
    pp = pp.parent_path();

  do
    {
      const auto& filename = pp.filename();
      if (is_dot(filename) || is_dotdot(filename))
	pp = pp.parent_path();
      else
	{
	  missing.push(std::move(pp));
	  if (missing.size() > 1000) // sanity check
	    {
	      ec = std::make_error_code(std::errc::filename_too_long);
	      return false;
	    }
	  pp = missing.top().parent_path();
	}

      if (pp.empty())
	break;

      st = status(pp, ec);
      if (exists(st))
	{
	  if (ec)
	    return false;
	  if (!is_directory(st))
	    {
	      ec = std::make_error_code(std::errc::not_a_directory);
	      return false;
	    }
	}

      if (ec && exists(st))
	return false;
    }
  while (st.type() == file_type::not_found);

  __glibcxx_assert(!missing.empty());

  bool created;
  do
    {
      const path& top = missing.top();
      created = create_directory(top, ec);
      if (ec)
	return false;
      missing.pop();
    }
  while (!missing.empty());

  return created;
}

namespace
{
  bool
  create_dir(const fs::path& p, fs::perms perm, std::error_code& ec)
  {
    bool created = false;
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
    posix::mode_t mode = static_cast<std::underlying_type_t<fs::perms>>(perm);
    if (posix::mkdir(p.c_str(), mode))
      {
	const int err = errno;
	if (err != EEXIST || !is_directory(p, ec))
	  ec.assign(err, std::generic_category());
      }
    else
      {
	ec.clear();
	created = true;
      }
#else
    ec = std::make_error_code(std::errc::function_not_supported);
#endif
    return created;
  }
} // namespace

bool
fs::create_directory(const path& p)
{
  error_code ec;
  bool result = create_directory(p, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot create directory", p,
	  ec));
  return result;
}

bool
fs::create_directory(const path& p, error_code& ec) noexcept
{
  return create_dir(p, perms::all, ec);
}


bool
fs::create_directory(const path& p, const path& attributes)
{
  error_code ec;
  bool result = create_directory(p, attributes, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot create directory", p,
	  ec));
  return result;
}

bool
fs::create_directory(const path& p, const path& attributes,
		     error_code& ec) noexcept
{
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
  stat_type st;
  if (posix::stat(attributes.c_str(), &st))
    {
      ec.assign(errno, std::generic_category());
      return false;
    }
  return create_dir(p, static_cast<perms>(st.st_mode), ec);
#else
  ec = std::make_error_code(std::errc::function_not_supported);
  return false;
#endif
}


void
fs::create_directory_symlink(const path& to, const path& new_symlink)
{
  error_code ec;
  create_directory_symlink(to, new_symlink, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot create directory symlink",
	  to, new_symlink, ec));
}

void
fs::create_directory_symlink(const path& to, const path& new_symlink,
			     error_code& ec) noexcept
{
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  ec = std::make_error_code(std::errc::function_not_supported);
#else
  create_symlink(to, new_symlink, ec);
#endif
}


void
fs::create_hard_link(const path& to, const path& new_hard_link)
{
  error_code ec;
  create_hard_link(to, new_hard_link, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot create hard link",
					     to, new_hard_link, ec));
}

void
fs::create_hard_link(const path& to, const path& new_hard_link,
		     error_code& ec) noexcept
{
#ifdef _GLIBCXX_HAVE_LINK
  if (::link(to.c_str(), new_hard_link.c_str()))
    ec.assign(errno, std::generic_category());
  else
    ec.clear();
#elif defined _GLIBCXX_FILESYSTEM_IS_WINDOWS
  if (CreateHardLinkW(new_hard_link.c_str(), to.c_str(), NULL))
    ec.clear();
  else
    ec = __last_system_error();
#else
  ec = std::make_error_code(std::errc::function_not_supported);
#endif
}

void
fs::create_symlink(const path& to, const path& new_symlink)
{
  error_code ec;
  create_symlink(to, new_symlink, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot create symlink",
	  to, new_symlink, ec));
}

void
fs::create_symlink(const path& to, const path& new_symlink,
		   error_code& ec) noexcept
{
#ifdef _GLIBCXX_HAVE_SYMLINK
  if (::symlink(to.c_str(), new_symlink.c_str()))
    ec.assign(errno, std::generic_category());
  else
    ec.clear();
#else
  ec = std::make_error_code(std::errc::function_not_supported);
#endif
}


fs::path
fs::current_path()
{
  error_code ec;
  path p = current_path(ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot get current path", ec));
  return p;
}

fs::path
fs::current_path(error_code& ec)
{
  path p;
#ifdef _GLIBCXX_HAVE_UNISTD_H
#if defined __GLIBC__ || defined _GLIBCXX_FILESYSTEM_IS_WINDOWS
  if (char_ptr cwd = char_ptr{posix::getcwd(nullptr, 0)})
    {
      p.assign(cwd.get());
      ec.clear();
    }
  else
    ec.assign(errno, std::generic_category());
#else
#ifdef _PC_PATH_MAX
  long path_max = pathconf(".", _PC_PATH_MAX);
  size_t size;
  if (path_max == -1)
      size = 1024;
  else if (path_max > 10240)
      size = 10240;
  else
      size = path_max;
#elif defined(PATH_MAX)
  size_t size = PATH_MAX;
#else
  size_t size = 1024;
#endif
  for (char_ptr buf; p.empty(); size *= 2)
    {
      using char_type = fs::path::value_type;
      buf.reset((char_type*)malloc(size * sizeof(char_type)));
      if (buf)
	{
	  if (getcwd(buf.get(), size))
	    {
	      p.assign(buf.get());
	      ec.clear();
	    }
	  else if (errno != ERANGE)
	    {
	      ec.assign(errno, std::generic_category());
	      return {};
	    }
	}
      else
	{
	  ec = std::make_error_code(std::errc::not_enough_memory);
	  return {};
	}
    }
#endif  // __GLIBC__
#else   // _GLIBCXX_HAVE_UNISTD_H
  ec = std::make_error_code(std::errc::function_not_supported);
#endif
  return p;
}

void
fs::current_path(const path& p)
{
  error_code ec;
  current_path(p, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot set current path", ec));
}

void
fs::current_path(const path& p, error_code& ec) noexcept
{
#ifdef _GLIBCXX_HAVE_UNISTD_H
  if (posix::chdir(p.c_str()))
    ec.assign(errno, std::generic_category());
  else
    ec.clear();
#else
  ec = std::make_error_code(std::errc::function_not_supported);
#endif
}

bool
fs::equivalent(const path& p1, const path& p2)
{
  error_code ec;
  auto result = equivalent(p1, p2, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot check file equivalence",
	  p1, p2, ec));
  return result;
}

bool
fs::equivalent(const path& p1, const path& p2, error_code& ec) noexcept
{
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
  int err = 0;
  file_status s1, s2;
  stat_type st1, st2;
  if (posix::stat(p1.c_str(), &st1) == 0)
    s1 = make_file_status(st1);
  else if (is_not_found_errno(errno))
    s1.type(file_type::not_found);
  else
    err = errno;

  if (posix::stat(p2.c_str(), &st2) == 0)
    s2 = make_file_status(st2);
  else if (is_not_found_errno(errno))
    s2.type(file_type::not_found);
  else
    err = errno;

  if (exists(s1) && exists(s2))
    {
      if (is_other(s1) && is_other(s2))
	{
	  ec = std::__unsupported();
	  return false;
	}
      ec.clear();
      if (is_other(s1) || is_other(s2))
	return false;
#if _GLIBCXX_FILESYSTEM_IS_WINDOWS
      // st_ino is not set, so can't be used to distinguish files
      if (st1.st_mode != st2.st_mode || st1.st_dev != st2.st_dev)
	return false;

      struct auto_handle {
	explicit auto_handle(const path& p_)
	: handle(CreateFileW(p_.c_str(), 0,
	      FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
	      0, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0))
	{ }

	~auto_handle()
	{ if (*this) CloseHandle(handle); }

	explicit operator bool() const
	{ return handle != INVALID_HANDLE_VALUE; }

	bool get_info()
	{ return GetFileInformationByHandle(handle, &info); }

	HANDLE handle;
	BY_HANDLE_FILE_INFORMATION info;
      };
      auto_handle h1(p1);
      auto_handle h2(p2);
      if (!h1 || !h2)
	{
	  if (!h1 && !h2)
	    ec = __last_system_error();
	  return false;
	}
      if (!h1.get_info() || !h2.get_info())
	{
	  ec = __last_system_error();
	  return false;
	}
      return h1.info.dwVolumeSerialNumber == h2.info.dwVolumeSerialNumber
	&& h1.info.nFileIndexHigh == h2.info.nFileIndexHigh
	&& h1.info.nFileIndexLow == h2.info.nFileIndexLow;
#else
      return st1.st_dev == st2.st_dev && st1.st_ino == st2.st_ino;
#endif
    }
  else if (!exists(s1) && !exists(s2))
    ec = std::make_error_code(std::errc::no_such_file_or_directory);
  else if (err)
    ec.assign(err, std::generic_category());
  else
    ec.clear();
  return false;
#else
  ec = std::make_error_code(std::errc::function_not_supported);
#endif
  return false;
}

std::uintmax_t
fs::file_size(const path& p)
{
  error_code ec;
  auto sz = file_size(p, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot get file size", p, ec));
  return sz;
}

namespace
{
  template<typename Accessor, typename T>
    inline T
    do_stat(const fs::path& p, std::error_code& ec, Accessor f, T deflt)
    {
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
      posix::stat_type st;
      if (posix::stat(p.c_str(), &st))
	{
	  ec.assign(errno, std::generic_category());
	  return deflt;
	}
      ec.clear();
      return f(st);
#else
      ec = std::make_error_code(std::errc::function_not_supported);
      return deflt;
#endif
    }
}

std::uintmax_t
fs::file_size(const path& p, error_code& ec) noexcept
{
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
  struct S
  {
    S(const stat_type& st) : type(make_file_type(st)), size(st.st_size) { }
    S() : type(file_type::not_found) { }
    file_type type;
    uintmax_t size;
  };
  auto s = do_stat(p, ec, [](const auto& st) { return S{st}; }, S{});
  if (s.type == file_type::regular)
    return s.size;
  if (!ec)
    {
      if (s.type == file_type::directory)
	ec = std::make_error_code(std::errc::is_a_directory);
      else
	ec = std::__unsupported();
    }
#else
  ec = std::make_error_code(std::errc::function_not_supported);
#endif
  return -1;
}

std::uintmax_t
fs::hard_link_count(const path& p)
{
  error_code ec;
  auto count = hard_link_count(p, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot get link count", p, ec));
  return count;
}

std::uintmax_t
fs::hard_link_count(const path& p, error_code& ec) noexcept
{
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
  return do_stat(p, ec, std::mem_fn(&stat_type::st_nlink),
		 static_cast<uintmax_t>(-1));
#else
  ec = std::make_error_code(std::errc::function_not_supported);
  return static_cast<uintmax_t>(-1);
#endif
}

bool
fs::is_empty(const path& p)
{
  error_code ec;
  bool e = is_empty(p, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot check if file is empty",
					     p, ec));
  return e;
}

bool
fs::is_empty(const path& p, error_code& ec)
{
  auto s = status(p, ec);
  if (ec)
    return false;
  bool empty = fs::is_directory(s)
    ? fs::directory_iterator(p, ec) == fs::directory_iterator()
    : fs::file_size(p, ec) == 0;
  return ec ? false : empty;
}

fs::file_time_type
fs::last_write_time(const path& p)
{
  error_code ec;
  auto t = last_write_time(p, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot get file time", p, ec));
  return t;
}

fs::file_time_type
fs::last_write_time(const path& p, error_code& ec) noexcept
{
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
  return do_stat(p, ec,
		 [&ec](const auto& st) {
		     return internal_file_clock::from_stat(st, ec);
		 },
		 file_time_type::min());
#else
  ec = std::make_error_code(std::errc::function_not_supported);
  return file_time_type::min();
#endif
}

void
fs::last_write_time(const path& p, file_time_type new_time)
{
  error_code ec;
  last_write_time(p, new_time, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot set file time", p, ec));
}

void
fs::last_write_time(const path& p,
		    file_time_type new_time, error_code& ec) noexcept
{
  auto d = internal_file_clock::_S_to_sys(new_time).time_since_epoch();
  auto s = chrono::duration_cast<chrono::seconds>(d);
#if _GLIBCXX_USE_UTIMENSAT
  auto ns = chrono::duration_cast<chrono::nanoseconds>(d - s);
  if (ns < ns.zero()) // tv_nsec must be non-negative and less than 10e9.
    {
      --s;
      ns += chrono::seconds(1);
    }
  struct ::timespec ts[2];
  ts[0].tv_sec = 0;
  ts[0].tv_nsec = UTIME_OMIT;
  ts[1].tv_sec = static_cast<std::time_t>(s.count());
  ts[1].tv_nsec = static_cast<long>(ns.count());
  if (::utimensat(AT_FDCWD, p.c_str(), ts, 0))
    ec.assign(errno, std::generic_category());
  else
    ec.clear();
#elif _GLIBCXX_USE_UTIME && _GLIBCXX_HAVE_SYS_STAT_H
  posix::utimbuf times;
  times.modtime = s.count();
  times.actime = do_stat(p, ec, [](const auto& st) { return st.st_atime; },
			 times.modtime);
  if (posix::utime(p.c_str(), &times))
    ec.assign(errno, std::generic_category());
  else
    ec.clear();
#else
  ec = std::make_error_code(std::errc::function_not_supported);
#endif
}

void
fs::permissions(const path& p, perms prms, perm_options opts)
{
  error_code ec;
  permissions(p, prms, opts, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot set permissions", p, ec));
}

void
fs::permissions(const path& p, perms prms, perm_options opts,
		error_code& ec) noexcept
{
  const bool replace = is_set(opts, perm_options::replace);
  const bool add = is_set(opts, perm_options::add);
  const bool remove = is_set(opts, perm_options::remove);
  const bool nofollow = is_set(opts, perm_options::nofollow);
  if (((int)replace + (int)add + (int)remove) != 1)
    {
      ec = std::make_error_code(std::errc::invalid_argument);
      return;
    }

  prms &= perms::mask;

  file_status st;
  if (add || remove || nofollow)
    {
      st = nofollow ? symlink_status(p, ec) : status(p, ec);
      if (ec)
	return;
      auto curr = st.permissions();
      if (add)
	prms |= curr;
      else if (remove)
	prms = curr & ~prms;
    }

  int err = 0;
#if _GLIBCXX_USE_FCHMODAT
  const int flag = (nofollow && is_symlink(st)) ? AT_SYMLINK_NOFOLLOW : 0;
  if (::fchmodat(AT_FDCWD, p.c_str(), static_cast<mode_t>(prms), flag))
    err = errno;
#else
  if (nofollow && is_symlink(st))
    ec = std::__unsupported();
  else if (posix::chmod(p.c_str(), static_cast<posix::mode_t>(prms)))
    err = errno;
#endif

  if (err)
    ec.assign(err, std::generic_category());
  else
    ec.clear();
}

fs::path
fs::proximate(const path& p, const path& base)
{
  return weakly_canonical(p).lexically_proximate(weakly_canonical(base));
}

fs::path
fs::proximate(const path& p, const path& base, error_code& ec)
{
  path result;
  const auto p2 = weakly_canonical(p, ec);
  if (!ec)
    {
      const auto base2 = weakly_canonical(base, ec);
      if (!ec)
	result = p2.lexically_proximate(base2);
    }
  return result;
}

fs::path
fs::read_symlink(const path& p)
{
  error_code ec;
  path tgt = read_symlink(p, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("read_symlink", p, ec));
  return tgt;
}

fs::path fs::read_symlink(const path& p, error_code& ec)
{
  path result;
#if defined(_GLIBCXX_HAVE_READLINK) && defined(_GLIBCXX_HAVE_SYS_STAT_H)
  stat_type st;
  if (posix::lstat(p.c_str(), &st))
    {
      ec.assign(errno, std::generic_category());
      return result;
    }
  else if (!fs::is_symlink(make_file_status(st)))
    {
      ec.assign(EINVAL, std::generic_category());
      return result;
    }

  std::string buf(st.st_size ? st.st_size + 1 : 128, '\0');
  do
    {
      ssize_t len = ::readlink(p.c_str(), buf.data(), buf.size());
      if (len == -1)
	{
	  ec.assign(errno, std::generic_category());
	  return result;
	}
      else if (len == (ssize_t)buf.size())
	{
	  if (buf.size() > 4096)
	    {
	      ec.assign(ENAMETOOLONG, std::generic_category());
	      return result;
	    }
	  buf.resize(buf.size() * 2);
	}
      else
	{
	  buf.resize(len);
	  result.assign(buf);
	  ec.clear();
	  break;
	}
    }
  while (true);
#else
  ec = std::make_error_code(std::errc::function_not_supported);
#endif
  return result;
}

fs::path
fs::relative(const path& p, const path& base)
{
  return weakly_canonical(p).lexically_relative(weakly_canonical(base));
}

fs::path
fs::relative(const path& p, const path& base, error_code& ec)
{
  auto result = weakly_canonical(p, ec);
  fs::path cbase;
  if (!ec)
    cbase = weakly_canonical(base, ec);
  if (!ec)
    result = result.lexically_relative(cbase);
  if (ec)
    result.clear();
  return result;
}

bool
fs::remove(const path& p)
{
  error_code ec;
  const bool result = fs::remove(p, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot remove", p, ec));
  return result;
}

bool
fs::remove(const path& p, error_code& ec) noexcept
{
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  auto st = symlink_status(p, ec);
  if (exists(st))
    {
      if ((is_directory(p, ec) && RemoveDirectoryW(p.c_str()))
	  || DeleteFileW(p.c_str()))
	{
	  ec.clear();
	  return true;
	}
      else if (!ec)
	ec = __last_system_error();
    }
  else if (status_known(st))
    ec.clear();
#else
  if (::remove(p.c_str()) == 0)
    {
      ec.clear();
      return true;
    }
  else if (errno == ENOENT)
    ec.clear();
  else
    ec.assign(errno, std::generic_category());
#endif
  return false;
}

std::uintmax_t
fs::remove_all(const path& p)
{
  error_code ec;
  uintmax_t count = 0;
  recursive_directory_iterator dir(p, directory_options{64|128}, ec);
  switch (ec.value()) // N.B. assumes ec.category() == std::generic_category()
  {
  case 0:
    // Iterate over the directory removing everything.
    {
      const recursive_directory_iterator end;
      while (dir != end)
	{
	  dir.__erase(); // throws on error
	  ++count;
	}
    }
    // Directory is empty now, will remove it below.
    break;
  case ENOENT:
    // Our work here is done.
    return 0;
  case ENOTDIR:
  case ELOOP:
    // Not a directory, will remove below.
    break;
  default:
    // An error occurred.
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot remove all", p, ec));
  }

  // Remove p itself, which is either a non-directory or is now empty.
  return count + fs::remove(p);
}

std::uintmax_t
fs::remove_all(const path& p, error_code& ec)
{
  uintmax_t count = 0;
  recursive_directory_iterator dir(p, directory_options{64|128}, ec);
  switch (ec.value()) // N.B. assumes ec.category() == std::generic_category()
  {
  case 0:
    // Iterate over the directory removing everything.
    {
      const recursive_directory_iterator end;
      while (dir != end)
	{
	  dir.__erase(&ec);
	  if (ec)
	    return -1;
	  ++count;
	}
    }
    // Directory is empty now, will remove it below.
    break;
  case ENOENT:
    // Our work here is done.
    ec.clear();
    return 0;
  case ENOTDIR:
  case ELOOP:
    // Not a directory, will remove below.
    break;
  default:
    // An error occurred.
    return -1;
  }

  // Remove p itself, which is either a non-directory or is now empty.
  if (int last = fs::remove(p, ec); !ec)
    return count + last;
  return -1;
}

void
fs::rename(const path& from, const path& to)
{
  error_code ec;
  rename(from, to, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot rename", from, to, ec));
}

void
fs::rename(const path& from, const path& to, error_code& ec) noexcept
{
#if _GLIBCXX_FILESYSTEM_IS_WINDOWS
  const auto to_status = fs::status(to, ec);
  if (to_status.type() == file_type::not_found)
    ec.clear();
  else if (ec)
    return;

  if (fs::exists(to_status))
  {
    const auto from_status = fs::status(from, ec);
    if (ec)
      return;

    if (fs::is_directory(to_status))
    {
      if (!fs::is_directory(from_status))
      {
	// Cannot rename a non-directory over an existing directory.
	ec = std::make_error_code(std::errc::is_a_directory);
	return;
      }
    }
    else if (fs::is_directory(from_status))
    {
      // Cannot rename a directory over an existing non-directory.
      ec = std::make_error_code(std::errc::not_a_directory);
      return;
    }
  }
#endif
  if (posix::rename(from.c_str(), to.c_str()))
    ec.assign(errno, std::generic_category());
  else
    ec.clear();
}

void
fs::resize_file(const path& p, uintmax_t size)
{
  error_code ec;
  resize_file(p, size, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot resize file", p, ec));
}

void
fs::resize_file(const path& p, uintmax_t size, error_code& ec) noexcept
{
  if (size > static_cast<uintmax_t>(std::numeric_limits<posix::off_t>::max()))
    ec.assign(EINVAL, std::generic_category());
  else if (posix::truncate(p.c_str(), size))
    ec.assign(errno, std::generic_category());
  else
    ec.clear();
}


fs::space_info
fs::space(const path& p)
{
  error_code ec;
  space_info s = space(p, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot get free space", p, ec));
  return s;
}

fs::space_info
fs::space(const path& p, error_code& ec) noexcept
{
  space_info info = {
    static_cast<uintmax_t>(-1),
    static_cast<uintmax_t>(-1),
    static_cast<uintmax_t>(-1)
  };
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
#if _GLIBCXX_FILESYSTEM_IS_WINDOWS
  path dir = absolute(p);
  dir.remove_filename();
  auto str = dir.c_str();
#else
  auto str = p.c_str();
#endif

  do_space(str, info.capacity, info.free, info.available, ec);
#endif // _GLIBCXX_HAVE_SYS_STAT_H

  return info;
}

#ifdef _GLIBCXX_HAVE_SYS_STAT_H
fs::file_status
fs::status(const fs::path& p, error_code& ec) noexcept
{
  file_status status;
  auto str = p.c_str();

#if _GLIBCXX_FILESYSTEM_IS_WINDOWS
  // stat() fails if there's a trailing slash (PR 88881)
  path p2;
  if (p.has_relative_path() && !p.has_filename())
    {
      __try
	{
	  p2 = p.parent_path();
	  str = p2.c_str();
	}
      __catch(const bad_alloc&)
	{
	  ec = std::make_error_code(std::errc::not_enough_memory);
	  return status;
	}
      str = p2.c_str();
    }
#endif

  stat_type st;
  if (posix::stat(str, &st))
    {
      int err = errno;
      ec.assign(err, std::generic_category());
      if (is_not_found_errno(err))
	status.type(file_type::not_found);
#ifdef EOVERFLOW
      else if (err == EOVERFLOW)
	status.type(file_type::unknown);
#endif
    }
  else
    {
      status = make_file_status(st);
      ec.clear();
    }
  return status;
}

fs::file_status
fs::symlink_status(const fs::path& p, std::error_code& ec) noexcept
{
  file_status status;
  auto str = p.c_str();

#if _GLIBCXX_FILESYSTEM_IS_WINDOWS
  // stat() fails if there's a trailing slash (PR 88881)
  path p2;
  if (p.has_relative_path() && !p.has_filename())
    {
      __try
	{
	  p2 = p.parent_path();
	  str = p2.c_str();
	}
      __catch(const bad_alloc&)
	{
	  ec = std::make_error_code(std::errc::not_enough_memory);
	  return status;
	}
      str = p2.c_str();
    }
#endif

  stat_type st;
  if (posix::lstat(str, &st))
    {
      int err = errno;
      ec.assign(err, std::generic_category());
      if (is_not_found_errno(err))
	status.type(file_type::not_found);
    }
  else
    {
      status = make_file_status(st);
      ec.clear();
    }
  return status;
}
#endif

fs::file_status
fs::status(const fs::path& p)
{
  std::error_code ec;
  auto result = status(p, ec);
  if (result.type() == file_type::none)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("status", p, ec));
  return result;
}

fs::file_status
fs::symlink_status(const fs::path& p)
{
  std::error_code ec;
  auto result = symlink_status(p, ec);
  if (result.type() == file_type::none)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("symlink_status", p, ec));
  return result;
}

fs::path
fs::temp_directory_path()
{
  error_code ec;
  path p = fs::get_temp_directory_from_env(ec);
  if (!ec)
    {
      auto st = status(p, ec);
      if (!ec && !is_directory(st))
	ec = std::make_error_code(std::errc::not_a_directory);
    }
  if (ec)
    {
      if (p.empty())
	_GLIBCXX_THROW_OR_ABORT(filesystem_error("temp_directory_path", ec));
      else
	_GLIBCXX_THROW_OR_ABORT(filesystem_error("temp_directory_path", p, ec));
    }
  return p;
}

fs::path
fs::temp_directory_path(error_code& ec)
{
  path p = fs::get_temp_directory_from_env(ec);
  if (!ec)
    {
      auto st = status(p, ec);
      if (ec)
	p.clear();
      else if (!is_directory(st))
	{
	  p.clear();
	  ec = std::make_error_code(std::errc::not_a_directory);
	}
    }
  return p;
}

fs::path
fs::weakly_canonical(const path& p)
{
  path result;
  if (exists(status(p)))
    return canonical(p);

  path tmp;
  auto iter = p.begin(), end = p.end();
  // find leading elements of p that exist:
  while (iter != end)
    {
      tmp = result / *iter;
      if (exists(status(tmp)))
	swap(result, tmp);
      else
	break;
      ++iter;
    }
  // canonicalize:
  if (!result.empty())
    result = canonical(result);
  // append the non-existing elements:
  while (iter != end)
    result /= *iter++;
  // normalize:
  return result.lexically_normal();
}

fs::path
fs::weakly_canonical(const path& p, error_code& ec)
{
  path result;
  file_status st = status(p, ec);
  if (exists(st))
    return canonical(p, ec);
  else if (status_known(st))
    ec.clear();
  else
    return result;

  path tmp;
  auto iter = p.begin(), end = p.end();
  // find leading elements of p that exist:
  while (iter != end)
    {
      tmp = result / *iter;
      st = status(tmp, ec);
      if (exists(st))
	swap(result, tmp);
      else
	{
	  if (status_known(st))
	    ec.clear();
	  break;
	}
      ++iter;
    }
  // canonicalize:
  if (!ec && !result.empty())
    result = canonical(result, ec);
  if (ec)
    result.clear();
  else
    {
      // append the non-existing elements:
      while (iter != end)
	result /= *iter++;
      // normalize:
      result = result.lexically_normal();
    }
  return result;
}
