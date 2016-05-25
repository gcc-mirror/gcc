// Filesystem operations -*- C++ -*-

// Copyright (C) 2014-2016 Free Software Foundation, Inc.
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

#include <experimental/filesystem>
#include <functional>
#include <stack>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <limits.h>  // PATH_MAX
#ifdef _GLIBCXX_HAVE_UNISTD_H
# include <unistd.h>
# if defined(_GLIBCXX_HAVE_SYS_STAT_H) && defined(_GLIBCXX_HAVE_SYS_TYPES_H)
#  include <sys/types.h>
#  include <sys/stat.h>
# endif
#endif
#ifdef _GLIBCXX_HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef _GLIBCXX_HAVE_SYS_STATVFS_H
# include <sys/statvfs.h>
#endif
#ifdef _GLIBCXX_USE_SENDFILE
# include <sys/sendfile.h>
#else
# include <ext/stdio_filebuf.h>
# include <ostream>
#endif
#if _GLIBCXX_HAVE_UTIME_H
# include <utime.h>
#endif

#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
# undef utime
# define utime _wutime
# undef chmod
# define chmod _wchmod
#endif

namespace fs = std::experimental::filesystem;

fs::path
fs::absolute(const path& p, const path& base)
{
  const bool has_root_dir = p.has_root_directory();
  const bool has_root_name = p.has_root_name();
  path abs;
  if (has_root_dir && has_root_name)
    abs = p;
  else
    {
      abs = base.is_absolute() ? base : absolute(base);
      if (has_root_dir)
	abs = abs.root_name() / p;
      else if (has_root_name)
	abs = p.root_name() / abs.root_directory() / abs.relative_path()
	  / p.relative_path();
      else
	abs = abs / p;
    }
  return abs;
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

  using char_ptr = std::unique_ptr<char[], free_as_in_malloc>;
}

fs::path
fs::canonical(const path& p, const path& base, error_code& ec)
{
  const path pa = absolute(p, base);
  path result;

#ifdef _GLIBCXX_USE_REALPATH
  char_ptr buf{ nullptr };
# if _XOPEN_VERSION < 700
  // Not safe to call realpath(path, NULL)
  buf.reset( (char*)::malloc(PATH_MAX) );
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
    return result;
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

      if (is_dot(f))
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
			result.remove_filename();

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
fs::canonical(const path& p, error_code& ec)
{
  path cur = current_path(ec);
  if (ec.value())
    return {};
  return canonical(p, cur, ec);
}

fs::path
fs::canonical(const path& p, const path& base)
{
  error_code ec;
  path can = canonical(p, base, ec);
  if (ec)
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot canonicalize", p, base,
					     ec));
  return can;
}

void
fs::copy(const path& from, const path& to, copy_options options)
{
  error_code ec;
  copy(from, to, options, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot copy", from, to, ec));
}

namespace
{
  template<typename Bitmask>
    inline bool is_set(Bitmask obj, Bitmask bits)
    {
      return (obj & bits) != Bitmask::none;
    }
}

#ifdef _GLIBCXX_HAVE_SYS_STAT_H
namespace
{
  typedef struct ::stat stat_type;

  inline fs::file_type
  make_file_type(const stat_type& st) noexcept
  {
    using fs::file_type;
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
    else if (S_ISLNK(st.st_mode))
      return file_type::symlink;
    else if (S_ISSOCK(st.st_mode))
      return file_type::socket;
#endif
    return file_type::unknown;

  }

  inline fs::file_status
  make_file_status(const stat_type& st) noexcept
  {
    return fs::file_status{
	make_file_type(st),
	static_cast<fs::perms>(st.st_mode) & fs::perms::mask
    };
  }

  inline bool
  is_not_found_errno(int err) noexcept
  {
    return err == ENOENT || err == ENOTDIR;
  }

  inline fs::file_time_type
  file_time(const stat_type& st) noexcept
  {
    using namespace std::chrono;
    return fs::file_time_type{
#ifdef _GLIBCXX_USE_ST_MTIM
	seconds{st.st_mtim.tv_sec} + nanoseconds{st.st_mtim.tv_nsec}
#else
	seconds{st.st_mtime}
#endif
    };
  }

  // Returns true if the file descriptor was successfully closed,
  // otherwise returns false and the reason will be in errno.
  inline bool
  close_fd(int fd)
  {
    while (::close(fd))
      if (errno != EINTR)
	return false;
    return true;
  }

  bool
  do_copy_file(const fs::path& from, const fs::path& to,
	       fs::copy_options option,
	       stat_type* from_st, stat_type* to_st,
	       std::error_code& ec) noexcept
  {
    stat_type st1, st2;
    fs::file_status t, f;

    if (to_st == nullptr)
      {
	if (::stat(to.c_str(), &st1))
	  {
	    int err = errno;
	    if (!is_not_found_errno(err))
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
      t = fs::file_status{fs::file_type::not_found};
    else
      t = make_file_status(*to_st);

    if (from_st == nullptr)
      {
	if (::stat(from.c_str(), &st2))
	  {
	    ec.assign(errno, std::generic_category());
	    return false;
	  }
	else
	  from_st = &st2;
      }
    f = make_file_status(*from_st);

    using opts = fs::copy_options;

    if (exists(t))
      {
	if (!is_other(t) && !is_other(f)
	    && to_st->st_dev == from_st->st_dev
	    && to_st->st_ino == from_st->st_ino)
	  {
	    ec = std::make_error_code(std::errc::file_exists);
	    return false;
	  }

	if (is_set(option, opts::skip_existing))
	  {
	    ec.clear();
	    return false;
	  }
	else if (is_set(option, opts::update_existing))
	  {
	    if (file_time(*from_st) <= file_time(*to_st))
	      {
		ec.clear();
		return false;
	      }
	  }
	else if (!is_set(option, opts::overwrite_existing))
	  {
	    ec = std::make_error_code(std::errc::file_exists);
	    return false;
	  }
      }

    struct CloseFD {
      ~CloseFD() { if (fd != -1) close_fd(fd); }
      bool close() { return close_fd(std::exchange(fd, -1)); }
      int fd;
    };

    CloseFD in = { ::open(from.c_str(), O_RDONLY) };
    if (in.fd == -1)
      {
	ec.assign(errno, std::generic_category());
	return false;
      }
    int oflag = O_WRONLY|O_CREAT;
    if (is_set(option, opts::overwrite_existing|opts::update_existing))
      oflag |= O_TRUNC;
    else
      oflag |= O_EXCL;
    CloseFD out = { ::open(to.c_str(), oflag, S_IWUSR) };
    if (out.fd == -1)
      {
	if (errno == EEXIST && is_set(option, opts::skip_existing))
	  ec.clear();
	else
	  ec.assign(errno, std::generic_category());
	return false;
      }

#ifdef _GLIBCXX_USE_FCHMOD
    if (::fchmod(out.fd, from_st->st_mode))
#elif _GLIBCXX_USE_FCHMODAT
    if (::fchmodat(AT_FDCWD, to.c_str(), from_st->st_mode, 0))
#else
    if (::chmod(to.c_str(), from_st->st_mode))
#endif
      {
	ec.assign(errno, std::generic_category());
	return false;
      }

#ifdef _GLIBCXX_USE_SENDFILE
    const auto n = ::sendfile(out.fd, in.fd, nullptr, from_st->st_size);
    if (n != from_st->st_size)
      {
	ec.assign(errno, std::generic_category());
	return false;
      }
    if (!out.close() || !in.close())
      {
	ec.assign(errno, std::generic_category());
	return false;
      }
#else
    __gnu_cxx::stdio_filebuf<char> sbin(in.fd, std::ios::in);
    __gnu_cxx::stdio_filebuf<char> sbout(out.fd, std::ios::out);
    if (sbin.is_open())
      in.fd = -1;
    if (sbout.is_open())
      out.fd = -1;
    if (from_st->st_size && !(std::ostream(&sbout) << &sbin))
      {
	ec = std::make_error_code(std::errc::io_error);
	return false;
      }
    if (sbout.close() || sbin.close())
      {
	ec.assign(errno, std::generic_category());
	return false;
      }
#endif

    ec.clear();
    return true;
  }
}
#endif

void
fs::copy(const path& from, const path& to, copy_options options,
	 error_code& ec) noexcept
{
  const bool skip_symlinks = is_set(options, copy_options::skip_symlinks);
  const bool create_symlinks = is_set(options, copy_options::create_symlinks);
  const bool copy_symlinks = is_set(options, copy_options::copy_symlinks);
  const bool use_lstat = create_symlinks || skip_symlinks;

  file_status f, t;
  stat_type from_st, to_st;
  // N4099 doesn't check copy_symlinks here, but I think that's a defect.
  if (use_lstat || copy_symlinks
      ? ::lstat(from.c_str(), &from_st)
      : ::stat(from.c_str(), &from_st))
    {
      ec.assign(errno, std::generic_category());
      return;
    }
  if (use_lstat
      ? ::lstat(to.c_str(), &to_st)
      : ::stat(to.c_str(), &to_st))
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
      ec = std::make_error_code(std::errc::not_supported);
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
	do_copy_file(from, to / from.filename(), options, &from_st, 0, ec);
      else
	{
	  auto ptr = exists(t) ? &to_st : &from_st;
	  do_copy_file(from, to, options, &from_st, ptr,  ec);
	}
    }
  else if (is_directory(f) && (is_set(options, copy_options::recursive)
			       || options == copy_options::none))
    {
      if (!exists(t))
	if (!create_directory(to, from, ec))
	  return;
      // set an unused bit in options to disable further recursion
      if (!is_set(options, copy_options::recursive))
	options |= static_cast<copy_options>(4096);
      for (const directory_entry& x : directory_iterator(from))
	copy(x.path(), to/x.path().filename(), options, ec);
    }
  // "Otherwise no effects." (should ec.clear() be called?)
}

bool
fs::copy_file(const path& from, const path& to, copy_options option)
{
  error_code ec;
  bool result = copy_file(from, to, option, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot copy file", from, to,
	  ec));
  return result;
}

bool
fs::copy_file(const path& from, const path& to, copy_options option,
	      error_code& ec) noexcept
{
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
  return do_copy_file(from, to, option, nullptr, nullptr, ec);
#else
  ec = std::make_error_code(std::errc::not_supported);
  return false;
#endif
}


void
fs::copy_symlink(const path& existing_symlink, const path& new_symlink)
{
  error_code ec;
  copy_symlink(existing_symlink, new_symlink, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot copy symlink",
	  existing_symlink, new_symlink, ec));
}

void
fs::copy_symlink(const path& existing_symlink, const path& new_symlink,
		 error_code& ec) noexcept
{
  auto p = read_symlink(existing_symlink, ec);
  if (ec.value())
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
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot create directories", p,
	  ec));
  return result;
}

bool
fs::create_directories(const path& p, error_code& ec) noexcept
{
  if (p.empty())
    {
      ec = std::make_error_code(errc::invalid_argument);
      return false;
    }
  std::stack<path> missing;
  path pp = p;

  while (!pp.empty() && status(pp, ec).type() == file_type::not_found)
    {
      ec.clear();
      const auto& filename = pp.filename();
      if (!is_dot(filename) && !is_dotdot(filename))
	missing.push(pp);
      pp.remove_filename();
    }

  if (ec || missing.empty())
    return false;

  do
    {
      const path& top = missing.top();
      create_directory(top, ec);
      if (ec && is_directory(top))
	ec.clear();
      missing.pop();
    }
  while (!missing.empty() && !ec);

  return missing.empty();
}

namespace
{
  bool
  create_dir(const fs::path& p, fs::perms perm, std::error_code& ec)
  {
    bool created = false;
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
    ::mode_t mode = static_cast<std::underlying_type_t<fs::perms>>(perm);
    if (::mkdir(p.c_str(), mode))
      {
	const int err = errno;
	if (err != EEXIST || !is_directory(p))
	  ec.assign(err, std::generic_category());
	else
	  ec.clear();
      }
    else
      {
	ec.clear();
	created = true;
      }
#else
    ec = std::make_error_code(std::errc::not_supported);
#endif
    return created;
  }
} // namespace

bool
fs::create_directory(const path& p)
{
  error_code ec;
  bool result = create_directory(p, ec);
  if (ec.value())
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
  if (ec.value())
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
  if (::stat(attributes.c_str(), &st))
    {
      ec.assign(errno, std::generic_category());
      return false;
    }
  return create_dir(p, static_cast<perms>(st.st_mode), ec);
#else
  ec = std::make_error_code(std::errc::not_supported);
  return false;
#endif
}


void
fs::create_directory_symlink(const path& to, const path& new_symlink)
{
  error_code ec;
  create_directory_symlink(to, new_symlink, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot create directory symlink",
	  to, new_symlink, ec));
}

void
fs::create_directory_symlink(const path& to, const path& new_symlink,
			     error_code& ec) noexcept
{
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  ec = std::make_error_code(std::errc::not_supported);
#else
  create_symlink(to, new_symlink, ec);
#endif
}


void
fs::create_hard_link(const path& to, const path& new_hard_link)
{
  error_code ec;
  create_hard_link(to, new_hard_link, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot create hard link",
	  to, new_hard_link, ec));
}

void
fs::create_hard_link(const path& to, const path& new_hard_link,
		     error_code& ec) noexcept
{
#ifdef _GLIBCXX_HAVE_UNISTD_H
  if (::link(to.c_str(), new_hard_link.c_str()))
    ec.assign(errno, std::generic_category());
  else
    ec.clear();
#else
  ec = std::make_error_code(std::errc::not_supported);
#endif
}

void
fs::create_symlink(const path& to, const path& new_symlink)
{
  error_code ec;
  create_symlink(to, new_symlink, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot create symlink",
	  to, new_symlink, ec));
}

void
fs::create_symlink(const path& to, const path& new_symlink,
		   error_code& ec) noexcept
{
#ifdef _GLIBCXX_HAVE_UNISTD_H
  if (::symlink(to.c_str(), new_symlink.c_str()))
    ec.assign(errno, std::generic_category());
  else
    ec.clear();
#else
  ec = std::make_error_code(std::errc::not_supported);
#endif
}


fs::path
fs::current_path()
{
  error_code ec;
  path p = current_path(ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot get current path", ec));
  return p;
}

fs::path
fs::current_path(error_code& ec)
{
  path p;
#ifdef _GLIBCXX_HAVE_UNISTD_H
#ifdef __GLIBC__
  if (char_ptr cwd = char_ptr{::getcwd(nullptr, 0)})
    {
      p.assign(cwd.get());
      ec.clear();
    }
  else
    ec.assign(errno, std::generic_category());
#else
  long path_max = pathconf(".", _PC_PATH_MAX);
  size_t size;
  if (path_max == -1)
      size = 1024;
  else if (path_max > 10240)
      size = 10240;
  else
      size = path_max;
  for (char_ptr buf; p.empty(); size *= 2)
    {
      buf.reset((char*)malloc(size));
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
  ec = std::make_error_code(std::errc::not_supported);
#endif
  return p;
}

void
fs::current_path(const path& p)
{
  error_code ec;
  current_path(p, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot set current path", ec));
}

void
fs::current_path(const path& p, error_code& ec) noexcept
{
#ifdef _GLIBCXX_HAVE_UNISTD_H
  if (::chdir(p.c_str()))
    ec.assign(errno, std::generic_category());
  else
    ec.clear();
#else
  ec = std::make_error_code(std::errc::not_supported);
#endif
}

bool
fs::equivalent(const path& p1, const path& p2)
{
  error_code ec;
  auto result = equivalent(p1, p2, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot check file equivalence",
	  p1, p2, ec));
  return result;
}

bool
fs::equivalent(const path& p1, const path& p2, error_code& ec) noexcept
{
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
  stat_type st1, st2;
  if (::stat(p1.c_str(), &st1) == 0 && ::stat(p2.c_str(), &st2) == 0)
    {
      file_status s1 = make_file_status(st1);
      file_status s2 = make_file_status(st2);
      if (is_other(s1) && is_other(s2))
	{
	  ec = std::make_error_code(std::errc::not_supported);
	  return false;
	}
      ec.clear();
      return st1.st_dev == st2.st_dev && st1.st_ino == st2.st_ino;
    }
  else if (is_not_found_errno(errno))
    {
      ec = std::make_error_code(std::errc::no_such_file_or_directory);
      return false;
    }
  ec.assign(errno, std::generic_category());
#else
  ec = std::make_error_code(std::errc::not_supported);
#endif
  return false;
}

std::uintmax_t
fs::file_size(const path& p)
{
  error_code ec;
  auto sz = file_size(p, ec);
  if (ec.value())
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
      stat_type st;
      if (::stat(p.c_str(), &st))
	{
	  ec.assign(errno, std::generic_category());
	  return deflt;
	}
      ec.clear();
      return f(st);
#else
      ec = std::make_error_code(std::errc::not_supported);
      return deflt;
#endif
    }
}

std::uintmax_t
fs::file_size(const path& p, error_code& ec) noexcept
{
  struct S
  {
    S(const stat_type& st) : type(make_file_type(st)), size(st.st_size) { }
    S() : type(file_type::not_found) { }
    file_type type;
    size_t size;
  };
  auto s = do_stat(p, ec, [](const auto& st) { return S{st}; }, S{});
  if (s.type == file_type::regular)
    return s.size;
  if (!ec)
    {
      if (s.type == file_type::directory)
	ec = std::make_error_code(std::errc::is_a_directory);
      else
	ec = std::make_error_code(std::errc::not_supported);
    }
  return -1;
}

std::uintmax_t
fs::hard_link_count(const path& p)
{
  error_code ec;
  auto count = hard_link_count(p, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot get link count", p, ec));
  return count;
}

std::uintmax_t
fs::hard_link_count(const path& p, error_code& ec) noexcept
{
  return do_stat(p, ec, std::mem_fn(&stat::st_nlink),
		 static_cast<uintmax_t>(-1));
}

bool
fs::is_empty(const path& p)
{
  return fs::is_directory(status(p))
    ? fs::directory_iterator(p) == fs::directory_iterator()
    : fs::file_size(p) == 0;
}

bool
fs::is_empty(const path& p, error_code& ec) noexcept
{
  auto s = status(p, ec);
  if (ec.value())
    return false;
  return fs::is_directory(s)
    ? fs::directory_iterator(p, ec) == fs::directory_iterator()
    : fs::file_size(p, ec) == 0;
}

fs::file_time_type
fs::last_write_time(const path& p)
{
  error_code ec;
  auto t = last_write_time(p, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot get file time", p, ec));
  return t;
}

fs::file_time_type
fs::last_write_time(const path& p, error_code& ec) noexcept
{
  return do_stat(p, ec, [](const auto& st) { return file_time(st); },
		 file_time_type::min());
}

void
fs::last_write_time(const path& p, file_time_type new_time)
{
  error_code ec;
  last_write_time(p, new_time, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot set file time", p, ec));
}

void
fs::last_write_time(const path& p __attribute__((__unused__)),
		    file_time_type new_time, error_code& ec) noexcept
{
  auto d = new_time.time_since_epoch();
  auto s = chrono::duration_cast<chrono::seconds>(d);
#if _GLIBCXX_USE_UTIMENSAT
  auto ns = chrono::duration_cast<chrono::nanoseconds>(d - s);
  struct ::timespec ts[2];
  ts[0].tv_sec = 0;
  ts[0].tv_nsec = UTIME_OMIT;
  ts[1].tv_sec = static_cast<std::time_t>(s.count());
  ts[1].tv_nsec = static_cast<long>(ns.count());
  if (::utimensat(AT_FDCWD, p.c_str(), ts, 0))
    ec.assign(errno, std::generic_category());
  else
    ec.clear();
#elif _GLIBCXX_HAVE_UTIME_H
  ::utimbuf times;
  times.modtime = s.count();
  times.actime = do_stat(p, ec, [](const auto& st) { return st.st_atime; },
			 times.modtime);
  if (::utime(p.c_str(), &times))
    ec.assign(errno, std::generic_category());
  else
    ec.clear();
#else
  ec = std::make_error_code(std::errc::not_supported);
#endif
}

void
fs::permissions(const path& p, perms prms)
{
  error_code ec;
  permissions(p, prms, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot set permissions", p, ec));
}

void fs::permissions(const path& p, perms prms, error_code& ec) noexcept
{
#if _GLIBCXX_USE_FCHMODAT
  if (::fchmodat(AT_FDCWD, p.c_str(), static_cast<mode_t>(prms), 0))
#else
  if (::chmod(p.c_str(), static_cast<mode_t>(prms)))
#endif
    ec.assign(errno, std::generic_category());
  else
    ec.clear();
}

fs::path
fs::read_symlink(const path& p)
{
  error_code ec;
  path tgt = read_symlink(p, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("read_symlink", p, ec));
  return tgt;
}

fs::path fs::read_symlink(const path& p, error_code& ec)
{
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
  stat_type st;
  if (::lstat(p.c_str(), &st))
    {
      ec.assign(errno, std::generic_category());
      return {};
    }
  std::string buf(st.st_size, '\0');
  ssize_t len = ::readlink(p.c_str(), &buf.front(), buf.size());
  if (len == -1)
    {
      ec.assign(errno, std::generic_category());
      return {};
    }
  return path{buf.data(), buf.data()+len};
#else
  ec = std::make_error_code(std::errc::not_supported);
  return {};
#endif
}


bool
fs::remove(const path& p)
{
  error_code ec;
  bool result = fs::remove(p, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot remove", p, ec));
  return result;
}

bool
fs::remove(const path& p, error_code& ec) noexcept
{
  if (exists(symlink_status(p, ec)))
    {
      if (::remove(p.c_str()) == 0)
	{
	  ec.clear();
	  return true;
	}
      else
	ec.assign(errno, std::generic_category());
    }
  return false;
}


std::uintmax_t
fs::remove_all(const path& p)
{
  error_code ec;
  bool result = remove_all(p, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot remove all", p, ec));
  return result;
}

std::uintmax_t
fs::remove_all(const path& p, error_code& ec) noexcept
{
  auto fs = symlink_status(p, ec);
  uintmax_t count = 0;
  if (ec.value() == 0 && fs.type() == file_type::directory)
    for (directory_iterator d(p, ec), end; ec.value() == 0 && d != end; ++d)
      count += fs::remove(d->path(), ec);
  if (ec.value())
    return -1;
  return fs::remove(p, ec) ? ++count : -1;  // fs:remove() calls ec.clear()
}

void
fs::rename(const path& from, const path& to)
{
  error_code ec;
  rename(from, to, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot rename", from, to, ec));
}

void
fs::rename(const path& from, const path& to, error_code& ec) noexcept
{
  if (::rename(from.c_str(), to.c_str()))
    ec.assign(errno, std::generic_category());
  else
    ec.clear();
}

void
fs::resize_file(const path& p, uintmax_t size)
{
  error_code ec;
  resize_file(p, size, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("cannot resize file", p, ec));
}

void
fs::resize_file(const path& p, uintmax_t size, error_code& ec) noexcept
{
#ifdef _GLIBCXX_HAVE_UNISTD_H
  if (size > static_cast<uintmax_t>(std::numeric_limits<off_t>::max()))
    ec.assign(EINVAL, std::generic_category());
  else if (::truncate(p.c_str(), size))
    ec.assign(errno, std::generic_category());
  else
    ec.clear();
#else
  ec = std::make_error_code(std::errc::not_supported);
#endif
}


fs::space_info
fs::space(const path& p)
{
  error_code ec;
  space_info s = space(p, ec);
  if (ec.value())
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
#ifdef _GLIBCXX_HAVE_SYS_STATVFS_H
  struct ::statvfs f;
  if (::statvfs(p.c_str(), &f))
      ec.assign(errno, std::generic_category());
  else
    {
      info = space_info{
	f.f_blocks * f.f_frsize,
	f.f_bfree * f.f_frsize,
	f.f_bavail * f.f_frsize
      };
      ec.clear();
    }
#else
  ec = std::make_error_code(std::errc::not_supported);
#endif
  return info;
}

#ifdef _GLIBCXX_HAVE_SYS_STAT_H
fs::file_status
fs::status(const fs::path& p, std::error_code& ec) noexcept
{
  file_status status;
  stat_type st;
  if (::stat(p.c_str(), &st))
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

fs::file_status
fs::symlink_status(const fs::path& p, std::error_code& ec) noexcept
{
  file_status status;
  stat_type st;
  if (::lstat(p.c_str(), &st))
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
fs::system_complete(const path& p)
{
  error_code ec;
  path comp = system_complete(p, ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("system_complete", p, ec));
  return comp;
}

fs::path
fs::system_complete(const path& p, error_code& ec)
{
  path base = current_path(ec);
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  if (p.is_absolute() || !p.has_root_name()
      || p.root_name() == base.root_name())
    return absolute(p, base);
  // else TODO
  ec = std::make_error_code(std::errc::not_supported);
  return {};
#else
  if (ec.value())
    return {};
  return absolute(p, base);
#endif
}

fs::path fs::temp_directory_path()
{
  error_code ec;
  path tmp = temp_directory_path(ec);
  if (ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem_error("temp_directory_path", ec));
  return tmp;
}

fs::path fs::temp_directory_path(error_code& ec)
{
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  ec = std::make_error_code(std::errc::not_supported);
  return {}; // TODO
#else
  const char* tmpdir = nullptr;
  const char* env[] = { "TMPDIR", "TMP", "TEMP", "TEMPDIR", nullptr };
  for (auto e = env; tmpdir == nullptr && *e != nullptr; ++e)
    tmpdir = ::getenv(*e);
  path p = tmpdir ? tmpdir : "/tmp";
  if (exists(p) && is_directory(p))
    {
      ec.clear();
      return p;
    }
  ec = std::make_error_code(std::errc::not_a_directory);
  return {};
#endif
}

