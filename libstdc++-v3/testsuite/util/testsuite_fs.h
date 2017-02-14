// -*- C++ -*-
// Filesystem utils for the C++ library testsuite.
//
// Copyright (C) 2014-2017 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
//

#ifndef _TESTSUITE_FS_H
#define _TESTSUITE_FS_H 1

#include <experimental/filesystem>
#include <fstream>
#include <string>
#include <cstdio>
#include <stdlib.h>
#include <unistd.h>

namespace __gnu_test
{
#define PATH_CHK(p1, p2, fn) \
    if ( p1.fn() != p2.fn() ) \
      throw std::experimental::filesystem::filesystem_error( #fn, p1, p2, \
	  std::make_error_code(std::errc::invalid_argument) )

  void
  compare_paths(const std::experimental::filesystem::path& p1,
		const std::experimental::filesystem::path& p2)
  {
    PATH_CHK( p1, p2, string );
    PATH_CHK( p1, p2, empty );
    PATH_CHK( p1, p2, has_root_path );
    PATH_CHK( p1, p2, has_root_name );
    PATH_CHK( p1, p2, has_root_directory );
    PATH_CHK( p1, p2, has_relative_path );
    PATH_CHK( p1, p2, has_parent_path );
    PATH_CHK( p1, p2, has_filename );
    PATH_CHK( p1, p2, has_stem );
    PATH_CHK( p1, p2, has_extension );
    PATH_CHK( p1, p2, is_absolute );
    PATH_CHK( p1, p2, is_relative );
    auto d1 = std::distance(p1.begin(), p1.end());
    auto d2 = std::distance(p2.begin(), p2.end());
    if( d1 != d2 )
      throw std::experimental::filesystem::filesystem_error(
	  "distance(begin, end)", p1, p2,
	  std::make_error_code(std::errc::invalid_argument) );
  }

  const std::string test_paths[] = {
    "", "/", "//", "/.", "/./", "/a", "/a/", "/a//", "/a/b/c/d", "/a//b",
    "a", "a/b", "a/b/", "a/b/c", "a/b/c.d", "a/b/..", "a/b/c.", "a/b/.c"
  };

  // This is NOT supposed to be a secure way to get a unique name!
  // We just need a path that doesn't exist for testing purposes.
  std::experimental::filesystem::path
  nonexistent_path()
  {
    std::experimental::filesystem::path p;
#if defined(_GNU_SOURCE) || _XOPEN_SOURCE >= 500 || _POSIX_C_SOURCE >= 200112L
    char tmp[] = "filesystem-ts-test.XXXXXX";
    int fd = ::mkstemp(tmp);
    if (fd == -1)
      throw std::experimental::filesystem::filesystem_error("mkstemp failed",
	  std::error_code(errno, std::generic_category()));
    ::unlink(tmp);
    ::close(fd);
    p = tmp;
#else
    char buf[64];
    static int counter;
#if _GLIBCXX_USE_C99_STDIO
    std::snprintf(buf, 64,
#else
    std::sprintf(buf,
#endif
      "filesystem-ts-test.%d.%lu", counter++, (unsigned long) ::getpid());
    p = buf;
#endif
    return p;
  }

  // RAII helper to remove a file on scope exit.
  struct scoped_file
  {
    using path_type = std::experimental::filesystem::path;

    enum adopt_file_t { adopt_file };

    explicit
    scoped_file(const path_type& p = nonexistent_path()) : path(p)
    { std::ofstream{p.native()}; }

    scoped_file(path_type p, adopt_file_t) : path(p) { }

    ~scoped_file() { if (!path.empty()) remove(path); }

    path_type path;
  };

} // namespace __gnu_test
#endif
