// -*- C++ -*-
// Filesystem utils for the C++ library testsuite.
//
// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

// Assume we want std::filesystem in C++17, unless USE_FILESYSTEM_TS defined:
#if __cplusplus >= 201703L && ! defined USE_FILESYSTEM_TS
#include <filesystem>
namespace test_fs = std::filesystem;
#else
#include <experimental/filesystem>
namespace test_fs = std::experimental::filesystem;
#endif
#include <fstream>
#include <string>
#include <cstdio>
#include <stdlib.h>
#include <unistd.h>

namespace __gnu_test
{
#define PATH_CHK(p1, p2, fn) \
    if ( p1.fn() != p2.fn() ) \
      throw test_fs::filesystem_error("comparing '" #fn "' failed", p1, p2, \
	  std::make_error_code(std::errc::invalid_argument) )

  void
  compare_paths(const test_fs::path& p1,
		const test_fs::path& p2)
  {
    PATH_CHK( p1, p2, native );
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
      throw test_fs::filesystem_error(
	  "distance(begin, end)", p1, p2,
	  std::make_error_code(std::errc::invalid_argument) );
  }

  const std::string test_paths[] = {
    "", "/", "//", "/.", "/./", "/a", "/a/", "/a//", "/a/b/c/d", "/a//b",
    "a", "a/b", "a/b/", "a/b/c", "a/b/c.d", "a/b/..", "a/b/c.", "a/b/.c"
  };

  test_fs::path
  root_path()
  {
#if defined(__MING32__) || defined(__MINGW64__)
    return L"c:/";
#else
    return "/";
#endif
  }

  // This is NOT supposed to be a secure way to get a unique name!
  // We just need a path that doesn't exist for testing purposes.
  test_fs::path
  nonexistent_path()
  {
    test_fs::path p;
#if defined(_GNU_SOURCE) || _XOPEN_SOURCE >= 500 || _POSIX_C_SOURCE >= 200112L
    char tmp[] = "filesystem-test.XXXXXX";
    int fd = ::mkstemp(tmp);
    if (fd == -1)
      throw test_fs::filesystem_error("mkstemp failed",
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
      "filesystem-test.%d.%lu", counter++, (unsigned long) ::getpid());
    p = buf;
#endif
    return p;
  }

  // RAII helper to remove a file on scope exit.
  struct scoped_file
  {
    using path_type = test_fs::path;

    enum adopt_file_t { adopt_file };

    explicit
    scoped_file(const path_type& p = nonexistent_path()) : path(p)
    { std::ofstream{p.c_str()}; }

    scoped_file(path_type p, adopt_file_t) : path(p) { }

    ~scoped_file() { if (!path.empty()) remove(path); }

    path_type path;
  };

} // namespace __gnu_test
#endif
