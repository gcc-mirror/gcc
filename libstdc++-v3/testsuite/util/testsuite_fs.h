// -*- C++ -*-
// Filesystem utils for the C++ library testsuite.
//
// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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
#include <algorithm>
#include <fstream>
#include <random>   // std::random_device
#include <string>
#include <system_error>
#include <cstdio>
#include <unistd.h> // unlink, close, getpid, geteuid

#if defined(_GNU_SOURCE) || _XOPEN_SOURCE >= 500 || _POSIX_C_SOURCE >= 200112L
#include <stdlib.h> // mkstemp
#endif

#ifndef _GLIBCXX_HAVE_SYMLINK
#define NO_SYMLINKS
#endif

#if !defined (_GLIBCXX_HAVE_SYS_STATVFS_H) \
  && !defined (_GLIBCXX_FILESYSTEM_IS_WINDOWS)
#define NO_SPACE
#endif

#if !(_GLIBCXX_HAVE_SYS_STAT_H \
      && (_GLIBCXX_USE_UTIMENSAT || _GLIBCXX_USE_UTIME))
#define NO_LAST_WRITE_TIME 1
#endif

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
    if (d1 != d2)
      throw test_fs::filesystem_error(
	  "distance(begin1, end1) != distance(begin2, end2)", p1, p2,
	  std::make_error_code(std::errc::invalid_argument) );
    if (!std::equal(p1.begin(), p1.end(), p2.begin()))
      throw test_fs::filesystem_error(
	  "!equal(begin1, end1, begin2)", p1, p2,
	  std::make_error_code(std::errc::invalid_argument) );

  }

  const std::string test_paths[] = {
    "", "/", "//", "/.", "/./", "/a", "/a/", "/a//", "/a/b/c/d", "/a//b",
    "a", "a/b", "a/b/", "a/b/c", "a/b/c.d", "a/b/..", "a/b/c.", "a/b/.c"
  };

  test_fs::path
  root_path()
  {
#if defined(__MINGW32__) || defined(__MINGW64__)
    return L"c:/";
#else
    return "/";
#endif
  }

  // This is NOT supposed to be a secure way to get a unique name!
  // We just need a path that doesn't exist for testing purposes.
  test_fs::path
  nonexistent_path(std::string file = __builtin_FILE())
  {
    // Include the caller's filename to help identify tests that fail to
    // clean up the files they create.
    // Remove .cc extension:
    if (file.length() > 3 && file.compare(file.length() - 3, 3, ".cc") == 0)
      file.resize(file.length() - 3);
    // And directory:
    auto pos = file.find_last_of("/\\");
    if (pos != file.npos)
      file.erase(0, pos+1);

    file.reserve(file.size() + 40);
    file.insert(0, "filesystem-test.");

    // A counter, starting from a random value, to be included as part
    // of the filename being returned, and incremented each time
    // this function is used.  It allows us to ensure that two calls
    // to this function can never return the same filename, something
    // testcases do when they need multiple non-existent filenames
    // for their purposes.
    static unsigned counter = std::random_device{}();
    file += '.';
    file += std::to_string(counter++);
    file += '.';

    test_fs::path p;
#if defined(_GNU_SOURCE) || _XOPEN_SOURCE >= 500 || _POSIX_C_SOURCE >= 200112L

    // Use mkstemp to determine the name of a file which does not exist yet.
    //
    // Note that we have seen on some systems (such as RTEMS, for instance)
    // that mkstemp behaves very predictably, causing it to always try
    // the same sequence of file names.  In other words, if we call mkstemp
    // with a pattern, delete the file it created (which is what we do, here),
    // and call mkstemp with the same pattern again, it returns the same
    // filename once more.  While most implementations introduce a degree
    // of randomness, it is not mandated by the standard, and this is why
    // we also include a counter in the template passed to mkstemp.
    file += "XXXXXX";
    int fd = ::mkstemp(&file[0]);
    if (fd == -1)
      throw test_fs::filesystem_error("mkstemp failed",
	  std::error_code(errno, std::generic_category()));
    ::unlink(file.c_str());
    ::close(fd);
    p = std::move(file);
#else
    if (file.length() > 64)
      file.resize(64);
    // The combination of random counter and PID should be unique for a given
    // run of the testsuite.  N.B. getpid() returns a pointer type on vxworks
    // in kernel mode.
    file += std::to_string((unsigned long) ::getpid());
    p = std::move(file);
    if (test_fs::exists(p))
      throw test_fs::filesystem_error("Failed to generate unique pathname", p,
	  std::make_error_code(std::errc::file_exists));
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

    scoped_file(scoped_file&&) = default;
    scoped_file& operator=(scoped_file&&) = default;

    path_type path;
  };

  inline bool
  permissions_are_testable(bool print_msg = true)
  {
    bool testable = false;
#if !(defined __MINGW32__ || defined __MINGW64__)
    if (geteuid() != 0)
      testable = true;
    // XXX on Linux the CAP_DAC_OVERRIDE and CAP_DAC_READ_SEARCH capabilities
    // can give normal users extra permissions for files and directories.
    // We ignore that possibility here.
#endif
    if (print_msg && !testable)
      std::puts("Skipping tests that depend on filesystem permissions");
    return testable;
  }

} // namespace __gnu_test
#endif
