// Copyright (C) 2021-2025 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }
// { dg-xfail-run-if "rename is not POSIX-compliant" { *-*-rtems* } }

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

namespace fs = std::filesystem;

void
test01()
{
  std::error_code ec;
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);

  auto p1 = __gnu_test::nonexistent_path();
  auto p2 = __gnu_test::nonexistent_path();

  fs::rename(p1, p2, ec);
  VERIFY( ec );

  ec.clear();
  fs::rename(p1, "", ec);
  VERIFY( ec );

  ec.clear();
  fs::rename("", p1, ec);
  VERIFY( ec );

  ec = bad_ec;
  std::ofstream{p1}; // create file
  fs::rename(p1, p1, ec); // no-op
  VERIFY( !ec );
  VERIFY( is_regular_file(p1) );

  ec.clear();
  rename(p2, p1, ec);
  VERIFY( ec );
  VERIFY( ec.value() == ENOENT );
  VERIFY( is_regular_file(p1) );

  ec = bad_ec;
  fs::rename(p1, p2, ec);
  VERIFY( !ec );
  VERIFY( !exists(p1) );
  VERIFY( is_regular_file(p2) );

  ec = bad_ec;
  std::ofstream{p1}; // create file
  fs::rename(p1, p2, ec);
  VERIFY( !ec );
  VERIFY( !exists(p1) );
  VERIFY( is_regular_file(p2) );

  fs::remove(p2, ec);
}

void
test_symlinks()
{
#ifndef NO_SYMLINKS
  std::error_code ec;
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);

  const auto dir = __gnu_test::nonexistent_path();
  fs::create_directory(dir);

  create_symlink(dir/"nonesuch", dir/"link");  // dangling symlink
  ec = bad_ec;
  fs::rename(dir/"link", dir/"newlink", ec);
  VERIFY( !ec );
  VERIFY( !exists(symlink_status(dir/"link")) );
  VERIFY( is_symlink(dir/"newlink") );

  __gnu_test::scoped_file f(dir/"file");
  create_symlink(dir/"file", dir/"link");
  ec = bad_ec;
  fs::rename(dir/"link", dir/"newerlink", ec);
  VERIFY( !ec );
  VERIFY( !exists(symlink_status(dir/"link")) );
  VERIFY( is_symlink(dir/"newerlink") );
  VERIFY( is_regular_file(dir/"file") );

  fs::remove_all(dir, ec);
  f.path.clear();
#endif
}

void
test_directories()
{
  std::error_code ec;
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);

  const auto dir = __gnu_test::nonexistent_path();
  fs::create_directory(dir);
  __gnu_test::scoped_file f(dir/"file");
  fs::create_directory(dir/"subdir");

  // Rename directory.
  ec = bad_ec;
  fs::rename(dir/"subdir", dir/"subdir2", ec);
  VERIFY( !ec );
  VERIFY( is_directory(dir/"subdir2") );
  VERIFY( !exists(dir/"subdir") );

  // Cannot rename a directory to a sub-directory of itself.
  fs::rename(dir/"subdir2", dir/"subdir2/subsubdir", ec);
  VERIFY( ec );
  VERIFY( is_directory(dir/"subdir2") );
  VERIFY( !exists(dir/"subdir2"/"subsubdir") );

  // Cannot rename a file to the name of an existing directory.
  ec.clear();
  fs::rename(dir/"file", dir/"subdir2", ec);
  VERIFY( ec );
  VERIFY( is_directory(dir/"subdir2") );
  VERIFY( is_regular_file(dir/"file") );

  // Cannot rename a directory to the name of an existing non-directory
  ec.clear();
  fs::rename(dir/"subdir2", dir/"file", ec);
  VERIFY( ec );
  VERIFY( is_regular_file(dir/"file") );
  VERIFY( is_directory(dir/"subdir2") );

  // Cannot rename directory to the name of a non-empty directory.
  ec.clear();
  __gnu_test::scoped_file f2(dir/"subdir2/file");
  fs::create_directory(dir/"subdir");
  fs::rename(dir/"subdir", dir/"subdir2", ec);
  VERIFY( ec );
  VERIFY( is_directory(dir/"subdir") );
  VERIFY( is_directory(dir/"subdir2") );
  VERIFY( is_regular_file(dir/"subdir2/file") );

#if defined(__MINGW32__) || defined(__MINGW64__)
  // Cannot rename a directory to an existing directory
#else
  // Can rename a non-empty directory to the name of an empty directory.
  ec = bad_ec;
  fs::rename(dir/"subdir2", dir/"subdir", ec);
  VERIFY( !ec );
  VERIFY( is_directory(dir/"subdir") );
  VERIFY( !exists(dir/"subdir2") );
  VERIFY( is_regular_file(dir/"subdir/file") );
#endif

  f2.path.clear();
  f.path.clear();

  fs::remove_all(dir, ec);
}

int
main()
{
  test01();
  test_symlinks();
  test_directories();
}
