// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

namespace fs = std::filesystem;

__gnu_test::scoped_file
create_dir(fs::path dir = __gnu_test::nonexistent_path())
{
  fs::create_directory(dir);
  return { dir, __gnu_test::scoped_file::adopt_file };
}

void
test01()
{
  const auto testdir = create_dir();
  __gnu_test::scoped_file file(testdir.path / "file");

  fs::recursive_directory_iterator r(testdir.path);
  VERIFY( r.recursion_pending() );

  r.disable_recursion_pending();
  VERIFY( !r.recursion_pending() );
}

void
test02()
{
  const auto testdir = create_dir();
  __gnu_test::scoped_file file(testdir.path / "file");

  fs::recursive_directory_iterator r(testdir.path);
  VERIFY( r.recursion_pending() );
  const auto r2 = r;
  // recursion pending flag should be copied:
  VERIFY( r2.recursion_pending() == r.recursion_pending() );

  r.disable_recursion_pending();
  VERIFY( !r.recursion_pending() );
  const auto r3 = r;
  // recursion pending flag should be copied:
  VERIFY( r3.recursion_pending() == r.recursion_pending() );
}

void
test03()
{
  std::error_code ec = make_error_code(std::errc::invalid_argument);

  const auto testdir = create_dir();
  __gnu_test::scoped_file file1(testdir.path / "file1");
  __gnu_test::scoped_file file2(testdir.path / "file2");
  __gnu_test::scoped_file file3(testdir.path / "file3");
  __gnu_test::scoped_file file4(testdir.path / "file4");

  fs::recursive_directory_iterator r(testdir.path);
  r.disable_recursion_pending();
  VERIFY( !r.recursion_pending() );
  ++r;
  // recursion pending flag should be true after incrementing:
  VERIFY( r.recursion_pending() );

  r.disable_recursion_pending();
  VERIFY( !r.recursion_pending() );
  r.increment(ec);
  VERIFY( !ec );
  // recursion pending flag should be true after incrementing:
  VERIFY( r.recursion_pending() );

  r.disable_recursion_pending();
  VERIFY( !r.recursion_pending() );
  r++;
  // recursion pending flag should be true after post-incrementing:
  VERIFY( r.recursion_pending() );

  VERIFY( ++r == fs::recursive_directory_iterator() );
}

void
test04()
{
  const auto testdir = create_dir();
  const auto sub1 = create_dir(testdir.path/"sub1");
  __gnu_test::scoped_file file1(sub1.path / "file");
  const auto sub2 = create_dir(testdir.path/"sub2");
  __gnu_test::scoped_file file2(sub2.path / "file");

  fs::recursive_directory_iterator r(testdir.path);
  ++r;
  r.pop();
  // recursion pending flag should be true after popping:
  VERIFY( r.recursion_pending() );

  // and recursion should actually happen:
  ++r;
  VERIFY( r.depth() == 1 );
  VERIFY( r->is_regular_file() );
  // recursion pending flag should still be true:
  VERIFY( r.recursion_pending() );

  r = fs::recursive_directory_iterator(testdir.path);
  r.disable_recursion_pending();
  ++r;
  // when recursion is disabled, should not enter subdirectories:
  VERIFY( r.depth() == 0 );
  r.disable_recursion_pending();
  ++r;
  VERIFY( r == fs::recursive_directory_iterator() );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
}
