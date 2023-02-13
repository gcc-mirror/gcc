// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

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
  auto testdir = create_dir();
  __gnu_test::scoped_file file1(testdir.path/"file1");
  __gnu_test::scoped_file file2(testdir.path/"file2");

  fs::directory_iterator it(testdir.path);
  VERIFY( it->is_regular_file() );
  ++it;
  VERIFY( it->is_regular_file() );
  ++it;
  VERIFY( it == fs::directory_iterator{} );
}

void
test02()
{
  auto testdir = create_dir();
  const auto sub1 = create_dir(testdir.path/"sub1");
  __gnu_test::scoped_file file1(sub1.path / "file");
  const auto sub2 = create_dir(testdir.path/"sub2");
  __gnu_test::scoped_file file2(sub2.path / "file");

  fs::recursive_directory_iterator it(testdir.path);
  VERIFY( it->is_directory() );
  ++it;
  VERIFY( it->is_regular_file() );
  ++it;
  VERIFY( it->is_directory() );
  ++it;
  VERIFY( it->is_regular_file() );
  ++it;
  VERIFY( it == fs::recursive_directory_iterator{} );
}

int
main()
{
  test01();
  test02();
}
