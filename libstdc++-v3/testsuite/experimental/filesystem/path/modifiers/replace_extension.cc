// { dg-options "-lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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

// 8.4.5 path modifiers [path.modifiers]

#include <experimental/filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

using std::experimental::filesystem::path;

void
test01()
{
  VERIFY( path("/foo.txt").replace_extension("cpp") == "/foo.cpp" );
  VERIFY( path("/foo.txt").replace_extension(".cpp") == "/foo.cpp" );
  VERIFY( path("/").replace_extension("bar") == "/.bar" );
}

void
test02()
{
  for (const path& p : __gnu_test::test_paths)
  {
    path p2 = p;
    VERIFY(p2.replace_extension(p2.extension()) == p);
  }
}

int
main()
{
  test01();
  test02();
}
