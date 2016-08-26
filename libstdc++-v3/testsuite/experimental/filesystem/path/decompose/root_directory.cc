// { dg-options "-lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 8.4.9 path decomposition [path.decompose]

#include <experimental/filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::experimental::filesystem::path;

void
test01()
{
  path p1 = "foo/bar";
  VERIFY( p1.root_directory() == path() );
  path p2 = "/foo/bar";
  VERIFY( p2.root_directory() == path("/") );
  path p3 = "//foo";
  VERIFY( p3.root_directory() == path() );
  path p4 = "///foo";
  VERIFY( p4.root_directory() == path("/") );
}

void
test02()
{
  for (const path& p : __gnu_test::test_paths)
  {
    path rootdir = p.root_directory();
    // If root-directory is composed of 'slash name',
    // 'slash' is excluded from the returned string.
    if (!rootdir.empty() && rootdir.native() != "/")
      VERIFY( rootdir.native()[0] != '/' );
  }
}

int
main()
{
  test01();
  test02();
}
