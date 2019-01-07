// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

// Copyright (C) 2014-2019 Free Software Foundation, Inc.
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

// 30.10.7.4.9 path decomposition [fs.path.decompose]

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::filesystem::path;

void
test01()
{
  path p1 = "foo/bar";
  VERIFY( p1.root_directory() == path() );
  path p2 = "/foo/bar";
  VERIFY( p2.root_directory() == path("/") );
  path p3 = "//foo";
#ifdef __CYGWIN__
  VERIFY( p3.root_directory() == path() );
#else
  VERIFY( p3.root_directory() == path("/") );
#endif
  path p4 = "///foo";
  VERIFY( p4.root_directory() == path("/") );
}

void
test02()
{
  for (const path& p : __gnu_test::test_paths)
  {
    path rootdir = p.root_directory();
    VERIFY( !rootdir.has_relative_path() );
    if (!rootdir.empty())
#if defined(__MINGW32__) || defined(__MINGW64__)
      VERIFY( rootdir.string() == "/" || rootdir.string() == "\\" );
#else
      VERIFY( rootdir.string() == "/" );
#endif
  }
}

int
main()
{
  test01();
  test02();
}
