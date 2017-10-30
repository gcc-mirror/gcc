// { dg-options "-std=gnu++17 -lstdc++fs" }
// { dg-do run { target c++17 } }
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

// C++17 30.10.7.4.9 path decomposition [fs.path.decompose]

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::filesystem::path;

void
test01()
{
  path p1 = "foo/bar";
  VERIFY( p1.root_path() == path() );
  path p2 = "/foo/bar";
  VERIFY( p2.root_path() == path("/") );
}

#undef VERIFY
#define VERIFY(X) do { if (!(X)) { __builtin_puts("FAIL: " #X); } } while(false)
#define DUMP(X, Y, Z) do { if (!(Y == Z)) { __builtin_printf("%s %s %s\n", X.c_str(), Y.c_str(), Z.c_str()); } } while(false)

void
test02()
{
  for (const path& p : __gnu_test::test_paths)
  {
    path rootp = p.root_path();
    path rootn = p.root_name();
    path rootd = p.root_directory();
    VERIFY( rootp == (rootn / rootd) );
    DUMP(p,  rootp , (rootn / rootd) );
  }
}

int
main()
{
  test01();
  test02();
}
