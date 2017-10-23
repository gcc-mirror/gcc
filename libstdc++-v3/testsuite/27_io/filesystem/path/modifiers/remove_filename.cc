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

// C++17 30.10.7.4.5 path modifiers [fs.path.modifiers]

#include <filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

using std::filesystem::path;

void
test01()
{
  // C++17 [fs.path.modifiers] p8
  VERIFY( path("foo/bar").remove_filename() == "foo/" );
  VERIFY( path("foo/").remove_filename()    == "foo/" );
  VERIFY( path("/foo").remove_filename()    == "/" );
  VERIFY( path("/").remove_filename()       == "/" );
}

#undef VERIFY
#define VERIFY(X) do { if (!(X)) { __builtin_puts("FAIL: " #X); } } while(false)
#define DUMP(X, Y) do { if (!(X == Y)) { __builtin_printf("%s %s\n", X.c_str(), Y.c_str()); } } while(false)

void
test02()
{
  for (const path& p : __gnu_test::test_paths)
  {
    path p2(p);
    p2.remove_filename();
    p2 /= p.filename();
    VERIFY( p2 == p );
    DUMP( p2 , p );
  }
}

int
main()
{
  test01();
  test02();
}
