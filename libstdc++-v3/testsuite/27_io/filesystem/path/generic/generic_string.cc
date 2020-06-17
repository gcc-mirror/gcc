// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

// Copyright (C) 2017-2020 Free Software Foundation, Inc.
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

// C++17 30.10.7.4.7 path generic format observers [fs.path.generic.obs]

#include <filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

using std::filesystem::path;

void
test01()
{
  VERIFY( path().generic_string() == "" );
  VERIFY( path("/").generic_string() == "/" );
  VERIFY( path("////").generic_string() == "/" );
#ifdef __CYGWIN__
  VERIFY( path("//a").generic_string() == "//a" );
  VERIFY( path("//a/").generic_string() == "//a/" );
  VERIFY( path("//a//").generic_string() == "//a/" );
  VERIFY( path("//a/b").generic_string() == "//a/b" );
  VERIFY( path("//a//b").generic_string() == "//a/b" );
#else
  VERIFY( path("//a").generic_string() == "/a" );
  VERIFY( path("//a/").generic_string() == "/a/" );
  VERIFY( path("//a//").generic_string() == "/a/" );
  VERIFY( path("//a/b").generic_string() == "/a/b" );
  VERIFY( path("//a//b").generic_string() == "/a/b" );
#endif
  VERIFY( path("/a//b").generic_string() == "/a/b" );
  VERIFY( path("/a//b/").generic_string() == "/a/b/" );
  VERIFY( path("/a//b//").generic_string() == "/a/b/" );
  VERIFY( path("/a//b//.").generic_string() == "/a/b/." );
}

void
test02()
{
  if constexpr (path::preferred_separator == L'\\')
  {
    // PR libstdc++/93244
    VERIFY( path("C:\\foo\\bar").generic_string() == "C:/foo/bar" );
    VERIFY( path("C://foo//bar").generic_string() == "C:/foo/bar" );
  }
}

void
test03()
{
  for (path p : __gnu_test::test_paths)
  {
    // A path constructed from the generic format string should compare equal
    // to the original, because they represent the same path.
    VERIFY( path(p.generic_string()) == p );
    VERIFY( path(p.generic_wstring()) == p );
    VERIFY( path(p.generic_u8string()) == p );
    VERIFY( path(p.generic_u16string()) == p );
    VERIFY( path(p.generic_u32string()) == p );
  }

  for (path p : { "a///b//c", "///a//b//c", "a:b//c", "a://b///c" })
  {
    // A path constructed from the generic format string should compare equal
    // to the original, because they represent the same path.
    VERIFY( path(p.generic_string()) == p );
    VERIFY( path(p.generic_wstring()) == p );
    VERIFY( path(p.generic_u8string()) == p );
    VERIFY( path(p.generic_u16string()) == p );
    VERIFY( path(p.generic_u32string()) == p );
  }
}

int
main()
{
  test01();
  test02();
  test03();
}
