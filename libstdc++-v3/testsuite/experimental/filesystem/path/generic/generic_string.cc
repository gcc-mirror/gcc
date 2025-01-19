// { dg-options "-DUSE_FILESYSTEM_TS -lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

// 8.4.7 path generic format observers [path.generic.obs]

#include <experimental/filesystem>
#include <testsuite_fs.h>
#include <testsuite_allocator.h>

using std::experimental::filesystem::path;

void
test01()
{
  __gnu_test::compare_paths( path("///a//b///").generic_string(), "/a/b/." );
  __gnu_test::compare_paths( path("///a//b").generic_u16string(), "/a/b" );
  __gnu_test::compare_paths( path("//a//b").generic_u16string(), "//a/b" );
}

using __gnu_test::SimpleAllocator;

void
test02()
{
  path p = "//foo//bar//.";
  using C = char16_t;
  auto g = p.generic_string<C, std::char_traits<C>, SimpleAllocator<C>>();
  VERIFY( g == u"//foo/bar/." );
}


void
test03()
{
  for (path p : { "/a///b//c", "///a//b//c", "a:b//c", "a://b///c" })
  {
    // A path constructed from the generic format string should compare equal
    // to the original, because they represent the same path.
    VERIFY( path(p.generic_string()) == p );
#ifdef _GLIBCXX_USE_WCHAR_T
    VERIFY( path(p.generic_wstring()) == p );
#endif
    VERIFY( path(p.generic_u8string()) == p );
    VERIFY( path(p.generic_u16string()) == p );
    VERIFY( path(p.generic_u32string()) == p );
  }

  // Except when the original consists entirely of a root-directory with
  // multiple slashes, because path("///").native() is "///" but the
  // generic format string is "/". In the Filesystem TS path::compare just
  // compares native strings, so path("///") != path("/").
  VERIFY( path("///").generic_string() == "/" );
}

int
main()
{
  test01();
  test02();
  test03();
}
