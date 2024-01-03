// { dg-do run { target c++17 } }

// Copyright (C) 2017-2024 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

using std::filesystem::path;

void
test01()
{
  VERIFY( path().generic_u32string() == U"" );
  VERIFY( path("/").generic_u32string() == U"/" );
  VERIFY( path("////").generic_u32string() == U"/" );
#ifdef __CYGWIN__
  VERIFY( path("//a").generic_u32string() == U"//a" );
  VERIFY( path("//a/").generic_u32string() == U"//a/" );
  VERIFY( path("//a/b").generic_u32string() == U"//a/b" );
#else
  VERIFY( path("//a").generic_u32string() == U"/a" );
  VERIFY( path("//a/").generic_u32string() == U"/a/" );
  VERIFY( path("//a/b").generic_u32string() == U"/a/b" );
#endif
  VERIFY( path("/a//b").generic_u32string() == U"/a/b" );
  VERIFY( path("/a//b/").generic_u32string() == U"/a/b/" );
  VERIFY( path("/a//b//").generic_u32string() == U"/a/b/" );
  VERIFY( path("/a//b//.").generic_u32string() == U"/a/b/." );
}

void
test02()
{
  if constexpr (path::preferred_separator == L'\\')
  {
    // PR libstdc++/93244
    VERIFY( path("C:\\foo\\bar").generic_u32string() == U"C:/foo/bar" );
    VERIFY( path("C://foo//bar").generic_u32string() == U"C:/foo/bar" );
  }
}

int
main()
{
  test01();
  test02();
}
