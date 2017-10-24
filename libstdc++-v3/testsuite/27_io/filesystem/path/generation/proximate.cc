// Copyright (C) 2017 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17 -lstdc++fs" }
// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <testsuite_hooks.h>

using std::filesystem::path;

void
test01()
{
  // C++17 [fs.path.gen] p5
  VERIFY( path("/a/d").lexically_proximate("/a/b/c") == "../../d" );
  VERIFY( path("/a/b/c").lexically_proximate("/a/d") == "../b/c" );
  VERIFY( path("a/b/c").lexically_proximate("a") == "b/c" );
  VERIFY( path("a/b/c").lexically_proximate("a/b/c/x/y") == "../.." );
  VERIFY( path("a/b/c").lexically_proximate("a/b/c") == "." );
  VERIFY( path("a/b").lexically_proximate("c/d") == "../../a/b" );
}

void
test02()
{
  path p = "a/b/c";
  VERIFY( p.lexically_proximate(p) == "." );
  VERIFY( p.lexically_proximate("a/../a/b/../b/c/../c/.") == "../../b/c" );
  VERIFY( p.lexically_proximate("../../../") == p );
}

int
main()
{
  test01();
  test02();
}
