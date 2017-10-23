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

// 8.4.3 path appends [path.append]

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::filesystem::path;

void
test01()
{
  const path p("/foo/bar");

  path pp = p;
  pp /= p;
  VERIFY( pp.native() == p.native() );

  path q("baz");

  path qq = q;
  qq /= q;
  VERIFY( qq.native() == "baz/baz" );

  q /= p;
  VERIFY( q.native() == p.native() );

  path r = "";
  r /= path();
  VERIFY( r.empty() );

  r /= path("rel");
  VERIFY( !r.is_absolute() );

  path s = "dir/";
  s /= path("/file");
  VERIFY( s.native() == "/file" );

  s = "dir/";
  s /= path("file");
  VERIFY( s.native() == "dir/file" );
}

void
test02()
{
  // C++17 [fs.path.append] p4

  path p = path("//host") / "foo";
  VERIFY( p == "//host/foo" );

  path pp = path("//host/") / "foo";
  VERIFY( pp == "//host/foo" );

  path q = path("foo") / "";
  VERIFY( q == "foo/" );

  path qq = path("foo") / "/bar";
  VERIFY( qq == "/bar" );
}

int
main()
{
  test01();
  test02();
}
