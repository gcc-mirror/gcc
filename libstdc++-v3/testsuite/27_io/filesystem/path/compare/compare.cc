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

// 8.4.8 path compare [path.compare]

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::filesystem::path;

void
test01()
{
  path p("/foo/bar");
  VERIFY( p.compare(p) == 0 );
  VERIFY( p.compare("/foo//bar") == 0 );

  path q("/foo/baz");
  VERIFY( p.compare(q) < 0 );
  VERIFY( q.compare(p) > 0 );

  path r("/foo/bar/.");
  VERIFY( p.compare(r) < 0 );

  VERIFY( path("a/b/").compare("a/b//") == 0 );
}

int
main()
{
  test01();
}
