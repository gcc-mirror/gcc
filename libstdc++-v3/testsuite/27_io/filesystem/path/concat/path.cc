// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

// Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

// 30.10.7.4.4 path concatenation [fs.path.concat]

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::filesystem::path;
using __gnu_test::compare_paths;

void
test01()
{
  const path p("/foo/bar");

  path pp = p;
  pp += p;
  compare_paths( pp, "/foo/bar/foo/bar" );

  path q("foo/bar");

  path qq = q;
  qq += q;
  compare_paths( qq, "foo/barfoo/bar" );

  q += p;
  compare_paths( q, "foo/bar/foo/bar" );
}

void
test02()
{
  for (path p : __gnu_test::test_paths)
  {
    auto prior_native = p.native();
    path x("//blah/di/blah");
    p += x;
    VERIFY( p.native() == prior_native + x.native() );
    path copy(p);
    compare_paths( copy, p );
  }
}

void
test03()
{
  path p = "a/";
  p += path("/b");
  compare_paths(p, "a//b");
}

void
test04()
{
  // Concat every test path onto every test path.
  for (path p : __gnu_test::test_paths)
  {
    for (path x : __gnu_test::test_paths)
    {
      auto prior_native = p.native();
      p += x;
      VERIFY( p.native() == prior_native + x.native() );
      path copy(p); // PR libstdc++/98523
      compare_paths( copy, p );
    }
  }
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
