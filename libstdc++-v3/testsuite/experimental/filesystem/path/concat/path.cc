// { dg-options "-DUSE_FILESYSTEM_TS -lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

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

// 8.4.4 path concatenation [path.concat]

#include <experimental/filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::experimental::filesystem::path;

void
test01()
{
  const path p("/foo/bar");

  path pp = p;
  pp += p;
  VERIFY( pp.string() == "/foo/bar/foo/bar" );
  VERIFY( std::distance(pp.begin(), pp.end()) == 5 );

  path q("foo/bar");

  path qq = q;
  qq += q;
  VERIFY( qq.string() == "foo/barfoo/bar" );
  VERIFY( std::distance(qq.begin(), qq.end()) == 3 );

  q += p;
  VERIFY( q.string() == "foo/bar/foo/bar" );
  VERIFY( std::distance(q.begin(), q.end()) == 4 );
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
  }
}

int
main()
{
  test01();
  test02();
}
