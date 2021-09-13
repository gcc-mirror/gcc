// { dg-do run { target c++17 } }

// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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

// C++17 30.10.8.6 path non-member functions [fs.path.nonmember]

#include <filesystem>
#include <testsuite_fs.h>

using std::filesystem::path;
using __gnu_test::compare_paths;

// operator/(const path&, const path&)
// Equivalent to: return path(lhs) /= rhs;

void test(const path& lhs, const path& rhs)
{
  compare_paths( lhs / rhs, path(lhs) /= rhs );
}

void
test01()
{
  test( "/foo/bar", "/foo/" );

  test( "baz", "baz" );
  test( "baz/", "baz" );
  test( "baz", "/foo/bar" );
  test( "baz/", "/foo/bar" );

  test( "", "" );
  test( "", "rel" );

  test( "dir/", "/file" );
  test( "dir/", "file" );
}

void
test02()
{
  // C++17 [fs.path.append] p4
  test( "//host", "foo" );
  test( "//host/", "foo" );
  test( "foo", "" );
  test( "foo", "/bar" );
  test( "foo", "c:/bar" );
  test( "foo", "c:" );
  test( "c:", "" );
  test( "c:foo", "/bar" );
  test( "foo", "c:\\bar" );
}

void
test03()
{
  for (const path p : __gnu_test::test_paths)
    for (const path q : __gnu_test::test_paths)
      test(p, q);
}

int
main()
{
  test01();
  test02();
  test03();
}
