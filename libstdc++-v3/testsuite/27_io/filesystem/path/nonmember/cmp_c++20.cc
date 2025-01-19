// { dg-do run { target c++20 } }

// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// C++20 29.11.7.7 Non-member functions [fs.path.nonmember]

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::filesystem::path;

void
test01()
{
  path p("/foo/bar");
  VERIFY( p == p );
  VERIFY( p == "/foo//bar" );
  VERIFY( std::is_eq(p <=> p) );
  VERIFY( std::is_eq(p <=> "/foo//bar") );

  path q("/foo/baz");
  VERIFY( p < q );
  VERIFY( q > p );
  VERIFY( std::is_lt(p <=> q) );
  VERIFY( std::is_gt(q <=> p) );

  path r("/foo/bar/.");
  VERIFY( p < r );
  VERIFY( std::is_lt(p <=> r) );

  VERIFY( path("a/b/") == path("a/b//") );
  VERIFY( std::is_eq(path("a/b/") <=> path("a/b//")) );
}

void
test02()
{
  const path p0 = "/a/a/b/b";
  for (const path p : __gnu_test::test_paths)
  {
    VERIFY( std::is_eq(p <=> p) );
    VERIFY( (p <=> p0) == (p.compare(p0) <=> 0) );
    VERIFY( (p0 <=> p) == (p0.compare(p) <=> 0) );
  }
}

void
test03()
{
  VERIFY( std::is_eq(path("/") <=> path("////")) );
  VERIFY( std::is_gt(path("/a") <=> path("/")) );
  VERIFY( std::is_lt(path("/") <=> path("/a")) );
  VERIFY( std::is_gt(path("/ab") <=> path("/a")) );
  VERIFY( std::is_gt(path("/ab") <=> path("/a/b")) );
}

int
main()
{
  test01();
  test02();
  test03();
}
