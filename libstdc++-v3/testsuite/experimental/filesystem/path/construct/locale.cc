// { dg-options "-DUSE_FILESYSTEM_TS -lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

// 8.4.1 path constructors [path.construct]

#include <experimental/filesystem>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using std::experimental::filesystem::path;

void
test01()
{
  path p("/foo/bar", std::locale::classic());
  VERIFY( p.string() == "/foo/bar" );
}

void
test02()
{
  using __gnu_test::test_container;
  using __gnu_test::input_iterator_wrapper;
  // Test with input iterators and const value_types

  const std::locale loc;
  const std::string s = "foo/bar/";
  const path p0(s);

  test_container<char, input_iterator_wrapper>
      r1((char*)s.c_str(), (char*)s.c_str() + s.size());
  path p1(r1.begin(), r1.end(), loc);
  VERIFY( p1 == p0 );

  test_container<char, input_iterator_wrapper>
    r2((char*)s.c_str(), (char*)s.c_str() + s.size() + 1); // includes null-terminator
  path p2(r2.begin(), loc);
  VERIFY( p2 == p0 );

  test_container<const char, input_iterator_wrapper>
    r3(s.c_str(), s.c_str() + s.size());
  path p3(r3.begin(), r3.end(), loc);
  VERIFY( p3 == p0 );

  test_container<const char, input_iterator_wrapper>
    r4(s.c_str(), s.c_str() + s.size() + 1); // includes null-terminator
  path p4(r4.begin(), loc);
  VERIFY( p4 == p0 );
}

int
main()
{
  test01();
  test02();
}
