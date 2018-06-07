// Copyright (C) 2017-2018 Free Software Foundation, Inc.
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
#include <string.h>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using std::filesystem::path;

void
test01()
{
  // path(string_type&&, format)
  auto s = [&]() -> path::string_type { return path("foo/bar").native(); };
  path p0(s());
  path p1(s(), path::auto_format);
  VERIFY( p1 == p0 );
  path p2(s(), path::native_format);
  VERIFY( p2 == p0 );
  path p3(s(), path::generic_format);
  VERIFY( p3 == p0 );
}

void
test02()
{
  // path(const Source&, format)
  const path::string_type s = path("foo/bar").native();
  path p0(s);
  path p1(s, path::auto_format);
  VERIFY( p1 == p0 );
  path p2(s, path::native_format);
  VERIFY( p2 == p0 );
  path p3(s, path::generic_format);
  VERIFY( p3 == p0 );
}

void
test03()
{
  // path(const Source&, format)
  const std::string s = "foo/bar";
  path p0(s);
  path p1(s, path::auto_format);
  VERIFY( p1 == p0 );
  path p2(s, path::native_format);
  VERIFY( p2 == p0 );
  path p3(s, path::generic_format);
  VERIFY( p3 == p0 );
}

void
test04()
{
#ifdef _GLIBCXX_USE_WCHAR_T
  // path(const Source&, format)
  const std::wstring s = L"foo/bar";
  path p0(s);
  path p1(s, path::auto_format);
  VERIFY( p1 == p0 );
  path p2(s, path::native_format);
  VERIFY( p2 == p0 );
  path p3(s, path::generic_format);
  VERIFY( p3 == p0 );
#endif
}

void
test05()
{
  // path(const Source&, format)
  const char* s = "foo/bar";
  path p0(s);
  path p1(s, path::auto_format);
  VERIFY( p1 == p0 );
  path p2(s, path::native_format);
  VERIFY( p2 == p0 );
  path p3(s, path::generic_format);
  VERIFY( p3 == p0 );
}

void
test06()
{
  // path(InputIterator, InputIterator, format)
  const char s[] = "foo/bar";
  using namespace __gnu_test;
  const test_container<const char, input_iterator_wrapper> c(s, s + strlen(s));
  auto c0 = c;
  path p0(std::begin(c0), std::end(c0));
  auto c1 = c;
  path p1(std::begin(c1), std::end(c1), path::auto_format);
  VERIFY( p1 == p0 );
  auto c2 = c;
  path p2(std::begin(c2), std::end(c2), path::native_format);
  VERIFY( p2 == p0 );
  auto c3 = c;
  path p3(std::begin(c3), std::end(c3), path::generic_format);
  VERIFY( p3 == p0 );
}

void
test07()
{
  // path(const Source&, const locale&, format)
  const char* s = "foo/bar";
  std::locale loc;
  path p0(s, loc);
  path p1(s, loc, path::auto_format);
  VERIFY( p1 == p0 );
  path p2(s, loc, path::native_format);
  VERIFY( p2 == p0 );
  path p3(s, loc, path::generic_format);
  VERIFY( p3 == p0 );
}

void
test08()
{
  // path(InputIterator, InputIterator, const locale&, format)
  const char s[] = "foo/bar";
  using namespace __gnu_test;
  const test_container<const char, input_iterator_wrapper> c(s, s + strlen(s));
  std::locale loc;
  auto c0 = c;
  path p0(std::begin(c0), std::end(c0), loc);
  auto c1 = c;
  path p1(std::begin(c1), std::end(c1), loc, path::auto_format);
  VERIFY( p1 == p0 );
  auto c2 = c;
  path p2(std::begin(c2), std::end(c2), loc, path::native_format);
  VERIFY( p2 == p0 );
  auto c3 = c;
  path p3(std::begin(c3), std::end(c3), loc, path::generic_format);
  VERIFY( p3 == p0 );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();
}
