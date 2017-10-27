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
  auto s = [&]() -> path::string_type { return "foo/bar"; };
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
  path::string_type s = "foo/bar";
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
test04()
{
  const char s[] = "foo/bar";
  path p0(std::begin(s), std::end(s));
  path p1(std::begin(s), std::end(s), path::auto_format);
  VERIFY( p1 == p0 );
  path p2(std::begin(s), std::end(s), path::native_format);
  VERIFY( p2 == p0 );
  path p3(std::begin(s), std::end(s), path::generic_format);
  VERIFY( p3 == p0 );
}

void
test05()
{
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
test06()
{
  const char s[] = "foo/bar";
  std::locale loc;
  path p0(std::begin(s), std::end(s), loc);
  path p1(std::begin(s), std::end(s), loc, path::auto_format);
  VERIFY( p1 == p0 );
  path p2(std::begin(s), std::end(s), loc, path::native_format);
  VERIFY( p2 == p0 );
  path p3(std::begin(s), std::end(s), loc, path::generic_format);
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
}
