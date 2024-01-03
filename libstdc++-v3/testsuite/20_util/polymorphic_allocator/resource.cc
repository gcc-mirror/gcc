// Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }
// { dg-skip-if "" { *-*-* } { -fno-aligned-new } }

#include <memory_resource>
#include <testsuite_allocator.h>

struct X { int i = 0; };

using test_type = std::pmr::polymorphic_allocator<X>;

void
test01()
{
  __gnu_test::memory_resource r;
  test_type a(&r), b(&r);
  VERIFY( a == a );
  VERIFY( ! (a != a) );
  VERIFY( a == b );
  VERIFY( ! (a != b) );
  VERIFY( a.resource() == &r );
  VERIFY( a.resource() == b.resource() );

  __gnu_test::memory_resource r2(r);
  test_type c(&r2);
  VERIFY( c.resource() == &r2 );
  VERIFY( c.resource() != a.resource() );
#if __cpp_rtti
  VERIFY( c == a );
#endif
}

void
test02()
{
  __gnu_test::memory_resource r1, r2;
  test_type a(&r1), b(&r2);
  VERIFY( a == a );
  VERIFY( b == b );
  VERIFY( ! (a == b) );
  VERIFY( ! (b == a) );
  VERIFY( a != b );
  VERIFY( b != a );
  VERIFY( a.resource() == &r1 );
  VERIFY( a.resource() != b.resource() );

  test_type c;
  VERIFY( c == c );
  VERIFY( ! (a == c) );
  VERIFY( ! (c == a) );
  VERIFY( ! (b == c) );
  VERIFY( ! (c == b) );
  VERIFY( a.resource() != c.resource() );
  VERIFY( c.resource() == std::pmr::get_default_resource() );

  std::pmr::set_default_resource(&r1);
  VERIFY( c.resource() != &r1 );

  test_type d;
  VERIFY( d.resource() == &r1 );
  VERIFY( d != c );
  VERIFY( d == a );

  std::pmr::set_default_resource(nullptr);
}

int
main()
{
  test01();
  test02();
}
