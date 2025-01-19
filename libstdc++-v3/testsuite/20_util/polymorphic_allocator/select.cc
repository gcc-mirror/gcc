// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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
  test_type a, b;
  VERIFY( a.select_on_container_copy_construction() == a );
  VERIFY( a.select_on_container_copy_construction() == b );

  __gnu_test::memory_resource r;
  test_type c(&r);
  VERIFY( c.select_on_container_copy_construction() != c );
  VERIFY( c.select_on_container_copy_construction() == a );
}

void
test02()
{
  __gnu_test::memory_resource r;
  test_type a(&r);
  VERIFY( a.select_on_container_copy_construction() != a );
  std::pmr::set_default_resource(&r);
  VERIFY( a.select_on_container_copy_construction() == a );
  std::pmr::set_default_resource(nullptr);
}

int
main()
{
  test01();
  test02();
}
