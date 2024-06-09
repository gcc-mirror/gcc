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

#include <memory_resource>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

void
test01()
{
  __gnu_test::memory_resource test_mr1, test_mr2;
  __gnu_test::default_resource_mgr mgr(&test_mr1);

  const std::pmr::pool_options opts{1, 2};
  using std::pmr::unsynchronized_pool_resource;

  unsynchronized_pool_resource p1 = {opts, &test_mr2};
  VERIFY( p1.upstream_resource() == &test_mr2 );
  unsynchronized_pool_resource p2;
  VERIFY( p2.upstream_resource() == std::pmr::get_default_resource() );
  unsynchronized_pool_resource p3{&test_mr2};
  VERIFY( p3.upstream_resource() == &test_mr2 );
  unsynchronized_pool_resource p4{opts};
  VERIFY( p4.upstream_resource() == std::pmr::get_default_resource() );

  static_assert(!std::is_copy_constructible_v<unsynchronized_pool_resource>);
  static_assert(!std::is_copy_assignable_v<unsynchronized_pool_resource>);
  static_assert(std::is_destructible_v<unsynchronized_pool_resource>);
}

void
test02()
{
  __gnu_test::memory_resource test_mr1, test_mr2;
  __gnu_test::default_resource_mgr mgr(&test_mr1);

  const std::pmr::pool_options opts{1, 2};

  struct derived : std::pmr::unsynchronized_pool_resource
  {
    using unsynchronized_pool_resource::unsynchronized_pool_resource;
  };

  derived p1 = {opts, &test_mr2};
  VERIFY( p1.upstream_resource() == &test_mr2 );
  derived p2;
  VERIFY( p2.upstream_resource() == std::pmr::get_default_resource() );
  derived p3{&test_mr2};
  VERIFY( p3.upstream_resource() == &test_mr2 );
  derived p4{opts};
  VERIFY( p4.upstream_resource() == std::pmr::get_default_resource() );

  static_assert(!std::is_copy_constructible_v<derived>);
  static_assert(!std::is_copy_assignable_v<derived>);
  static_assert(std::is_destructible_v<derived>);
}

int
main()
{
  test01();
  test02();
}
