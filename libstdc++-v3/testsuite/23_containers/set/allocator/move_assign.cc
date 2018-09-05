// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }

#include <set>
#include <random>

#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

struct T { int i; };

bool operator<(T l, T r) { return l.i < r.i; }

using Cmp = std::less<T>;

using __gnu_test::propagating_allocator;

void test01()
{
  typedef propagating_allocator<T, false> alloc_type;
  typedef std::set<T, Cmp, alloc_type> test_type;
  test_type v1(alloc_type(1));
  v1 = { test_type::value_type{} };
  test_type v2(alloc_type(2));
  v2 = { test_type::value_type{} };
  v2 = std::move(v1);
  VERIFY(1 == v1.get_allocator().get_personality());
  VERIFY(2 == v2.get_allocator().get_personality());
}

void test02()
{
  typedef propagating_allocator<T, true> alloc_type;
  typedef std::set<T, Cmp, alloc_type> test_type;
  test_type v1(alloc_type(1));
  v1 = { test_type::value_type{} };
  auto it = v1.begin();
  test_type v2(alloc_type(2));
  v2 = { test_type::value_type{} };
  v2 = std::move(v1);
  VERIFY(0 == v1.get_allocator().get_personality());
  VERIFY(1 == v2.get_allocator().get_personality());
  VERIFY( it == v2.begin() );
}

void test03()
{
  using namespace __gnu_test;

  typedef propagating_allocator<int, false, tracker_allocator<int>> alloc_type;
  typedef std::set<int, std::less<int>, alloc_type> test_type;

  tracker_allocator_counter::reset();

  test_type v1(alloc_type(1));
  v1 = { 0, 1 };

  test_type v2(alloc_type(2));
  v2 = { 2, 3 };

  auto allocs = tracker_allocator_counter::get_allocation_count();
  auto constructs = tracker_allocator_counter::get_construct_count();

  // Check no allocation on move assignment with non propagating allocators.
  v1 = std::move(v2);

  VERIFY( 1 == v1.get_allocator().get_personality() );
  VERIFY( 2 == v2.get_allocator().get_personality() );

  VERIFY( tracker_allocator_counter::get_allocation_count() == allocs );
  VERIFY( tracker_allocator_counter::get_construct_count() == constructs + 2 );
}

void test04()
{
  using namespace __gnu_test;

  typedef tracker_allocator<int> alloc_type;
  typedef std::set<int, std::less<int>, alloc_type> test_type;

  std::mt19937 rng;
  std::uniform_int_distribution<int> d;
  std::uniform_int_distribution<int>::param_type p{0, 100};
  std::uniform_int_distribution<int>::param_type x{0, 1000};

  for (int i = 0; i < 10; ++i)
  {
    test_type l, r;
    for (int n = d(rng, p); n > 0; --n)
    {
      int i = d(rng, x);
      l.insert(i);
      r.insert(i);

      tracker_allocator_counter::reset();

      l = r;

      VERIFY( tracker_allocator_counter::get_allocation_count() == 0 );
    }
  }
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}
