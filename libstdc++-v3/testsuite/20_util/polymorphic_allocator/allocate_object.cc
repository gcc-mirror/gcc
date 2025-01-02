// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

#include <memory_resource>
#include <cstring>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

void
test01()
{
  __gnu_test::memory_resource res;
  std::pmr::polymorphic_allocator<> alloc(&res);
  static_assert( std::is_same_v<decltype(alloc)::value_type, std::byte> );

  void* p = alloc.allocate_bytes(100);
  VERIFY( res.number_of_active_allocations() == 1 );
  alloc.deallocate_bytes(p, 100);
  VERIFY( res.number_of_active_allocations() == 0 );
  p = alloc.allocate_bytes(100, 64);
  VERIFY( res.number_of_active_allocations() == 1 );
  alloc.deallocate_bytes(p, 100, 64);
  VERIFY( res.number_of_active_allocations() == 0 );

  int* p1 = alloc.allocate_object<int>();
  int* p2 = alloc.allocate_object<int>(2);
  struct X { double d[10]; };
  X* px = alloc.allocate_object<X>(20);
  VERIFY( res.number_of_active_allocations() == 3 );

  alloc.deallocate_object(p1);
  alloc.deallocate_object(p2, 2);
  alloc.deallocate_object(px, 20);
  VERIFY( res.number_of_active_allocations() == 0 );

  struct Y {
    Y(int i, const char* s, bool* alive)
    : i(i), s(s), alive(alive)
    { *alive = true; }

    ~Y() { *alive = false; }

    int i;
    const char* s;
    bool* alive;
  };

  bool alive = false;
  Y* py = alloc.new_object<Y>(1, "two", &alive);
  VERIFY( alive );
  VERIFY( py->i == 1 );
  VERIFY( std::strcmp(py->s, "two") == 0 );
  VERIFY( res.number_of_active_allocations() == 1 );

  alloc.delete_object(py);
  VERIFY( alive == false );
}

int
main()
{
  test01();
}
