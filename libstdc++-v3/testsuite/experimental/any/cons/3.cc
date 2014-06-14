// { dg-options "-std=gnu++14" }
// { dg-do run }

// Copyright (C) 2014 Free Software Foundation, Inc.
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

#include <experimental/any>
#include <testsuite_allocator.h>

using std::experimental::any;
using __gnu_test::CustomPointerAlloc;
using __gnu_test::tracker_allocator;
using __gnu_test::tracker_allocator_counter;

struct NotSmall { char c[64]; };

bool test [[gnu::unused]] = true;

void test01()
{
  CustomPointerAlloc<int> alloc;

  any x(std::allocator_arg, alloc, 1);
  VERIFY( !x.empty() );

  any y(std::allocator_arg, alloc, std::move(x));
  VERIFY( x.empty() );
  VERIFY( !y.empty() );
}

void test02()
{
  tracker_allocator<int> alloc;

  any x(std::allocator_arg, alloc, 1);
  auto allocated = tracker_allocator_counter::get_allocation_count();
  VERIFY( allocated == 0 );  // no allocation for small object

  any y(std::allocator_arg, alloc, std::move(x));
  VERIFY( tracker_allocator_counter::get_allocation_count() == 0 );

  y = {};
  VERIFY( tracker_allocator_counter::get_deallocation_count() == 0 );
}

void test03()
{
  tracker_allocator<int> alloc;


  any x(std::allocator_arg, alloc, NotSmall{});
  auto allocated = tracker_allocator_counter::get_allocation_count();
  __builtin_printf("ALLOCATED %lu\n", (unsigned long)allocated);
  VERIFY( allocated >= sizeof(NotSmall) );

  any y(std::allocator_arg, alloc, std::move(x));
  VERIFY( tracker_allocator_counter::get_allocation_count() == allocated );

  y = {};
  VERIFY( tracker_allocator_counter::get_deallocation_count() == allocated );
}


int main()
{
  test01();
  test02();
  test03();
}
