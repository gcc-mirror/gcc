// { dg-options "-std=gnu++0x" }

// Copyright (C) 2007-2014 Free Software Foundation, Inc.
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

// 20.6.6.2 Template class shared_ptr [util.smartptr.shared]

#include <memory>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

using __gnu_test::tracker_allocator_counter;
using __gnu_test::tracker_allocator;

struct A { };
struct B : A { };
struct D
{
  void operator()(B* p) { delete p; ++delete_count; }
  static long delete_count;
};
long D::delete_count = 0;

// 20.6.6.2.4 shared_ptr modifiers [util.smartptr.shared.mod]

// Reset with allocator
int
test01()
{
  bool test __attribute__((unused)) = true;
  tracker_allocator_counter::reset();

  {
    std::shared_ptr<A> p1;
    p1.reset(new B, D(), tracker_allocator<B>());
    VERIFY( tracker_allocator_counter::get_allocation_count() > 0 );
  }
  VERIFY( D::delete_count == 1 );
  VERIFY( tracker_allocator_counter::get_allocation_count() == tracker_allocator_counter::get_deallocation_count() );

  return 0;
}   

int
main()
{
  test01();
  return 0;
}
