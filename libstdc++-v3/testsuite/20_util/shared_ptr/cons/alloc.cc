// { dg-options "-std=gnu++0x" }

// Copyright (C) 2007-2013 Free Software Foundation, Inc.
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
void deletefunc(A* p) { delete p; }
struct D
{
  void operator()(A* p) { delete p; ++delete_count; }
  static long delete_count;
};
long D::delete_count = 0;

// 20.6.6.2.1 shared_ptr constructors [util.smartptr.shared.const]

// Construction with allocator
int
test01()
{
  bool test __attribute__((unused)) = true;
  tracker_allocator_counter::reset();

  std::shared_ptr<A> p1(new A, deletefunc, tracker_allocator<A>());
  std::size_t const sz = tracker_allocator_counter::get_allocation_count();
  VERIFY( sz > 0 );
  {
    std::shared_ptr<A> p2(p1);
    VERIFY( p2.use_count() == 2 );
    VERIFY( tracker_allocator_counter::get_allocation_count() == sz );
    VERIFY( tracker_allocator_counter::get_deallocation_count() == 0 );
  }
  VERIFY( p1.use_count() == 1 );
  VERIFY( tracker_allocator_counter::get_allocation_count() == sz );
  VERIFY( tracker_allocator_counter::get_deallocation_count() == 0 );
  p1.reset();
  VERIFY( p1.use_count() == 0 );
  VERIFY( tracker_allocator_counter::get_allocation_count() == sz );
  VERIFY( tracker_allocator_counter::get_deallocation_count() == sz );

  return 0;
}

// Construction with allocator
int
test02()
{
  bool test __attribute__((unused)) = true;
  tracker_allocator_counter::reset();

  std::shared_ptr<A> p1(new A, deletefunc, tracker_allocator<A>());
  std::size_t const sz1 = tracker_allocator_counter::get_allocation_count();
  VERIFY( sz1 > 0 );
  std::shared_ptr<A> p2(new A, D(), tracker_allocator<A>());
  std::size_t const sz2 = tracker_allocator_counter::get_allocation_count();
  VERIFY( sz2 > sz1 );
  VERIFY( tracker_allocator_counter::get_deallocation_count() == 0 );
  p1 = p2;
  VERIFY( p2.use_count() == 2 );
  VERIFY( tracker_allocator_counter::get_allocation_count() == sz2 );
  VERIFY( tracker_allocator_counter::get_deallocation_count() == sz1 );
  p1.reset();
  VERIFY( p2.use_count() == 1 );
  VERIFY( tracker_allocator_counter::get_allocation_count() == sz2 );
  VERIFY( tracker_allocator_counter::get_deallocation_count() == sz1 );
  p2.reset();
  VERIFY( tracker_allocator_counter::get_allocation_count() == sz2 );
  VERIFY( tracker_allocator_counter::get_deallocation_count() == sz2 );
  VERIFY( D::delete_count == 1 );

  return 0;
}

int
main()
{
  test01();
  test02();
  return 0;
}
