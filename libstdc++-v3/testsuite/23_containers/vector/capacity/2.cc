// 1999-05-07
// bkoz 

// Copyright (C) 1999-2019 Free Software Foundation, Inc.
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

// 23.2.4.2 vector capacity

#include <vector>
#include <stdexcept>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

using __gnu_test::copy_tracker;
using __gnu_test::tracker_allocator_counter;
using __gnu_test::tracker_allocator;
using __gnu_test::copy_constructor;
using __gnu_test::assignment_operator;
using __gnu_test::destructor;

// Verifies basic functionality of reserve() with forced reallocation.
void
test_reserve()
{
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  tracker_allocator_counter::reset();
  {
    X a(3);
    const X::size_type old_size     = a.size();
    const X::size_type old_capacity = a.capacity();
    const X::size_type new_capacity = old_capacity + 10;
    T::reset();
    
    a.reserve(new_capacity);

    // [23.2.4.1 (2)]
    VERIFY(new_capacity <= a.capacity());
    // [23.2.4.1 (3)]
    VERIFY(old_size == a.size());
    VERIFY(copy_constructor::count() <= old_size);
    VERIFY(destructor::count() <= old_size);
  }
  // check for memory leaks
  VERIFY(tracker_allocator_counter::get_allocation_count() == tracker_allocator_counter::get_deallocation_count());
}

// Verifies that reserve() with reallocation offers the strong
// exception guarantee.
void
test_reserve_exception_guarantee()
{
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  tracker_allocator_counter::reset();
  {
    X a(7);
    const X::size_type old_size __attribute__((unused)) = a.size();
    const X::size_type old_capacity = a.capacity();
    const X::size_type new_capacity = old_capacity + 10;
    T::reset();
    copy_constructor::throw_on(3);
    
    try
    {
      a.reserve(new_capacity);
      VERIFY(false);
    }
    catch (...)
    {
    }

    VERIFY(old_capacity == a.capacity());
    VERIFY(copy_constructor::count() == destructor::count()+1);
  }
  VERIFY(tracker_allocator_counter::get_allocation_count() == tracker_allocator_counter::get_deallocation_count());
}

int main()
{
  test_reserve();
  test_reserve_exception_guarantee();
  return 0;
}
