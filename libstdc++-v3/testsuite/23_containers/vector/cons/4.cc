// 1999-06-29 bkoz

// Copyright (C) 1999-2001, 2002, 2003, 2009 Free Software Foundation, Inc.
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

// 23.2.4.1 vector constructors, copy, and assignment

#include <vector>
#include <string>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

using __gnu_test::copy_tracker;
using __gnu_test::tracker_allocator_counter;
using __gnu_test::tracker_allocator;
using __gnu_test::copy_constructor;
using __gnu_test::assignment_operator;

// @fn test_default_ctor_exception_gurantee This test verifies that if
// one of the vector's contained objects throws an exception from its
// constructor while the vector is being constructed and filled with
// default values, all memory is returned to the allocator whence it
// came.
void
test_default_ctor_exception_gurantee()
{
  // setup
  bool test __attribute__((unused)) = true;
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  copy_tracker::reset();
  copy_constructor::throw_on(3);
  tracker_allocator_counter::reset();

  // run test
  try
  {
    X a(7);
    VERIFY(false);
  }
  catch (...)
  {
  }

  // assert postconditions
  VERIFY( tracker_allocator_counter::get_allocation_count() == tracker_allocator_counter::get_deallocation_count() );

  // teardown
}

// @fn test_copy_ctor_exception_gurantee This test verifies that if
// one of the vector's contained objects throws an exception from its
// constructor while the vector is being copy constructed, all memory
// is returned to the allocator whence it came.
void
test_copy_ctor_exception_gurantee()
{
  // setup
  bool test __attribute__((unused)) = true;
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  tracker_allocator_counter::reset();
  {
    X a(7);
    copy_tracker::reset();
    copy_constructor::throw_on(3);

    // run test
    try
    {
      X u(a);
      VERIFY(false);
    }
    catch (...)
    {
    }
  }

  // assert postconditions
  VERIFY(tracker_allocator_counter::get_allocation_count() == tracker_allocator_counter::get_deallocation_count());

  // teardown
  copy_tracker::reset();
  tracker_allocator_counter::reset();
}

// operator=()
//
// case 1: lhs.size() > rhs.size()
// case 2: lhs.size() < rhs.size() < lhs.capacity()
// case 3: lhs.capacity() < rhs.size()
//
void
test_assignment_operator_1()
{
  // setup
  bool test __attribute__((unused)) = true;
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  X r(9);
  X a(r.size() - 2);
  copy_tracker::reset();
  tracker_allocator_counter::reset();

  // preconditions
  VERIFY(r.size() > a.size());

  // run test
  r = a;

  // assert postconditions
  VERIFY(r == a);
  VERIFY(tracker_allocator_counter::get_allocation_count() == 0);

  // teardown
  copy_tracker::reset();
  tracker_allocator_counter::reset();
}

void
test_assignment_operator_2()
{
  // setup
  bool test __attribute__((unused)) = true;
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  X r(1);
  r.reserve(17);
  X a(r.size() + 7);
  copy_tracker::reset();
  tracker_allocator_counter::reset();

  // preconditions
  VERIFY(r.size() < a.size());
  VERIFY(a.size() < r.capacity());

  // run test
  r = a;

  // assert postconditions
  VERIFY(r == a);
  VERIFY(tracker_allocator_counter::get_allocation_count() == 0);

  // teardown
  copy_tracker::reset();
  tracker_allocator_counter::reset();
}

void
test_assignment_operator_3()
{
  // setup
  bool test __attribute__((unused)) = true;
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  tracker_allocator_counter::reset();
  {
    X r(1);
    X a(r.capacity() + 7);
    copy_tracker::reset();

    // preconditions
    VERIFY(r.capacity() < a.size());

    // run test
    r = a;

    // assert postconditions
    VERIFY(r == a);
  }
  VERIFY(tracker_allocator_counter::get_allocation_count() == tracker_allocator_counter::get_deallocation_count());

  // teardown
  copy_tracker::reset();
  tracker_allocator_counter::reset();
}

void
test_assignment_operator_3_exception_guarantee()
{
  // setup
  bool test __attribute__((unused)) = true;
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  tracker_allocator_counter::reset();
  {
    X r(1);
    X a(r.capacity() + 7);
    copy_tracker::reset();
    copy_constructor::throw_on(3);

    // preconditions
    VERIFY(r.capacity() < a.size());

    // run test
    try
    {
      r = a;
      VERIFY(false);
    }
    catch (...)
    {
    }
  }

  // assert postconditions
  VERIFY(tracker_allocator_counter::get_allocation_count() == tracker_allocator_counter::get_deallocation_count());

  // teardown
  copy_tracker::reset();
  tracker_allocator_counter::reset();
}

// fill assign()
//
// case 1: [23.2.4.1 (3)] n <= size()
// case 2: [23.2.4.1 (3)] size() < n <= capacity()
// case 3: [23.2.4.1 (3)] n > capacity()
// case 4: [23.2.4.1 (3)] n > capacity(), exception guarantees
// case 5: [23.1.1 (9)] fill assign disguised as a range assign
//
void
test_fill_assign_1()
{
  // setup
  bool test __attribute__((unused)) = true;
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  X a(7);
  X::size_type old_size = a.size();
  X::size_type new_size = old_size - 2;
  const T t;

  copy_tracker::reset();
  tracker_allocator_counter::reset();

  // run test
  a.assign(new_size, t);

  // assert postconditions
  VERIFY(a.size() == new_size);
  VERIFY(tracker_allocator_counter::get_allocation_count() == 0);

  // teardown
  copy_tracker::reset();
  tracker_allocator_counter::reset();
}

void
test_fill_assign_2()
{
  // setup
  bool test __attribute__((unused)) = true;
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  X a(7);
  a.reserve(11);
  X::size_type old_size     = a.size();
  X::size_type old_capacity = a.capacity();
  X::size_type new_size     = old_size + 2;
  const T t;

  copy_tracker::reset();
  tracker_allocator_counter::reset();

  // assert preconditions
  VERIFY(old_size < new_size);
  VERIFY(new_size <= old_capacity);

  // run test
  a.assign(new_size, t);

  // assert postconditions
  VERIFY(a.size() == new_size);
  VERIFY(tracker_allocator_counter::get_allocation_count() == 0);

  // teardown
  copy_tracker::reset();
  tracker_allocator_counter::reset();
}

void
test_fill_assign_3()
{
  // setup
  bool test __attribute__((unused)) = true;
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  tracker_allocator_counter::reset();
  {
    X a(7);
    X::size_type old_capacity = a.capacity();
    X::size_type new_size     = old_capacity + 4;
    const T t;

    copy_tracker::reset();

    // assert preconditions
    VERIFY(new_size > old_capacity);

    // run test
    a.assign(new_size, t);

    // assert postconditions
    VERIFY(a.size() == new_size);
  }

  VERIFY(tracker_allocator_counter::get_allocation_count() > 0);
  VERIFY(tracker_allocator_counter::get_allocation_count() == tracker_allocator_counter::get_deallocation_count());

  // teardown
  copy_tracker::reset();
  tracker_allocator_counter::reset();
}

void
test_fill_assign_3_exception_guarantee()
{
  // setup
  bool test __attribute__((unused)) = true;
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  tracker_allocator_counter::reset();
  {
    X a(7);
    X::size_type old_size     = a.size();
    X::size_type old_capacity = a.capacity();
    X::size_type new_size     = old_capacity + 4;
    const T t;

    copy_tracker::reset();
    copy_constructor::throw_on(3);

    // assert preconditions
    VERIFY(new_size > old_capacity);

    // run test
    try
    {
      a.assign(new_size, t);
      VERIFY(false);
    }
    catch (...)
    {
    }

    // assert postconditions
    VERIFY(a.size() == old_size);
    VERIFY(a.capacity() == old_capacity);
  }

  VERIFY(tracker_allocator_counter::get_allocation_count() > 0);
  VERIFY(tracker_allocator_counter::get_allocation_count() == tracker_allocator_counter::get_deallocation_count());

  // teardown
  copy_tracker::reset();
  tracker_allocator_counter::reset();
}

void
test_fill_assign_4()
{
  // setup
  bool test __attribute__((unused)) = true;
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  X a(7);
  X::size_type old_size  = a.size();
  X::size_type new_size  = old_size - 2;
  X::size_type new_value = 117;

  copy_tracker::reset();
  tracker_allocator_counter::reset();

  // run test
  a.assign(new_size, new_value);

  // assert postconditions
  VERIFY(a.size() == new_size);
  VERIFY(tracker_allocator_counter::get_allocation_count() == 0);

  // teardown
  copy_tracker::reset();
  tracker_allocator_counter::reset();
}

// range assign()
//
// case 1: [23.2.4.1 (2)] input iterator
// case 2: [23.2.4.1 (2)] forward iterator, distance(first, last) <= size()
// case 3: [23.2.4.1 (2)] 
//         forward iterator, size() < distance(first, last) <= capacity()
// case 4: [23.2.4.1 (2)] forward iterator, distance(first, last) > capacity()
// case 5: [23.2.4.1 (2)] 
//         forward iterator, distance(first, last) > capacity(), 
//         exception guarantees
void
test_range_assign_1()
{
  // @TODO
}

void
test_range_assign_2()
{
  // setup
  bool test __attribute__((unused)) = true;
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  X a(7);
  X b(3);

  copy_tracker::reset();
  tracker_allocator_counter::reset();

  // assert preconditions
  VERIFY(b.size() < a.capacity());

  // run test
  a.assign(b.begin(), b.end());

  // assert postconditions
  VERIFY(a.size() == b.size());
  VERIFY(a == b);
  VERIFY(tracker_allocator_counter::get_allocation_count() == 0);

  // teardown
  copy_tracker::reset();
  tracker_allocator_counter::reset();
}

void
test_range_assign_3()
{
  // setup
  bool test __attribute__((unused)) = true;
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  X a(7);
  a.reserve(a.size() + 7);
  X b(a.size() + 3);

  copy_tracker::reset();
  tracker_allocator_counter::reset();

  // assert preconditions
  VERIFY(a.size() < b.size());
  VERIFY(b.size() < a.capacity());

  // run test
  a.assign(b.begin(), b.end());

  // assert postconditions
  VERIFY(a.size() == b.size());
  VERIFY(a == b);
  VERIFY(tracker_allocator_counter::get_allocation_count() == 0);

  // teardown
  copy_tracker::reset();
  tracker_allocator_counter::reset();
}

void
test_range_assign_4()
{
  // setup
  bool test __attribute__((unused)) = true;
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  tracker_allocator_counter::reset();
  {
    X a(7);
    X b(a.capacity() + 7);

    copy_tracker::reset();

    // assert preconditions
    VERIFY(b.size() > a.capacity());

    // run test
    a.assign(b.begin(), b.end());

    // assert postconditions
    VERIFY(a.size() == b.size());
    VERIFY(a == b);
  }
  VERIFY(tracker_allocator_counter::get_allocation_count() > 0);
  VERIFY(tracker_allocator_counter::get_allocation_count() == tracker_allocator_counter::get_deallocation_count());

  // teardown
  copy_tracker::reset();
  tracker_allocator_counter::reset();
}

void
test_range_assign_4_exception_guarantee()
{
  // setup
  bool test __attribute__((unused)) = true;
  typedef copy_tracker T;
  typedef std::vector<T, tracker_allocator<T> > X;

  tracker_allocator_counter::reset();
  {
    X a(7);
    X b(a.capacity() + 7);

    copy_tracker::reset();
    copy_constructor::throw_on(3);

    // assert preconditions
    VERIFY(b.size() > a.capacity());

    // run test
    try
    {
      a.assign(b.begin(), b.end());
      VERIFY(false);
    }
    catch (...)
    {
    }
  }

  // assert postconditions
  VERIFY(tracker_allocator_counter::get_allocation_count() > 0);
  VERIFY(tracker_allocator_counter::get_allocation_count() == tracker_allocator_counter::get_deallocation_count());

  // teardown
  copy_tracker::reset();
  tracker_allocator_counter::reset();
}


int main()
{
  test_default_ctor_exception_gurantee();
  test_copy_ctor_exception_gurantee();
  test_assignment_operator_1();
  test_assignment_operator_2();
  test_assignment_operator_3();
  test_assignment_operator_3_exception_guarantee();
  test_fill_assign_1();
  test_fill_assign_2();
  test_fill_assign_3();
  test_fill_assign_3_exception_guarantee();
  test_fill_assign_4();
  test_range_assign_1();
  test_range_assign_2();
  test_range_assign_3();
  test_range_assign_4();
  test_range_assign_4_exception_guarantee();

  return 0;
}
