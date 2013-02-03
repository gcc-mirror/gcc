// Copyright (C) 2009-2013 Free Software Foundation, Inc.
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

// 25.3.1.2 [lib.stable.sort]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;
using __gnu_test::copy_tracker;
using __gnu_test::copy_constructor;
using __gnu_test::assignment_operator;
using __gnu_test::destructor;

typedef test_container<copy_tracker, random_access_iterator_wrapper> Container;

const int A[] = {10, 20, 1, 11, 2, 21, 28, 29, 12, 35, 15, 27, 6, 16, 7, 
                 25, 17, 8, 23, 18, 9, 19, 24, 30, 13, 4, 14, 22, 26, 0};

void
test_mem1(int throw_count)
{
  bool test __attribute__((unused)) = true;

  copy_tracker vals[30];
  for(int i = 0; i < 30; ++i)
    vals[i] = A[i];

  Container con(vals, vals + 30);
  copy_tracker::reset();
  copy_constructor::throw_on(throw_count);
  int throw_occurred = 0;
  try
    {
      std::stable_sort(con.begin(), con.end());
    }
  catch(...) 
    {
      throw_occurred = 1;
    }

  // If a throw occurred in copy_constructor, we will end up with one more
  // copy_construct than destructor.
  VERIFY( destructor::count() == copy_constructor::count() - throw_occurred );
}

bool
is_ordered(const copy_tracker& lhs, const copy_tracker& rhs)
{ return lhs < rhs; }

void
test_mem2(int throw_count)
{
  bool test __attribute__((unused)) = true;

  copy_tracker vals[30];
  for(int i = 0; i < 30; ++i)
    vals[i] = A[i];

  Container con(vals, vals + 30);
  copy_tracker::reset();
  copy_constructor::throw_on(throw_count);
  int throw_occurred = 0;
  try
    {
      std::stable_sort(con.begin(), con.end(), is_ordered);
    }
  catch(...) 
    {
      throw_occurred = 1;
    }

  // If a throw occurred in copy_constructor, we will end up with one more
  // copy_construct than destructor.
  VERIFY( destructor::count() == copy_constructor::count() - throw_occurred );
}

int main()
{
  for(int i = 0; i < 60; ++i)
  {
    test_mem1(i);
    test_mem2(i);
  }

  return 0;
}
