// { dg-do run { target c++11 } }
// { dg-require-effective-target std_allocator_new }
// { dg-xfail-run-if "AIX operator new" { powerpc-ibm-aix* } }

// 2010-01-08  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2025 Free Software Foundation, Inc.
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

#include <deque>
#include <testsuite_hooks.h>
#include <replacement_memory_operators.h>

// libstdc++/42573
void test01()
{
  using namespace std;
  __gnu_test::counter::reset();

  const size_t buf_size = _GLIBCXX_STD_C::__deque_buf_size(sizeof(size_t));
  deque<size_t> d;
  for (size_t i = 0; i != buf_size; ++i)
    d.push_back(i);

  // No shrink if 1st buffer is full, create some front capacity.
  d.pop_front();

  // 1 node array allocation + 2 node allocation = 3.
  VERIFY( __gnu_test::counter::count() == 3 );
  VERIFY( __gnu_test::counter::get()._M_increments == 3 );

  d.shrink_to_fit();

  // No reallocation if no exception support, shrink_to_fit is then a
  // no-op.
#if __cpp_exceptions
  // 1 node array allocation + 1 node allocation = 2.
  const int expected_count = 2;
  const int expected_increments = 2;
#else
  const int expected_count = 3;
  const int expected_increments = 0;
#endif
  VERIFY( __gnu_test::counter::count() == expected_count );
  VERIFY( __gnu_test::counter::get()._M_increments == 3 + expected_increments );
}

int main()
{
  test01();
  return 0;
}
