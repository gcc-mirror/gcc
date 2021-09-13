// { dg-do run { target c++11 } }

// Copyright (C) 2011-2021 Free Software Foundation, Inc.
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

#undef _GLIBCXX_CONCEPT_CHECKS

// XXX FIXME:  parallel-mode should deal correctly with moveable-only types
// per C++0x, at minimum smoothly fall back to serial.
#undef _GLIBCXX_PARALLEL

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;
using __gnu_test::rvalstruct;

typedef test_container<rvalstruct, random_access_iterator_wrapper> Container;

const int A[] = { 10 };
const int N = 1;

bool order(const rvalstruct& lhs, const rvalstruct& rhs)
{ return lhs < rhs; }

// libstdc++/49559
void test01()
{
  rvalstruct s1[1];
  std::copy(A, A + 1, s1);
  Container con1(s1, s1 + 1);
  std::stable_sort(con1.begin(), con1.end());
  VERIFY( s1[0] == 10 );
  VERIFY( s1[0].valid );

  rvalstruct s2[1];
  std::copy(A, A + 1, s2);
  Container con2(s2, s2 + 1);
  std::stable_sort(con2.begin(), con2.end(), order);
  VERIFY( s2[0] == 10 );
  VERIFY( s2[0].valid );
}

int
main()
{
  test01();
  return 0;
}
