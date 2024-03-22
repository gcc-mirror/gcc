// { dg-do run { target c++11 } }

// Copyright (C) 2011-2024 Free Software Foundation, Inc.
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
using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::rvalstruct;

typedef test_container<rvalstruct, bidirectional_iterator_wrapper> Container;

const int A[] = { 0, 1, 2, 3, 4, 5 };
const int N = 6;

bool are_ordered(const rvalstruct& lhs, const rvalstruct& rhs)
{ return lhs < rhs; }

// libstdc++/49559
void test01()
{
  rvalstruct s1[6];
  std::copy(A, A + N, s1);
  Container con1(s1, s1 + N);
  std::inplace_merge(con1.begin(), con1.it(4), con1.end());
  VERIFY( s1[0] == 0 && s1[1] == 1 && s1[2] == 2
	  && s1[3] == 3 && s1[4] == 4 && s1[5] == 5 );
  VERIFY( s1[0].valid && s1[1].valid && s1[2].valid
	  && s1[3].valid && s1[4].valid && s1[5].valid );

  rvalstruct s2[6];
  std::copy(A, A + N, s2);
  Container con2(s2, s2 + N);
  std::inplace_merge(con2.begin(), con2.it(4), con2.end(), are_ordered);
  VERIFY( s2[0] == 0 && s2[1] == 1 && s2[2] == 2
	  && s2[3] == 3 && s2[4] == 4 && s2[5] == 5 );
  VERIFY( s2[0].valid && s2[1].valid && s2[2].valid
	  && s2[3].valid && s2[4].valid && s2[5].valid );
}

int
main()
{
  test01();
  return 0;
}
