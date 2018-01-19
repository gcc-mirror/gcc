// { dg-do run { target c++11 } }

// Copyright (C) 2011-2018 Free Software Foundation, Inc.
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

// 25.3.1 algorithms, sort

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

typedef __gnu_test::rvalstruct_compare_by_value V;
typedef test_container<V, random_access_iterator_wrapper> Container;

void
test01()
{
  V s1[] = { 10, 20, 1, 11, 2, 12, 3, 13, 4, 14, 5, 15, 6, 16, 7, 
	     17, 8, 18, 9, 19 };
  const int N = sizeof(s1) / sizeof(V);
  Container con(s1, s1 + N);
  std::sort(con.begin(), con.end());
  VERIFY( s1[0].ok );
  for(int i = 1; i < N; ++i)
    VERIFY( s1[i].val > s1[i - 1].val && s1[i].ok );
}

void
test02()
{
  V s1[] = { 10, 20, 1, 11, 2, 12, 3, 13, 4, 14, 5, 15, 6, 16, 7, 
	     17, 8, 18, 9, 19 };
  const int N = sizeof(s1) / sizeof(V);
  Container con(s1, s1 + N);
  std::sort(con.begin(), con.end(), __gnu_test::order);
  VERIFY( s1[0].ok );
  for(int i = 1; i < N; ++i)
    VERIFY( s1[i].val > s1[i - 1].val && s1[i].ok );
}

void test03()
{
  V vvs[] = { 2, 0 };
  std::sort(vvs, vvs + 2);
  VERIFY( vvs[0].ok && vvs[0].val == 0 );
  VERIFY( vvs[1].ok && vvs[1].val == 2 );
}

int
main()
{
  test01();
  test02();
  test03();
  return 0;
}
