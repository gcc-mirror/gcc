// Copyright (C) 2005-2022 Free Software Foundation, Inc.
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

// 25.3.4 [lib.alg.merge]

// <testsuite_new_operators.h> requires malloc/free.
// { dg-require-effective-target hosted }

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_new_operators.h>

using __gnu_test::test_container;
using __gnu_test::bidirectional_iterator_wrapper;
using std::inplace_merge;

typedef test_container<int, bidirectional_iterator_wrapper> container;

void
test1()
{
  int array[] = { 1 };
  container con1(array, array);
  inplace_merge(con1.begin(), con1.end(), con1.end());
  container con2(array, array + 1);
  inplace_merge(con2.begin(), con2.end(), con2.end());
  inplace_merge(con2.begin(), con2.begin(), con2.end());
}

void
test2()
{
  int array[] = { 0, 2, 4, 1, 3, 5 };
  container con(array, array + 6);
  inplace_merge(con.begin(), con.it(3), con.end());
  VERIFY( array[0] == 0 && array[1] == 1 && array[2] == 2
	  && array[3] == 3 && array[4] == 4 && array[5] == 5 );
}

struct S
{
  int a;
  int b;
  S(int _a, int _b) : a(_a), b(_b) { }
  S() { }
  bool 
  operator<(const S& _s) const 
  { return a < _s.a; }
};

void 
test3()
{
  S s[8];
  s[0].a = 0;
  s[1].a = 1;
  s[2].a = 2;
  s[3].a = 3;
  s[4].a = 0;
  s[5].a = 1;
  s[6].a = 2;
  s[7].a = 3;

  s[0].b = 0;
  s[1].b = 1;
  s[2].b = 2;
  s[3].b = 3;
  s[4].b = 4;
  s[5].b = 5;
  s[6].b = 6;
  s[7].b = 7;

  inplace_merge(s, s + 4, s + 8);
  VERIFY( s[0].b == 0 && s[1].b == 4 && s[2].b == 1 && s[3].b == 5 );
}

void
test4()
{
  S s[8];
  for (int pivot_idx = 0; pivot_idx < 8; ++pivot_idx)
    {
      int bval = 0;
      for (int i = 0; i != pivot_idx; ++i)
	{
	  s[i].a = i;
	  s[i].b = bval++;
	}

      for (int i = pivot_idx; i != 8; ++i)
	{
	  s[i].a = i - pivot_idx;
	  s[i].b = bval++;
	}

      inplace_merge(s, s + pivot_idx, s + 8);

      for (int i = 1; i < 8; ++i)
	{
	  VERIFY( !(s[i] < s[i - 1]) );
	  if (s[i - 1].a == s[i].a)
	    VERIFY( s[i - 1].b < s[i].b );
	}
    }
}

int 
main()
{
  test1();
  test2();
  test3();

  __gnu_test::set_new_limit(sizeof(S) * 4);
  test3();
  test4();

  __gnu_test::set_new_limit(sizeof(S));
  test3();
  test4();

  __gnu_test::set_new_limit(0);
  test3();
  test4();

  return 0;
}
