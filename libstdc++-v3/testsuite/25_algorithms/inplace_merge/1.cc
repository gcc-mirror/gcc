// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 25.3.4 [lib.alg.merge]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::bidirectional_iterator_wrapper;
using std::inplace_merge;

typedef test_container<int, bidirectional_iterator_wrapper> container;


void 
test1()
{
  int array[]={1};
  container con1(array, array);
  inplace_merge(con1.begin(), con1.end(), con1.end());
  container con2(array, array + 1);
  inplace_merge(con2.begin(), con2.end(), con2.end());
  inplace_merge(con2.begin(), con2.begin(), con2.end());
}

void 
test2()
{
  int array[]={0,2,4,1,3,5};
  container con(array, array + 6);
  inplace_merge(con.begin(), con.it(3), con.end());
  VERIFY(array[0] == 0 && array[1] == 1 && array[2] == 2 &&
	 array[3] == 3 && array[4] == 4 && array[5] == 5);
}

struct S
{
  int a;
  int b;
  S(int _a, int _b) : a(_a), b(_b) { }
  S() { }
  bool 
  operator<(const S& _s) const 
  { return _s.a < a; }
};

void 
test3()
{
  S s[4];
  s[0].a = 0;
  s[1].a = 1;
  s[2].a = 0;
  s[3].a = 1;
  s[0].b = 0;
  s[1].b = 0;
  s[2].b = 1;
  s[3].b = 1;
  inplace_merge(s, s + 2, s + 4);
  VERIFY(s[0].b == 0 && s[1].b == 1 && s[2].b == 0 && s[3].b == 1);
}

int 
main()
{
  test1();
}
