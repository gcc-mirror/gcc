// Copyright (C) 2004-2016 Free Software Foundation, Inc.
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

// 25.2.1 [lib.alg.copy] Copy.

#include <algorithm>
#include <vector>
#include <list>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std;

  const int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
  const int N = sizeof(A) / sizeof(int);
  const list<int> a(A, A + N);
  
  int i1[N];
  copy(a.begin(), a.end(), i1);
  VERIFY( equal(i1, i1 + N, a.begin()) );

  vector<int> v1(N);
  copy(a.begin(), a.end(), v1.begin());
  VERIFY( equal(v1.begin(), v1.end(), a.begin()) );

  short s1[N];
  copy(a.begin(), a.end(), s1);
  VERIFY( equal(s1, s1 + N, a.begin()) );

  int i2[N];
  copy_backward(a.begin(), a.end(), i2 + N);
  VERIFY( equal(i2, i2 + N, a.begin()) );

  vector<int> v2(N);
  copy_backward(a.begin(), a.end(), v2.end());
  VERIFY( equal(v2.begin(), v2.end(), a.begin()) );

  short s2[N];
  copy_backward(a.begin(), a.end(), s2 + N);
  VERIFY( equal(s2, s2 + N, a.begin()) );
}

int
main()
{
  test01();
  return 0;
}
