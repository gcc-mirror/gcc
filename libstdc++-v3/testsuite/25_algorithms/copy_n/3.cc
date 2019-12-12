// { dg-do run { target c++11 } }

// 2008-06-29  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008-2019 Free Software Foundation, Inc.
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

#include <algorithm>
#include <vector>
#include <deque>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std;

  const int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
  const int N = sizeof(A) / sizeof(int);
  const deque<int> a(A, A + N);

  int i1[N];
  copy_n(a.begin(), N, i1);
  VERIFY( equal(i1, i1 + N, a.begin()) );

  vector<int> v1(N);
  copy_n(a.begin(), N, v1.begin());
  VERIFY( equal(v1.begin(), v1.end(), a.begin()) );

  short s1[N];
  copy_n(a.begin(), N, s1);
  VERIFY( equal(s1, s1 + N, a.begin()) );
}

int
main()
{
  test01();
  return 0;
}
