// Copyright (C) 2001, 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 25.2.1 [lib.alg.copy] Copy.

#include <algorithm>
#include <vector>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  const int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
  const int N = sizeof(A) / sizeof(int);
  
  int i1[N];
  copy(A, A + N, i1);
  VERIFY( equal(i1, i1 + N, A) );

  vector<int> v1(N);
  copy(A, A + N, v1.begin());
  VERIFY( equal(v1.begin(), v1.end(), A) );

  short s1[N];
  copy(A, A + N, s1);
  VERIFY( equal(s1, s1 + N, A) );

  int i2[N];
  copy_backward(A, A + N, i2 + N);
  VERIFY( equal(i2, i2 + N, A) );

  vector<int> v2(N);
  copy_backward(A, A + N, v2.end());
  VERIFY( equal(v2.begin(), v2.end(), A) );

  short s2[N];
  copy_backward(A, A + N, s2 + N);
  VERIFY( equal(s2, s2 + N, A) );
}

int
main()
{
  test01();
  return 0;
}
