// Copyright (C) 2001-2016 Free Software Foundation, Inc.
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

// 23.2.2.4 list operations [lib.list.ops]

#include <testsuite_hooks.h>

// splice(p, x, f, l) + sort + merge + unique
template<typename _Tp>
void
operations03()
{
  typedef _Tp list_type;
  typedef typename list_type::iterator iterator;

  const int A[] = {103, 203, 603, 303, 403, 503};
  const int B[] = {417, 417, 417, 417, 417};
  const int E[] = {103, 417, 417, 203, 603, 303, 403, 503};
  const int F[] = {103, 203, 303, 403, 417, 417, 503, 603};
  const int C[] = {103, 203, 303, 403, 417, 417, 417, 417, 417, 503, 603};
  const int D[] = {103, 203, 303, 403, 417, 503, 603};
  const int N = sizeof(A) / sizeof(int);
  const int M = sizeof(B) / sizeof(int);
  const int P = sizeof(C) / sizeof(int);
  const int Q = sizeof(D) / sizeof(int);
  const int R = sizeof(E) / sizeof(int);

  list_type list0301(A, A + N);
  list_type list0302(B, B + M);
  list_type list0303(C, C + P);
  list_type list0304(D, D + Q);
  list_type list0305(E, E + R);
  list_type list0306(F, F + R);
  iterator p = list0301.begin();
  iterator q = list0302.begin();

  ++p; ++q; ++q;
  list0301.splice(p, list0302, list0302.begin(), q);
  VERIFY(list0301 == list0305);
  VERIFY(list0301.size() == N + 2);
  VERIFY(list0302.size() == M - 2);

  list0301.sort();
  VERIFY(list0301 == list0306);

  list0301.merge(list0302);
  VERIFY(list0301.size() == N + M);
  VERIFY(list0302.size() == 0);
  VERIFY(list0301 == list0303);

  list0301.unique();
  VERIFY(list0301 == list0304);
}
