// Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

// splice(p, x) + remove + reverse
template<typename _Tp>
void
operations01()
{
  typedef _Tp list_type;
  typedef typename list_type::iterator iterator;

  const int K = 417;
  const int A[] = {1, 2, 3, 4, 5};
  const int B[] = {K, K, K, K, K};
  const std::size_t N = sizeof(A) / sizeof(int);
  const std::size_t M = sizeof(B) / sizeof(int);

  list_type list0101(A, A + N);
  list_type list0102(B, B + M);
  iterator p = list0101.begin();

  VERIFY(list0101.size() == N);
  VERIFY(list0102.size() == M);

  ++p;
  list0101.splice(p, list0102); // [1 K K K K K 2 3 4 5]
  VERIFY(list0101.size() == N + M);
  VERIFY(list0102.size() == 0);

  // remove range from middle
  list0101.remove(K);
  VERIFY(list0101.size() == N);

  // remove first element
  list0101.remove(1);
  VERIFY(list0101.size() == N - 1);

  // remove last element
  list0101.remove(5);
  VERIFY(list0101.size() == N - 2);

  // reverse
  list0101.reverse();
  p = list0101.begin();
  VERIFY(*p == 4); ++p;
  VERIFY(*p == 3); ++p;
  VERIFY(*p == 2); ++p;
  VERIFY(p == list0101.end());
}
