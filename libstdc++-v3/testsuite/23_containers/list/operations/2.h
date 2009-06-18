// Copyright (C) 2001, 2004, 2005, 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 23.2.2.4 list operations [lib.list.ops]

#include <testsuite_hooks.h>

// splice(p, x, i) + remove_if + operator==
template<typename _Tp>
void
operations02()
{
  bool test __attribute__((unused)) = true;
  typedef _Tp list_type;
  typedef typename list_type::iterator iterator;

  const int A[] = {1, 2, 3, 4, 5};
  const int B[] = {2, 1, 3, 4, 5};
  const int C[] = {1, 3, 4, 5, 2};
  const int N = sizeof(A) / sizeof(int);
  list_type list0201(A, A + N);
  list_type list0202(A, A + N);
  list_type list0203(B, B + N);
  list_type list0204(C, C + N);
  iterator i = list0201.begin();

  // result should be unchanged
  list0201.splice(list0201.begin(), list0201, i);
  VERIFY(list0201 == list0202);

  // result should be [2 1 3 4 5]
  ++i;
  list0201.splice(list0201.begin(), list0201, i);
  VERIFY(list0201 != list0202);
  VERIFY(list0201 == list0203);

  // result should be [1 3 4 5 2]
  list0201.splice(list0201.end(), list0201, i);
  VERIFY(list0201 == list0204);
}
