// Copyright (C) 2001-2014 Free Software Foundation, Inc.
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

// 23.2.2.1 list constructors, copy, and assignment

#include <testsuite_hooks.h>

// Range assign
//
// This test verifies the following.
// 23.2.2.1     void assign(InputIterator f, InputIterator l)
// 23.2.2       const_iterator begin() const
// 23.2.2       const_iterator end() const
// 23.2.2       size_type size() const
//
template<typename _Tp>
void
cons06()
{
  bool test __attribute__((unused)) = true;
  const int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
  const int B[] = {101, 102, 103, 104, 105};
  const std::size_t N = sizeof(A) / sizeof(int);
  const std::size_t M = sizeof(B) / sizeof(int);
  std::size_t count;

  typedef _Tp list_type;
  typedef typename list_type::const_iterator const_iterator;
  const_iterator i;

  list_type list0501;

  // make it bigger
  list0501.assign(A, A + N);
  for (i = list0501.begin(), count = 0;
       i != list0501.end();
       ++i, ++count)
    VERIFY(*i == A[count]);
  VERIFY(count == N);
  VERIFY(list0501.size() == N);

  // make it smaller
  list0501.assign(B, B + M);
  for (i = list0501.begin(), count = 0;
       i != list0501.end();
       ++i, ++count)
    VERIFY(*i == B[count]);
  VERIFY(count == M);
  VERIFY(list0501.size() == M);
}
