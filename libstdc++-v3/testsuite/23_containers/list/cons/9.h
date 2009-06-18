// Copyright (C) 2001, 2004, 2005, 2009 Free Software Foundation, Inc.
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

// Assignment operator
//
// This test verifies the following.
// 23.2.2       operator=(const list& x)
// 23.2.2       iterator begin()
// 23.2.2       iterator end()
// 23.2.2       size_type size() const
// 23.2.2       bool operator==(const list& x, const list& y)
//
template<typename _Tp>
void
cons09()
{
  bool test __attribute__((unused)) = true;
  typedef _Tp list_type;
  typedef typename list_type::iterator iterator;

  const int A[] = {701, 702, 703, 704, 705};
  const std::size_t N = sizeof(A) / sizeof(int);
  std::size_t count;

  iterator i;

  list_type list0701(A, A + N);
  VERIFY(list0701.size() == N);

  list_type list0702;
  VERIFY(list0702.size() == 0);

  list0702 = list0701;
  VERIFY(list0702.size() == N);
  for (i = list0702.begin(), count = 0;
       i != list0702.end();
       ++i, ++count)
    VERIFY(*i == A[count]);
  VERIFY(count == N);
  VERIFY(list0702 == list0701);
}
