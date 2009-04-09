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

#include <list>
#include <testsuite_hooks.h>

bool test __attribute__((unused)) = true;

// Range constructor
//
// This test verifies the following.
// 23.2.2.1     template list(InputIterator f, InputIterator l, const Allocator& a = Allocator())
// 23.2.2       const_iterator begin() const
// 23.2.2       const_iterator end() const
// 23.2.2       size_type size() const
//
void
test03()
{
  const int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
  const std::size_t N = sizeof(A) / sizeof(int);
  std::size_t count;
  std::list<int>::const_iterator i;

  // construct from a dissimilar range
  std::list<int> list0301(A, A + N);
  for (i = list0301.begin(), count = 0;
       i != list0301.end();
       ++i, ++count)
    VERIFY(*i == A[count]);
  VERIFY(count == N);
  VERIFY(list0301.size() == N);

  // construct from a similar range
  std::list<int> list0302(list0301.begin(), list0301.end());
  for (i = list0302.begin(), count = 0;
       i != list0302.end();
       ++i, ++count)
    VERIFY(*i == A[count]);
  VERIFY(count == N);
  VERIFY(list0302.size() == N);
}

int main()
{
  test03();
  return 0;
}

