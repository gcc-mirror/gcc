// Copyright (C) 2001, 2004 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 23.2.2.1 list constructors, copy, and assignment

#include <list>
#include <testsuite_hooks.h>

bool test __attribute__((unused)) = true;

// Assignment operator
//
// This test verifies the following.
// 23.2.2       operator=(const list& x)
// 23.2.2       iterator begin()
// 23.2.2       iterator end()
// 23.2.2       size_type size() const
// 23.2.2       bool operator==(const list& x, const list& y)
//
void
test07()
{
  const int A[] = {701, 702, 703, 704, 705};
  const std::size_t N = sizeof(A) / sizeof(int);
  std::size_t count;
  std::list<int>::iterator i;

  std::list<int> list0701(A, A + N);
  VERIFY(list0701.size() == N);

  std::list<int> list0702;
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

#if !__GXX_WEAK__ && _MT_ALLOCATOR_H
// Explicitly instantiate for systems with no COMDAT or weak support.
template class __gnu_cxx::__mt_alloc<std::_List_node<int> >;
#endif

int main()
{
  test07();
  return 0;
}
// vi:set sw=2 ts=2:
