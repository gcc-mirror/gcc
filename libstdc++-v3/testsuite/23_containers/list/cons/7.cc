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

// Fill assign
//
// This test verifies the following.
// 23.2.2.1     void assign(size_type n, const T& v)
// 23.2.2       const_iterator begin() const
// 23.2.2       const_iterator end() const
// 23.2.2       size_type size() const
//
void
test06()
{
  const std::size_t BIG_LIST_SIZE = 11;
  const int BIG_INIT_VALUE = 7;
  const std::size_t SMALL_LIST_SIZE = 5;
  const int SMALL_INIT_VALUE = 17;
  std::size_t count;
  std::list<int>::const_iterator i;

  std::list<int> list0601;
  VERIFY(list0601.size() == 0);

  // make it bigger
  list0601.assign(BIG_LIST_SIZE, BIG_INIT_VALUE);
  for (i = list0601.begin(), count = 0;
       i != list0601.end();
       ++i, ++count)
    VERIFY(*i == BIG_INIT_VALUE);
  VERIFY(count == BIG_LIST_SIZE);
  VERIFY(list0601.size() == BIG_LIST_SIZE);

  // make it shrink
  list0601.assign(SMALL_LIST_SIZE, SMALL_INIT_VALUE);
  for (i = list0601.begin(), count = 0;
       i != list0601.end();
       ++i, ++count)
    VERIFY(*i == SMALL_INIT_VALUE);
  VERIFY(count == SMALL_LIST_SIZE);
  VERIFY(list0601.size() == SMALL_LIST_SIZE);
}

#if !__GXX_WEAK__ && _MT_ALLOCATOR_H
// Explicitly instantiate for systems with no COMDAT or weak support.
template class __gnu_cxx::__mt_alloc<std::_List_node<int> >;
#endif

int main()
{
  test06();
  return 0;
}

