// Copyright (C) 2001-2018 Free Software Foundation, Inc.
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

// 23.2.2.2 list capacity [lib.list.capacity]

#include <testsuite_hooks.h>

// This test verifies the following.
//
// 23.2.2       bool empty() const
// 23.2.2       size_type size() const
// 23.2.2       iterator begin()
// 23.2.2       iterator end()
// 23.2.2.3     void push_back(const T&)
// 23.2.2       size_type max_size() const
// 23.2.2.2     void resize(size_type s, T c = T())
//
template<typename _Tp>
void
capacity01()
{
  typedef _Tp list_type;
  typedef typename list_type::iterator iterator_type;

  list_type list0101;
  VERIFY(list0101.empty());
  VERIFY(list0101.size() == 0);

  list0101.push_back(1);
  VERIFY(!list0101.empty());
  VERIFY(list0101.size() == 1);

  list0101.resize(3, 2);
  VERIFY(!list0101.empty());
  VERIFY(list0101.size() == 3);

  iterator_type i = list0101.begin();
  VERIFY(*i == 1); ++i;
  VERIFY(*i == 2); ++i;
  VERIFY(*i == 2); ++i;
  VERIFY(i == list0101.end());

  list0101.resize(0);
  VERIFY(list0101.empty());
  VERIFY(list0101.size() == 0);
}
