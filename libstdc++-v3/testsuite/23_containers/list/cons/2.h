// Copyright (C) 2001-2017 Free Software Foundation, Inc.
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

// A nontrivial type.
template<typename T>
  struct A { };

// Another nontrivial type
struct B { };

// Fill constructor
//
// This test verifies the following.
// 23.2.2.1 explicit list(size_type n, const T& v = T(), const a& = Allocator())
// 23.2.2   const_iterator begin() const
// 23.2.2   const_iterator end() const
// 23.2.2   size_type size() const
//
template<typename _Tp>
void
cons021()
{
  const std::size_t LIST_SIZE = 5;
  const int INIT_VALUE = 7;
  std::size_t count;

  typedef _Tp list_type;
  typedef typename list_type::const_iterator const_iterator;
  const_iterator i;

  // default value
  list_type list0202(LIST_SIZE);
  for (i = list0202.begin(), count = 0;
       i != list0202.end();
       ++i, ++count)
    VERIFY(*i == 0);
  VERIFY(count == LIST_SIZE);
  VERIFY(list0202.size() == LIST_SIZE);

  // explicit value
  list_type list0203(LIST_SIZE, INIT_VALUE);
  for (i = list0203.begin(), count = 0;
       i != list0203.end();
       ++i, ++count)
    VERIFY(*i == INIT_VALUE);
  VERIFY(count == LIST_SIZE);
  VERIFY(list0203.size() == LIST_SIZE);
}

template<typename _Tp>
void
cons022()
{
  // nontrivial value_type
  typedef _Tp list_type;
  const std::size_t LIST_SIZE = 5;
  list_type list0201(LIST_SIZE);
}
