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

// 23.2.2.3 list modifiers [lib.list.modifiers]

#include <testsuite_hooks.h>

// This test verifies the following.
//
// 23.2.2.3     void push_front(const T& x)
// 23.2.2.3     void push_back(const T& x)
// 23.2.2.3 (1) iterator and reference non-invalidation
// 23.2.2.3 (1) exception effects
// 23.2.2.3 (2) complexity requirements
//
// 23.2.2.3     void pop_front()
// 23.2.2.3     void pop_back()
// 23.2.2.3 (3) iterator and reference non-invalidation
// 23.2.2.3 (5) complexity requirements
//
// 23.2.2       const_iterator begin() const
// 23.2.2       iterator end()
// 23.2.2       const_reverse_iterator rbegin() const
// 23.2.2       _reference front()
// 23.2.2       const_reference front() const
// 23.2.2       reference back()
// 23.2.2       const_reference back() const
//
template<typename _Tp>
void
modifiers3()
{
  typedef _Tp list_type;
  typedef typename list_type::iterator iterator;
  typedef typename list_type::value_type value_type;
  typedef typename list_type::const_iterator const_iterator;
  typedef typename list_type::const_reverse_iterator const_reverse_iterator;

  using __gnu_test::copy_constructor;
  using __gnu_test::destructor;

  list_type list0101;
  const_iterator i;
  const_reverse_iterator j;
  iterator k;
  value_type::reset();

  list0101.push_back(value_type(1));     // list should be [1]
  VERIFY(list0101.size() == 1);
  VERIFY(copy_constructor::count() == 1);

  k = list0101.end();
  --k;
  VERIFY(k->id() == 1);
  VERIFY(k->id() == list0101.front().id());
  VERIFY(k->id() == list0101.back().id());

  list0101.push_front(value_type(2));    // list should be [2 1]
  VERIFY(list0101.size() == 2);
  VERIFY(copy_constructor::count() == 2);
  VERIFY(k->id() == 1);

  list0101.push_back(value_type(3));     // list should be [2 1 3]
  VERIFY(list0101.size() == 3);
  VERIFY(copy_constructor::count() == 3);
  VERIFY(k->id() == 1);

  try
  {
    list0101.push_back(value_type(4, true));
    VERIFY(false);
  }
  catch (...)
  {
    VERIFY(list0101.size() == 3);
    VERIFY(copy_constructor::count() == 4);
  }

  i = list0101.begin();
  VERIFY(i->id() == 2);
  VERIFY(i->id() == list0101.front().id());

  j = list0101.rbegin();
  VERIFY(j->id() == 3);
  VERIFY(j->id() == list0101.back().id());

  ++i;
  VERIFY(i->id() == 1);

  ++j;
  VERIFY(j->id() == 1);

  value_type::reset();

  list0101.pop_back();          // list should be [2 1]
  VERIFY(list0101.size() == 2);
  VERIFY(destructor::count() == 1);
  VERIFY(i->id() == 1);
  VERIFY(k->id() == 1);

  list0101.pop_front();          // list should be [1]
  VERIFY(list0101.size() == 1);
  VERIFY(destructor::count() == 2);
  VERIFY(i->id() == 1);
  VERIFY(k->id() == 1);
}
