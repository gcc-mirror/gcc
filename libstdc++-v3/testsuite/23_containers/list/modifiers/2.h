// Copyright (C) 2001-2020 Free Software Foundation, Inc.
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

// general single insert/erase + swap
template<typename _Tp>
void
modifiers2()
{
  typedef _Tp list_type;
  typedef typename list_type::value_type value_type;
  typedef typename list_type::iterator iterator;
  typedef typename list_type::const_iterator const_iterator;

  using __gnu_test::copy_constructor;
  using __gnu_test::destructor;

  list_type list0201;
  value_type::reset();

  list0201.insert(list0201.begin(), value_type(1));     // list should be [1]
  VERIFY(list0201.size() == 1);
  VERIFY(copy_constructor::count() == 1);

  list0201.insert(list0201.end(), value_type(2));     // list should be [1 2]
  VERIFY(list0201.size() == 2);
  VERIFY(copy_constructor::count() == 2);

  iterator i = list0201.begin();
  const_iterator j = i;
  VERIFY(i->id() == 1); ++i;
  VERIFY(i->id() == 2);

  list0201.insert(i, value_type(3));     // list should be [1 3 2]
  VERIFY(list0201.size() == 3);
  VERIFY(copy_constructor::count() == 3);

  const_iterator k = i;
  VERIFY(i->id() == 2); --i;
  VERIFY(i->id() == 3); --i;
  VERIFY(i->id() == 1);
  VERIFY(j->id() == 1);

  ++i; // will point to '3'
  value_type::reset();
  list0201.erase(i); // should be [1 2]
  VERIFY(list0201.size() == 2);
  VERIFY(destructor::count() == 1);
  VERIFY(k->id() == 2);
  VERIFY(j->id() == 1);

  list_type list0202;
  value_type::reset();
  VERIFY(list0202.size() == 0);
  VERIFY(copy_constructor::count() == 0);
  VERIFY(destructor::count() == 0);

  // member swap
  list0202.swap(list0201);
  VERIFY(list0201.size() == 0);
  VERIFY(list0202.size() == 2);
  VERIFY(copy_constructor::count() == 0);
  VERIFY(destructor::count() == 0);

  // specialized swap
  swap(list0201, list0202);
  VERIFY(list0201.size() == 2);
  VERIFY(list0202.size() == 0);
  VERIFY(copy_constructor::count() == 0);
  VERIFY(destructor::count() == 0);
}
