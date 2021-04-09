// Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

// range and fill insert/erase + clear
// missing: o  fill insert disguised as a range insert in all its variants
//          o  exception effects
template<typename _Tp>
void
modifiers1()
{
  typedef _Tp list_type;
  typedef typename list_type::iterator iterator;
  typedef typename list_type::value_type value_type;
  
  using __gnu_test::copy_constructor;
  using __gnu_test::destructor;

  list_type list0301;
  value_type::reset();

  // fill insert at beginning of list / empty list
  list0301.insert(list0301.begin(), 3, value_type(11)); // should be [11 11 11]
  VERIFY(list0301.size() == 3);
  VERIFY(copy_constructor::count() == 3);

  // save iterators to verify post-insert validity
  iterator b = list0301.begin();
  iterator m = list0301.end(); --m;
  iterator e = list0301.end();

  // fill insert at end of list
  value_type::reset();
  list0301.insert(list0301.end(), 3, value_type(13)); // should be [11 11 11 13 13 13]
  VERIFY(list0301.size() == 6);
  VERIFY(copy_constructor::count() == 3);
  VERIFY(b == list0301.begin() && b->id() == 11);
  VERIFY(e == list0301.end());
  VERIFY(m->id() == 11);

  // fill insert in the middle of list
  ++m;
  value_type::reset();
  list0301.insert(m, 3, value_type(12)); // should be [11 11 11 12 12 12 13 13 13]
  VERIFY(list0301.size() == 9);
  VERIFY(copy_constructor::count() == 3);
  VERIFY(b == list0301.begin() && b->id() == 11);
  VERIFY(e == list0301.end());
  VERIFY(m->id() == 13);

  // single erase
  value_type::reset();
  m = list0301.erase(m); // should be [11 11 11 12 12 12 13 13]
  VERIFY(list0301.size() == 8);
  VERIFY(destructor::count() == 1);
  VERIFY(b == list0301.begin() && b->id() == 11);
  VERIFY(e == list0301.end());
  VERIFY(m->id() == 13);

  // range erase
  value_type::reset();
  m = list0301.erase(list0301.begin(), m); // should be [13 13]
  VERIFY(list0301.size() == 2);
  VERIFY(destructor::count() == 6);
  VERIFY(m->id() == 13);

  // range fill at beginning
  const int A[] = {321, 322, 333};
  const int N = sizeof(A) / sizeof(int);
  value_type::reset();
  b = list0301.begin();
  list0301.insert(b, A, A + N); // should be [321 322 333 13 13]
  VERIFY(list0301.size() == 5);
#if __cplusplus >= 201103L
  VERIFY(copy_constructor::count() == 0);
#else
  VERIFY(copy_constructor::count() == 3);
#endif
  VERIFY(m->id() == 13);

  // range fill at end
  value_type::reset();
  list0301.insert(e, A, A + N); // should be [321 322 333 13 13 321 322 333]
  VERIFY(list0301.size() == 8);
#if __cplusplus >= 201103L
  VERIFY(copy_constructor::count() == 0);
#else
  VERIFY(copy_constructor::count() == 3);
#endif
  VERIFY(e == list0301.end());
  VERIFY(m->id() == 13);

  // range fill in middle
  value_type::reset();
  list0301.insert(m, A, A + N);
  VERIFY(list0301.size() == 11);
#if __cplusplus >= 201103L
  VERIFY(copy_constructor::count() == 0);
#else
  VERIFY(copy_constructor::count() == 3);
#endif
  VERIFY(e == list0301.end());
  VERIFY(m->id() == 13);

  value_type::reset();
  list0301.clear();
  VERIFY(list0301.size() == 0);
  VERIFY(destructor::count() == 11);
  VERIFY(e == list0301.end());
}
