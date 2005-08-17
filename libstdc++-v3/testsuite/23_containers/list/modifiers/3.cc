// Copyright (C) 2001, 2003, 2004, 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 23.2.2.3 list modifiers [lib.list.modifiers]

#include <list>
#include <testsuite_hooks.h>

typedef __gnu_test::copy_tracker  T;

bool test __attribute__((unused)) = true;


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
void
test01()
{
  std::list<T> list0101;
  std::list<T>::const_iterator i;
  std::list<T>::const_reverse_iterator j;
  std::list<T>::iterator k;
  T::reset();

  list0101.push_back(T(1));     // list should be [1]
  VERIFY(list0101.size() == 1);
  VERIFY(T::copyCount() == 1);

  k = list0101.end();
  --k;
  VERIFY(k->id() == 1);
  VERIFY(k->id() == list0101.front().id());
  VERIFY(k->id() == list0101.back().id());

  list0101.push_front(T(2));    // list should be [2 1]
  VERIFY(list0101.size() == 2);
  VERIFY(T::copyCount() == 2);
  VERIFY(k->id() == 1);

  list0101.push_back(T(3));     // list should be [2 1 3]
  VERIFY(list0101.size() == 3);
  VERIFY(T::copyCount() == 3);
  VERIFY(k->id() == 1);

  try
  {
    list0101.push_back(T(4, true));
    VERIFY(false);
  }
  catch (...)
  {
    VERIFY(list0101.size() == 3);
    VERIFY(T::copyCount() == 4);
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

  T::reset();

  list0101.pop_back();          // list should be [2 1]
  VERIFY(list0101.size() == 2);
  VERIFY(T::dtorCount() == 1);
  VERIFY(i->id() == 1);
  VERIFY(k->id() == 1);

  list0101.pop_front();          // list should be [1]
  VERIFY(list0101.size() == 1);
  VERIFY(T::dtorCount() == 2);
  VERIFY(i->id() == 1);
  VERIFY(k->id() == 1);
}

int main()
{
  test01();
  return 0;
}
