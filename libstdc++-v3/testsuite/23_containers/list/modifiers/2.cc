// Copyright (C) 2001, 2003, 2004, 2005, 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 23.2.2.3 list modifiers [lib.list.modifiers]

#include <list>
#include <testsuite_hooks.h>

typedef __gnu_test::copy_tracker  T;

bool test __attribute__((unused)) = true;

// general single insert/erase + swap
void
test02()
{
  std::list<T> list0201;
  T::reset();

  list0201.insert(list0201.begin(), T(1));     // list should be [1]
  VERIFY(list0201.size() == 1);
  VERIFY(T::copyCount() == 1);

  list0201.insert(list0201.end(), T(2));     // list should be [1 2]
  VERIFY(list0201.size() == 2);
  VERIFY(T::copyCount() == 2);

  std::list<T>::iterator i = list0201.begin();
  std::list<T>::const_iterator j = i;
  VERIFY(i->id() == 1); ++i;
  VERIFY(i->id() == 2);

  list0201.insert(i, T(3));     // list should be [1 3 2]
  VERIFY(list0201.size() == 3);
  VERIFY(T::copyCount() == 3);

  std::list<T>::const_iterator k = i;
  VERIFY(i->id() == 2); --i;
  VERIFY(i->id() == 3); --i;
  VERIFY(i->id() == 1); 
  VERIFY(j->id() == 1); 

  ++i; // will point to '3'
  T::reset();
  list0201.erase(i); // should be [1 2]
  VERIFY(list0201.size() == 2);
  VERIFY(T::dtorCount() == 1);
  VERIFY(k->id() == 2);
  VERIFY(j->id() == 1); 

  std::list<T> list0202;
  T::reset();
  VERIFY(list0202.size() == 0);
  VERIFY(T::copyCount() == 0);
  VERIFY(T::dtorCount() == 0);

  // member swap
  list0202.swap(list0201);
  VERIFY(list0201.size() == 0);
  VERIFY(list0202.size() == 2);
  VERIFY(T::copyCount() == 0);
  VERIFY(T::dtorCount() == 0);

  // specialized swap
  swap(list0201, list0202);
  VERIFY(list0201.size() == 2);
  VERIFY(list0202.size() == 0);
  VERIFY(T::copyCount() == 0);
  VERIFY(T::dtorCount() == 0);
}

int main()
{
  test02();
  return 0;
}
