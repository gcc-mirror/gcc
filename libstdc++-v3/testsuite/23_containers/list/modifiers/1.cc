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

// range and fill insert/erase + clear
// missing: o  fill insert disguised as a range insert in all its variants
//          o  exception effects
void
test03()
{
  std::list<T> list0301;
  T::reset();

  // fill insert at beginning of list / empty list
  list0301.insert(list0301.begin(), 3, T(11)); // should be [11 11 11]
  VERIFY(list0301.size() == 3);
  VERIFY(T::copyCount() == 3);

  // save iterators to verify post-insert validity
  std::list<T>::iterator b = list0301.begin();          
  std::list<T>::iterator m = list0301.end(); --m;          
  std::list<T>::iterator e = list0301.end();

  // fill insert at end of list
  T::reset();
  list0301.insert(list0301.end(), 3, T(13)); // should be [11 11 11 13 13 13]
  VERIFY(list0301.size() == 6);
  VERIFY(T::copyCount() == 3);
  VERIFY(b == list0301.begin() && b->id() == 11);
  VERIFY(e == list0301.end());
  VERIFY(m->id() == 11);

  // fill insert in the middle of list
  ++m;
  T::reset();
  list0301.insert(m, 3, T(12)); // should be [11 11 11 12 12 12 13 13 13]
  VERIFY(list0301.size() == 9);
  VERIFY(T::copyCount() == 3);
  VERIFY(b == list0301.begin() && b->id() == 11);
  VERIFY(e == list0301.end());
  VERIFY(m->id() == 13);

  // single erase
  T::reset();
  m = list0301.erase(m); // should be [11 11 11 12 12 12 13 13]
  VERIFY(list0301.size() == 8);
  VERIFY(T::dtorCount() == 1);
  VERIFY(b == list0301.begin() && b->id() == 11);
  VERIFY(e == list0301.end());
  VERIFY(m->id() == 13);

  // range erase
  T::reset();
  m = list0301.erase(list0301.begin(), m); // should be [13 13]
  VERIFY(list0301.size() == 2);
  VERIFY(T::dtorCount() == 6);
  VERIFY(m->id() == 13);

  // range fill at beginning
  const int A[] = {321, 322, 333};
  const int N = sizeof(A) / sizeof(int);
  T::reset();
  b = list0301.begin();          
  list0301.insert(b, A, A + N); // should be [321 322 333 13 13]
  VERIFY(list0301.size() == 5);
  VERIFY(T::copyCount() == 3);
  VERIFY(m->id() == 13);
  
  // range fill at end
  T::reset();
  list0301.insert(e, A, A + N); // should be [321 322 333 13 13 321 322 333]
  VERIFY(list0301.size() == 8);
  VERIFY(T::copyCount() == 3);
  VERIFY(e == list0301.end());
  VERIFY(m->id() == 13);
  
  // range fill in middle
  T::reset();
  list0301.insert(m, A, A + N); 
  VERIFY(list0301.size() == 11);
  VERIFY(T::copyCount() == 3);
  VERIFY(e == list0301.end());
  VERIFY(m->id() == 13);

  T::reset();
  list0301.clear();
  VERIFY(list0301.size() == 0);
  VERIFY(T::dtorCount() == 11);
  VERIFY(e == list0301.end());
}

int main()
{
  test03();
  return 0;
}
