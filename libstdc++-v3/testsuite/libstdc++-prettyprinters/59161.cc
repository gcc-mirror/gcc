// { dg-do run }
// { dg-options "-g -O0" }

// Copyright (C) 2011-2019 Free Software Foundation, Inc.
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

#include <deque>
#include <list>
#include <ext/slist>
#include <set>
#include <vector>
#include <debug/vector>
#include <iostream>

struct C {
  C(int& i) : ref(i) { }
  int& ref;
  bool operator<(const C& c) const { return ref < c.ref; }
};

int main()
{
  int i = 1;
  C c(i);

  std::deque<C> d;
  d.push_back(c);
  std::deque<C>::iterator diter = d.begin();
// { dg-final { regexp-test diter {ref = @0x.*} } }

  std::list<C> l;
  l.push_back(c);
  std::list<C>::iterator liter = l.begin();
// { dg-final { regexp-test liter {ref = @0x.*} } }

  __gnu_cxx::slist<C> sl;
  sl.push_front(c);
  __gnu_cxx::slist<C>::iterator sliter = sl.begin();
// { dg-final { regexp-test sliter {ref = @0x.*} } }

  std::set<C> s;
  s.insert(c);
  std::set<C>::iterator siter = s.begin();
// { dg-final { regexp-test siter {ref = @0x.*} } }

  std::vector<C> v;
  v.push_back(c);
  std::vector<C>::iterator viter = v.begin();
// { dg-final { regexp-test viter {ref = @0x.*} } }

  std::cout << "\n";
  return 0;			// Mark SPOT
}
// { dg-final { gdb-test SPOT } }
