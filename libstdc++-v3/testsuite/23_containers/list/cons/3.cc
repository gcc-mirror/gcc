// Copyright (C) 2001, 2004, 2005, 2009 Free Software Foundation, Inc.
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

#include <list>
#include <testsuite_hooks.h>

bool test __attribute__((unused)) = true;

// A nontrivial type.
template<typename T>
  struct A { };

// Another nontrivial type
struct B { };

// A nontrivial type convertible from an int
struct C {
  C(int i) : i_(i) { }
  bool operator==(const C& rhs) { return i_ == rhs.i_; }
  int i_;
};

// Fill constructor disguised as a range constructor
void
test02D()
{
  const std::size_t LIST_SIZE = 5;
  const int INIT_VALUE = 7;
  std::size_t count = 0;
  std::list<C> list0204(LIST_SIZE, INIT_VALUE);
  std::list<C>::iterator i = list0204.begin();
  for (; i != list0204.end(); ++i, ++count)
    VERIFY(*i == INIT_VALUE);
  VERIFY(count == LIST_SIZE);
  VERIFY(list0204.size() == LIST_SIZE);
}

int main()
{
  test02D(); 
  return 0;
}
// vi:set sw=2 ts=2:
