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

// Fill constructor
//
// This test verifies the following.
// 23.2.2.1     explicit list(size_type n, const T& v = T(), const a& = Allocator())
// 23.2.2       const_iterator begin() const
// 23.2.2       const_iterator end() const
// 23.2.2       size_type size() const
//
void
test02()
{
  const std::size_t LIST_SIZE = 5;
  const int INIT_VALUE = 7;
  std::size_t count;
  std::list<int>::const_iterator i;

  // nontrivial value_type
  std::list< A<B> > list0201(LIST_SIZE);

  // default value
  std::list<int> list0202(LIST_SIZE);
  for (i = list0202.begin(), count = 0;
       i != list0202.end();
       ++i, ++count)
    VERIFY(*i == 0);
  VERIFY(count == LIST_SIZE);
  VERIFY(list0202.size() == LIST_SIZE);

  // explicit value
  std::list<int> list0203(LIST_SIZE, INIT_VALUE);
  for (i = list0203.begin(), count = 0;
       i != list0203.end();
       ++i, ++count)
    VERIFY(*i == INIT_VALUE);
  VERIFY(count == LIST_SIZE);
  VERIFY(list0203.size() == LIST_SIZE);
}

int main()
{
  test02(); 
  return 0;
}
