// Copyright (C) 2001, 2004 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

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

// Fill Assignment disguised as a Range Assignment
void
test06D()
{
  const std::size_t LIST_SIZE = 5;
  const int INIT_VALUE = 7;
  std::size_t count = 0;
  std::list<C> list0604;
  VERIFY(list0604.size() == 0);
  
  list0604.assign(LIST_SIZE, INIT_VALUE);
  std::list<C>::iterator i = list0604.begin();
  for (; i != list0604.end(); ++i, ++count)
    VERIFY(*i == INIT_VALUE);
  VERIFY(count == LIST_SIZE);
  VERIFY(list0604.size() == LIST_SIZE);
}

#if !__GXX_WEAK__ && _MT_ALLOCATOR_H
// Explicitly instantiate for systems with no COMDAT or weak support.
template class __gnu_cxx::__mt_alloc<std::_List_node<C> >;
#endif

int main()
{
  test06D();
  return 0;
}

