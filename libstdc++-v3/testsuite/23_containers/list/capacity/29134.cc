// Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
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

// 23.2.2.2 list capacity [lib.list.capacity]

#include <list>
#include <testsuite_hooks.h>

// libstdc++/29134
void test01()
{
  bool test __attribute__((unused)) = true;

  typedef std::list<int> list_type;
  list_type l;

#ifndef _GLIBCXX_DEBUG
  using std::_List_node;
#else
  using std::_GLIBCXX_STD_D::_List_node;
#endif

  VERIFY( l.max_size() == std::allocator<_List_node<int> >().max_size() );
}

int main()
{
  test01();
  return 0;
}
