// Copyright (C) 2006-2019 Free Software Foundation, Inc.
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

// 23.2.2.2 list capacity [lib.list.capacity]

#include <list>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// libstdc++/29134
void test01()
{
  typedef std::list<int> list_type;
  list_type l;

#ifdef _GLIBCXX_DEBUG
  using std::_GLIBCXX_STD_C::_List_node;
#else
  using std::_List_node;
#endif

  std::allocator<_List_node<int> > a;
  VERIFY( l.max_size() == __gnu_test::max_size(a) );
}

int main()
{
  test01();
  return 0;
}
