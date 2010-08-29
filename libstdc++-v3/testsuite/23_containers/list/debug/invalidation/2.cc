// List iterator invalidation tests

// Copyright (C) 2003, 2004, 2005, 2006, 2009 Free Software Foundation, Inc.
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

#include <debug/list>
#include <iterator>
#include <testsuite_hooks.h>

// Resize
void test02()
{
  using std::advance;
  
  bool test __attribute__((unused)) = true;
 
  typedef __gnu_debug::list<int> list_type;

  list_type v(10, 17);

  list_type::iterator before = v.begin();
  advance(before, 6);
  list_type::iterator at = before;
  advance(at, 1);
  list_type::iterator after = at;
  advance(after, 1);
  list_type::iterator finish = v.end();

  // Shrink
  v.resize(7);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_singular());
  VERIFY(after._M_singular());
  VERIFY(!finish._M_singular() && !finish._M_dereferenceable());
}

int main()
{
  test02();
  return 0;
}
