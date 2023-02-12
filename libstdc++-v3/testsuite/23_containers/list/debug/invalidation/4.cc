// List iterator invalidation tests

// Copyright (C) 2003-2023 Free Software Foundation, Inc.
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

// Splice
void test04()
{
  using std::advance;

  typedef __gnu_debug::list<int>  list_type;

  list_type l1(10, 17);
  list_type l2(10, 42);
  
  list_type::iterator start2 = l2.begin();
  list_type::iterator end2 = start2;
  advance(end2, 5);
  list_type::iterator after2 = end2;
  advance(after2, 2);
  
  l1.splice(l1.begin(), l2, start2, end2);
  VERIFY(start2._M_dereferenceable());
  VERIFY(end2._M_dereferenceable());
  VERIFY(after2._M_dereferenceable());
  VERIFY(start2._M_attached_to(&l1));
  VERIFY(end2._M_attached_to(&l2));
  VERIFY(after2._M_attached_to(&l2));
}

int main()
{
  test04();
  return 0;
}
