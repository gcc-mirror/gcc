// List iterator invalidation tests

// Copyright (C) 2003, 2004 Free Software Foundation, Inc.
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

#include <debug/list>
#include <iterator>
#include <testsuite_hooks.h>

using __gnu_debug::list;
using std::advance;

bool test = true;

// Splice
void test04()
{
  list<int> l1(10, 17);
  list<int> l2(10, 42);
  
  list<int>::iterator start2 = l2.begin();
  list<int>::iterator end2 = start2;
  advance(end2, 5);
  list<int>::iterator after2 = end2;
  advance(after2, 2);
  
  l1.splice(l1.begin(), l2, start2, end2);
  VERIFY(start2._M_dereferenceable());
  VERIFY(end2._M_dereferenceable());
  VERIFY(after2._M_dereferenceable());
  VERIFY(start2._M_attached_to(&l1));
  VERIFY(end2._M_attached_to(&l2));
  VERIFY(after2._M_attached_to(&l2));
}

#if !__GXX_WEAK__ && _MT_ALLOCATOR_H
// Explicitly instantiate for systems with no COMDAT or weak support.
template class __gnu_cxx::__mt_alloc<std::_List_node<int> >;
#endif

int main()
{
  test04();
  return 0;
}
