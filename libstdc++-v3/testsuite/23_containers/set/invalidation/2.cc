// Set iterator invalidation tests

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

#include <debug/set>
#include <iterator>
#include <testsuite_hooks.h>

using __gnu_debug::set;
using std::advance;

// Erase
void test02()
{
  bool test __attribute__((unused)) = true;

  set<int> v;
  for (int i = 0; i < 20; ++i)
    v.insert(i);

  // Single element erase (middle)
  set<int>::iterator before = v.begin();
  set<int>::iterator at = before;
  advance(at, 3);
  set<int>::iterator after = at;
  ++after;
  v.erase(at);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_singular());
  VERIFY(after._M_dereferenceable());

  // Multiple element erase
  before = v.begin();
  at = before;
  advance(at, 3);
  after = at;
  advance(after, 4);
  v.erase(at, after);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_singular());

  // clear()
  before = v.begin();
  set<int>::iterator finish = v.end();
  VERIFY(before._M_dereferenceable());
  v.clear();
  VERIFY(before._M_singular());
  VERIFY(!finish._M_singular() && !finish._M_dereferenceable());
}

#if !__GXX_WEAK__ && _MT_ALLOCATOR_H
// Explicitly instantiate for systems with no COMDAT or weak support.
template class __gnu_cxx::__mt_alloc<std::_Rb_tree_node<int> >;
#endif

int main()
{
  test02();
  return 0;
}
