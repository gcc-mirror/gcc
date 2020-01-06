// Vector iterator invalidation tests

// Copyright (C) 2003-2020 Free Software Foundation, Inc.
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

// We need to be pedantic about reallocations for this testcase to be correct.
// { dg-options "-D_GLIBCXX_DEBUG_PEDANTIC" }

#ifndef _GLIBCXX_DEBUG_PEDANTIC
#  define _GLIBCXX_DEBUG_PEDANTIC 1
#endif

#include <debug/vector>
#include <testsuite_hooks.h>

using __gnu_debug::vector;

bool test = true;

// Erase
void test04()
{
  vector<int> v(20, 42);

  // Single element erase
  vector<int>::iterator before = v.begin();
  vector<int>::iterator at = before + 3;
  vector<int>::iterator after = at;
  at = v.erase(at);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_dereferenceable());
  VERIFY(after._M_singular());

  // Multiple element erase
  before = v.begin();
  at = before + 3;
  v.erase(at, at + 3);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_singular());

  // clear()
  before = v.begin();
  VERIFY(before._M_dereferenceable());
  v.clear();
  VERIFY(before._M_singular());
}

int main()
{
  test04();
  return 0;
}
