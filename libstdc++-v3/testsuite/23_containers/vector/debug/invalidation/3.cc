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

// Insert
void test03()
{
  vector<int> v(10, 17);
  v.reserve(30);

  // Insert a single element
  vector<int>::iterator before = v.begin() + 6;
  vector<int>::iterator at = before + 1;
  vector<int>::iterator after = at;
  at = v.insert(at, 42);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_dereferenceable());
  VERIFY(after._M_singular());

  // Insert multiple copies
  before = v.begin() + 6;
  at = before + 1;
  v.insert(at, 3, 42);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_singular());

  // Insert iterator range
  static int data[] = { 2, 3, 5, 7 };
  before = v.begin() + 6;
  at = before + 1;
  v.insert(at, &data[0], &data[0] + 4);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_singular());

  // Insert with reallocation
  before = v.begin() + 6;
  at = before + 1;
  v.insert(at, 30, 17);
  VERIFY(before._M_singular());
  VERIFY(at._M_singular());

  // Single insert with reallocation
  vector<int> v2;
  v2.reserve(100);
  at = v2.begin();
  v2.insert(at, 100, 17);
  at = v2.end() - 1;
  before = v2.begin();
  VERIFY(at._M_dereferenceable());
  VERIFY(before._M_dereferenceable());
  at = v2.insert(at, 42);
  VERIFY(at._M_dereferenceable());
  VERIFY(before._M_singular());
}

int main()
{
  test03();
  return 0;
}
