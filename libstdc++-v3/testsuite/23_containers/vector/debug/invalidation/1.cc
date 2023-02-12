// Vector iterator invalidation tests

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

// We need to be pedantic about reallocations for this testcase to be correct.
// { dg-options "-D_GLIBCXX_DEBUG_PEDANTIC" }

#ifndef _GLIBCXX_DEBUG_PEDANTIC
#  define _GLIBCXX_DEBUG_PEDANTIC 1
#endif

#include <debug/vector>
#include <testsuite_hooks.h>

using __gnu_debug::vector;

bool test = true;

// Assignment
void test01()
{
  vector<int> v1;
  vector<int> v2;

  vector<int>::iterator i = v1.end();
  VERIFY(!i._M_dereferenceable() && !i._M_singular());

  v1 = v2;
  VERIFY(i._M_singular());
  
  i = v1.end();
  v1.assign(v2.begin(), v2.end());
  VERIFY(i._M_singular());

  i = v1.end();
  v1.assign(17, 42);
  VERIFY(i._M_singular());
}

int main()
{
  test01();
  return 0;
}
