// Vector iterator invalidation tests

// Copyright (C) 2003-2025 Free Software Foundation, Inc.
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

// Resize
void test02()
{
  vector<int> v(10, 17);
  v.reserve(20);

  vector<int>::iterator before = v.begin() + 6;
  vector<int>::iterator at = before + 1;
  vector<int>::iterator after = at + 1;

  // Shrink
  v.resize(7);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_singular());
  VERIFY(after._M_singular());

  // Grow, without reallocation
  before = v.begin() + 6;
  v.resize(17);
  VERIFY(before._M_dereferenceable());

  // Grow, with reallocation
  v.resize(42);
  VERIFY(before._M_singular());
}

int main()
{
  test02();
  return 0;
}
