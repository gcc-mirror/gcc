// Deque iterator invalidation tests

// Copyright (C) 2003-2024 Free Software Foundation, Inc.
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

#include <debug/deque>
#include <testsuite_hooks.h>

using __gnu_debug::deque;

bool test = true;

// Erase
void test04()
{
  deque<int> v(20, 42);

  // Single element erase (middle)
  deque<int>::iterator before = v.begin();
  deque<int>::iterator at = before + 3;
  deque<int>::iterator after = at;
  at = v.erase(at);
  VERIFY(before._M_singular());
  VERIFY(at._M_dereferenceable());
  VERIFY(after._M_singular());

  // Single element erase (end)
  before = v.begin();
  at = before;
  after = at + 1;
  at = v.erase(at);
  VERIFY(before._M_singular());
  VERIFY(at._M_dereferenceable());
  VERIFY(after._M_dereferenceable());

  // Multiple element erase
  before = v.begin();
  at = before + 3;
  v.erase(at, at + 3);
  VERIFY(before._M_singular());
  VERIFY(at._M_singular());

  // Multiple element erase at end
  before = v.begin();
  at = before + 3;
  v.erase(at, v.end());
  (void) *before;

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
