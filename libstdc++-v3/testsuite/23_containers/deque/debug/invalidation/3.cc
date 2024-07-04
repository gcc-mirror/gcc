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

// Insert
void test03()
{
  deque<int> v(10, 17);

  // Insert a single element
  deque<int>::iterator before = v.begin() + 6;
  deque<int>::iterator at = before + 1;
  deque<int>::iterator after = at;
  at = v.insert(at, 42);
  VERIFY(before._M_singular());
  VERIFY(at._M_dereferenceable());
  VERIFY(after._M_singular());

  // Insert multiple copies
  before = v.begin() + 6;
  at = before + 1;

  v.insert(at, 3, 42);
  VERIFY(before._M_singular());
  VERIFY(at._M_singular());

  // Insert iterator range
  static int data[] = { 2, 3, 5, 7 };
  before = v.begin() + 6;
  at = before + 1;
  v.insert(at, &data[0], &data[0] + 4);
  VERIFY(before._M_singular());
  VERIFY(at._M_singular());
}

int main()
{
  test03();
  return 0;
}
