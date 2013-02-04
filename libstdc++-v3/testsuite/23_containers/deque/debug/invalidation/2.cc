// Deque iterator invalidation tests

// Copyright (C) 2003-2013 Free Software Foundation, Inc.
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

// Resize
void test02()
{
  deque<int> v(10, 17);

  deque<int>::iterator before = v.begin() + 6;
  deque<int>::iterator at = before + 1;
  deque<int>::iterator after = at + 1;

  // Shrink
  v.resize(7);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_singular());
  VERIFY(after._M_singular());

  // Grow
  before = v.begin() + 6;
  v.resize(17);
  VERIFY(before._M_singular());
}

int main()
{
  test02();
  return 0;
}
