// 2001-12-27 pme
//
// Copyright (C) 2001-2013 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 23.2.1.1 deque constructors, copy, and assignment

#include <deque>
#include <iterator>
#include <sstream>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

typedef std::deque<__gnu_test::object_counter>   gdeque;

bool test __attribute__((unused)) = true;

// see http://gcc.gnu.org/ml/libstdc++/2001-11/msg00139.html
void
test01()
{
  assert_count (0);
  {
     gdeque   d(10);
     assert_count (10);
  }
  assert_count (0);
}

int main()
{
  // specific bug fix checks
  test01();
  return 0;
}
