// { dg-options "-std=gnu++0x" }

// 2008-09-16  Chris Fairles  <chris.fairles@gmail.com>

// Copyright (C) 2008-2014 Free Software Foundation, Inc.
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

#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>

struct compare_counter
  : std::binary_function<int, int, bool>
{
  static int count;
  
  bool operator()(int a, int b) const
  {
    ++count;
    return a < b;
  }
};

int compare_counter::count = 0;

void test01()
{
  bool test __attribute__((unused)) = true;

  std::minmax({1, 2, 3, 4, 5, 6, 7, 8}, compare_counter());

  // If N is the number of arguments in the minmax function call, 
  // 25.3.7 specifies that at most 3N/2 comparisons are allowed.
  VERIFY(compare_counter::count <= (3 * 8 / 2));
}

int main()
{
  test01();
  return 0;
}
