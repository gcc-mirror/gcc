// { dg-do compile { target c++11 } }

// Copyright (C) 2011-2016 Free Software Foundation, Inc.
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

// NOTE: This makes use of the fact that we know how moveable
// is implemented on pair, and also vector. If the implementation 
// changes this test may begin to fail.

#include <memory>
#include <testsuite_hooks.h>

void
test1()
{
  bool test __attribute__((unused)) = true;
  typedef std::pair<int, float> pair_type;
  constexpr pair_type p1 __attribute__((unused)) = std::make_pair(22, 22.222);
}

int 
main() 
{
  test1();
  return 0;
}
