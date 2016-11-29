// { dg-do run { target c++11 } }

// 2011-06-07  Paolo Carlini  <paolo.carlini@oracle.com>

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

#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_rvalref.h>

void
test01()
{
  using namespace __gnu_test;

  std::vector<throwing_move_constructor> v(5);

  v.reserve(50);
  VERIFY( v.capacity() >= 50 );

  v.reserve(500);
  VERIFY( v.capacity() >= 500 );
}

int main()
{
  test01();
  return 0;
}
