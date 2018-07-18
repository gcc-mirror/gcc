// { dg-do run { target c++11 } }

// 2011-06-07  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2011-2018 Free Software Foundation, Inc.
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

  v.resize(50);
  VERIFY( v.size() == 50 );

  v.reserve(200);
  VERIFY( v.capacity() >= 200 );

  v.resize(100);
  VERIFY( v.size() == 100 );

  v.resize(500, throwing_move_constructor());
  VERIFY( v.size() == 500 );

  v.reserve(2000);
  VERIFY( v.capacity() >= 2000 );

  v.resize(1000, throwing_move_constructor());
  VERIFY( v.size() == 1000 );
}

int main()
{
  test01();
  return 0;
}
