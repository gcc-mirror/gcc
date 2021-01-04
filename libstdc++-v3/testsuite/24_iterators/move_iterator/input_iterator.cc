// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <iterator>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

void
test01()
{
  int a[2] = { 1, 2 };
  __gnu_test::test_container<int, __gnu_test::input_iterator_wrapper> c(a);
  auto miter = std::make_move_iterator(c.begin());
  VERIFY( *miter == 1 );
  miter++;
  VERIFY( *miter == 2 );

  static_assert( std::is_void_v<decltype(miter++)> );
}

int
main()
{
  test01();
}
