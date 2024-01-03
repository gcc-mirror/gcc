// Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

#include <forward_list>
#include <testsuite_hooks.h>

using test_type = std::forward_list<int>;

void
test01()
{
  test_type x{1, 2, 3, 4, 3, 2, 1};
  static_assert(std::is_same_v<decltype(x.remove(0)), test_type::size_type>);
  test_type::size_type r = x.remove(0);
  VERIFY( r == 0 );
  r = x.remove(1);
  VERIFY( r == 2 );
  r = x.remove(1);
  VERIFY( r == 0 );
  r = x.remove(4);
  VERIFY( r == 1 );
}

void
test02()
{
  int i = 0;
  auto pred = [&i](int val) { return val == i; };
  test_type x{1, 2, 3, 4, 3, 2, 1};
  static_assert(std::is_same_v<decltype(x.remove_if(pred)),
			       test_type::size_type>);
  test_type::size_type r = x.remove_if(pred);
  VERIFY( r == 0 );
  i = 1;
  r = x.remove_if(pred);
  VERIFY( r == 2 );
  r = x.remove_if(pred);
  VERIFY( r == 0 );
  i = 4;
  r = x.remove_if(pred);
  VERIFY( r == 1 );
}

int
main()
{
  test01();
  test02();
}
