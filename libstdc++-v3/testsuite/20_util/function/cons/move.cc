// { dg-options "-std=gnu++11" }

// Copyright (C) 2009-2015 Free Software Foundation, Inc.
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

#include <functional>
#include <testsuite_hooks.h>

int f1() { return 1; }
struct { int operator()() { return 2; } } f2;

void test01()
{
  bool test __attribute__((unused)) = true;

  typedef std::function<int()> function;

  function fo(f1);
  function fo1(std::move(fo));
  VERIFY( static_cast<bool>(fo1) );
  VERIFY( fo1() == 1 );

  fo = function(f2);
  function fo2(std::move(fo));
  VERIFY( static_cast<bool>(fo2) );
  VERIFY( fo2() == 2 );

  static_assert(std::is_nothrow_move_constructible<function>::value,
		"PR libstdc++/81017");
}

int main()
{
  test01();
}
