// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2016 Free Software Foundation, Inc.
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

#include <type_traits>
#include <testsuite_tr1.h>

void test01()
{
  using std::alignment_of;
  using namespace __gnu_test;

  static_assert(test_property<alignment_of, char>(__alignof__(char)), "");
  static_assert(test_property<alignment_of, short>(__alignof__(short)), "");
  static_assert(test_property<alignment_of, int>(__alignof__(int)), "");
  static_assert(test_property<alignment_of, double>(__alignof__(double)), "");
  static_assert(test_property<alignment_of, int[4]>(__alignof__(int[4])), "");
  static_assert(test_property<alignment_of,
		ClassType>(__alignof__(ClassType)), "");
}
