// { dg-do run { target c++11 } }
//
// Copyright (C) 2013-2019 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

void test01()
{
  using std::true_type;
  using std::false_type;

  static_assert( true_type::value == true, "" );
  static_assert( false_type::value == false, "" );
  static_assert( true_type::type::value == true, "" );
  static_assert( false_type::type::value == false, "" );

  VERIFY( true_type::value == true );
  VERIFY( false_type::value == false );
  VERIFY( true_type::type::value == true );
  VERIFY( false_type::type::value == false );
}

int main()
{
  test01();
  return 0;
}
