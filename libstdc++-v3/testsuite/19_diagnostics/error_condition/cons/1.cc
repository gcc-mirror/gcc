// { dg-do run { target c++11 } }
// { dg-additional-options "-static-libstdc++" { target *-*-mingw* } }

// Copyright (C) 2008-2019 Free Software Foundation, Inc.
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

#include <system_error>
#include <testsuite_error.h>

void test01()
{
  // 1
  std::error_condition e1;
  VERIFY( e1.value() == 0 );
  VERIFY( e1.category() == std::generic_category() );

  // 2
  const __gnu_test::test_category cat;
  std::error_condition e2(e1.value(), cat);
  VERIFY( e2.value() == e1.value() );
  VERIFY( e2.category() == cat );

  // 3
  std::error_condition e3(std::errc::operation_not_supported);
  VERIFY( e3.value() == int(std::errc::operation_not_supported) );
  VERIFY( e3.category() == std::generic_category() );
}

int main()
{
  test01();
  return 0;
}
