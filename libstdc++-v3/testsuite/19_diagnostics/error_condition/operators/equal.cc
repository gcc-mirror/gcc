// { dg-options "-std=gnu++0x" }

// Copyright (C) 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <system_error>
#include <testsuite_error.h>

// unspecified bool operator positive tests
void test01()
{
  bool test __attribute__((unused)) = true;

  std::error_condition e1;
  std::error_condition e2(std::errc::operation_not_supported);

  VERIFY( e1 == e1 );
  VERIFY( !(e1 == e2) );

  const __gnu_test::test_category cat;
  std::error_condition e3(e2.value(), cat);
  VERIFY( !(e2 == e3) );
}

int main()
{
  test01();
  return 0;
}
