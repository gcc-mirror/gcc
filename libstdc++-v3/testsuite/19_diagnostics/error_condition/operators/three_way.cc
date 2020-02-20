// Copyright (C) 2020 Free Software Foundation, Inc.
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

#include <system_error>
#include <testsuite_error.h>

void
test01()
{
  std::error_condition e1;
  std::error_condition e2(std::errc::operation_not_supported);

  VERIFY( std::is_eq(e1 <=> e1) );
  VERIFY( std::is_lteq(e1 <=> e1) );
  VERIFY( std::is_gteq(e1 <=> e1) );

  VERIFY( std::is_neq(e1 <=> e2) );
  VERIFY( std::is_lt(e1 <=> e2) || std::is_gt(e1 <=> e2) );
  VERIFY( (e1 <=> e2) == (e1.value() <=> e2.value()) );

  VERIFY( e1 == e1 );
  VERIFY( !(e1 == e2) );

  VERIFY( !(e1 < e1) );
  VERIFY( !(e2 < e2) );

  const __gnu_test::test_category cat;
  std::error_condition e3(e2.value(), cat);

  VERIFY( std::is_neq(e2 <=> e3) );
  VERIFY( std::is_lt(e2 <=> e3) || std::is_gt(e2 <=> e3) );
  VERIFY( (e2 <=> e3) == (e2.category() <=> e3.category()) );

  VERIFY( !(e2 == e3) );

  VERIFY( !(e3 < e3) );
  VERIFY( (e2 < e3) == (e2.category() < e3.category()) );
}

int main()
{
  test01();
}
