// { dg-do run { target c++11 } }
// { dg-additional-options "-static-libstdc++" { target *-*-mingw* } }

// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

int main()
{
  std::error_condition e1;
  std::error_condition e2(std::errc::operation_not_supported);

  VERIFY( !(e1 < e1) );
  VERIFY( !(e2 < e2) );

  // e1.category() == e2.category(), so comparison depends on values:
  VERIFY( (e1 < e2) == (e1.value() < e2.value()) );

  const __gnu_test::test_category cat;
  std::error_condition e3(e2.value(), cat);
  VERIFY( !(e3 < e3) );
  VERIFY( (e2 < e3) == (e2.category() < e3.category()) );
}
