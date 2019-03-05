// { dg-options "-fno-inline -g0" }
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler-not "_ZNSt10unique_ptrIiSt14default_deleteIiEEC2Ev" } }
// { dg-final { scan-assembler-not "_ZNSt10unique_ptrIiSt14default_deleteIiEEC2EDn" } }

// Copyright (C) 2010-2019 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_common_types.h>

int main()
{
  __gnu_test::constexpr_default_constructible test1;  //not literal
  test1.operator()<std::unique_ptr<int>>();

  __gnu_test::constexpr_single_value_constructible test2;  //not literal
  test2.operator()<std::unique_ptr<int>, std::nullptr_t>();

  return 0;
}
