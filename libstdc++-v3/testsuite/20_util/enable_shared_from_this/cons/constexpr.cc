// { dg-options "-fno-inline -g0" }
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler-not "_ZNSt23enable_shared_from_thisIiEC2Ev" } }
// { dg-final { scan-assembler-not "_ZN7derivedC2Ev" } }

// Copyright (C) 2010-2016 Free Software Foundation, Inc.
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

struct derived : public std::enable_shared_from_this<int>
{
  constexpr derived() { }
};

int main()
{
  __gnu_test::constexpr_default_constructible test;
  test.operator()<derived>();
  return 0;
}
