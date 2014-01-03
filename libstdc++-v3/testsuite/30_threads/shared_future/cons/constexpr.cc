// { dg-do compile }
// { dg-options "-std=gnu++0x -fno-inline -save-temps -g0" }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }
// { dg-require-atomic-builtins "" }
// { dg-final { scan-assembler-not "_ZNSt13shared_futureIvEC2Ev" } }
// { dg-final { scan-assembler-not "_ZNSt13shared_futureIiEC2Ev" } }

// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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

#include <future>
#include <testsuite_common_types.h>

int main()
{
  __gnu_test::constexpr_default_constructible test; //not literal
  test.operator()<std::shared_future<int>>();
  test.operator()<std::shared_future<void>>();
  return 0;
}
