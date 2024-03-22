// { dg-do run { target c++23 } }
// Copyright (C) 2023-2024 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <generator>
#include <ranges>
#include <vector>
#include <iostream>

std::generator<int&>
yield_vector()
{
  std::vector foo { 1, 2, 3 };
  auto x = 123;
  co_yield x;
  co_yield std::ranges::elements_of { foo };
  x = 456;
  co_yield x;
}

int
main()
{
  for (auto x : yield_vector())
    std::cout << x << '\n';
}
