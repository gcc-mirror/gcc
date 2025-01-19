// { dg-do run { target c++23 } }
// Copyright (C) 2023-2025 Free Software Foundation, Inc.
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

#include <generator>
#include <iostream>

// basic example
std::generator<int>
bar()
{
  co_yield 3;
  co_yield 4;
}

std::generator<int>
foo()
{
  co_yield 1;
  co_yield 2;
  co_yield std::ranges::elements_of { bar() };
  co_yield 5;
}

int
main()
{
  for (auto x : foo())
    std::cout << x << '\n';
}

// { dg-output {1(\n|\r\n|\r)} }
// { dg-output {2(\n|\r\n|\r)} }
// { dg-output {3(\n|\r\n|\r)} }
// { dg-output {4(\n|\r\n|\r)} }
// { dg-output {5(\n|\r\n|\r)} }
