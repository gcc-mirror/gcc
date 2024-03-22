// { dg-do run { target c++20 } }

// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_hooks.h>

struct aggressive_aggregate
{
    int a;
    int b;
};

void test01()
{
  std::pair<aggressive_aggregate, int> x{std::piecewise_construct,
					 std::tuple{1, 2}, std::tuple{42}};
  VERIFY(x.first.a == 1);
  VERIFY(x.first.b == 2);
  std::pair<aggressive_aggregate, int> y{std::piecewise_construct,
					 std::tuple{1}, std::tuple{42}};
  VERIFY(y.first.a == 1);
  VERIFY(y.first.b == 0);
  std::pair<aggressive_aggregate, int> z{std::piecewise_construct,
					 std::tuple{}, std::tuple{42}};
  VERIFY(z.first.a == 0);
  VERIFY(z.first.b == 0);
}

int main()
{
  test01();
}
