// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

// Copyright (C) 2020 Free Software Foundation, Inc.
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

#include <any>
#include <testsuite_hooks.h>

struct aggressive_aggregate
{
    int a;
    int b;
};

void test01()
{
  std::any x{std::in_place_type<aggressive_aggregate>, 1, 2};
  VERIFY(std::any_cast<aggressive_aggregate>(x).a == 1);
  VERIFY(std::any_cast<aggressive_aggregate>(x).b == 2);
  std::any y{std::in_place_type<aggressive_aggregate>, 1};
  VERIFY(std::any_cast<aggressive_aggregate>(y).a == 1);
  VERIFY(std::any_cast<aggressive_aggregate>(y).b == 0);
  std::any z{std::in_place_type<aggressive_aggregate>};
  VERIFY(std::any_cast<aggressive_aggregate>(z).a == 0);
  VERIFY(std::any_cast<aggressive_aggregate>(z).b == 0);
}

int main()
{
  test01();
}
