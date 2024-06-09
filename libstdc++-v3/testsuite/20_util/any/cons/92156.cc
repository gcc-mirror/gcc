// { dg-do run { target c++17 } }
// { dg-options "-Wno-init-list-lifetime" }

// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

#include <any>
#include <utility>
#include <tuple>
#include <testsuite_hooks.h>

void
test01()
{
  auto a = std::any(std::in_place_type<std::any>, 5);
  VERIFY( std::any_cast<int>(std::any_cast<std::any>(a)) == 5 );

  auto b = std::any(std::in_place_type<std::any>, {1});
  (void) std::any_cast<std::initializer_list<int>>(std::any_cast<std::any>(b));
}

void
test02()
{
  std::any p = std::pair<std::any, std::any>(1, 1);
  auto pt = std::any_cast<std::pair<std::any, std::any>>(p);
  VERIFY( std::any_cast<int>(pt.first) == 1 );
  VERIFY( std::any_cast<int>(pt.second) == 1 );

  std::any t = std::tuple<std::any>(1);
  auto tt = std::any_cast<std::tuple<std::any>>(t);
  VERIFY( std::any_cast<int>(std::get<0>(tt)) == 1 );
}

int main()
{
  test01();
  test02();
}
