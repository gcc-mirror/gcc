// Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }

#include <variant>
#include <testsuite_hooks.h>

void
test01()
{
  std::variant<std::monostate> v1, v2;
  std::swap(v1, v2);
}

void
test02()
{
  std::variant<int> v1{1}, v2{2};
  std::swap(v1, v2);
  VERIFY( std::get<0>(v1) == 2 );
  VERIFY( std::get<0>(v2) == 1 );
}

void
test03()
{
  std::variant<double, int> v1{1}, v2{0.5};
  std::swap(v1, v2);
  VERIFY( std::get<double>(v1) == 0.5 );
  VERIFY( std::get<int>(v2) == 1 );
}

int
main()
{
  test01();
  test02();
  test03();
}
