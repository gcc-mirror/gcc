// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

// Copyright (C) 2020 Free Software Foundation, Inc.
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
  // PR libstdc++/90415
  static_assert( std::is_copy_constructible<std::tuple<std::any>>::value );
}

struct wrapper
{
  wrapper() = default;

  wrapper(const std::any& t);

  wrapper(const wrapper& w);

  auto& operator=(const std::any& t);

  auto& operator=(const wrapper& w)
  {
    value = w.value;
    return *this;
  }

  std::any value;
};

void
test02()
{
  // PR libstdc++/91630
  wrapper a, b;
  a = b;
}

int main()
{
  test01();
  test02();
}
