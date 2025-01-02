// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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
// { dg-add-options no_pch }

#include <tuple>

#ifndef __cpp_lib_make_from_tuple
# error "Feature-test macro for make_from_tuple missing in <tuple>"
#elif __cpp_lib_make_from_tuple < 201606
# error "Feature-test macro for make_from_tuple has the wrong value in <tuple>"
#endif

#include <testsuite_hooks.h>

template <class T, class U, class V>
struct ThreeParam
{
  T t;
  U u;
  V v;
  ThreeParam(const T& t, const U& u, const V& v)
    : t(t),
      u(u),
      v(v) {}
};

void
test01()
{
  auto x = std::make_tuple(1024, 'x', 2048);
  ThreeParam<int, char, int> y
    = std::make_from_tuple<ThreeParam<int, char, int>>(x);
  VERIFY(y.t == 1024 && y.u == 'x' && y.v == 2048);
  auto x2 = std::make_tuple(4096, 'z');
  std::pair<int, char> z = std::make_from_tuple<std::pair<int, char>>(x2);
  VERIFY(z.first == 4096 && z.second == 'z');
  auto x3 = std::make_tuple(8192);
  int i = std::make_from_tuple<int>(x3);
  VERIFY(i == 8192);
}

int
main()
{
  test01();
}
