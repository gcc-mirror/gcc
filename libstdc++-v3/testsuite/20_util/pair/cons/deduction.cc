// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

// Copyright (C) 2017 Free Software Foundation, Inc.
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

#include <utility>

template<typename T, typename U> struct require_same;
template<typename T> struct require_same<T, T> { using type = void; };

template<typename T, typename U>
  typename require_same<T, U>::type
  check_type(U&) { }

struct MoveOnly
{
  MoveOnly() = default;
  MoveOnly(MoveOnly&&);
  MoveOnly& operator=(MoveOnly&&);
};

void
test01()
{
  std::pair x{5, 6u};
  check_type<std::pair<int, unsigned>>(x);
  int y = 42;
  std::pair x2{y, 48u};
  check_type<std::pair<int, unsigned>>(x2);
  const int z = 666;
  std::pair x3{z, y};
  check_type<std::pair<int, int>>(x3);
  std::pair x4{1, x};
  check_type<std::pair<int, std::pair<int, unsigned>>>(x4);
  std::pair mo{MoveOnly(), 2l};
  check_type<std::pair<MoveOnly, long>>(mo);
  mo = {MoveOnly(), 3l};

  std::pair copy = x;
  check_type<decltype(x)>(copy);
  std::pair copy2{x};
  check_type<decltype(x)>(copy2);
  std::pair move = std::move(mo);
  check_type<decltype(mo)>(move);
}
