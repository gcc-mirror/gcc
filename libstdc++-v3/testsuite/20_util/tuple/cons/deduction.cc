// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

// Copyright (C) 2017-2019 Free Software Foundation, Inc.
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

#include <tuple>

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
test00()
{
  std::tuple x;
  check_type<std::tuple<>>(x);

  std::tuple copy = x;
  check_type<decltype(x)>(copy);
  std::tuple move = std::move(x);
  check_type<decltype(x)>(move);
}

void
test01()
{
  std::tuple x = 5;
  check_type<std::tuple<int>>(x);
  int y = 42;
  std::tuple x2 = y;
  check_type<std::tuple<int>>(x2);
  const int z = 666;
  std::tuple x3 = z;
  check_type<std::tuple<int>>(x3);
  std::tuple mo = MoveOnly();
  check_type<std::tuple<MoveOnly>>(mo);
  mo = MoveOnly();

  std::tuple copy = x;
  check_type<decltype(x)>(copy);
  std::tuple move = std::move(mo);
  check_type<decltype(mo)>(move);
}

void
test02()
{
  std::tuple x{5, 6u};
  check_type<std::tuple<int, unsigned>>(x);
  int y = 42;
  std::tuple x2{y, 48u};
  check_type<std::tuple<int, unsigned>>(x2);
  const int z = 666;
  std::tuple x3{z, y};
  check_type<std::tuple<int, int>>(x3);
  std::tuple x4{1, x};
  check_type<std::tuple<int, decltype(x)>>(x4);
  std::tuple mo{MoveOnly(), 2l};
  check_type<std::tuple<MoveOnly, long>>(mo);
  mo = {MoveOnly(), 3l};

  std::tuple copy = x;
  check_type<decltype(x)>(copy);
  std::tuple copy2{x};
  check_type<decltype(x)>(copy2);
  std::tuple move = std::move(mo);
  check_type<decltype(mo)>(move);
}

void
test03()
{
  std::tuple x{5, 6u, '7'};
  check_type<std::tuple<int, unsigned, char>>(x);
  int y = 42;
  std::tuple x2{y, 48u, 54l};
  check_type<std::tuple<int, unsigned, long>>(x2);
  const int z = 666;
  std::tuple x3{z, y, x};
  check_type<std::tuple<int, int, decltype(x)>>(x3);
  std::tuple x4{1, x, x2};
  check_type<std::tuple<int, decltype(x), decltype(x2)>>(x4);
  std::tuple mo{MoveOnly(), 2l};
  check_type<std::tuple<MoveOnly, long>>(mo);
  mo = {MoveOnly(), 3l};

  std::tuple copy = x;
  check_type<decltype(x)>(copy);
  std::tuple copy2{x};
  check_type<decltype(x)>(copy2);
  std::tuple move = std::move(mo);
  check_type<decltype(mo)>(move);
}

void
test04()
{
  std::pair<int, unsigned> p;
  std::tuple x = p;
  check_type<std::tuple<int, unsigned>>(x);
  int y = 42;
  std::tuple x2{p};
  check_type<std::tuple<int, unsigned>>(x2);
  const int z = 666;
  std::pair<const int, unsigned> p2;
  std::tuple x3{p2};
  check_type<std::tuple<const int, unsigned>>(x3);
  std::pair<int&, const unsigned&> p3{p.first, p.second};
  std::tuple x4{p3};
  check_type<std::tuple<int&, const unsigned&>>(x4);
  std::tuple mo = std::pair<MoveOnly, MoveOnly>();
  check_type<std::tuple<MoveOnly, MoveOnly>>(mo);

  std::tuple copy = x4;
  check_type<decltype(x4)>(copy);
  std::tuple copy2{x4};
  check_type<decltype(x4)>(copy2);
  std::tuple move = std::move(mo);
  check_type<decltype(mo)>(move);
}

void
test05()
{
  std::allocator<double> a;
  std::tuple x{std::allocator_arg, a, 1};
  check_type<std::tuple<int>>(x);
  std::tuple x2{std::allocator_arg, a, 1, '2'};
  check_type<std::tuple<int, char>>(x2);

  std::pair<float, const short> p{};
  std::tuple x3{std::allocator_arg, a, p};
  check_type<std::tuple<float, const short>>(x3);
  std::tuple x4{std::allocator_arg, a, std::move(p)};
  check_type<std::tuple<float, const short>>(x4);

  std::tuple x5{std::allocator_arg, a, x};
  check_type<decltype(x)>(x5);
  std::tuple x6{std::allocator_arg, a, std::move(x)};
  check_type<decltype(x)>(x6);
}
