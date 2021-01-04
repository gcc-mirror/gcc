// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <tuple>
#include <memory>

struct X { };

struct Y
{
  Y(const std::tuple<X>&) = delete;
  Y(std::tuple<X>&&) { throw 1; }
  Y(const X&) { }
};

struct Z
{
  Z(X&&) { }
  Z(const std::tuple<X>&) { throw 1; }
  Z(std::tuple<X>&&) = delete;
};

void
test01()
{
  // PR libstdc++/90700 wrong constraints on constructor
  const std::allocator<int> a;
  const std::tuple<X> x;

  static_assert(!std::is_convertible<const std::tuple<X>&, Y>::value, "");
  static_assert(!std::is_constructible<Y, const std::tuple<X>&>::value, "");
  static_assert(!std::is_same<Y, X>::value, "");
  // should use tuple<Y>::tuple<X>(allocator_arg_t, const A&, const tuple<X>&)
  // and construct Y from X:
  std::tuple<Y> y(std::allocator_arg, a, x);
}

void
test02()
{
  const std::allocator<int> a;
  std::tuple<X> x;

  static_assert(!std::is_convertible<std::tuple<X>, Z>::value, "");
  static_assert(!std::is_constructible<Z, std::tuple<X>>::value, "");
  static_assert(!std::is_same<Z, X>::value, "");
  // should use tuple<Z>::tuple<X>(allocator_arg_t, const A&, tuple<X>&&)
  // and construct Z from X:
  std::tuple<Z> z(std::allocator_arg, a, std::move(x));
}
