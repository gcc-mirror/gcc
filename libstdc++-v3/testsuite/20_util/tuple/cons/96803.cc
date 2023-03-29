// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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
// FIXME [!HOSTED]: avoidable std::allocator usage
// { dg-require-effective-target hosted }

#include <tuple>
#include <memory>

struct X
{
  using allocator_type = std::allocator<int>;

  X(X&&) { }
  X(std::allocator_arg_t, const allocator_type&, X&&) { }

  explicit X(int) { }
  explicit X(int, allocator_type) { }
};

void
test01()
{
  // PR libstdc++/96803
  // std::tuple chooses wrong constructor for uses-allocator construction
  std::tuple<int> o;
  std::tuple<X> nok(std::allocator_arg, std::allocator<int>(), o);

  std::tuple<int, int> oo;
  std::tuple<X, X> nn(std::allocator_arg, std::allocator<int>(), oo);
}

struct Y
{
  using allocator_type = std::allocator<int>;

  Y(const X&) { }
  Y(const X&, const allocator_type&) { }

  Y(X&&) { }
  Y(std::allocator_arg_t, const allocator_type&, X&&) { }
};

void
test02()
{
  std::tuple<X, X> o{1, 1};
  std::tuple<Y, Y> oo(std::allocator_arg, std::allocator<int>(), o);
}
