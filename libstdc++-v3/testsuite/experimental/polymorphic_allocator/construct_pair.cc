// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++14 } }

#include <experimental/memory_resource>
#include <utility>
#include <tuple>

struct A { };

void
test01()
{
  struct X {
    X(A&&) { }
  };

  using pair = std::pair<X, int>;
  std::experimental::pmr::polymorphic_allocator<pair> a;
  auto ptr = a.allocate(1);
  a.construct(ptr, std::piecewise_construct,
      std::tuple<A>{}, std::make_tuple(1));
  a.deallocate(ptr, 1);
}

void
test02()
{
  struct X {
    using allocator_type = std::experimental::pmr::polymorphic_allocator<int>;
    X(A&&, const allocator_type&) { }
  };

  using pair = std::pair<X, int>;
  std::experimental::pmr::polymorphic_allocator<pair> a;
  auto ptr = a.allocate(1);
  a.construct(ptr, std::piecewise_construct,
      std::tuple<A>{}, std::make_tuple(1));
  a.deallocate(ptr, 1);
}

void
test03()
{
  struct X {
    using allocator_type = std::experimental::pmr::polymorphic_allocator<int>;
    X(std::allocator_arg_t, const allocator_type&, A&&) { }
  };

  using pair = std::pair<X, int>;
  std::experimental::pmr::polymorphic_allocator<pair> a;
  auto ptr = a.allocate(1);
  a.construct(ptr, std::piecewise_construct,
      std::tuple<A>{}, std::make_tuple(1));
  a.deallocate(ptr, 1);
}

int main()
{
  test01();
  test02();
  test03();
}
