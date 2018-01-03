// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <utility>
#include <tuple>
#include <scoped_allocator>

struct do_not_copy {
  do_not_copy() = default;
  do_not_copy(const do_not_copy&) { throw 1; }
};

void
test01()
{
  struct X {
    X(do_not_copy&&) { }
  };

  using pair = std::pair<X, int>;
  std::scoped_allocator_adaptor<std::allocator<pair>> a;
  auto ptr = a.allocate(1);
  a.construct(ptr, std::piecewise_construct,
      std::tuple<do_not_copy>{}, std::make_tuple(1));
  a.deallocate(ptr, 1);
}

void
test02()
{
  struct X {
    using allocator_type = std::allocator<int>;
    X(do_not_copy&&, const allocator_type&) { }
  };

  using pair = std::pair<X, int>;
  std::scoped_allocator_adaptor<std::allocator<pair>> a;
  auto ptr = a.allocate(1);
  a.construct(ptr, std::piecewise_construct,
      std::tuple<do_not_copy>{}, std::make_tuple(1));
  a.deallocate(ptr, 1);
}

void
test03()
{
  struct X {
    using allocator_type = std::allocator<int>;
    X(std::allocator_arg_t, const allocator_type&, do_not_copy&&) { }
  };

  using pair = std::pair<X, int>;
  std::scoped_allocator_adaptor<std::allocator<pair>> a;
  auto ptr = a.allocate(1);
  a.construct(ptr, std::piecewise_construct,
      std::tuple<do_not_copy>{}, std::make_tuple(1));
  a.deallocate(ptr, 1);
}

int main()
{
  test01();
  test02();
  test03();
}
