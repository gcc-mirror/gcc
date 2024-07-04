// Copyright (C) 2016-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

#include <memory_resource>
#include <utility>
#include <tuple>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

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
  std::pmr::polymorphic_allocator<pair> a;
  auto ptr = a.allocate(1);
  a.construct(ptr, std::piecewise_construct,
      std::tuple<do_not_copy>{}, std::make_tuple(1));
  a.deallocate(ptr, 1);
}

void
test02()
{
  struct X {
    using allocator_type = std::pmr::polymorphic_allocator<int>;
    X(do_not_copy&&, const allocator_type&) { }
  };

  using pair = std::pair<X, int>;
  std::pmr::polymorphic_allocator<pair> a;
  auto ptr = a.allocate(1);
  a.construct(ptr, std::piecewise_construct,
      std::tuple<do_not_copy>{}, std::make_tuple(1));
  a.deallocate(ptr, 1);
}

void
test03()
{
  struct X {
    using allocator_type = std::pmr::polymorphic_allocator<int>;
    X(std::allocator_arg_t, const allocator_type&, do_not_copy&&) { }
  };

  using pair = std::pair<X, int>;
  std::pmr::polymorphic_allocator<pair> a;
  auto ptr = a.allocate(1);
  a.construct(ptr, std::piecewise_construct,
      std::tuple<do_not_copy>{}, std::make_tuple(1));
  a.deallocate(ptr, 1);
}

void
test04()
{
  struct X
  {
    using allocator_type = std::pmr::polymorphic_allocator<int>;
    X() = default;
    X(const X&) { throw 1; }
    X(const X&, const allocator_type&) { }
  };

  struct Y
  {
    using allocator_type = std::pmr::polymorphic_allocator<int>;
    Y() = default;
    Y(const Y&) = delete;
    Y(std::allocator_arg_t, const allocator_type&, const Y&) { }
  };

  using pair_type = std::pair<X, Y>;
  std::pmr::polymorphic_allocator<pair_type> a;
  auto ptr = a.allocate(1);
  /* not const */ pair_type p;
  a.construct(ptr, p); // LWG 2975
  a.deallocate(ptr, 1);
}

void
test05()
{
  struct X {
    using allocator_type = std::pmr::polymorphic_allocator<char>;
    X(int);
    X(int, const allocator_type&) { }
  };
  std::pmr::polymorphic_allocator<X> a;
  auto ptr = a.allocate(1);
  a.construct(ptr, 1);
  a.deallocate(ptr, 1);
}

// P0591R4 makes uses-allocator construction apply recursively for nested pairs
void
test06()
{
  struct X
  {
    using allocator_type = std::pmr::polymorphic_allocator<int>;
    X() = default;
    X(const X&) { throw 1; }
    X(const X&, const allocator_type& a) : mr(a.resource()) { }

    std::pmr::memory_resource* mr = nullptr;
  };

  struct Y
  {
    using allocator_type = std::pmr::polymorphic_allocator<int>;
    Y() = default;
    Y(const Y&) = delete;
    Y(std::allocator_arg_t, const allocator_type& a, const Y&)
    : mr(a.resource()) { }

    std::pmr::memory_resource* mr = nullptr;
  };

  using value_type = std::pair<std::pair<X, int>, std::pair<int, Y>>;
  __gnu_test::memory_resource mr;
  std::pmr::polymorphic_allocator<int> a(&mr);
  std::pmr::vector<value_type> v(a);
  VERIFY( v.get_allocator().resource() == &mr );

  value_type val;
  val.first.second = 2;
  val.second.first = 3;
  v.push_back(val);
  X& x = v.back().first.first;
  VERIFY( x.mr != val.first.first.mr );
  VERIFY( x.mr == &mr );

  Y& y = v.back().second.second;
  VERIFY( y.mr != val.second.second.mr );
  VERIFY( y.mr == &mr );

  // Check other members of the pairs are correctly initialized too:
  VERIFY( v.back().first.second == val.first.second );
  VERIFY( v.back().second.first == val.second.first );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
}
