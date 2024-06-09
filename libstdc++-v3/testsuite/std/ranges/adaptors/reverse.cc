// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

#include <algorithm>
#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_range;
using __gnu_test::bidirectional_iterator_wrapper;

namespace ranges = std::ranges;
namespace views = ranges::views;

void
test01()
{
  int x[] = {1,2,3,4,5};
  auto v = x | views::reverse;
  VERIFY( ranges::equal(v, (int[]){5,4,3,2,1}) );
  VERIFY( ranges::equal(v | views::reverse, x) );
  static_assert(ranges::view<decltype(v)>);
  static_assert(ranges::sized_range<decltype(v)>);
  static_assert(ranges::common_range<decltype(v)>);
  static_assert(ranges::random_access_range<decltype(v)>);
}

void
test02()
{
  int x[] = {1,2,3,4,5};
  test_range<int, bidirectional_iterator_wrapper> rx(x);
  auto v = views::reverse(rx);
  VERIFY( ranges::equal(v, (int[]){5,4,3,2,1}) );
  VERIFY( ranges::equal(v | views::reverse, rx) );
  static_assert(ranges::view<decltype(v)>);
  static_assert(!ranges::sized_range<decltype(v)>);
  static_assert(ranges::common_range<decltype(v)>);
  static_assert(!ranges::random_access_range<decltype(v)>);
  static_assert(ranges::bidirectional_range<decltype(v)>);
}

void
test03()
{
  int x[] = {1,7,3,6,5,2,4,8};
  auto is_even = [] (int i) { return i%2==0; };
  int sum = 0;
  for (auto i : x | views::reverse | views::filter(is_even))
    sum += i;
  VERIFY( sum == 20 );
}

void
test04()
{
  int x[] = {1,2,3,4,5};
  VERIFY( ranges::equal(x | views::reverse | (views::reverse | views::reverse),
			(int[]){5,4,3,2,1}) );
}

// The following tests that reverse_view::begin caches its result.

template<typename T>
struct test_wrapper : bidirectional_iterator_wrapper<T>
{
  static inline int increment_count = 0;

  using bidirectional_iterator_wrapper<T>::bidirectional_iterator_wrapper;

  test_wrapper() : bidirectional_iterator_wrapper<T>()
  { }

  test_wrapper
  operator++(int)
  {
    auto tmp = *this;
    ++*this;
    return tmp;
  }

  test_wrapper&
  operator++()
  {
    ++increment_count;
    bidirectional_iterator_wrapper<T>::operator++();
    return *this;
  }

  test_wrapper
  operator--(int)
  {
    auto tmp = *this;
    --*this;
    return tmp;
  }

  test_wrapper&
  operator--()
  {
    bidirectional_iterator_wrapper<T>::operator--();
    return *this;
  }
};

void
test05()
{
  int x[] = {1,2,3,4,5};
  test_range<int, test_wrapper> rx(x);
  auto v = rx | views::reverse;
  VERIFY( ranges::equal(v, (int[]){5,4,3,2,1}) );
  VERIFY( ranges::equal(v, (int[]){5,4,3,2,1}) );
  VERIFY( test_wrapper<int>::increment_count == 5 );
}

namespace test_ns
{
  struct A {};
  template <typename T>
  void make_reverse_iterator(T&&) {}
} // namespace test_ns

void
test06()
{
  // Check that views::reverse works and does not use ADL which could lead
  // to accidentally finding test_ns::make_reverse_iterator(A*).
  test_ns::A as[] = {{}, {}};
  auto v = as | std::views::reverse;
  using V = decltype(v);
  static_assert( std::ranges::view<V> );
  static_assert( std::ranges::range<const V> );
}

template<auto reverse = views::reverse>
void
test07()
{
  // Verify SFINAE behavior.
  static_assert(!requires { reverse(); });
  static_assert(!requires { reverse(0, 0); });
  static_assert(!requires { reverse(0); });
  static_assert(!requires { 0 | reverse; });
}

void
test08()
{
  // PR libstdc++/100639
  auto v = views::iota(1701ll, 3000ll) | views::reverse | views::take(5);
  for (auto x : v)
    ;
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();
}
