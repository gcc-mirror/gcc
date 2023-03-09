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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <algorithm>
#include <ranges>
#include <sstream>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

namespace ranges = std::ranges;
namespace views = std::views;

struct X : __gnu_test::rvalstruct
{
  char c;

  friend std::istream&
  operator>>(std::istream& is, X& m)
  {
    is >> m.c;
    return is;
  }
};


void
test01()
{
  std::string s = "0123456789";
  auto ss = std::istringstream{s};
  auto v = ranges::istream_view<X>(ss);
  VERIFY( ranges::equal(v, s, {}, &X::c) );
}

void
test02()
{
  auto ints = std::istringstream{"0 1  2   3     4"};
  int x[5];
  ranges::copy(ranges::istream_view<int>(ints), x);
  VERIFY( ranges::equal(x, (int[]){0,1,2,3,4}) );
}

void
test03()
{
  auto input = std::istringstream{"0 1 2 3 4 5 6 7 8 9"};
  auto small = [](const auto x) noexcept { return x < 5; };
  auto v = ranges::istream_view<int>(input) | views::take_while(small);
  VERIFY( ranges::equal(v, (int[]){0,1,2,3,4}) );
}

template<typename T>
concept has_iterator_category = requires { typename T::iterator_category; };

void
test04()
{
  std::istringstream s("12345");
  auto v = ranges::istream_view<char>(s);
  // LWG 3397
  using It = ranges::iterator_t<decltype(v)>;
  static_assert(!has_iterator_category<It>);
  static_assert(std::input_iterator<It>);
  static_assert(!std::forward_iterator<It>);
}

void
test05()
{
  // PR libstdc++/101231
  auto words = std::istringstream{"42"};
  auto is = ranges::istream_view<int>(words);
  auto r = is | views::filter([](auto) { return true; });
  for (auto x : r)
    ;
}

void
test06()
{
  // Default template argument
  using V = std::ranges::basic_istream_view<int, char>;
  using W = std::ranges::basic_istream_view<int, char, std::char_traits<char>>;
  static_assert( std::is_same_v<V, W> );
}

void
test07()
{
  // P2432R1, views::istream
  auto nums = std::istringstream("0 1 2 3 4");
  ranges::istream_view<int> v(nums);
  int sum = 0;
  for (int val : views::istream<int>(nums))
    sum += val;
  VERIFY( sum == 10 );
}

template<class T, class U>
concept can_istream_view = requires (U u) { views::istream<T>(u); };

void
test08()
{
  // Verify SFINAE behavior.
  struct S { };
  static_assert(!can_istream_view<S, std::istringstream>);
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
