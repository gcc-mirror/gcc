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
#include <array>
#include <ranges>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;
namespace views = std::ranges::views;

void
test01()
{
  using namespace std::literals;
  std::string_view cs[] = {"the", "quick", "brown", "fox"};
  auto v = cs | views::join;
  VERIFY( ranges::equal(v, "thequickbrownfox"sv) );
  using R = decltype(v);
  static_assert(ranges::bidirectional_range<R>);
  static_assert(ranges::bidirectional_range<const R>);
  static_assert(ranges::common_range<R>);
  static_assert(ranges::common_range<const R>);
}

void
test02()
{
  auto v = (views::iota(0,4)
	    | views::transform([] (int i) { return views::iota(0,i); })
	    | views::join);
  VERIFY( ranges::equal(v, (int[]){0,0,1,0,1,2}) );
  using R = decltype(v);
  static_assert(ranges::input_range<R>);
  static_assert(!ranges::range<const R>);
  static_assert(!ranges::forward_range<R>);
  static_assert(!ranges::common_range<const R>);
}

void
test03()
{
  auto v = (views::iota(0,4)
	    | views::transform([] (int i) { return views::iota(0,i); })
	    | views::filter([] (auto) { return true; })
	    | views::join);
  VERIFY( ranges::equal(v, (int[]){0,0,1,0,1,2}) );
  using R = decltype(v);
  static_assert(ranges::input_range<R>);
  static_assert(!ranges::range<const R>);
  static_assert(!ranges::forward_range<R>);
  static_assert(!ranges::common_range<const R>);
}

void
test04()
{
  auto v = (views::iota(0,4)
	    | views::transform([] (int i) { return views::iota(0,i); }));
  auto v2 = ranges::ref_view{v};
  VERIFY( ranges::equal(v2 | views::join, (int[]){0,0,1,0,1,2}) );
  using R = decltype(v2);
  static_assert(ranges::random_access_range<R>);
  static_assert(ranges::range<const R>);
  static_assert(ranges::common_range<const R>);
  static_assert(ranges::random_access_range<ranges::range_reference_t<R>>);
  static_assert(!std::is_reference_v<ranges::range_reference_t<R>>);
}

void
test05()
{
  using namespace std::literals;
  std::vector<std::string> x = {"the", " ", "quick", " ", "brown", " ", "fox"};
  auto v = x | views::join | views::lazy_split(' ');
  auto i = v.begin();
  VERIFY( ranges::equal(*i++, "the"sv) );
  VERIFY( ranges::equal(*i++, "quick"sv) );
  VERIFY( ranges::equal(*i++, "brown"sv) );
  VERIFY( ranges::equal(*i++, "fox"sv) );
  VERIFY( i == v.end() );
}

void
test06()
{
  std::vector<std::string> x = {""};
  auto i = std::counted_iterator(x.begin(), 1);
  auto r = ranges::subrange{i, std::default_sentinel};
  auto v = r | views::transform(std::identity{}) | views::join;

  // Verify that _Iterator<false> is implicitly convertible to _Iterator<true>.
  static_assert(!std::same_as<decltype(ranges::begin(v)),
			      decltype(std::as_const(v).begin())>);
  auto a = std::as_const(v).begin();
  a = ranges::begin(v);

  // Verify that _Sentinel<false> is implicitly convertible to _Sentinel<true>.
  static_assert(!ranges::common_range<decltype(v)>);
  static_assert(!std::same_as<decltype(ranges::end(v)),
			      decltype(std::as_const(v).end())>);
  auto b = std::as_const(v).end();
  b = ranges::end(v);
}

void
test07()
{
  // LWG 3474. Nesting join_views is broken because of CTAD
  std::vector<std::vector<std::vector<int>>> nested_vectors = {
    {{1, 2, 3}, {4, 5}, {6}},
    {{7},       {8, 9}, {10, 11, 12}},
    {{13}}
  };
  auto joined = nested_vectors | std::views::join | std::views::join;

  using V = decltype(joined);
  static_assert( std::same_as<std::ranges::range_value_t<V>, int> );
}

void
test08()
{
  // LWG 3500. join_view::iterator::operator->() is bogus
  struct X { int a; };
  ranges::single_view<ranges::single_view<X>> s{std::in_place, std::in_place, 5};
  auto v = s | views::join;
  auto i = v.begin();
  VERIFY( i->a == 5 );
}

template<auto join = views::join>
void
test09()
{
  // Verify SFINAE behavior.
  static_assert(!requires { join(); });
  static_assert(!requires { join(0, 0); });
  static_assert(!requires { join(0); });
  static_assert(!requires { 0 | join; });
}

void
test10()
{
  // PR libstdc++/100290
  auto v = views::single(0)
    | views::transform([](const auto& s) { return views::single(s); })
    | views::join;
  VERIFY( ranges::next(v.begin()) == v.end() );
}

void
test11()
{
  // Verify P2328 changes.
  int r[] = {1, 2, 3};
  auto v = r
    | views::transform([] (int n) { return std::vector{{n, -n}}; })
    | views::join;
  VERIFY( ranges::equal(v, (int[]){1, -1, 2, -2, 3, -3}) );

  struct S {
    S() = default;
    S(const S&) = delete;
    S(S&&) = delete;
  };
  auto w = r
    | views::transform([] (int) { return std::array<S, 2>{}; })
    | views::join;
  for (auto& i : w)
    ;
}

void
test12()
{
  // PR libstdc++/101263
  constexpr auto b = [] {
    auto r = std::views::iota(0, 5)
      | std::views::lazy_split(0)
      | std::views::join;
    return r.begin() != r.end();
  }();
}

void
test13()
{
  // PR libstdc++/106320
  auto l = std::views::transform([](auto x) {
    return x | std::views::transform([i=0](auto y) {
      return y;
    });
  });
  std::vector<std::vector<int>> v{{5, 6, 7}};
  v | l | std::views::join;
}

void
test14()
{
  // LWG 3569: join_view fails to support ranges of ranges with
  // non-default_initializable iterators
  auto ss = std::istringstream{"1 2 3"};
  auto v = views::single(views::istream<int>(ss));
  using inner = ranges::range_reference_t<decltype(v)>;
  static_assert(ranges::input_range<inner>
		&& !ranges::forward_range<inner>
		&& !std::default_initializable<ranges::iterator_t<inner>>);
  VERIFY( ranges::equal(v | views::join, (int[]){1, 2, 3}) );
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
  test09();
  test10();
  test11();
  test12();
  test13();
  test14();
}
