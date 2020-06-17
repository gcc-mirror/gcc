// Copyright (C) 2020 Free Software Foundation, Inc.
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
#include <string>
#include <string_view>
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
  auto v = x | views::join | views::split(' ');
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
			      decltype(ranges::cbegin(v))>);
  auto a = ranges::cbegin(v);
  a = ranges::begin(v);

  // Verify that _Sentinel<false> is implicitly convertible to _Sentinel<true>.
  static_assert(!ranges::common_range<decltype(v)>);
  static_assert(!std::same_as<decltype(ranges::end(v)),
			      decltype(ranges::cend(v))>);
  auto b = ranges::cend(v);
  b = ranges::end(v);
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
}
