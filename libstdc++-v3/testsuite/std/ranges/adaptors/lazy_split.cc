// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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
#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_range;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::input_iterator_wrapper;

namespace ranges = std::ranges;
namespace views = std::ranges::views;

using namespace std::literals;

void
test01()
{
  auto x = "the  quick  brown  fox"sv;
  auto p = std::string{"  "};
  auto v = x | views::lazy_split(views::all(p)); // views::all is needed here after P2281.
  auto i = v.begin();
  VERIFY( ranges::equal(*i++, "the"sv) );
  VERIFY( ranges::equal(*i++, "quick"sv) );
  VERIFY( ranges::equal(*i++, "brown"sv) );
  VERIFY( ranges::equal(*i++, "fox"sv) );
  VERIFY( i == v.end() );
}

void
test02()
{
  auto x = "the quick brown fox"sv;
  auto v = x | views::lazy_split(' ');
  auto i = v.begin();
  VERIFY( ranges::equal(*i++, "the"sv) );
  VERIFY( ranges::equal(*i++, "quick"sv) );
  VERIFY( ranges::equal(*i++, "brown"sv) );
  VERIFY( ranges::equal(*i++, "fox"sv) );
  VERIFY( i == v.end() );
}

void
test03()
{
  char x[] = "the quick brown fox";
  test_range<char, forward_iterator_wrapper> rx(x, x+sizeof(x)-1);
  auto v = rx | views::lazy_split(' ');
  auto i = v.begin();
  VERIFY( ranges::equal(*i++, "the"sv) );
  VERIFY( ranges::equal(*i++, "quick"sv) );
  VERIFY( ranges::equal(*i++, "brown"sv) );
  VERIFY( ranges::equal(*i++, "fox"sv) );
  VERIFY( i == v.end() );
}

void
test04()
{
  auto x = "the  quick  brown  fox"sv;
  std::initializer_list<char> p = {' ', ' '};
  static_assert(!ranges::view<decltype(p)>);
  static_assert(std::same_as<decltype(p | views::all),
			     ranges::ref_view<decltype(p)>>);
  auto v = x | views::lazy_split(views::all(p)); // views::all is needed here after P2281.
  auto i = v.begin();
  VERIFY( ranges::equal(*i++, "the"sv) );
  VERIFY( ranges::equal(*i++, "quick"sv) );
  VERIFY( ranges::equal(*i++, "brown"sv) );
  VERIFY( ranges::equal(*i++, "fox"sv) );
  VERIFY( i == v.end() );
}

void
test05()
{
  auto as_string = [](ranges::view auto rng) {
    auto in = rng | views::common;
    return std::string(in.begin(), in.end());
  };
  std::string str
    = "Now is the time for all good men to come to the aid of their county.";
  auto rng
    = str | views::lazy_split(' ') | views::transform(as_string) | views::common;
  std::vector<std::string> words(rng.begin(), rng.end());
  auto not_space_p = [](char c) { return c != ' '; };
  VERIFY( ranges::equal(words | views::join,
			str | views::filter(not_space_p)) );
}

void
test06()
{
  std::string str = "hello world";
  auto v = str | views::transform(std::identity{}) | views::lazy_split(' ');

  // Verify that _Iterator<false> is implicitly convertible to _Iterator<true>.
  static_assert(!std::same_as<decltype(ranges::begin(v)),
			      decltype(ranges::cbegin(v))>);
  auto b = ranges::cbegin(v);
  b = ranges::begin(v);
}

void
test07()
{
  char str[] = "banana split";
  auto split = str | views::lazy_split(' ');
  auto val = *split.begin();
  auto b = val.begin();
  auto b2 = b++;
  static_assert( noexcept(iter_move(b)) );
  static_assert( noexcept(iter_swap(b, b2)) );
}

void
test08()
{
  char x[] = "the quick brown fox";
  test_range<char, input_iterator_wrapper> rx(x, x+sizeof(x)-1);
  auto v = rx | views::lazy_split(' ');
  auto i = v.begin();
  VERIFY( ranges::equal(*i, "the"sv) );
  ++i;
  VERIFY( ranges::equal(*i, "quick"sv) );
  ++i;
  VERIFY( ranges::equal(*i, "brown"sv) );
  ++i;
  VERIFY( ranges::equal(*i, "fox"sv) );
  ++i;
  VERIFY( i == v.end() );
}

template<auto lazy_split = views::lazy_split>
void
test09()
{
  // Verify SFINAE behavior.
  std::string s, p;
  static_assert(!requires { lazy_split(); });
  static_assert(!requires { lazy_split(s, p, 0); });
  static_assert(!requires { lazy_split(p)(); });
  static_assert(!requires { s | lazy_split; });

  // Test the case where the closure object is used as an rvalue and therefore
  // the copy of p is forwarded as an rvalue.
  // This used to be invalid, but is now well-formed after P2415R2 relaxed
  // the requirements of viewable_range to admit rvalue non-view non-borrowed
  // ranges such as std::string&&.
  static_assert(requires { s | lazy_split(p); });
  static_assert(requires { lazy_split(p)(s); });
  static_assert(requires { s | (lazy_split(p) | views::all); });
  static_assert(requires { (lazy_split(p) | views::all)(s); });

  static_assert(requires { s | lazy_split(views::all(p)); });
  static_assert(requires { lazy_split(views::all(p))(s); });
  static_assert(requires { s | (lazy_split(views::all(p)) | views::all); });
  static_assert(requires { (lazy_split(views::all(p)) | views::all)(s); });

  auto adapt = lazy_split(p);
  static_assert(requires { s | adapt; });
  static_assert(requires { adapt(s); });

  auto adapt2 = lazy_split(p) | views::all;
  static_assert(requires { s | adapt2; });
  static_assert(requires { adapt2(s); });
}

void
test10()
{
  // LWG 3505
  auto to_string = [] (auto r) {
    return std::string(r.begin(), ranges::next(r.begin(), r.end()));
  };
  auto v = "xxyx"sv | views::lazy_split("xy"sv) | views::transform(to_string);
  VERIFY( ranges::equal(v, (std::string_view[]){"x", "x"}) );
}

void
test11()
{
  // LWG 3478
  auto v = views::lazy_split("text"sv, "text"sv);
  auto i = v.begin();
  VERIFY( ranges::empty(*i++) );
  VERIFY( ranges::empty(*i++) );
  VERIFY( i == v.end() );

  static_assert(ranges::distance(views::lazy_split(" text "sv, ' ')) == 3);
  static_assert(ranges::distance(views::lazy_split(" t e x t "sv, ' ')) == 6);
  static_assert(ranges::distance(views::lazy_split("  text  "sv, "  "sv)) == 3);
  static_assert(ranges::distance(views::lazy_split("  text    "sv, "  "sv)) == 4);
  static_assert(ranges::distance(views::lazy_split("  text     "sv, "  "sv)) == 4);
  static_assert(ranges::distance(views::lazy_split("t"sv, 't')) == 2);
  static_assert(ranges::distance(views::lazy_split("text"sv, ""sv)) == 4);
}

constexpr bool
test12()
{
  // LWG 3904
  auto r = views::single(0) | views::lazy_split(0);
  auto i = r.begin();
  ++i;
  VERIFY( i != r.end() );
  decltype(std::as_const(r).begin()) j = i;
  VERIFY( j != r.end() );

  return true;
}

void
test13()
{
  // PR libstdc++/119962 - __maybe_present_t misses initialization
  constexpr decltype(views::lazy_split(views::single(0), 0).begin()) it;
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
  static_assert(test12());
  test13();
}
