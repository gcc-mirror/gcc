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

#include <ranges>
#include <algorithm>
#include <string>
#include <cctype>
#include <testsuite_hooks.h>

namespace ranges = std::ranges;
namespace views = std::views;
using namespace std::literals;

// P2017R1 "Conditionally borrowed ranges"
auto trim(std::string const& s) {
    auto isalpha = [](unsigned char c){ return std::isalpha(c); };
    auto b = ranges::find_if(s, isalpha);
    auto e = ranges::find_if(s | views::reverse, isalpha).base();
    return ranges::subrange(b, e);
}

void
test01()
{
  VERIFY( ranges::equal( trim("  abc    "), "abc"sv ) );

  std::string s = "0123456789";
  auto odd = [](char c){ return (c - '0') % 2; };
  using namespace std::views;
  auto pos = ranges::find(s | take(5) | drop(1) | drop_while(odd), '2');
  VERIFY( *pos == '2' );

  static_assert( ranges::borrowed_range<decltype(s | reverse)> );
  static_assert( ranges::borrowed_range<decltype(s | take(1))> );
  static_assert( ranges::borrowed_range<decltype(s | drop(1))> );
  static_assert( ranges::borrowed_range<decltype(s | drop_while(odd))> );

  ranges::subrange r(s.begin(), s.cend());
  static_assert( !ranges::common_range<decltype(r)> );
  auto pos2 = ranges::find(r | views::common, '2');
  VERIFY( *pos2 == '2' );
}

void
test02()
{
  std::pair<int, std::string_view> a[2]{ {1,"two"}, {3,"four"}};
  auto pos = ranges::find(a | views::values, "four");
  VERIFY( *pos == "four" );

  static_assert( ranges::borrowed_range<decltype(a | views::keys)> );
}

int main()
{
  test01();
  test02();
}
