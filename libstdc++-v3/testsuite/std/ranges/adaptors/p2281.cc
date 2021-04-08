// Copyright (C) 2021 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

namespace ranges = std::ranges;
namespace views = std::ranges::views;

// Verify P2281 changes to the forwarding semantics of partial application
// and composition of range adaptor objects.

void
test01()
{
  auto split_into_strings = [] (auto p) {
    return views::split(p) | views::transform([](auto r){
      return std::string(r.begin(), ranges::next(r.begin(), r.end()));
    });
  };
  constexpr std::string_view s = "hello world";
  constexpr std::string_view p = " ";
  constexpr auto v1 = s | split_into_strings(p);
  constexpr auto v2 = split_into_strings(p)(s);
  VERIFY( ranges::equal(v1, (std::string_view[]){"hello", "world"}) );
  VERIFY( ranges::equal(v2, (std::string_view[]){"hello", "world"}) );
}

struct move_only_range
{
  move_only_range() { }
  move_only_range(move_only_range&&);
  move_only_range& operator=(move_only_range&&);
  move_only_range(const move_only_range&) = delete;
  move_only_range& operator=(const move_only_range&) = delete;
  char* begin();
  char* end();
};

template<>
  inline constexpr bool std::ranges::enable_view<move_only_range> = true;

template<auto split = views::split>
void
test02()
{
  std::string_view s;
  move_only_range p;
  static_assert(requires { s | split(std::move(p)); });
  static_assert(requires { split(std::move(p))(s); });
  static_assert(requires { split(std::move(p)) | views::all; });
  static_assert(requires { views::all | split(std::move(p)); });
  static_assert(!requires { split(p); });
  static_assert(!requires { split(p) | views::all; });
  static_assert(!requires { views::all | split(p); });
}

int
main()
{
  test01();
  test02();
}
