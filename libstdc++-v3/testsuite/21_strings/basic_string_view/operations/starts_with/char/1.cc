// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

// Copyright (C) 2018-2019 Free Software Foundation, Inc.
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

// basic_string_view begins_with

#include <string_view>

void
test01()
{
  constexpr char cstr_dir[] = "slugs/";
  constexpr std::string_view sv_dir("slugs/");
  constexpr char cstr_dir2[] = "worms/";
  constexpr std::string_view sv_dir2("worms/");

  constexpr std::string_view sv_test("slugs/slimy.jpg");

  constexpr auto cstr_in_slugs = sv_test.starts_with(cstr_dir);
  static_assert(cstr_in_slugs);
  constexpr auto sv_in_slugs = sv_test.starts_with(sv_dir);
  static_assert(sv_in_slugs);
  constexpr auto char_s = sv_test.starts_with('s');
  static_assert(char_s);

  constexpr auto cstr_in_worms = sv_test.starts_with(cstr_dir2);
  static_assert(!cstr_in_worms);
  constexpr auto sv_in_worms = sv_test.starts_with(sv_dir2);
  static_assert(!sv_in_worms);
  constexpr auto char_w = sv_test.starts_with('w');
  static_assert(!char_w);
}
