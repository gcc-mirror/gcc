// { dg-do run { target c++20 } }

// Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

// basic_string begins_with

#include <testsuite_string.h>
#include <testsuite_hooks.h>

void
test01()
{
  const char cstr_dir[] = "slugs/";
  const std::string_view sv_dir("slugs/");
  const char cstr_dir2[] = "worms/";
  const std::string_view sv_dir2("worms/");

  const __gnu_test::string s_test("slugs/slimy.jpg");

  const auto cstr_in_slugs = s_test.starts_with(cstr_dir);
  VERIFY( cstr_in_slugs );
  const auto sv_in_slugs = s_test.starts_with(sv_dir);
  VERIFY( sv_in_slugs );
  const auto char_s = s_test.starts_with('s');
  VERIFY( char_s );

  const auto cstr_in_worms = s_test.starts_with(cstr_dir2);
  VERIFY( !cstr_in_worms );
  const auto sv_in_worms = s_test.starts_with(sv_dir2);
  VERIFY( !sv_in_worms );
  const auto char_w = s_test.starts_with('w');
  VERIFY( !char_w );
}

int
main()
{
  test01();
  return 0;
}
