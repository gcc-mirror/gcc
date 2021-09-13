// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

// Copyright (C) 2017-2021 Free Software Foundation, Inc.
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

#include <string>

template<typename CT>
  constexpr bool
  test_move()
  {
    using char_type = typename CT::char_type;
    char_type s1[3] = {1, 2, 3};
    CT::move(s1+1, s1, 2);
    return s1[0]==char_type{1} && s1[1]==char_type{1} && s1[2]==char_type{2};
  }

#ifndef __cpp_lib_constexpr_string
# error Feature-test macro for constexpr char_traits is missing
#elif __cpp_lib_constexpr_string < 201811
# error Feature-test macro for constexpr char_traits has the wrong value
#endif

// We also provide this non-standard macro for P0426R1 and P1032R1.
#ifndef __cpp_lib_constexpr_char_traits
# error Feature-test macro for constexpr char_traits is missing
#elif __cpp_lib_constexpr_char_traits != 201811
# error Feature-test macro for constexpr char_traits has the wrong value
#endif

static_assert( test_move<std::char_traits<char>>() );
#ifdef _GLIBCXX_USE_WCHAR_T
static_assert( test_move<std::char_traits<wchar_t>>() );
#endif
#ifdef _GLIBCXX_USE_CHAR8_T
static_assert( test_move<std::char_traits<char8_t>>() );
#endif
static_assert( test_move<std::char_traits<char16_t>>() );
static_assert( test_move<std::char_traits<char32_t>>() );

struct C { unsigned char c; };
constexpr bool operator==(const C& c1, const C& c2) { return c1.c == c2.c; }
constexpr bool operator<(const C& c1, const C& c2) { return c1.c < c2.c; }
static_assert( test_move<std::char_traits<C>>() );
