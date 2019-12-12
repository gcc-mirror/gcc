// Copyright (C) 2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <string_view>

// C++98 21.1 [lib.char.traits] p3
// C++03 21.1 [lib.char.traits] p3
// C++11 21.2 [char.traits] p3
// C++14 21.2 [char.traits] p3
// C++17 24.2 [char.traits] p3
// "Traits::char_type shall be the same as CharT."
// C++17 24.4.2 [string.view.template] p1
// "the type traits::char_type shall name the same type as charT"
// C++2a 21.2 [char.traits] p3 (post-P1148R0)
// "If X::char_type is not the same type as C, the program is ill-formed."

std::basic_string_view<char, std::char_traits<char16_t>> s1; // { dg-error "here" }
std::basic_string_view<char32_t, std::char_traits<char>> s2; // { dg-error "here" }

// { dg-prune-output "static assertion failed" }
