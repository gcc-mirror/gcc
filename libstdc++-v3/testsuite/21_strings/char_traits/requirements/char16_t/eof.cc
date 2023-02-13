// Copyright (C) 2017-2023 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <string>


constexpr bool not_equal_to_eof(char16_t c)
{
  using T = std::char_traits<char16_t>;
  return T::eq_int_type(T::eof(), T::to_int_type(c)) == false;
}

// Last two code points of the BMP are noncharacters:
static_assert(not_equal_to_eof(u'\uFFFE'), "U+FFFE compares unequal to eof");
static_assert(not_equal_to_eof(u'\uFFFF'), "U+FFFF compares unequal to eof");
