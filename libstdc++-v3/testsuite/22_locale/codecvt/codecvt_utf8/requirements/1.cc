// Copyright (C) 2015-2016 Free Software Foundation, Inc.
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
// { dg-require-cstdint "" }

#include <codecvt>
#include <type_traits>

template<typename C>
  using codecvt = std::codecvt<C, char, std::mbstate_t>;

using std::is_base_of;

static_assert(
    is_base_of<codecvt<char16_t>, std::codecvt_utf8<char16_t>>::value,
    "codecvt_utf8<char16_t> has wrong base class");

static_assert(
    is_base_of<codecvt<char32_t>, std::codecvt_utf8<char32_t>>::value,
    "codecvt_utf8<char32_t> has wrong base class");
