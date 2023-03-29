// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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
// { dg-do compile { target c++2a } }

#include <utility>

bool a = std::cmp_greater_equal('1', 49); // { dg-error "constexpr" }
bool b = std::cmp_greater_equal(50, '2'); // { dg-error "constexpr" }
bool c = std::cmp_greater_equal(2, L'2'); // { dg-error "constexpr" }
bool d = std::cmp_greater_equal(L'2', 2); // { dg-error "constexpr" }
bool e = std::cmp_greater_equal(true, 1); // { dg-error "constexpr" }
bool f = std::cmp_greater_equal(0, false); // { dg-error "constexpr" }
bool g = std::cmp_greater_equal(97, u8'a'); // { dg-error "constexpr" }
bool h = std::cmp_greater_equal(u8'a', 97); // { dg-error "constexpr" }
bool i = std::cmp_greater_equal(97, u'a'); // { dg-error "constexpr" }
bool j = std::cmp_greater_equal(u'a', 97); // { dg-error "constexpr" }
bool k = std::cmp_greater_equal(97, U'a'); // { dg-error "constexpr" }
bool l = std::cmp_greater_equal(U'a', 97); // { dg-error "constexpr" }

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
