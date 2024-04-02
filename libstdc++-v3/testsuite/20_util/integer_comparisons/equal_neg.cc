// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <utility>

bool a = std::cmp_equal('1', 49); // { dg-error "here" }
bool b = std::cmp_equal(50, '2'); // { dg-error "here" }
bool c = std::cmp_equal(2, L'2'); // { dg-error "here" }
bool d = std::cmp_equal(L'2', 2); // { dg-error "here" }
bool e = std::cmp_equal(true, 1); // { dg-error "here" }
bool f = std::cmp_equal(0, false); // { dg-error "here" }
bool g = std::cmp_equal(97, u8'a'); // { dg-error "here" "" { target { no-opts "-fno-char8_t" } } }
bool h = std::cmp_equal(u8'a', 97); // { dg-error "here" "" { target { no-opts "-fno-char8_t" } } }
bool i = std::cmp_equal(97, u'a'); // { dg-error "here" }
bool j = std::cmp_equal(u'a', 97); // { dg-error "here" }
bool k = std::cmp_equal(97, U'a'); // { dg-error "here" }
bool l = std::cmp_equal(U'a', 97); // { dg-error "here" }

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
