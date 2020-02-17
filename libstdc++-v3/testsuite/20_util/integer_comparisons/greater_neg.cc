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
// { dg-do compile { target c++2a } }

#include <utility>

bool a = std::cmp_greater('1', 49); // { dg-error "here" }
bool b = std::cmp_greater(50, '2'); // { dg-error "here" }
bool c = std::cmp_greater(2, L'2'); // { dg-error "here" }
bool d = std::cmp_greater(L'2', 2); // { dg-error "here" }
bool e = std::cmp_greater(true, 1); // { dg-error "here" }
bool f = std::cmp_greater(0, false); // { dg-error "here" }
bool g = std::cmp_greater(97, u8'a'); // { dg-error "here" }
bool h = std::cmp_greater(u8'a', 97); // { dg-error "here" }
bool i = std::cmp_greater(97, u'a'); // { dg-error "here" }
bool j = std::cmp_greater(u'a', 97); // { dg-error "here" }
bool k = std::cmp_greater(97, U'a'); // { dg-error "here" }
bool l = std::cmp_greater(U'a', 97); // { dg-error "here" }

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
