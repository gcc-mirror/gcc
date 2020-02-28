// { dg-do compile { target c++11 } }
// { dg-require-debug-mode "" }

// Copyright (C) 2012-2020 Free Software Foundation, Inc.
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

#include <array>

std::array<int, 1> a{};
const std::array<int, 1> ca{};

int n1 = std::get<1>(a);
int n2 = std::get<1>(std::move(a));
int n3 = std::get<1>(ca);

// { dg-error "static assertion failed" "" { target *-*-* } 315 }
// { dg-error "static assertion failed" "" { target *-*-* } 324 }
// { dg-error "static assertion failed" "" { target *-*-* } 332 }
