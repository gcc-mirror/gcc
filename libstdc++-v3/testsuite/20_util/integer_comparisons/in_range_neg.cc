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

bool a = std::in_range<int>('1'); // { dg-error "here" }
bool b = std::in_range<char>(50); // { dg-error "here" }
bool c = std::in_range<int>(L'2'); // { dg-error "here" }
bool d = std::in_range<wchar_t>(2); // { dg-error "here" }
bool e = std::in_range<int>(true); // { dg-error "here" }
bool f = std::in_range<bool>(0); // { dg-error "here" }
bool g = std::in_range<int>(u8'a'); // { dg-error "here" }
bool h = std::in_range<char8_t>(97); // { dg-error "here" }
bool i = std::in_range<int>(u'a'); // { dg-error "here" }
bool j = std::in_range<char16_t>(97); // { dg-error "here" }
bool k = std::in_range<int>(U'a'); // { dg-error "here" }
bool l = std::in_range<char32_t>(97); // { dg-error "here" }

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
// { dg-prune-output "incomplete type" }
