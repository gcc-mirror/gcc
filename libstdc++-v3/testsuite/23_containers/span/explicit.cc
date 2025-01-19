// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#include <span>

struct Range
{
  int* begin();
  int* end();
  unsigned size() const;
} r;

auto first = std::begin(r), last = std::end(r);

// span(It, size_type)
std::span<int> s1 = {first, 2};
std::span<int, 2> s2 = {first, 2}; // { dg-error "explicit constructor" }

// span(It, End)
std::span<int> s3 = {first, last};
std::span<int, 2> s4 = {first, last}; // { dg-error "explicit constructor" }

// span(R&&)
std::span<int> s5 = r;
std::span<int, 2> s6 = r; // { dg-error "conversion from" }

// span(const span<OtherElement, OtherExtent>&)
std::span<const int> s7 = s5;
std::span<const int> s8 = s6;
std::span<const int, 1> s9 = s5.first(1);  // { dg-error "conversion from" }
std::span<const int, 1> s10 = s7.first(1); // { dg-error "conversion from" }
