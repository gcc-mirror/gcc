// Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

#include <cmath>
// Also make names from <cmath> available in the global namespace:
#include <math.h>

bool foo(double d)
{
  return ::isfinite(d); // PR libstdc++/14608
}

int bar(double d)
{
  return ::signbit(d); // PR libstdc++/44611
}

constexpr bool is_double(double) { return true; }
template<typename T> constexpr bool is_double(T) { return false; }
using type = decltype(::abs(1.5));
static_assert(is_double(type{}), "::abs(double) overload exists in <math.h>");
