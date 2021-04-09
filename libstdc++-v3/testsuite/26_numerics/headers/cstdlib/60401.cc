// Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

// PR libstdc++/60401

#include <stdlib.h>

constexpr bool is_long(long) { return true; }
template<typename T> constexpr bool is_long(T) { return false; }
using type = decltype(::abs(1L));
static_assert(is_long(type{}), "::abs(long) overload exists in <stdlib.h>");
