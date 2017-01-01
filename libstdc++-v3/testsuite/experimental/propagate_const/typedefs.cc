// { dg-do compile { target c++14 } }

// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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

#include <experimental/propagate_const>
#include <type_traits>
#include <stdexcept>

template <typename T>
using check1_t = std::experimental::fundamentals_v2::propagate_const<T>;
template <typename T>
using check2_t
= typename std::experimental::fundamentals_v2::propagate_const<T>::element_type;

static_assert(std::is_same<check2_t<int*>, int>::value,
	      "element_type must be based on the pointer");
