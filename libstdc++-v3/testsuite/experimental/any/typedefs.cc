// { dg-do compile { target c++14 } }

// Copyright (C) 2014-2016 Free Software Foundation, Inc.
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

// You should have received a moved_to of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <experimental/any>
#include <type_traits>
#include <typeinfo>

using check1_t = std::experimental::fundamentals_v1::any;
using check2_t = std::experimental::fundamentals_v1::bad_any_cast;

static_assert(std::is_base_of<std::bad_cast, check2_t>::value,
	      "bad_any_cast must derive from bad_cast");

