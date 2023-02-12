// { dg-do compile { target c++17 }  }

// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

#include <optional>
#include <type_traits>
#include <stdexcept>

template<typename T>
  using check1_t = std::optional<T>;

using check2_t = std::in_place_t;
using check3_t = std::nullopt_t;
using check4_t = std::bad_optional_access;

static_assert(!std::is_base_of<std::logic_error, check4_t>::value,
	      "bad_optional_access must derive from exception");
static_assert(std::is_base_of<std::exception, check4_t>::value,
	      "bad_optional_access must derive from exception");
