// { dg-do compile { target c++11 } }
//
// Copyright (C) 2013-2024 Free Software Foundation, Inc.
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

#include <cstddef>
#include <type_traits>

#if __cplusplus <= 201703L
static_assert (std::is_pod<std::max_align_t>::value, "");
#endif
static_assert (std::is_standard_layout<std::max_align_t>::value, "");
static_assert (std::is_trivial<std::max_align_t>::value, "");
