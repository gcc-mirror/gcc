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
// { dg-add-options no_pch }

#include <latch>

#ifndef __cpp_lib_latch
# error "Feature-test macro for latch missing in <latch>"
#elif __cpp_lib_latch != 201907L
# error "Feature-test macro for latch has wrong value in <latch>"
#endif

static_assert(std::latch::max() > 0);

constinit std::latch l0(0);
constinit std::latch l1(1);
constinit std::latch l2(std::latch::max());

#ifdef _GLIBCXX_RELEASE
static_assert(alignof(std::latch) == std::__detail::__platform_wait_alignment);
#endif
