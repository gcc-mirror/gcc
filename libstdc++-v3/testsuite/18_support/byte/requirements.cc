// Copyright (C) 2017 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <cstddef>

#ifndef __cpp_lib_byte
# error "Feature-test macro for byte missing"
#elif __cpp_lib_byte != 201603
# error "Feature-test macro for byte has wrong value"
#endif

static_assert( sizeof(std::byte) == sizeof(unsigned char) );
static_assert( alignof(std::byte) == alignof(unsigned char) );

// Use the built-in to avoid depending on <type_traits>
__underlying_type(std::byte)* p = (unsigned char*)nullptr;
