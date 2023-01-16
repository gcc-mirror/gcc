// Copyright (C) 2021-2023 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2b" }
// { dg-do compile { target c++23 } }

#include <version>

#ifndef __cpp_lib_byteswap
# error "Feature-test macro for bit_cast missing in <version>"
#elif __cpp_lib_byteswap != 202110L
# error "Feature-test macro for byteswap has wrong value in <version>"
#endif
