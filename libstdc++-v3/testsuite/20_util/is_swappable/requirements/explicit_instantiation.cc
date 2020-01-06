// { dg-options "-std=gnu++17" }
// { dg-do compile }

// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

#include <type_traits>

#ifndef __cpp_lib_is_swappable
# error "Feature-test macro for is_swappable missing"
#elif __cpp_lib_is_swappable != 201603
# error "Feature-test macro for is_swappable has wrong value"
#endif

namespace std
{
  typedef short test_type;
  template struct is_swappable<test_type>;
}
