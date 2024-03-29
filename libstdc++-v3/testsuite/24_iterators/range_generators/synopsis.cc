// { dg-do compile { target c++23 } }
// { dg-add-options no_pch }
// Copyright (C) 2023-2024 Free Software Foundation, Inc.
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

#include <generator>

#if !defined(__cpp_lib_generator) || __cpp_lib_generator < 202207L
# error "__cpp_lib_generator undefined or has wrong value"
#endif

namespace test {
  using std::generator;
#if __STDC_HOSTED__
  namespace pmr {
    using std::pmr::generator;
  }
#endif
}
