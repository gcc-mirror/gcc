// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }
// { dg-add-options strict_std }

#include <tr1/cmath>

void
test01()
{
  // For C++17 most TR1 special functions re-use the C++17 functions
  // in namespace std, but in strict -std=c++17 mode the hypergeometric
  // functions are not defined in namespace std. This test ensures they
  // are still available in namespace std::tr1.
  (void) std::tr1::hyperg(1.0, 2.0, 3.0, 4.0);
}
