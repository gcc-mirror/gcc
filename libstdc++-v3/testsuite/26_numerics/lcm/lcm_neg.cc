// Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

#include <numeric>

void
test01()
{
  std::lcm(true, 1);    // { dg-error "from here" }
  std::lcm(1, true);    // { dg-error "from here" }
  std::lcm(true, true); // { dg-error "from here" }
  std::lcm<const bool, int>(true, 1);    // { dg-error "from here" }
  std::lcm<int, const bool>(1, true);    // { dg-error "from here" }
  std::lcm<const bool, const bool>(true, true); // { dg-error "from here" }
  std::lcm<const bool&, int>(true, 1);    // { dg-error "from here" }
  std::lcm<int, const bool&>(1, true);    // { dg-error "from here" }
  std::lcm<const bool&, const bool&>(true, true); // { dg-error "from here" }
  std::lcm<const volatile bool, int>(true, 1);    // { dg-error "from here" }
  std::lcm<int, const volatile bool>(1, true);    // { dg-error "from here" }
  std::lcm<const volatile bool,
	   const volatile bool>(true, true); // { dg-error "from here" }
  std::lcm<volatile bool, int>(true, 1);    // { dg-error "from here" }
  std::lcm<int, volatile bool>(1, true);    // { dg-error "from here" }
  std::lcm<volatile bool,
	   volatile bool>(true, true); // { dg-error "from here" }
  std::lcm(0.1, 1);     // { dg-error "from here" }
  std::lcm(1, 0.1);     // { dg-error "from here" }
  std::lcm(0.1, 0.1);   // { dg-error "from here" }
  std::lcm<const int&, const int&>(0.1, 0.1);   // { dg-error "from here" }
}

// { dg-error "must be integers" "" { target *-*-* } 183 }
// { dg-error "must be integers" "" { target *-*-* } 184 }
// { dg-error "must not be bool" "" { target *-*-* } 185 }
// { dg-error "must not be bool" "" { target *-*-* } 186 }
// { dg-prune-output "deleted function" }
// { dg-prune-output "incomplete type .*make_unsigned" }
