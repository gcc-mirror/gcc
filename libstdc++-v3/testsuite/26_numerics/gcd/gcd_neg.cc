// Copyright (C) 2016-2022 Free Software Foundation, Inc.
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
  std::gcd(true, 1);    // { dg-error "from here" }
  std::gcd(1, true);    // { dg-error "from here" }
  std::gcd(true, true); // { dg-error "from here" }
  std::gcd<const bool, int>(true, 1);    // { dg-error "from here" }
  std::gcd<int, const bool>(1, true);    // { dg-error "from here" }
  std::gcd<const bool, const bool>(true, true); // { dg-error "from here" }
  std::gcd<const bool&, int>(true, 1);    // { dg-error "from here" }
  std::gcd<int, const bool&>(1, true);    // { dg-error "from here" }
  std::gcd<const bool&, const bool&>(true, true); // { dg-error "from here" }
  std::gcd<const volatile bool, int>(true, 1);    // { dg-error "from here" }
  std::gcd<int, const volatile bool>(1, true);    // { dg-error "from here" }
  std::gcd<const volatile bool,
	   const volatile bool>(true, true); // { dg-error "from here" }
  std::gcd<volatile bool, int>(true, 1);    // { dg-error "from here" }
  std::gcd<int, volatile bool>(1, true);    // { dg-error "from here" }
  std::gcd<volatile bool,
	   volatile bool>(true, true); // { dg-error "from here" }
  std::gcd(0.1, 1);     // { dg-error "from here" }
  std::gcd(1, 0.1);     // { dg-error "from here" }
  std::gcd(0.1, 0.1);   // { dg-error "from here" }
  std::gcd<const int&, const int&>(0.1, 0.1);   // { dg-error "from here" }
}

// { dg-error "must be integers" "" { target *-*-* } 0 }
// { dg-error "must not be bool" "" { target *-*-* } 0 }
// These prunes could be removed if a fix for PR c++/96286 stops them.
// { dg-prune-output "deleted function" }
// { dg-prune-output "incomplete type .*make_unsigned" }
// { dg-prune-output "does not have integral type" }
// { dg-prune-output "non-integral type" }
// { dg-prune-output "invalid specialization" }
