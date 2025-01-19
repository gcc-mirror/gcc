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

#include <ranges>

using std::ranges::subrange;

// LWG 3282. subrange converting constructor should disallow derived to base
// conversions

struct Base {};
struct Derived : Base {};
subrange<Derived*> sd;
subrange<Base*> sb = sd; // { dg-error "conversion" }

void
test_lwg3404()
{
  // LWG 3404. Finish removing subrange's conversions from pair-like
  std::pair<char*, char*> p;
  subrange sb1(p);			// { dg-error "no matching function" }
  // { dg-error "class template argument deduction" "" { target *-*-* } 37 }
  subrange sb2(p, p.second - p.first);	// { dg-error "no matching function" }
  // { dg-error "class template argument deduction" "" { target *-*-* } 39 }

  // { dg-prune-output "in requirements with" }
}
