// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-options "-Wno-unused-result" }
// { dg-do compile { target c++20 } }

#include <compare>

// C++20 [cmp.categories.pre]
// "an argument other than a literal 0 is undefined"

void
test01()
{
  std::partial_ordering::equivalent == 0; // OK
  std::weak_ordering::equivalent == 0;    // OK
  std::strong_ordering::equivalent == 0;  // OK

  std::partial_ordering::equivalent == 1; // { dg-error "invalid conversion" }
  std::weak_ordering::equivalent == 1;    // { dg-error "invalid conversion" }
  std::strong_ordering::equivalent == 1;  // { dg-error "invalid conversion" }

  constexpr int z = 0;
  std::partial_ordering::equivalent == z; // { dg-error "invalid conversion" }
  std::weak_ordering::equivalent == z;    // { dg-error "invalid conversion" }
  std::strong_ordering::equivalent == z;  // { dg-error "invalid conversion" }

  constexpr void* p = nullptr;
  std::partial_ordering::equivalent == p; // { dg-error "invalid conversion" }
  std::weak_ordering::equivalent == p;    // { dg-error "invalid conversion" }
  std::strong_ordering::equivalent == p;  // { dg-error "invalid conversion" }

  // Ideally these would be ill-formed, but the current code accepts it.
  std::partial_ordering::equivalent == nullptr;
  std::weak_ordering::equivalent == nullptr;
  std::strong_ordering::equivalent == nullptr;
}

// { dg-prune-output "reinterpret_cast.* is not a constant expression" }
// { dg-prune-output "cast from 'void.' is not allowed" }
// { dg-prune-output "not a constant expression" }
