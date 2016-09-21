// Copyright (C) 2016 Free Software Foundation, Inc.
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

// { dg-options "-fno-show-column" }
// { dg-do compile { target c++14 } }
// { dg-error "in range" "" { target *-*-* } 1342 }

#include <tuple>

void
test01()
{
  using test_type = std::tuple<>;
  test_type t;
  std::get<0>(t);				// { dg-error "no match" }
  std::get<0>(const_cast<const test_type&>(t));	// { dg-error "no match" }
  std::get<0>(static_cast<test_type&&>(t));	// { dg-error "no match" }
  std::get<5>(t);				// { dg-error "no match" }
  std::get<5>(const_cast<const test_type&>(t));	// { dg-error "no match" }
  std::get<5>(static_cast<test_type&&>(t));	// { dg-error "no match" }
}

void
test02()
{
  using test_type = std::tuple<int>;
  test_type t;
  std::get<1>(t);				// { dg-error "no match" }
  std::get<1>(const_cast<const test_type&>(t));	// { dg-error "no match" }
  std::get<1>(static_cast<test_type&&>(t));	// { dg-error "no match" }
  std::get<5>(t);				// { dg-error "no match" }
  std::get<5>(const_cast<const test_type&>(t));	// { dg-error "no match" }
  std::get<5>(static_cast<test_type&&>(t));	// { dg-error "no match" }
}

void
test03()
{
  using test_type = std::tuple<int, int, int, int>;
  test_type t;
  std::get<5>(t);				// { dg-error "no match" }
  std::get<5>(const_cast<const test_type&>(t));	// { dg-error "no match" }
  std::get<5>(static_cast<test_type&&>(t));	// { dg-error "no match" }
  std::get<6>(t);				// { dg-error "no match" }
  std::get<6>(const_cast<const test_type&>(t));	// { dg-error "no match" }
  std::get<6>(static_cast<test_type&&>(t));	// { dg-error "no match" }
}
