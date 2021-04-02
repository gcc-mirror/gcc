// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

// Copyright (C) 2019-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <atomic>

void test01()
{
  // Not global scoped, only namespace std.
  using memory_order; // { dg-error "expected nested-name-specifier" }
  constexpr auto relaxed = memory_order::relaxed; // { dg-error "has not been declared" }
  constexpr auto consume = memory_order::consume; // { dg-error "has not been declared" }
  constexpr auto acquire = memory_order::acquire; // { dg-error "has not been declared" }
  constexpr auto release = memory_order::release; // { dg-error "has not been declared" }
  constexpr auto acq_rel = memory_order::acq_rel; // { dg-error "has not been declared" }
  constexpr auto seq_cst = memory_order::seq_cst; // { dg-error "has not been declared" }
}
