// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <memory>

struct T
{
  T() { }
  T(const T&) = delete;
};

static_assert(__is_trivially_assignable(T&, const T&) &&
  !__is_trivial(T), "T is only trivially copy assignable");

void
test01(T* result)
{
  T t[1];
  std::uninitialized_copy(t, t+1, result); // { dg-error "here" }
}
// { dg-error "must be constructible from input type" "" { target *-*-* } 0 }
