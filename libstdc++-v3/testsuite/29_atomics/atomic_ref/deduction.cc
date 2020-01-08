// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <atomic>

void
test01()
{
  int i = 0;
  std::atomic_ref a0(i);
  static_assert(std::is_same_v<decltype(a0), std::atomic_ref<int>>);

  float f = 1.0f;
  std::atomic_ref a1(f);
  static_assert(std::is_same_v<decltype(a1), std::atomic_ref<float>>);

  int* p = &i;
  std::atomic_ref a2(p);
  static_assert(std::is_same_v<decltype(a2), std::atomic_ref<int*>>);

  struct X { } x;
  std::atomic_ref a3(x);
  static_assert(std::is_same_v<decltype(a3), std::atomic_ref<X>>);
}
