// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target { { i?86-*-* x86_64-*-* } && { ! x32 } } } }
// { dg-require-effective-target c++11 }

#include <functional>

int bar(int) __attribute__((ms_abi));
int baz(int) __attribute__((sysv_abi));

void
test01()
{
  // PR libstdc++/91371
  std::bind(bar, 5)();
  std::bind(baz, 5)();

  static_assert(std::is_function<decltype(bar)>::value, "");
  static_assert(std::is_function<decltype(baz)>::value, "");
  static_assert(std::is_pointer<std::decay<decltype(bar)>::type>::value, "");
  static_assert(std::is_pointer<std::decay<decltype(baz)>::type>::value, "");
}
