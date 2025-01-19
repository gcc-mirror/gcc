// { dg-do compile { target c++17 } }

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

// { dg-error "must be a complete class" "" { target *-*-* } 0 }

#include <type_traits>

class X;

void test01()
{
  std::invoke_result<int(X), X>();		// { dg-error "required from here" }
  std::invoke_result<int(int, X), int, X>();		// { dg-error "required from here" }
  std::invoke_result<int(int, X), X, int>();		// { dg-error "required from here" }


  std::invoke_result<int(X&), X&>();		// { dg-bogus "required from here" }
  std::invoke_result<int(int, X&), int, X&>();		// { dg-bogus "required from here" }

  std::invoke_result<int(X&&), X&&>();		// { dg-bogus "required from here" }
  std::invoke_result<int(int, X&&), int, X&&>();		// { dg-bogus "required from here" }

  std::invoke_result<int(const X&&), const X&&>();		// { dg-bogus "required from here" }
  std::invoke_result<int(int, const X&&), int, const X&&>();		// { dg-bogus "required from here" }

  std::invoke_result<int(const X&), const X&>();		// { dg-bogus "required from here" }
  std::invoke_result<int(int, const X&), int, const X&>();		// { dg-bogus "required from here" }

  std::invoke_result<int(const X&), X&>();		// { dg-bogus "required from here" }
  std::invoke_result<int(int, const X&), int, X&>();		// { dg-bogus "required from here" }
}
