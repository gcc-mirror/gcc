// { dg-do compile { target c++14 } }

// Copyright (C) 2013-2024 Free Software Foundation, Inc.
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

#include <chrono>
#include <type_traits>

void
test03()
{
  using namespace std::literals::chrono_literals;

  static_assert(std::is_same<decltype(1h), std::chrono::hours>::value,
		"1h is std::chrono::hours");

  static_assert(std::is_same<decltype(1.0h),
	std::chrono::duration<long double, std::ratio<3600L, 1L>>>::value,
	"1.0h is std::chrono::duration<long double, std::ratio<3600L, 1L>>");

  static_assert(std::is_same<decltype(1min), std::chrono::minutes>::value,
		"1min is std::chrono::minutes");

  static_assert(std::is_same<decltype(1.0min),
	std::chrono::duration<long double, std::ratio<60L, 1L>>>::value,
	"1.0min is std::chrono::duration<long double, std::ratio<60L, 1L>>");

  static_assert(std::is_same<decltype(1s), std::chrono::seconds>::value,
		"1s is std::chrono::seconds");

  static_assert(std::is_same<decltype(1.0s),
	std::chrono::duration<long double, std::ratio<1L, 1L>>>::value,
	"1.0s is std::chrono::duration<long double, std::ratio<1L, 1L>>");

  static_assert(std::is_same<decltype(1ms), std::chrono::milliseconds>::value,
		"1ms is std::chrono::milliseconds");

  static_assert(std::is_same<decltype(1.0ms),
	std::chrono::duration<long double, std::ratio<1L, 1000L>>>::value,
	"1.0ms is std::chrono::duration<long double, std::ratio<1L, 1000L>>");

  static_assert(std::is_same<decltype(1us), std::chrono::microseconds>::value,
		"1us is std::chrono::microseconds");

  static_assert(std::is_same<decltype(1.0us),
	std::chrono::duration<long double, std::ratio<1L, 1000000L>>>::value,
	"1.0us is std::chrono::duration<long double, std::ratio<1L, 1000000L>>");

  static_assert(std::is_same<decltype(1ns), std::chrono::nanoseconds>::value,
		"1ns is std::chrono::nanoseconds");

  static_assert(std::is_same<decltype(1.0ns),
	std::chrono::duration<long double, std::ratio<1L, 1000000000L>>>::value,
	"1.0ns is std::chrono::duration<long double, std::ratio<1L, 1000000000L>>");
}
