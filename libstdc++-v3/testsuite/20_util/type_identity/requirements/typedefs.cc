// Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <type_traits>

template<typename T, typename = typename std::type_identity<T>::type>
  struct test; // undefined

template<typename T>
  struct test<T, T> : std::true_type { };

enum test_enum { };
struct test_class { };
struct incomplete_class;

void test01()
{
  static_assert(test<float>::value, "");
  static_assert(test<const unsigned int>::value, "");
  static_assert(test<const unsigned int>::value, "");
  static_assert(test<volatile unsigned int>::value, "");
  static_assert(test<const volatile unsigned int>::value, "");
  static_assert(test<const unsigned char>::value, "");
  static_assert(test<volatile wchar_t>::value, "" );

  // Pointers
  static_assert(test<void*>::value, "");
  static_assert(test<long*>::value, "");
  // References
  static_assert(test<short&>::value, "");
  static_assert(test<char&&>::value, "");
  static_assert(test<int*&>::value, "");
  // Arrays
  static_assert(test<int[]>::value, "");
  static_assert(test<int[2]>::value, "");
  static_assert(test<int[2][3]>::value, "");
  static_assert(test<int(*)[2]>::value, "");
  static_assert(test<int(&)[2]>::value, "");

  static_assert(test<test_enum>::value, "");
  static_assert(test<test_class>::value, "");
  static_assert(test<incomplete_class>::value, "");

  // Functions
  static_assert(test<void(*)()>::value, "");
  static_assert(test<int(*)(int) noexcept>::value, "");
  static_assert(test<void(&)()>::value, "");
  static_assert(test<long(&)(long) noexcept>::value, "");
  static_assert(test<void()>::value, "");
  static_assert(test<int(int, int)>::value, "");
  static_assert(test<void() noexcept>::value, "");
  static_assert(test<int(int, int) noexcept>::value, "");

  // Abominable function types
  static_assert(test<void(int) const>::value, "");
  static_assert(test<void(int) const volatile>::value, "");

  // Pointers to members
  static_assert(test<int incomplete_class::*>::value, "");
  static_assert(test<void(incomplete_class::*)(int)>::value, "");
  static_assert(test<void(incomplete_class::*)(int) noexcept>::value, "");
  static_assert(test<void(incomplete_class::*)() const>::value, "");
  static_assert(test<void(incomplete_class::*)() &>::value, "");
  static_assert(test<void(incomplete_class::*)() &&>::value, "");
  static_assert(test<void(incomplete_class::*)() volatile &&>::value, "");

#ifndef __STRICT_ANSI__
  // GNU Extensions.
#ifdef __SIZEOF_INT128__
  static_assert(test<unsigned __int128>::value, "");
  static_assert(test<unsigned __int128>::value, "");
#endif
#endif
}
