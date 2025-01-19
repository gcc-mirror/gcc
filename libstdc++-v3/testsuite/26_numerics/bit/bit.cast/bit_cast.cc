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
// { dg-add-options no_pch }

#include <bit>

#ifndef __cpp_lib_bit_cast
# error "Feature-test macro for bit_cast missing in <bit>"
#elif __cpp_lib_bit_cast != 201806L
# error "Feature-test macro for bit_cast has wrong value in <bit>"
#endif

#include <cstdint>
#include <cstring>
#include <testsuite_hooks.h>

template<typename To, typename From>
constexpr bool
check(const From& from)
{
  return std::bit_cast<From>(std::bit_cast<To>(from)) == from;
}

void
test01()
{
  static_assert( std::bit_cast<int>(123) == 123 );
  static_assert( std::bit_cast<int>(123u) == 123 );
  static_assert( std::bit_cast<int>(~0u) == ~0 );

  if constexpr (sizeof(int) == sizeof(float))
    static_assert( check<int>(12.34f) );
  if constexpr (sizeof(unsigned long long) == sizeof(double))
    static_assert( check<unsigned long long>(123.456) );
  if constexpr (sizeof(std::intptr_t) == sizeof(void(*)()))
    VERIFY( check<std::intptr_t>(&test01) );
}

void
test02()
{
  struct S
  {
    int i;

    bool operator==(const char* s) const
    { return std::memcmp(&i, s, sizeof(i)) == 0; }
  };

  char arr[sizeof(int)];
  char arr2[sizeof(int)];
  for (int i = 0; i < sizeof(int); ++i)
  {
    arr[i] = i + 1;
    arr2[i] = (i + 1) * -(i % 2);
  }
  VERIFY( std::bit_cast<S>(arr) == arr );
  VERIFY( std::bit_cast<S>(arr2) == arr2 );
}

int main()
{
  test01();
  test02();
}
