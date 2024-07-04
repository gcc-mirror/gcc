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

// { dg-do run { target c++20 } }

#include <array>
#include <testsuite_hooks.h>

template<typename T>
constexpr auto
cmp_array(T t)
{
  std::array<T, 2> a{ T{1}, t };
  std::array<T, 2> b{ T{1}, T{2} };
  return a <=> b;
}

void
test01()
{
  static_assert( cmp_array((unsigned char)1) < 0 );
  static_assert( cmp_array((unsigned char)2) == 0 );
  static_assert( cmp_array((unsigned char)3) > 0 );

  static_assert( cmp_array((signed char)-1) < 0 );
  static_assert( cmp_array((signed char)2) == 0 );
  static_assert( cmp_array((signed char)3) > 0 );

  static_assert( cmp_array(std::byte{1}) < 0 );
  static_assert( cmp_array(std::byte{2}) == 0 );
  static_assert( cmp_array(std::byte{3}) > 0 );

  static_assert( cmp_array((unsigned int)1) < 0 );
  static_assert( cmp_array((unsigned int)2) == 0 );
  static_assert( cmp_array((unsigned int)333) > 0 );
  static_assert( cmp_array((unsigned int)4444) > 0 );
  static_assert( cmp_array((unsigned int)55555) > 0 );
  static_assert( cmp_array((unsigned int)0x66666666) > 0 );

  static_assert( cmp_array((signed int)-1) < 0 );
  static_assert( cmp_array((signed int)2) == 0 );
  static_assert( cmp_array((signed int)333) > 0 );
  static_assert( cmp_array((signed int)-4444) < 0 );
  static_assert( cmp_array((signed int)55555) > 0 );
  static_assert( cmp_array((signed int)-0x66666666) < 0 );
}

void
test02()
{
  unsigned char uc = 1;
  VERIFY( cmp_array(uc) < 0 );
  uc = 2;
  VERIFY( cmp_array(uc) == 0 );
  uc = 3;
  VERIFY( cmp_array(uc) > 0 );

  signed char sc = -1;
  VERIFY( cmp_array(sc) < 0 );
  sc = 2;
  VERIFY( cmp_array(sc) == 0 );
  sc = 3;
  VERIFY( cmp_array(sc) > 0 );

  std::byte b{1};
  VERIFY( cmp_array(b) < 0 );
  b = std::byte{2};
  VERIFY( cmp_array(b) == 0 );
  b = std::byte{3};
  VERIFY( cmp_array(b) > 0 );

  unsigned int ui = 1;
  VERIFY( cmp_array(ui) < 0 );
  ui = 2;
  VERIFY( cmp_array(ui) == 0 );
  ui = 333;
  VERIFY( cmp_array(ui) > 0 );
  ui = 4444;
  VERIFY( cmp_array(ui) > 0 );
  ui = 555555;
  VERIFY( cmp_array(ui) > 0 );
  ui = 0x66666666;
  VERIFY( cmp_array(ui) > 0 );

  signed int si = -1;
  VERIFY( cmp_array(si) < 0 );
  si = 2;
  VERIFY( cmp_array(si) == 0 );
  si = 333;
  VERIFY( cmp_array(si) > 0 );
  si = -4444;
  VERIFY( cmp_array(si) < 0 );
  si = 555555;
  VERIFY( cmp_array(si) > 0 );
  si = -0x66666666;
  VERIFY( cmp_array(si) < 0 );
}

int
main()
{
  test01();
  test02();
}
