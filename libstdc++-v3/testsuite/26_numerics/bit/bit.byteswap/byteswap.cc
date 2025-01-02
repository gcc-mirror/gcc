// Copyright (C) 2021-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++23 } }
// { dg-add-options no_pch }

#include <bit>

#ifndef __cpp_lib_byteswap
# error "Feature-test macro for byteswap missing in <bit>"
#elif __cpp_lib_byteswap != 202110L
# error "Feature-test macro for byteswap has wrong value in <bit>"
#endif

#include <cstdint>
#include <cstring>
#include <testsuite_hooks.h>

void
test01()
{
  static_assert( std::byteswap<int8_t>(0x12) == 0x12 );
  static_assert( std::byteswap<int16_t>(0x1234) == 0x3412 );
  static_assert( std::byteswap<int32_t>(0x12345678) == 0x78563412 );
  static_assert( std::byteswap<int64_t>(0x123456789abcdef0)
		 == static_cast<int64_t>(0xf0debc9a78563412) );
  static_assert( std::byteswap<uint8_t>(0x21) == 0x21 );
  static_assert( std::byteswap<uint16_t>(0x4321) == 0x2143 );
  static_assert( std::byteswap<uint32_t>(0x87654321) == 0x21436587 );
  static_assert( std::byteswap<uint64_t>(0xfedcba9876543210)
		 == static_cast<uint64_t>(0x1032547698badcfe) );
#if !defined(__STRICT_ANSI__) && defined __SIZEOF_INT128__
  constexpr __int128_t c1 = (static_cast<__int128_t>(0x0102030405060708) << 64
			     | 0x090a0b0c0d0e0f10);
  constexpr __int128_t c2 = (static_cast<__int128_t>(0x100f0e0d0c0b0a09) << 64
			     | 0x0807060504030201);
  constexpr __int128_t c3 = (static_cast<__int128_t>(0xf1e2d3c4b5a69788) << 64
			     | 0x796a5b4c3d2e1f10);
  constexpr __int128_t c4 = (static_cast<__int128_t>(0x101f2e3d4c5b6a79) << 64
			     | 0x8897a6b5c4d3e2f1);
  static_assert( std::byteswap(c1) == c2 );
  static_assert( std::byteswap(static_cast<__uint128_t>(c1))
		 == static_cast<__uint128_t>(c2) );
  static_assert( std::byteswap(c3) == c4 );
#endif
  static_assert( std::byteswap<const uint32_t>(0xdeadbeef) == 0xefbeadde );
  static_assert( std::byteswap<volatile uint32_t>(0xdeadbeef) == 0xefbeadde );
  static_assert( std::byteswap<int32_t>(0xdeadbeef)
		 == static_cast<int32_t>(0xefbeadde) );
}

void
test02()
{
  volatile int8_t a = 0x12;
  volatile int16_t b = 0x1234;
  volatile int32_t c = 0x12345678;
  volatile int64_t d = 0x123456789abcdef0;
  volatile uint8_t e = 0x21;
  volatile uint16_t f = 0x4321;
  volatile uint32_t g = 0x87654321;
  volatile uint64_t h = 0xfedcba9876543210;
  VERIFY ( std::byteswap<int8_t>(a) == 0x12 );
  VERIFY ( std::byteswap<int16_t>(b) == 0x3412 );
  VERIFY ( std::byteswap(c) == 0x78563412 );
  VERIFY ( std::byteswap(d) == 0xf0debc9a78563412 );
  VERIFY ( std::byteswap<uint8_t>(e) == 0x21 );
  VERIFY ( std::byteswap<uint16_t>(f) == 0x2143 );
  VERIFY ( std::byteswap(g) == 0x21436587 );
  VERIFY ( std::byteswap(h) == 0x1032547698badcfe );
  VERIFY ( std::byteswap(std::byteswap<int8_t>(a)) == a );
  VERIFY ( std::byteswap(std::byteswap<int16_t>(b)) == b );
  VERIFY ( std::byteswap(std::byteswap(c)) == c );
  VERIFY ( std::byteswap(std::byteswap(d)) == d );
  VERIFY ( std::byteswap(std::byteswap<uint8_t>(e)) == e );
  VERIFY ( std::byteswap(std::byteswap<uint16_t>(f)) == f );
  VERIFY ( std::byteswap(std::byteswap(g)) == g );
  VERIFY ( std::byteswap(std::byteswap(h)) == h );
#if !defined(__STRICT_ANSI__) && defined __SIZEOF_INT128__
  volatile __int128_t c1 = (static_cast<__int128_t>(0x0102030405060708) << 64
			    | 0x090a0b0c0d0e0f10);
  volatile __int128_t c2 = (static_cast<__int128_t>(0x100f0e0d0c0b0a09) << 64
			    | 0x0807060504030201);
  VERIFY ( std::byteswap(c1) == c2 );
  VERIFY ( std::byteswap<__uint128_t>(c1) == static_cast<__uint128_t>(c2) );
  VERIFY ( std::byteswap(std::byteswap(c1)) == c1 );
  VERIFY ( std::byteswap(std::byteswap<__uint128_t>(c2))
	   == static_cast<__uint128_t>(c2) );
#endif
  VERIFY ( std::byteswap<const uint32_t>(0xdeadbeef) == 0xefbeadde );
  VERIFY ( std::byteswap<volatile uint32_t>(0xdeadbeef) == 0xefbeadde );
}

int main()
{
  test01();
  test02();
}
