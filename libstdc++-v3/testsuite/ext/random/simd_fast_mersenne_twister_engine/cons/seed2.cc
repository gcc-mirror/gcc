// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
// { dg-require-little-endian "" }
//
// 2008-11-24  Edward M. Smith-Rowland <3dw4rd@verizon.net>
// 2012-08-28  Ulrich Drepper  <drepper@gmail.com>, adapted for SFMT
//
// Copyright (C) 2008-2023 Free Software Foundation, Inc.
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

#include <ext/random>

void
test01()
{
  double seed = 2.0;
  __gnu_cxx::simd_fast_mersenne_twister_engine<
    uint64_t, 607, 2,
    15, 3, 13, 3,
    0xfdff37ffU, 0xef7f3f7dU,
    0xff777b7dU, 0x7ff7fb2fU,
    0x00000001U, 0x00000000U,
    0x00000000U, 0x5986f054U> x(seed);
}

int main()
{
  test01();
  return 0;
}
