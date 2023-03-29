// Copyright (C) 2013-2023 Free Software Foundation, Inc.
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
// { dg-require-effective-target dfp }

#include <decimal/decimal>

void
test01 ()
{
  std::decimal::decimal32 d32(0);
  std::decimal::decimal64 d64(0);
  std::decimal::decimal128 d128(0);

  static_cast<long long>(d32);
  static_cast<long long>(d64);
  static_cast<long long>(d128);
}
