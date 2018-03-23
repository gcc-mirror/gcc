// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2008-12-07  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2008-2018 Free Software Foundation, Inc.
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

#include <random>

void
test01()
{
  typedef std::subtract_with_carry_engine<unsigned long, 24, 10, 24>
    base_engine;

  base_engine b;

  std::discard_block_engine<base_engine, 389, 24> e(b);
}

int main()
{
  test01();
  return 0;
}
