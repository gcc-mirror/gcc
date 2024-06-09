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

// { dg-do run { target c++11 } }

#include <random>
#include <cstdint>
#include <testsuite_hooks.h>

void
test01()
{
  // PR libstdc++/97311

  using i64 = std::int_least64_t; // can hold all values of uint32_t
  std::vector<i64> v(10);
  std::seed_seq s;
  s.generate(v.begin(), v.end());

  const std::vector<i64> expected{
    0xbc199682,
    0x7a094407,
    0xac05bf42,
    0x10baa2f4,
    0x822d6fde,
    0xf08cdc22,
    0x30382aee,
    0xbd5fb4aa,
    0xb26c5a35,
    0xb9619724
  };
  VERIFY( v == expected );
}

int
main()
{
  test01();
}
