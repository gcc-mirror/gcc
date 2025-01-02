// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>
#include <testsuite_random.h>

void
test01()
{
  if (__gnu_test::random_device_available("mt19937"))
  {
    // std::random_device uses a Mersenne Twister with default seed,
    // and the test below will fail.  No point trying to test it.
    return;
  }

  unsigned v1[3], v2[3];
  std::random_device d1, d2;
  for (auto& v : v1)
    v = d1();
  for (auto& v : v2)
    v = d2();
  VERIFY (v1[0] != v2[0] || v1[1] != v2[1] || v1[2] != v2[2] );
}

int
main()
{
  test01();
}
