// Copyright (C) 2015-2019 Free Software Foundation, Inc.
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

// { dg-do run { target { c++11 && { ! simulator } } } }
// { dg-require-cstdint "" }

#include <random>
#include <testsuite_hooks.h>

// libstdc++/64351
void
test01()
{
  std::mt19937 rng(8890);
  std::uniform_real_distribution<float> dist;

  rng.discard(30e6);
  for (long i = 0; i < 10e6; ++i)
    {
      auto n = dist(rng);
      VERIFY( n != 1.f );
    }
}

// libstdc++/63176
void
test02()
{
  std::mt19937 rng(8890);
  std::seed_seq sequence{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  rng.seed(sequence);
  rng.discard(12 * 629143);
  std::mt19937 rng2{rng};
  for (int i = 0; i < 20; ++i)
  {
    float n =
      std::generate_canonical<float, std::numeric_limits<float>::digits>(rng);
    VERIFY( n != 1.f );

    // PR libstdc++/80137
    rng2.discard(1);
    VERIFY( rng == rng2 );
  }
}

int
main()
{
  test01();
  test02();
}
