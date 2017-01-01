// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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

// Class template uniform_inside_sphere_distribution
// 26.5.1.6 Random number distribution requirements [rand.req.dist]

#include <ext/random>
#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  bool test [[gnu::unused]] = true;
  std::minstd_rand0 rng;

  __gnu_cxx::uniform_inside_sphere_distribution<3> u3(2.0);

  for (size_t n = 0; n < 1000; ++n)
    {
      auto r = u3(rng);

      VERIFY (std::abs(r[0]) < 2.0
           && std::abs(r[1]) < 2.0
           && std::abs(r[2]) < 2.0);
    }

  __gnu_cxx::uniform_inside_sphere_distribution<2> u2(4000.0);

  for (size_t n = 0; n < 1000; ++n)
    {
      auto r = u2(rng);

      VERIFY (std::abs(r[0]) < 4000.0 && std::abs(r[1]) < 4000.0);
    }
}

int
main()
{
  test01();
  return 0;
}
