// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2014-08-09  Ulrich Drepper  <drepper@gmail.com>
//
// Copyright (C) 2014-2016 Free Software Foundation, Inc.
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

// Class template uniform_on_sphere_distribution
// 26.5.1.6 Random number distribution requirements [rand.req.dist]

#include <ext/random>
#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  bool test [[gnu::unused]] = true;
  std::minstd_rand0 rng;

  __gnu_cxx::uniform_on_sphere_distribution<3> u3;

  for (size_t n = 0; n < 1000; ++n)
    {
      auto r = u3(rng);

      VERIFY (r[0] != 0.0 || r[1] != 0.0 || r[2] != 0.0);
    }

  __gnu_cxx::uniform_on_sphere_distribution<2> u2;

  for (size_t n = 0; n < 1000; ++n)
    {
      auto r = u2(rng);

      VERIFY (r[0] != 0.0 || r[1] != 0.0);
    }
}

int
main()
{
  test01();
  return 0;
}
