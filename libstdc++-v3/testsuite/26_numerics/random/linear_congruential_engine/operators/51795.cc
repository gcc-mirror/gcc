// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// Copyright (C) 2012-2018 Free Software Foundation, Inc.
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

// 26.5.3.1 class template linear_congruential_engine [rand.eng.lcong]

#include <random>
#include <testsuite_hooks.h>

void test01()
{
  typedef std::linear_congruential_engine<std::uint64_t, 1103515245ULL,
					  12345, 2147483648ULL> engine;
  engine eng(1103527590ULL);

  for (unsigned it = 0; it < 1000; ++it)
    {
      std::uint64_t num = eng();
      VERIFY( (num >= eng.min() && num <= eng.max()) );
    }
}

int main()
{
  test01();
  return 0;
}
