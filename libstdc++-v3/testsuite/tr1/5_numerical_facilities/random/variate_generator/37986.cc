// { dg-do compile }
//
// Copyright (C) 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 5.1.3 class template variate_generator

#include <tr1/random>

// libtsdc++/37986
void test01()
{
  std::tr1::mt19937 mt;
  std::tr1::uniform_real<double> dist;

  std::tr1::variate_generator<
    std::tr1::mt19937,
    std::tr1::uniform_real<double>
    > g1(mt, dist);

  std::tr1::variate_generator<
    std::tr1::mt19937&,
    std::tr1::uniform_real<double>
    > g2(mt, dist);

  std::tr1::variate_generator<
    std::tr1::mt19937*,
    std::tr1::uniform_real<double>
    > g3(&mt, dist);
}
