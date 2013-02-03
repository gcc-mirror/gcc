// { dg-options "-std=c++0x" }
// { dg-require-cstdint "" }
//
// 2010-02-16  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2013 Free Software Foundation, Inc.
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
  typedef unsigned long value_type;

  std::mersenne_twister_engine<
    value_type, 32, 624, 397, 31,
    0x9908b0dful, 11,
    0xfffffffful, 7,
    0x9d2c5680ul, 15,
      0xefc60000ul, 18, 1812433253ul> e(1);

  const auto f(e);
  auto g(f);
  g = g; // Suppress unused warning.
}

int main()
{
  test01();
  return 0;
}
