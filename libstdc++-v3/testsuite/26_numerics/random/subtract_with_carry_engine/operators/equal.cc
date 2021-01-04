// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2008-11-24  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2008-2021 Free Software Foundation, Inc.
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

// 26.4.3.3 Class template subtract_with_carry_engine [rand.eng.sub]
// 26.4.2.2 Concept RandomNumberEngine [rand.concept.eng]

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  std::subtract_with_carry_engine<unsigned long, 24, 10, 24> u;
  std::subtract_with_carry_engine<unsigned long, 24, 10, 24> v;

  VERIFY( u == v );

  u.discard(100);
  v.discard(100);

  VERIFY( u == v );
}

int main()
{
  test01();
  return 0;
}
