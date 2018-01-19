// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
// { dg-require-little-endian "" }
//
// 2008-11-24  Edward M. Smith-Rowland <3dw4rd@verizon.net>
// 2012-08-28  Ulrich Drepper  <drepper@gmail.com>, adapted for SFMT
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

// 26.4.3.2 Class template mersenne_twister_engine [rand.eng.mers]
// 26.4.2.2 Concept RandomNumberEngine [rand.concept.eng]

#include <sstream>
#include <ext/random>
#include <testsuite_hooks.h>

void
test01()
{
  std::stringstream str;
  __gnu_cxx::sfmt19937 u, v;

  u(); // advance
  str << u;

  VERIFY( !(u == v) );

  str >> v;
  VERIFY( u == v );
  for (unsigned i = 0; i < 1000; ++i)
    VERIFY( u() == v() );

  str.clear();
  str << v;

  u();
  u();
  u();

  str >> u;
  VERIFY( u == v );
  for (unsigned i = 0; i < 1000; ++i)
    VERIFY( u() == v() );
}

int main()
{
  test01();
  return 0;
}
