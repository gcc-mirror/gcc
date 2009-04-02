// { dg-options "-std=c++0x" }
//
// 2008-12-07  Edward M. Smith-Rowland <3dw4rd@verizon.net>
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

// 26.4.4.2 class template independent_bits_engine [rand.adapt.ibits]
// 26.4.2.3 concept RandomNumberEngineAdaptor [rand.concept.adapt]

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

  typedef std::subtract_with_carry_engine<uint_fast64_t, 48, 5, 12>
    base_engine;

  base_engine b;

  std::independent_bits_engine<base_engine, 48, unsigned long> e(b);
}

int main()
{
  test01();
  return 0;
}
