// { dg-options "-std=c++0x" }
//
// 2008-11-24  Edward M. Smith-Rowland <3dw4rd@verizon.net>
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

// 26.4.3.1 class template linear_congruential_engine [rand.eng.lcong]
// 26.4.2.2 Concept RandomNumberEngine [rand.concept.eng]

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  double seed = 2.0;
  std::linear_congruential_engine<unsigned long, 48271, 0, 2147483647> x(seed);
}

int main()
{
  test01();
  return 0;
}
