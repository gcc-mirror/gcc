// { dg-options "-std=c++0x" }
//
// 2009-02-13  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2009 Free Software Foundation, Inc.
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

// 26.4.7.1 Class seed_seq [rand.util.seedseq]

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

  std::seed_seq seq({0, 1, 2, 3, 4, 5, 6, 7, 8, 9});

  std::vector<unsigned> foo(10000);
  seq.generate(foo.begin(), foo.end());

  VERIFY( seq.size() == 10 );
}

int main()
{
  test01();
  return 0;
}
