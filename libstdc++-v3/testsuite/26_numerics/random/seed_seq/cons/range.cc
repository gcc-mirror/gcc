// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2008-12-05  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2008-2025 Free Software Foundation, Inc.
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

// 26.4.7.1 Class seed_seq [rand.util.seedseq]

#include <random>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

void
test01()
{
  unsigned in[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  std::seed_seq seq(in, in + 10);

  std::vector<unsigned> foo(10000);
  seq.generate(foo.begin(), foo.end());

  VERIFY( seq.size() == 10 );
  //VERIFY();
}

void
test02()
{
  unsigned arr[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  __gnu_test::input_container<unsigned int> in(arr);
  std::seed_seq seq(in.begin(), in.end());

  std::vector<unsigned> foo(10000);
  seq.generate(foo.begin(), foo.end());

  VERIFY( seq.size() == 10 );
}

int
main()
{
  test01();
  test02();
}
