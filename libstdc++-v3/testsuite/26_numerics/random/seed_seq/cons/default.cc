// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2008-12-05  Edward M. Smith-Rowland <3dw4rd@verizon.net>
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

// 26.4.7.1 Class seed_seq [rand.util.seedseq]

#include <random>
#include <testsuite_hooks.h>

static_assert( std::is_nothrow_default_constructible<std::seed_seq>::value,
	       "LWG 3422" );

void
test01()
{
  std::seed_seq seq;

  std::vector<unsigned> foo(10000);
  seq.generate(foo.begin(), foo.end());

  VERIFY( seq.size() == 0 );
}

int
main()
{
  test01();
  return 0;
}
