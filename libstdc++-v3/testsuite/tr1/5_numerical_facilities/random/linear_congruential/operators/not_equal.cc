// 2006-06-04  Stephen M. Webb <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2006-2025 Free Software Foundation, Inc.
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

// 5.1.4.1 Class template linear_congruential
// 5.1 [3] Table 16

#include <tr1/random>
#include <testsuite_hooks.h>

void
test01()
{
  std::tr1::minstd_rand0 a;
  std::tr1::minstd_rand0 b;
  std::tr1::minstd_rand0 c(120);

  VERIFY( !(a != b) );
  VERIFY( a != c );
}

int main()
{
  test01();
  return 0;
}
