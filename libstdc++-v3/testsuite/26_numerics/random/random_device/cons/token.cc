// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2008-11-24  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2008-2017 Free Software Foundation, Inc.
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

// 26.4.6 class random_device [rand.device]
// 26.4.6 [3]

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
#ifdef _GLIBCXX_USE_RANDOM_TR1
  std::random_device x("/dev/random");
#else
  std::random_device x("0");
#endif

  VERIFY( x.min() == std::numeric_limits<std::random_device::result_type>::min() );
  VERIFY( x.max() == std::numeric_limits<std::random_device::result_type>::max() );
}

int main()
{
  test01();
  return 0;
}
