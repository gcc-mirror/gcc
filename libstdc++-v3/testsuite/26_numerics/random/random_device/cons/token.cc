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

// C++11 26.5.6 class random_device [rand.device]

#include <random>
#include <stdexcept>
#include <testsuite_hooks.h>
#include <testsuite_random.h>

void
test01()
{
  std::random_device x("default");
  using result_type = std::random_device::result_type;
  VERIFY( x.min() == std::numeric_limits<result_type>::min() );
  VERIFY( x.max() == std::numeric_limits<result_type>::max() );
}

void
test02()
{
#ifdef _GLIBCXX_USE_DEV_RANDOM
  std::random_device x1("/dev/urandom");
  std::random_device x2("/dev/random");
  VERIFY( x1() != x2() );
#endif
}

void
test03()
{
  // At least one of these tokens should be valid.
  const std::string tokens[] = {
    "rdseed", "rdrand", "rand_s", "/dev/urandom", "/dev/random", "mt19937",
    "prng"
  };
  int count = 0;
  for (const std::string& token : tokens)
  {
    if (__gnu_test::random_device_available(token))
      ++count;
  }
  VERIFY( count != 0 );
}

void
test04()
{
  if (__gnu_test::random_device_available("mt19937"))
  {
    std::random_device x("mt19937");
    std::random_device::result_type xval = x();

    // If "mt19937" is a valid token then numeric seeds should be too.
    std::random_device x1("0");
    std::random_device x2("1234");
    std::random_device x3("0xc0fefe");
    VERIFY( xval != x1() );
    VERIFY( x2() != x3() );
  }
}

int main()
{
  test01();
  test02();
  test03();
  test04();
}
