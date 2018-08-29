// { dg-do run { target c++14 } }
// { dg-require-cstdint "" }
// { dg-require-effective-target random_device }
// { dg-require-effective-target tls_runtime }
// { dg-add-options tls }

// Copyright (C) 2015-2018 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <experimental/random>
#include <testsuite_hooks.h>

void
test01()
{
  for (int i = 0; i < 100; ++i)
  {
    const int n = std::experimental::randint(-10, i);
    VERIFY( -10 <= n && n <= i );
  }

  std::experimental::reseed(99u);
  const long n1[] = {
    std::experimental::randint(0, 100),
    std::experimental::randint(0, 100),
    std::experimental::randint(0, 100),
    std::experimental::randint(0, 100),
    std::experimental::randint(0, 100)
  };
  std::experimental::reseed(99u);
  const long n2[] = {
    std::experimental::randint(0, 100),
    std::experimental::randint(0, 100),
    std::experimental::randint(0, 100),
    std::experimental::randint(0, 100),
    std::experimental::randint(0, 100)
  };
  for (int i = 0; i < 5; ++i)
    VERIFY( n1[i] == n2[i] );

  std::experimental::reseed();
  const long n3[] = {
    std::experimental::randint(0, 100),
    std::experimental::randint(0, 100),
    std::experimental::randint(0, 100)
  };
  VERIFY( !(n3[0] == n1[0] && n3[1] == n1[1] && n3[2] == n1[2]) );
}

void
test02()
{
  auto check = [](auto v) {
    auto n = std::experimental::randint(decltype(v)(0), v);
    static_assert(std::is_same<decltype(n), decltype(v)>::value,
        "return type is correct");
    VERIFY(0 <= n && n <= v);
  };
  check( (short)10 );
  check( 100 );
  check( 1000L );
  check( 10000LL );
  check( (unsigned short)10 );
  check( 100U );
  check( 1000UL );
  check( 10000ULL );
}

int main()
{
  test01();
  test02();
}
