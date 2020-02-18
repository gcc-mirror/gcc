// Copyright (C) 2020 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <utility>
#include <limits>
#include <testsuite_hooks.h>

void
test01()
{
  unsigned int u = std::numeric_limits<unsigned int>::max();
  int s = -1;
  VERIFY( !std::cmp_greater_equal(s, u) );
  VERIFY( std::cmp_greater_equal(u, s) );
  u = (unsigned) std::numeric_limits<int>::max() + 1U;
  VERIFY( !std::cmp_greater_equal(s, u) );
  VERIFY( std::cmp_greater_equal(u, s) );
}

constexpr bool
test02()
{
  unsigned int u = std::numeric_limits<unsigned int>::max();
  int s = -1;
  if (std::cmp_greater_equal(s, u))
    throw 1;
  if (!std::cmp_greater_equal(u, s))
    throw 2;
  return true;
}

void
test03()
{
  short ss = -1;
  int s = -1;
  VERIFY( std::cmp_greater_equal(s, ss) );
  VERIFY( std::cmp_greater_equal(ss, s) );
  VERIFY( std::cmp_greater_equal(ss, -2) );

  unsigned int u = (unsigned int) -1;
  VERIFY( !std::cmp_greater_equal(s, u) );
  VERIFY( std::cmp_greater_equal(u, s) );
  VERIFY( !std::cmp_greater_equal(ss, u) );
  VERIFY( std::cmp_greater_equal(u, ss) );
  VERIFY( std::cmp_greater_equal(u, -2U) );

  unsigned long long ul = (unsigned long long) -1;
  VERIFY( !std::cmp_greater_equal(s, ul) );
  VERIFY( std::cmp_greater_equal(ul, s) );
  VERIFY( !std::cmp_greater_equal(ss, ul) );
  VERIFY( std::cmp_greater_equal(ul, ss) );
  VERIFY( !std::cmp_greater_equal(u, ul) );
  VERIFY( std::cmp_greater_equal(ul, u) );
  VERIFY( std::cmp_greater_equal(ul, -2UL) );
}

int
main()
{
  test01();
  static_assert( test02() );
  test03();
}
