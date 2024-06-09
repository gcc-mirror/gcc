// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

#include <utility>
#include <limits>
#include <testsuite_hooks.h>

void
test01()
{
  unsigned int u = std::numeric_limits<unsigned int>::max();
  VERIFY( std::in_range<unsigned int>(u) );
  VERIFY( !std::in_range<int>(u) );
  int s = -1;
  VERIFY( !std::in_range<unsigned int>(s) );
  s = std::numeric_limits<int>::max();
  VERIFY( std::in_range<unsigned int>(s) );
}

constexpr bool
test02()
{
  unsigned int u = std::numeric_limits<unsigned int>::max();
  if (std::in_range<int>(u))
    throw 1;
  int s = -1;
  if (std::in_range<unsigned>(s))
    throw 2;
  s = std::numeric_limits<int>::max();
  if (!std::in_range<unsigned>(s))
    throw 3;
  return true;
}

void
test03()
{
  short ss = -1;
  VERIFY( std::in_range<int>(ss) );
  VERIFY( !std::in_range<unsigned>(ss) );
  int s = -1;
  VERIFY( std::in_range<short>(s) );
  VERIFY( !std::in_range<unsigned>(s) );
  VERIFY( !std::in_range<unsigned long>(s) );
  VERIFY( std::in_range<long>(s) );
  s = std::numeric_limits<short>::min() - 1;
  VERIFY( !std::in_range<short>(s) );

  unsigned int u = (unsigned int) -1;
  VERIFY( !std::in_range<int>(u) );
  VERIFY( !std::in_range<unsigned short>(u) );

  unsigned long ul = (unsigned long) -1;
  VERIFY( !std::in_range<unsigned short>(ul) );
  VERIFY( std::in_range<unsigned long long>(ul) );
}

int
main()
{
  test01();
  static_assert( test02() );
  test03();
}
