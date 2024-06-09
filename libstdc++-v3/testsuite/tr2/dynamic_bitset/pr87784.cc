// Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <tr2/dynamic_bitset>
#include <testsuite_hooks.h>

void
test01()
{
  std::tr2::dynamic_bitset<unsigned> b;
  VERIFY( b.size() == 0 );
  VERIFY( b.find_first() == b.size() );
  b.push_back(0);
  VERIFY( b.size() == 1 );
  VERIFY( b.find_first() == b.size() );
  b.push_back(0);
  VERIFY( b.size() == 2 );
  VERIFY( b.find_first() == b.size() );

  b.push_back(1);
  VERIFY( b.size() == 3 );
  VERIFY( b.find_first() == b.size() - 1 );
  b.push_back(1);
  VERIFY( b.size() == 4 );
  VERIFY( b.find_first() == b.size() - 2 );
  b.push_back(0);
  VERIFY( b.size() == 5 );
  VERIFY( b.find_first() == b.size() - 3 );

  b.clear();
  VERIFY( b.size() == 0 );
  VERIFY( b.find_first() == b.size() );
  b.push_back(1);
  VERIFY( b.size() == 1 );
  VERIFY( b.find_first() == 0 );
  b.push_back(1);
  VERIFY( b.size() == 2 );
  VERIFY( b.find_first() == 0 );
  b.push_back(1);
  VERIFY( b.size() == 3 );
  VERIFY( b.find_first() == 0 );

  b.clear();
  b.append(2u);
  VERIFY( b.size() == b.bits_per_block );
  VERIFY( b.find_first() == 1 );
  b <<= 1;
  VERIFY( b.find_first() == 2 );
  b <<= 3;
  VERIFY( b.find_first() == 5 );
  b <<= 6;
  VERIFY( b.find_first() == 11 );
  VERIFY( b.size() == b.bits_per_block );
}

int
main()
{
  test01();
}
