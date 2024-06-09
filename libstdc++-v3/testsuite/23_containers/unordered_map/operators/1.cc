// { dg-do run { target c++11 } }

// 2010-03-25  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2024 Free Software Foundation, Inc.
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

#include <unordered_map>
#include <testsuite_hooks.h>

void test01()
{
  typedef std::pair<const int, int> Pair;
  std::unordered_map<int, int> um1, um2;
  VERIFY( um1 == um2 );
  VERIFY( !(um1 != um2) );

  um1.insert(Pair(1, -1));
  um2.insert(Pair(1, -1));
  VERIFY( um1 == um2 );
  VERIFY( !(um1 != um2) );

  um1.insert(Pair(2, -1));
  um2.insert(Pair(2, -1));
  VERIFY( um1 == um2 );
  VERIFY( !(um1 != um2) );

  um1.insert(Pair(1, -1));
  um2.insert(Pair(1, -1));
  VERIFY( um1 == um2 );
  VERIFY( !(um1 != um2) );

  um1.insert(Pair(3, -3));
  VERIFY( um1 != um2 );
  VERIFY( !(um1 == um2) );

  um2.insert(Pair(3, -3));
  VERIFY( (um1 == um2) );
  VERIFY( !(um1 != um2) );

  um2.clear();
  VERIFY( um1 != um2 );
  VERIFY( !(um1 == um2) );

  um1.clear();
  VERIFY( um1 == um2 );
  VERIFY( !(um1 != um2) );

  um1.insert(Pair(1, -1));
  um2.insert(Pair(2, -2));
  VERIFY( um1 != um2 );
  VERIFY( !(um1 == um2) );

  um1.insert(Pair(2, -2));
  um2.insert(Pair(1, -1));
  VERIFY( um1 == um2 );
  VERIFY( !(um1 != um2) );

  um1.insert(Pair(3, -3));
  um2.insert(Pair(4, -4));
  VERIFY( um1 != um2 );
  VERIFY( !(um1 == um2) );

  um1.insert(Pair(4, -4));
  VERIFY( um1 != um2 );
  VERIFY( !(um1 == um2) );

  um2.insert(Pair(3, -3));
  VERIFY( um1 == um2 );
  VERIFY( !(um1 != um2) );

  um1.insert(Pair(1, -1));
  um2.insert(Pair(1, -1));
  VERIFY( um1 == um2 );
  VERIFY( !(um1 != um2) );

  um1.insert(Pair(4, -4));
  um2.insert(Pair(4, -4));
  VERIFY( um1 == um2 );
  VERIFY( !(um1 != um2) );

  um1.clear();
  um2.clear();
  VERIFY( um1 == um2 );
  VERIFY( !(um1 != um2) );

  um1.insert(Pair(1, -1));
  um2.insert(Pair(1, -2));
  VERIFY( um1 != um2 );
  VERIFY( !(um1 == um2) );

  um1.clear();
  um2.clear();
  VERIFY( um1 == um2 );
  VERIFY( !(um1 != um2) );

  um1.insert(Pair(1, -1));
  um2.insert(Pair(1, -1));
  VERIFY( um1 == um2 );
  VERIFY( !(um1 != um2) );

  um1.insert(Pair(2, -2));
  um2.insert(Pair(2, -3));
  VERIFY( um1 != um2 );
  VERIFY( !(um1 == um2) );

  um1.insert(Pair(2, -3));
  um2.insert(Pair(2, -2));
  VERIFY( um1 != um2 );
  VERIFY( !(um1 == um2) );

  um1.clear();
  um2.clear();
  VERIFY( um1 == um2 );
  VERIFY( !(um1 != um2) );

  um1.insert(Pair(2, -2));
  um2.insert(Pair(2, -3));
  VERIFY( um1 != um2 );
  VERIFY( !(um1 == um2) );

  um1.insert(Pair(1, -1));
  um2.insert(Pair(1, -1));
  VERIFY( um1 != um2 );
  VERIFY( !(um1 == um2) );

  um1.insert(Pair(2, -3));
  um2.insert(Pair(2, -2));
  VERIFY( um1 != um2 );
  VERIFY( !(um1 == um2) );

  um1.insert(Pair(1, -1));
  um2.insert(Pair(1, -1));
  VERIFY( um1 != um2 );
  VERIFY( !(um1 == um2) );

  um1.insert(Pair(1, -1));
  um2.insert(Pair(1, -2));
  VERIFY( um1 != um2 );
  VERIFY( !(um1 == um2) );

  const std::unordered_map<int, int> cum1(um1), cum2(um2);
  VERIFY( cum1 != cum2 );
  VERIFY( !(cum1 == cum2) );
  VERIFY( cum1 != um2 );
  VERIFY( !(um1 == cum2) );
}

int main()
{
  test01();
  return 0;
}
