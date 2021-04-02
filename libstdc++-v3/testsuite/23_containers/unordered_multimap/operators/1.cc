// { dg-do run { target c++11 } }

// 2010-03-25  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2021 Free Software Foundation, Inc.
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
  std::unordered_multimap<int, int> umm1, umm2;
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.insert(Pair(1, -1));
  umm2.insert(Pair(1, -1));
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.insert(Pair(2, -1));
  umm2.insert(Pair(2, -1));
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.insert(Pair(1, -1));
  umm2.insert(Pair(1, -1));
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.insert(Pair(3, -3));
  VERIFY( umm1 != umm2 );
  VERIFY( !(umm1 == umm2) );

  umm2.insert(Pair(3, -3));
  VERIFY( (umm1 == umm2) );
  VERIFY( !(umm1 != umm2) );

  umm2.clear();
  VERIFY( umm1 != umm2 );
  VERIFY( !(umm1 == umm2) );

  umm1.clear();
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.insert(Pair(1, -1));
  umm2.insert(Pair(2, -2));
  VERIFY( umm1 != umm2 );
  VERIFY( !(umm1 == umm2) );

  umm1.insert(Pair(2, -2));
  umm2.insert(Pair(1, -1));
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.insert(Pair(3, -3));
  umm2.insert(Pair(4, -4));
  VERIFY( umm1 != umm2 );
  VERIFY( !(umm1 == umm2) );

  umm1.insert(Pair(4, -4));
  VERIFY( umm1 != umm2 );
  VERIFY( !(umm1 == umm2) );

  umm2.insert(Pair(3, -3));
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.insert(Pair(1, -1));
  umm2.insert(Pair(1, -1));
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.insert(Pair(4, -4));
  umm2.insert(Pair(4, -4));
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.clear();
  umm2.clear();
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.insert(Pair(1, -1));
  umm2.insert(Pair(1, -2));
  VERIFY( umm1 != umm2 );
  VERIFY( !(umm1 == umm2) );

  umm1.clear();
  umm2.clear();
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.insert(Pair(1, -1));
  umm2.insert(Pair(1, -1));
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.insert(Pair(2, -2));
  umm2.insert(Pair(2, -3));
  VERIFY( umm1 != umm2 );
  VERIFY( !(umm1 == umm2) );

  umm1.insert(Pair(2, -3));
  umm2.insert(Pair(2, -2));
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.clear();
  umm2.clear();
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.insert(Pair(2, -2));
  umm2.insert(Pair(2, -3));
  VERIFY( umm1 != umm2 );
  VERIFY( !(umm1 == umm2) );

  umm1.insert(Pair(1, -1));
  umm2.insert(Pair(1, -1));
  VERIFY( umm1 != umm2 );
  VERIFY( !(umm1 == umm2) );

  umm1.insert(Pair(2, -3));
  umm2.insert(Pair(2, -2));
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.insert(Pair(1, -1));
  umm2.insert(Pair(1, -1));
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  umm1.insert(Pair(1, -1));
  umm2.insert(Pair(1, -2));
  VERIFY( umm1 != umm2 );
  VERIFY( !(umm1 == umm2) );

  umm1.insert(Pair(3, -3));
  umm2.insert(Pair(3, -3));
  VERIFY( umm1 != umm2 );
  VERIFY( !(umm1 == umm2) );

  umm1.insert(Pair(3, -4));
  umm2.insert(Pair(3, -3));
  VERIFY( umm1 != umm2 );
  VERIFY( !(umm1 == umm2) );

  umm1.insert(Pair(3, -3));
  VERIFY( umm1 != umm2 );
  VERIFY( !(umm1 == umm2) );
 
  umm1.insert(Pair(1, -2));
  umm2.insert(Pair(1, -1));
  VERIFY( umm1 != umm2 );
  VERIFY( !(umm1 == umm2) );

  umm2.insert(Pair(3, -4));
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  const std::unordered_multimap<int, int> cumm1(umm1), cumm2(umm2);
  VERIFY( cumm1 == cumm2 );
  VERIFY( !(cumm1 != cumm2) );
  VERIFY( cumm1 == umm2 );
  VERIFY( !(umm1 != cumm2) );
}

int main()
{
  test01();
  return 0;
}
