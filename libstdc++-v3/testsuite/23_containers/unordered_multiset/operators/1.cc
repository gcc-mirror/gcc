// { dg-do run { target c++11 } }

// 2010-03-25  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2018 Free Software Foundation, Inc.
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

#include <unordered_set>
#include <testsuite_hooks.h>

void test01()
{
  std::unordered_multiset<int> ums1, ums2;
  VERIFY( ums1 == ums2 );
  VERIFY( !(ums1 != ums2) );

  ums1.insert(1);
  ums2.insert(1);
  VERIFY( ums1 == ums2 );
  VERIFY( !(ums1 != ums2) );

  ums1.insert(2);
  ums2.insert(2);
  VERIFY( ums1 == ums2 );
  VERIFY( !(ums1 != ums2) );

  ums1.insert(1);
  ums2.insert(1);
  VERIFY( ums1 == ums2 );
  VERIFY( !(ums1 != ums2) );

  ums1.insert(3);
  VERIFY( ums1 != ums2 );
  VERIFY( !(ums1 == ums2) );

  ums2.insert(3);
  VERIFY( (ums1 == ums2) );
  VERIFY( !(ums1 != ums2) );

  ums2.clear();
  VERIFY( ums1 != ums2 );
  VERIFY( !(ums1 == ums2) );

  ums1.clear();
  VERIFY( ums1 == ums2 );
  VERIFY( !(ums1 != ums2) );

  ums1.insert(1);
  ums2.insert(2);
  VERIFY( ums1 != ums2 );
  VERIFY( !(ums1 == ums2) );

  ums1.insert(2);
  ums2.insert(1);
  VERIFY( ums1 == ums2 );
  VERIFY( !(ums1 != ums2) );

  ums1.insert(3);
  ums2.insert(4);
  VERIFY( ums1 != ums2 );
  VERIFY( !(ums1 == ums2) );

  ums1.insert(4);
  VERIFY( ums1 != ums2 );
  VERIFY( !(ums1 == ums2) );

  ums2.insert(3);
  VERIFY( ums1 == ums2 );
  VERIFY( !(ums1 != ums2) );

  ums1.insert(1);
  ums2.insert(1);
  VERIFY( ums1 == ums2 );
  VERIFY( !(ums1 != ums2) );

  ums1.insert(4);
  ums2.insert(4);
  VERIFY( ums1 == ums2 );
  VERIFY( !(ums1 != ums2) );

  const std::unordered_multiset<int> cums1(ums1), cums2(ums2);
  VERIFY( cums1 == cums2 );
  VERIFY( !(cums1 != cums2) );
  VERIFY( cums1 == ums2 );
  VERIFY( !(ums1 != cums2) );
}

int main()
{
  test01();
  return 0;
}
