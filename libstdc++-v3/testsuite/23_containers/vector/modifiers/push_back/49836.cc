// { dg-do run { target c++11 } }

// Copyright (C) 2011-2021 Free Software Foundation, Inc.
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

#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

// libstdc++/49836
void test01()
{
  using __gnu_test::CopyConsOnlyType;
  using __gnu_test::MoveConsOnlyType;
  using __gnu_test::assign::DelAnyAssign;

  std::vector<CopyConsOnlyType> v1;
  CopyConsOnlyType t1(1);
  v1.push_back(t1);
  v1.push_back(t1);
  v1.push_back(t1);
  VERIFY( v1.size() == 3 );

  std::vector<MoveConsOnlyType> v2;
  MoveConsOnlyType t2(1);
  v2.push_back(std::move(t2));
  v2.push_back(std::move(t2));
  v2.push_back(std::move(t2));
  VERIFY( v2.size() == 3 );

  std::vector<DelAnyAssign> v3;
  DelAnyAssign t3;
  v3.push_back(t3);
  v3.push_back(t3);
  v3.push_back(t3);
  VERIFY( v3.size() == 3 );

}

int main()
{
  test01();
  return 0;
}
