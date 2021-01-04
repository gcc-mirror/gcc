// { dg-do run { target c++11 } }

// Copyright (C) 2013-2021 Free Software Foundation, Inc.
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

// 23.3.8  class vector<bool>

#include <vector>
#include <testsuite_hooks.h>

void test01()
{
  std::vector<bool> v;
  v.emplace_back();
  VERIFY( v[0] == false );
  v.emplace_back(1);
  VERIFY( v[1] == true );
  VERIFY( v.size() == 2 );
}

void test02()
{
  std::vector<bool> v;
  auto it = v.emplace(v.cbegin());
  VERIFY( it == v.begin() );
  VERIFY( *it == false );
  it = v.emplace(it, 1);
  VERIFY( it == v.begin() );
  VERIFY( *it == true );
  VERIFY( v.size() == 2 );
}

int main()
{
  test01();
  test02();
}
