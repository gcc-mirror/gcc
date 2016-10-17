// { dg-do run { target c++11 } }

// Copyright (C) 2016 Free Software Foundation, Inc.
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

#include "testsuite_hooks.h"

void test01()
{
  std::vector<std::vector<int>> vv =
    {
      { 2, 3 },
      { 4, 5 },
      { 0, 1 }
    };

  // Make sure it doesn't reallocate during insertion.
  vv.reserve(4);

  vv.insert(vv.begin(), vv[0]);

  VERIFY( vv.size() == 4 );
  VERIFY( vv[0].size() == 2 );
  VERIFY( vv[0][0] == 2 );
  VERIFY( vv[0][1] == 3 );
}

void test02()
{
  std::vector<std::vector<int>> vv =
    {
      { 2, 3 },
      { 4, 5 },
      { 0, 1 }
    };

  // Make sure we will reallocate for insertion.
  VERIFY( vv.capacity() == 3 );

  vv.insert(vv.begin(), vv[0]);

  VERIFY( vv.size() == 4 );
  VERIFY( vv[0].size() == 2 );
  VERIFY( vv[0][0] == 2 );
  VERIFY( vv[0][1] == 3 );
}

int main()
{
  test01();
  test02();
}
