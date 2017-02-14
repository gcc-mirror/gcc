// Copyright (C) 2006-2017 Free Software Foundation, Inc.
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

#include <valarray>
#include <testsuite_hooks.h>

// DR 543. valarray slice default constructor
void test01()
{
  std::valarray<int> v1(10);
  std::valarray<int> v2 = v1[std::slice()];
  VERIFY( v2.size() == 0 );

  std::valarray<int> v3(10);
  std::valarray<int> v4 = v3[std::gslice()];
  VERIFY( v4.size() == 0 );
}

int main()
{
  test01();
  return 0;
}
