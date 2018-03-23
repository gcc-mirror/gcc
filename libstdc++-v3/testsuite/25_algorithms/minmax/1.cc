// { dg-do run { target c++11 } }

// 2007-11-01  Paolo Carlini  <pcarlini@suse.de

// Copyright (C) 2007-2018 Free Software Foundation, Inc.
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

#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>

void test01()
{
  std::pair<const int, const int> z = std::minmax(1, 2);
  std::pair<const int, const int> w = std::minmax(4, 3);
  VERIFY( z.first == 1 );
  VERIFY( z.second == 2 );
  VERIFY( w.first == 3 );
  VERIFY( w.second == 4 );

  std::pair<const int, const int> zc = std::minmax(1, 2, std::greater<int>());
  std::pair<const int, const int> wc = std::minmax(4, 3, std::greater<int>());
  VERIFY( zc.first == 2 );
  VERIFY( zc.second == 1 );
  VERIFY( wc.first == 4 );
  VERIFY( wc.second == 3 );
}

int main()
{
  test01();
  return 0;
}
