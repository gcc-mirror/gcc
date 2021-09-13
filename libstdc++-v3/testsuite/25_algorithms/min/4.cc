// { dg-do run { target c++11 } }

// Copyright (C) 2008-2021 Free Software Foundation, Inc.
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
  const int& z = std::min({1, 2, 3, 4, 5, 6, 7});
  const double& w = std::min({2.0, 1.0, 3.2, 4.5, 5.0, 6.0, 7.0});
  const int& y = std::min({2, 3, 1, 4, 5, 6, 7});
  const float& x = std::min({2.0f, 3.0f, 5.0f, 1.0f, 7.0f, 6.0f});
  VERIFY( z == 1 );
  VERIFY( w == 1.0 );
  VERIFY( y == 1 );
  VERIFY( x == 1.0f );

  
  const int& zc = std::min({1, 2, 3, 4, 5, 6, 7}, std::greater<int>());
  const double& wc = std::min({2.0, 1.0, 3.2, 4.5, 5.0, 6.0, 7.0},
			      std::greater<double>());
  const int& yc = std::min({2, 7, 1, 4, 5, 6, 3}, std::greater<int>());
  const float& xc = std::min({2.0f, 3.0f, 5.0f, 1.0f, 7.0f, 6.0f},
			     std::greater<float>());
  
  VERIFY( zc == 7 );
  VERIFY( wc == 7.0 );
  VERIFY( yc == 7 );
  VERIFY( xc == 7.0f );
}

int main()
{
  test01();
  return 0;
}
