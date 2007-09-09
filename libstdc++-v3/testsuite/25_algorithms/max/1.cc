// 2000-03-29 sss/bkoz

// Copyright (C) 2000, 2003, 2004, 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;

  const int& x = std::max(1, 2);
  const int& y = std::max(4, 3);
  VERIFY( x == 2 );
  VERIFY( y == 4 );

  const int& xc = std::max(1, 2, std::greater<int>());
  const int& yc = std::max(4, 3, std::greater<int>());
  VERIFY( xc == 1 );
  VERIFY( yc == 3 );
}

int main()
{
  test01();
  return 0;
}
