// Copyright (C) 2006-2016 Free Software Foundation, Inc.
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

// libstdc++/27867
void test01()
{
  std::valarray<int> v1(100, 1);
  std::valarray<int> v2(100, 1);
  std::valarray<bool> v3(true, 1);

  std::valarray<bool> resl(1);
  resl = ((v1 == v2) == v3);
  VERIFY( resl[0] == true );

  std::valarray<bool> resr(1);
  resr = (v3 == (v1 == v2));
  VERIFY( resr[0] == true );
}

int main()
{
  test01();
  return 0;
}
