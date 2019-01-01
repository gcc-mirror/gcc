// Copyright (C) 2013-2019 Free Software Foundation, Inc.
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
//
// { dg-require-debug-mode "" }

#include <vector>
#include <testsuite_hooks.h>

void test02()
{
  using namespace __gnu_debug;

  std::vector<int> v1(3, 1);
  VERIFY( !__check_singular(v1.begin()) );
  std::vector<int>::iterator it = v1.begin();
  VERIFY( !__check_singular(it) );

  VERIFY( !__check_singular(v1.end()) );
  it = v1.end();
  VERIFY( !__check_singular(it) );

  v1.clear();

  VERIFY( it._M_singular() );
  VERIFY( __check_singular(it) );

  it = v1.end();
  VERIFY( !it._M_singular() );
  VERIFY( !__check_singular(it) );
}

int main()
{
  test02();
  return 0;
}
