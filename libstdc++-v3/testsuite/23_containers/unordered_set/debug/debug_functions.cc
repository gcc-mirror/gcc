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
// { dg-do run { target c++11 } }
// { dg-require-debug-mode "" }

#include <unordered_set>
#include <testsuite_hooks.h>

void test02()
{
  using namespace __gnu_debug;

  std::unordered_set<int> u = { 0, 1, 2 };

  VERIFY( !__check_singular(u.end()) );
  auto it = u.end();
  VERIFY( !__check_singular(it) );

  VERIFY( !__check_singular(u.begin()) );
  it = u.begin();
  VERIFY( !__check_singular(it) );

  u.clear();

  VERIFY( it._M_singular() );
  VERIFY( __check_singular(it) );

  it = u.end();
  VERIFY( !it._M_singular() );
  VERIFY( !__check_singular(it) );

  u = { 0, 1, 2 };

  auto bucket = u.bucket(0);
  VERIFY( !__check_singular(u.begin(bucket)) );
  auto lit = u.begin(bucket);
  VERIFY( !__check_singular(lit) );

  VERIFY( !__check_singular(u.end(bucket)) );

  u.clear();
  VERIFY( __check_singular(lit) );
}

int main()
{
  test02();
  return 0;
}
