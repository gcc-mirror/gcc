// Copyright (C) 2013-2025 Free Software Foundation, Inc.
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
// { dg-do run { xfail *-*-* } }

#include <vector>
#include <debug/vector>
#include <debug/checks.h>

void test01()
{
  std::vector<bool> v;
  __gnu_debug::vector<bool> dv;
  for (int i = 0; i != 10; ++i)
    {
      v.push_back((i % 2) != 0);
      dv.push_back((i % 2) == 0);
    }

  dv.insert(dv.begin(), v.begin(), v.begin() + 5);
  VERIFY( dv.size() == 15 );
}

void test02()
{
  __gnu_test::check_insert4<__gnu_debug::vector<bool> >();
}

int main()
{
  test01();
  test02();
  return 0;
}
