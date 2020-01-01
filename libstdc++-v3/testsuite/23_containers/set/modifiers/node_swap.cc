// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

#include <set>
#include <testsuite_hooks.h>

void
test01()
{
  // PR libstdc++/82966
  std::set<int>::node_type n1, n2;
  n1.swap(n2);
  VERIFY( n1.empty() );
  VERIFY( n2.empty() );
}

void
test02()
{
  std::set<int> s{1, 2};
  std::set<int>::node_type n1 = s.extract(1), n2;
  swap(n1, n2);
  VERIFY( n1.empty() );
  VERIFY( !n2.empty() );
}

int main()
{
  test01();
  test02();
}
