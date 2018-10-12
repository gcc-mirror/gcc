// Copyright (C) 2018 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <utility>
#include <testsuite_hooks.h>

void
test01()
{
  typedef std::pair<int&, int> pair_type;
  int i = 1;
  int j = 2;
  pair_type p(i, 3);
  const pair_type q(j, 4);
  p = q;
  VERIFY( p.first == q.first );
  VERIFY( p.second == q.second );
  VERIFY( i == j );
}

void
test02()
{
  typedef std::pair<int, int&> pair_type;
  int i = 1;
  int j = 2;
  pair_type p(3, i);
  const pair_type q(4, j);
  p = q;
  VERIFY( p.first == q.first );
  VERIFY( p.second == q.second );
  VERIFY( i == j );
}

void
test03()
{
  typedef std::pair<int&, int&> pair_type;
  int i = 1;
  int j = 2;
  int k = 3;
  int l = 4;
  pair_type p(i, j);
  const pair_type q(k, l);
  p = q;
  VERIFY( p.first == q.first );
  VERIFY( p.second == q.second );
  VERIFY( i == k );
  VERIFY( j == l );
}

int
main()
{
  test01();
  test02();
  test03();
}
