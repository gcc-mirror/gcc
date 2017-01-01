// { dg-do run { target c++11 } }

// Copyright (C) 2011-2017 Free Software Foundation, Inc.
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

#include <deque>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

// libstdc++/49836
void test01()
{
  using __gnu_test::CopyConsOnlyType;
  using __gnu_test::MoveConsOnlyType;

  std::deque<CopyConsOnlyType> d1;
  CopyConsOnlyType t1(1);
  d1.push_front(t1);
  d1.push_front(t1);
  d1.push_front(t1);
  VERIFY( d1.size() == 3 );

  std::deque<MoveConsOnlyType> d2;
  MoveConsOnlyType t2(1);
  d2.push_front(std::move(t2));
  d2.push_front(std::move(t2));
  d2.push_front(std::move(t2));
  VERIFY( d2.size() == 3 );
}

int main()
{
  test01();
  return 0;
}
