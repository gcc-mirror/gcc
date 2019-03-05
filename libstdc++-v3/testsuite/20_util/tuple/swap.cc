// { dg-do run { target c++11 } }

// Copyright (C) 2007-2019 Free Software Foundation, Inc.
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


// NOTE: This makes use of the fact that we know how moveable
// is implemented on tuple.  If the implementation changed
// this test may begin to fail.

#include <tuple>
#include <utility>
#include <testsuite_hooks.h>

struct MoveOnly
{
  explicit MoveOnly (int j) : i(j) { }

  MoveOnly (MoveOnly&& m) : i(m.i) { }

  MoveOnly& operator=(MoveOnly&& m)
  { i = m.i; return *this; }

  MoveOnly(MoveOnly const&) = delete;
  MoveOnly& operator=(MoveOnly const&) = delete;

  bool operator==(MoveOnly const& m)
  { return i == m.i; }

  void swap(MoveOnly& m)
  { std::swap(m.i, i); }

  int i;
};

void swap(MoveOnly& m1, MoveOnly& m2)
{ m1.swap(m2); }

MoveOnly
make_move_only (int i)
{ return MoveOnly(i); }

void test01()
{
  std::tuple<> t1, t2;
  std::swap(t1, t2);

  VERIFY( t1 == t2 );
}

void test02()
{
  std::tuple<int> t1(1), t2(2);
  std::swap(t1, t2);
  
  VERIFY( std::get<0>(t1) == 2 && std::get<0>(t2) == 1 );
}

void test03()
{
  std::tuple<int, float> t1(1, 1.0f), t2(2, 2.0f);
  std::swap(t1, t2);

  VERIFY( std::get<0>(t1) == 2 && std::get<0>(t2) == 1 );
  VERIFY( std::get<1>(t1) == 2.0f && std::get<1>(t2) == 1.0f );
}

void test04()
{
  std::tuple<int, float, MoveOnly> 
    t1(1, 1.0f, make_move_only(1)), 
    t2(2, 2.0f, make_move_only(2));

  std::swap(t1, t2);

  VERIFY( std::get<0>(t1) == 2 && std::get<0>(t2) == 1 );
  VERIFY( std::get<1>(t1) == 2.0f && std::get<1>(t2) == 1.0f );
  VERIFY( std::get<2>(t1) == make_move_only(2) 
	  && std::get<2>(t2) == make_move_only(1) );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}
