// { dg-options "-std=gnu++0x" }

// Copyright (C) 2007 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

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

  void swap(MoveOnly&& m)
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
  bool test __attribute__((unused)) = true;

  std::tuple<int> t1(1), t2(2);
  std::swap(t1, t2);
  
  VERIFY( std::get<0>(t1) == 2 && std::get<0>(t2) == 1 );
}

void test03()
{
  bool test __attribute__((unused)) = true;

  std::tuple<int, float> t1(1, 1.0f), t2(2, 2.0f);
  std::swap(t1, t2);

  VERIFY( std::get<0>(t1) == 2 && std::get<0>(t2) == 1 );
  VERIFY( std::get<1>(t1) == 2.0f && std::get<1>(t2) == 1.0f );
}

void test04()
{
  bool test __attribute__((unused)) = true;

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
