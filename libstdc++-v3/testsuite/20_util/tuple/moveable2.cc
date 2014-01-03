// { dg-options "-std=gnu++0x" }

// Copyright (C) 2008-2014 Free Software Foundation, Inc.
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

#include <tuple>
#include <utility>

struct MoveOnly
{
  MoveOnly () { }
  
  MoveOnly (MoveOnly&&) { }

  MoveOnly& operator=(MoveOnly&&)
  { return *this; }

  MoveOnly(MoveOnly const&) = delete;
  MoveOnly& operator=(MoveOnly const&) = delete;
};

MoveOnly
make_move_only ()
{ return MoveOnly(); }

// http://gcc.gnu.org/ml/libstdc++/2008-02/msg00046.html
void test01()
{
  typedef std::tuple<MoveOnly> move_only_tuple;

  move_only_tuple t1(make_move_only());
  move_only_tuple t2(std::move(t1));
  move_only_tuple t3 = std::move(t2);
  t1 = std::move(t3);

  typedef std::tuple<MoveOnly, MoveOnly> move_only_tuple2;

  move_only_tuple2 t4(make_move_only(), make_move_only());
  move_only_tuple2 t5(std::move(t4));
  move_only_tuple2 t6 = std::move(t5);
  t4 = std::move(t6);
}

int main()
{
  test01();
  return 0;
}
