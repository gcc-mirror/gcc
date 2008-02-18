// { dg-options "-std=gnu++0x" }

// Copyright (C) 2008 Free Software Foundation, Inc.
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

#include <tuple>
#include <utility>

struct MoveOnly
{
  MoveOnly () { }
  
  MoveOnly (MoveOnly&&) { }

  MoveOnly& operator=(MoveOnly&&)
  { return *this; }

private:
  MoveOnly(MoveOnly const&); // = delete
  MoveOnly& operator=(MoveOnly const&); // = delete
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
