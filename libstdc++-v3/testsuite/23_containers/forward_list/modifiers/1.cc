// { dg-do run { target c++11 } }

// Copyright (C) 2008-2021 Free Software Foundation, Inc.
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

// 23.2.3.n forward_list xxx [lib.forward_list.xxx]

#include <forward_list>
#include <testsuite_hooks.h>

#include <array>

class PathPoint
{
public:
  PathPoint(char t, std::array<double, 3> & c)
  : type(t), coord(c) { }
  char getType() const { return type; }
private:
  char type;
  std::array<double, 3> coord;
};

// This test verifies the following.
//   emplace_front
//   pop_front
//   emplace_after
void
test01()
{
  std::forward_list<PathPoint> path;
  std::array<double, 3> coord1 = { { 0.0, 1.0, 2.0 } };
  path.emplace_front('a', coord1);

  std::forward_list<PathPoint>::const_iterator pos = path.cbegin();

  std::array<double, 3> coord2 = { { 3.0, 4.0, 5.0 } };
  path.emplace_after(pos, 'b', coord2);

  VERIFY(path.front().getType() == 'a');

  path.pop_front();

  VERIFY(path.front().getType() == 'b');

  path.pop_front();

  VERIFY(path.empty() == true);
}

int
main()
{
  test01();
  return 0;
}
