// Copyright (C) 2018-2019 Free Software Foundation, Inc.
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

// { dg-do run { xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <algorithm>
#include <list>

void
test01()
{
  std::list<int> l1, l2;
  l1.push_back(1);
  l1.push_back(2);
  l1.push_back(3);

  l2.push_back(1);
  std::copy_backward(++l1.begin(), l1.end(), l2.end());
}

int
main()
{
  test01();
  return 0;
}
