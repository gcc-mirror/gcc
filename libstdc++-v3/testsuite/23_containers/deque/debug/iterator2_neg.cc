// Copyright (C) 2022 Free Software Foundation, Inc.
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
// { dg-require-debug-mode "" }

#include <deque>

void test01()
{
  typedef typename std::deque<int>::iterator It;
  It it;
  {
    std::deque<int> dq;
    it = dq.begin();
  }

  It value_init_it = It();
  (void)(it != value_init_it);
}

int main()
{
  test01();
  return 0;
}
