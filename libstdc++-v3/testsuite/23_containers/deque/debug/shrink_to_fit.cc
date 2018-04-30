// Copyright (C) 2011-2018 Free Software Foundation, Inc.
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
// { dg-do run { target c++11 xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <deque>

void test01()
{
  using std::deque;
  deque<int> d;
  // Let's generate a hole at the beginning of the deque:
  d.push_back(0);
  d.push_back(1);
  d.pop_front();
  deque<int>::iterator it;
  do
    {
      d.push_back(2);
      it = d.begin();
      auto old_abegin = &*d.begin();
      d.shrink_to_fit();
      if (&*d.begin() != old_abegin)
	break;
    }
  while (true);
  // Following line should assert
  *it = 2;
}

int main()
{
  test01();
  return 0;
}
