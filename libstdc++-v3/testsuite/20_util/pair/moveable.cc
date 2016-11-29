// { dg-do run { target c++11 } }

// Copyright (C) 2005-2016 Free Software Foundation, Inc.
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
// is implemented on pair, and also vector. If the implementation 
// changes this test may begin to fail.

#include <vector>
#include <utility>
#include <testsuite_hooks.h>

void
test1()
{
  std::pair<int,int> a(1,1),b(2,2);
  a=std::move(b);
  VERIFY(a.first == 2 && a.second == 2 && b.first == 2 && b.second == 2);
  std::pair<int,int> c(std::move(a));
  VERIFY(c.first == 2 && c.second == 2 && a.first == 2 && a.second == 2);
}

void
test2()
{
  std::vector<int> v,w;
  v.push_back(1);
  w.push_back(2);
  w.push_back(2);
  std::pair<int, std::vector<int> > p = make_pair(1,v);
  std::pair<int, std::vector<int> > q = make_pair(2,w);
  p = std::move(q);
  VERIFY(p.first == 2 && q.first == 2 &&
	 p.second.size() == 2 && q.second.size() == 0);
  std::pair<int, std::vector<int> > r(std::move(p));
  VERIFY(r.first == 2 && p.first == 2 &&
         r.second.size() == 2 && p.second.size() == 0);
}

int 
main() 
{
  test1();
  test2();
}
