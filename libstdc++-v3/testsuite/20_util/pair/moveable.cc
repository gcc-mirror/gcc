// { dg-options "-std=gnu++0x" }

// Copyright (C) 2005, 2007 Free Software Foundation, Inc.
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
// is implemented on pair, and also vector. If the implementation 
// changes this test may begin to fail.

#include <vector>
#include <utility>
#include <testsuite_hooks.h>

bool test __attribute__((unused)) = true;

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
