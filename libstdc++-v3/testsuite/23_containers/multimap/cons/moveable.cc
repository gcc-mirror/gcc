// { dg-do run { target c++11 } }

// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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
// is implemented on multimap (via swap). If the implementation changed
// this test may begin to fail.

#include <map>
#include <utility>
#include <testsuite_hooks.h>

int main()
{
  std::multimap<int, int> a,b;
  a.insert(std::make_pair(2,0));
  b.insert(std::make_pair(1,0));
  b = std::move(a);
  VERIFY(b.find(2) != b.end() && a.find(1) == a.end());

  std::multimap<int, int> c(std::move(b));
  VERIFY( c.find(2) != c.end());
  VERIFY( b.find(2) == b.end());
  return 0;
}
