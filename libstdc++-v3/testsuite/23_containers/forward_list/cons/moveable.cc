// { dg-do run { target c++11 } }

// Copyright (C) 2008-2017 Free Software Foundation, Inc.
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
// is implemented on list (via swap). If the implementation changed
// this test may begin to fail.

#include <forward_list>
#include <utility>
#include <testsuite_hooks.h>

int main()
{
  std::forward_list<int> a, b;
  a.push_front(1);

  b = std::move(a);
  VERIFY(b.empty() == false);
  VERIFY(*b.begin() == 1);
  VERIFY(a.empty() == true);

  std::forward_list<int> c(std::move(b));
  VERIFY(c.empty() == false);
  (*c.begin() == 1 );
  VERIFY( b.empty() == true );

  return 0;
}
