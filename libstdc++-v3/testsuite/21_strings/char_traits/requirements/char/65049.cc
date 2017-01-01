// Copyright (C) 2015-2017 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

// libstdc++/65049

#include <string>
#include <testsuite_hooks.h>

using C = char;

void
test01()
{
  const C* p = 0;
  C* q = 0;
  auto c = std::char_traits<C>::compare(p, q, 0);
  VERIFY( c == 0 );
  auto r = std::char_traits<C>::find(p, 0, '0');
  VERIFY( r == nullptr );
  r = std::char_traits<C>::move(q, p, 0);
  VERIFY( r == q );
  r = std::char_traits<C>::copy(q, p, 0);
  VERIFY( r == q );
  r = std::char_traits<C>::assign(q, 0, '0');
  VERIFY( r == q );
}

int
main()
{
  test01();
}
