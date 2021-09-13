// { dg-do run { target c++11 } }
//
// Copyright (C) 2011-2021 Free Software Foundation, Inc.
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

#include <array>
#include <testsuite_hooks.h>

void
test01() 
{
  const size_t len = 0;
  typedef std::array<int, len> array_type;

  // 1: ?
  array_type a = { };

  // 2
  array_type b;

  // 3
  // begin() == end()
  VERIFY( a.begin() == a.end() );
  VERIFY( b.begin() == b.end() );
}

int main()
{
  test01();
  return 0;
}
