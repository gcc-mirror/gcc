// { dg-do run { target c++11 } }
//
// Copyright (C) 2011-2024 Free Software Foundation, Inc.
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
  const size_t len = 5;
  typedef std::array<int, len> array_type;
  array_type a = { { 0, 1, 2, 3, 4 } };
  array_type b = { { 0, 1, 2, 3, 4 } };
  array_type c = { { 0, 1, 2, 3 } };

  VERIFY( !(a != b) );
  VERIFY( a != c );
}

int main()
{
  test01();
  return 0;
}
