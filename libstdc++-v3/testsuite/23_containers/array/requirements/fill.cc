// { dg-do run { target c++11 } }

// 2008-06-13  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2008-2016 Free Software Foundation, Inc.
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

// DR 776. Undescribed assign function of std::array.
void test01()
{ 
  bool test __attribute__((unused)) = true;

  const size_t len = 3;
  typedef std::array<int, len> array_type;

  array_type a = { { 0, 1, 2 } };
  const int value = 5;

  a.fill(value);
  VERIFY( a[0] == value );
  VERIFY( a[1] == value );
  VERIFY( a[2] == value );  
}

int main()
{
  test01();
  return 0;
}
