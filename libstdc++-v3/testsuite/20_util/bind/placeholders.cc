// { dg-do run { target c++11 } }
// Copyright (C) 2009-2025 Free Software Foundation, Inc.
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

// 20.7.11 Function template bind

#include <functional>
#include <testsuite_hooks.h>

// Operations on empty function<> objects
void test01()
{
  using namespace std::placeholders;

  int five = 5;
  int seven = 7;
  VERIFY( std::bind(std::minus<int>(), _1, _2)(five, seven) == -2 );
  VERIFY( std::bind(std::minus<int>(), _2, _1)(five, seven) == 2 );
}

int main()
{
  test01();
  return 0;
}
