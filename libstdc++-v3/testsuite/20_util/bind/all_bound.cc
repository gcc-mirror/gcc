// { dg-options "-std=gnu++0x" }
// Copyright (C) 2009-2013 Free Software Foundation, Inc.
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
  bool test __attribute__((unused)) = true;

  VERIFY( std::bind(std::plus<int>(), 3, 5)() == 8 );
  VERIFY( std::bind(std::minus<int>(), 3, 5)() == -2 );
  VERIFY( std::bind<int>(std::plus<int>(), 3, 5)() == 8 );
  VERIFY( std::bind<int>(std::minus<int>(), 3, 5)() == -2 );
}

int main()
{
  test01();
  return 0;
}
