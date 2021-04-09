// 2005-03-20 Douglas Gregor <doug.gregor -at- gmail.com>
//
// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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

// 3.6 function object binders
#include <tr1/functional>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

using namespace __gnu_test;

// Operations on empty function<> objects
void test01()
{
  VERIFY( std::tr1::bind(std::plus<int>(), 3, 5)() == 8 );
  VERIFY( std::tr1::bind(std::minus<int>(), 3, 5)() == -2 );
  VERIFY( std::tr1::bind<int>(std::plus<int>(), 3, 5)() == 8 );
  VERIFY( std::tr1::bind<int>(std::minus<int>(), 3, 5)() == -2 );
}

int main()
{
  test01();
  return 0;
}
