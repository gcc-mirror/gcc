// 2005-03-20 Douglas Gregor <doug.gregor -at- gmail.com>
//
// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 3.6 function object binders
#include <tr1/functional>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

using namespace __gnu_test;

bool test __attribute__((unused)) = true;

// Operations on empty function<> objects
void test01()
{
  using std::tr1::bind;
  using namespace std::tr1::placeholders;

  int five = 5;
  int seven = 7;
  VERIFY( bind(std::multiplies<int>(), _1, bind(std::minus<int>(), 6, _2))(five, seven) == -5 );
  VERIFY( bind(std::multiplies<int>(), _1, bind(std::minus<int>(), 6, _2))(seven, five) == 7 );
}

int main()
{
  test01();
  return 0;
}
