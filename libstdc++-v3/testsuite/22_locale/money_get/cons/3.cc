// 2005-04-29  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005-2019 Free Software Foundation, Inc.
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

// 22.2.6.1  Template class money_get

// { dg-do compile }

#include <locale> 
#include <testsuite_character.h>

class gnu_money_get: public std::money_get<__gnu_test::pod_uint> 
{ };

// libstdc++/21238
void test01()
{ 
  gnu_money_get facet01;
}

int main()
{
  test01();
  return 0;
}
