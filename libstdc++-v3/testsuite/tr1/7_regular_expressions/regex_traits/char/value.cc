// { dg-do run }

// 2008-08-11  Stephen M. Webb  <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2008-2014 Free Software Foundation, Inc.
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

// tr1 [7.7] class template regex_traits value() function

#include <tr1/regex>
#include <testsuite_hooks.h>

// Tests the value() function of the regex_traits<char> class.
void test01()
{
  bool test __attribute__((unused)) = true;
  std::tr1::regex_traits<char> t;
  VERIFY( t.value('7', 8)  == 7 );
  VERIFY( t.value('7', 10) == 7 );
  VERIFY( t.value('7', 16) == 7 );
  VERIFY( t.value('9', 8)  == -1 );
  VERIFY( t.value('9', 10) == 9 );
  VERIFY( t.value('9', 16) == 9 );
  VERIFY( t.value('d', 8)  == -1 );
  VERIFY( t.value('d', 10) == -1 );
  VERIFY( t.value('d', 16) == 13 );
}

int
main()
{ 
  test01();
  return 0;
}
