// 2006-10-12  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006-2014 Free Software Foundation, Inc.
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

// { dg-options "-DWIDTH=500000" { target simulator } }

// 27.6.2.5.4 basic_ostream character inserters

#include <ostream>
#include <sstream>
#include <testsuite_hooks.h>

#ifndef WIDTH
#define WIDTH 50000000
#endif

// libstdc++/28277
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  wostringstream oss_01;

  oss_01.width(WIDTH);
  const streamsize width = oss_01.width();

  oss_01 << L'a';

  VERIFY( oss_01.good() );
  VERIFY( oss_01.str().size() == wstring::size_type(width) );
}

int main()
{
  test01();
  return 0;
}
