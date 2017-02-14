// 2006-07-15  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006-2017 Free Software Foundation, Inc.
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

// 27.6.2.5.4 basic_ostream character inserters

// { dg-options "-DMAX_SIZE=50000" { target simulator } }

#ifndef MAX_SIZE
#define MAX_SIZE 5000000
#endif

#include <ostream>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/28277
void test01()
{
  using namespace std;

  wostringstream oss_01;
  const string str_01(MAX_SIZE, 'a');

  oss_01 << str_01.c_str();

  VERIFY( oss_01.good() );
  VERIFY( oss_01.str().size() == str_01.size() );
}

int main()
{
  test01();
  return 0;
}
