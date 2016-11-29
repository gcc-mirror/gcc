// { dg-do run { target c++11 } }

// Copyright (C) 2007-2016 Free Software Foundation, Inc.
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

// 19.1 Exception classes

#include <string>
#include <system_error>
#include <testsuite_hooks.h>

using namespace std;

// libstdc++/1972
void test01()
{
  string s("lack of sunlight, no water error");

  // 1
  system_error obj1 = system_error(error_code(), s);

  // 2
  system_error obj2(error_code(), s);

  VERIFY( string(obj1.what()).find(s.data()) != string::npos );
  VERIFY( string(obj2.what()).find(s.data()) != string::npos );
}

void test02()
{
  string s("lack of sunlight error");
  system_error x(error_code(), s);
  
  VERIFY( string(x.what()).find(s.data()) != string::npos );
}

int main(void)
{
  test01();
  test02();
  return 0;
}
