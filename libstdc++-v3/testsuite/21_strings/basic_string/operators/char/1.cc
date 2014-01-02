// 1999-05-07 bkoz

// Copyright (C) 1999-2014 Free Software Foundation, Inc.
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

// 21.3.6 string operations

#include <string>
#include <cstdio>
#include <testsuite_hooks.h>

int test01(void)
{
  bool test __attribute__((unused)) = true;

  std::string str1;
  std::string str2;
  
  // Should get this:
  // 1:8-chars_8-chars_
  // 2:8-chars_8-chars_
  str1 = std::string("8-chars_") + "8-chars_";
  str1.c_str();
  // printf("1:%s\n", str1.c_str());
  str2 = str1 + "7-chars";
  // printf("2:%s\n", str1.c_str()); //str1 is gone
  str1.c_str();
  return 0;
}

int main()
{ 
  test01();
  return 0;
}
