// 1999-06-10 bkoz

// Copyright (C) 1994-2017 Free Software Foundation, Inc.
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

// 21.3.5.6 basic_string::replace

#include <string>
#include <testsuite_hooks.h>

// We wrongly used __n1 instead of __foldn1 in the length_error
// check at the beginning of replace(__pos, __n1, __s, __n2)
void
test05()
{
  std::string str01 = "londinium";
  std::string str02 = "cydonia";

  str01.replace(0, 20, str02.c_str(), 3);
  VERIFY(str01 == "cyd");
}

int main()
{ 
  test05();
  return 0;
}
