// 1999-05-07 bkoz

// Copyright (C) 1999, 2003 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 21.3.6 string operations

#include <string>
#include <cstdio>
#include <testsuite_hooks.h>

int test01(void)
{
  bool test __attribute__((unused)) = true;

  std::wstring str1;
  std::wstring str2;
  
  // Should get this:
  // 1:8-chars_8-chars_
  // 2:8-chars_8-chars_
  str1 = std::wstring(L"8-chars_") + L"8-chars_";
  str1.c_str();
  // wprintf("1:%s\n", str1.c_str());
  str2 = str1 + L"7-chars";
  // wprintf("2:%s\n", str1.c_str()); //str1 is gone
  str1.c_str();
  return 0;
}

int main()
{ 
  test01();
  return 0;
}
