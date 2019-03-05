// 2001-10-30 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2019 Free Software Foundation, Inc.
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

// 21.3.5 string modifiers

#include <string>
#include <cstdio>
#include <testsuite_hooks.h>

// assign(const basic_string& __str, size_type __pos, size_type __n)
void
test02()
{
  using namespace std;
  
  wstring one = L"Selling England by the pound";
  wstring two = one;
  wstring three = L"Brilliant trees";

  one.assign(one, 8, 100);
  VERIFY( one == L"England by the pound" );

  one.assign(one, 8, 0);
  VERIFY( one == L"" );
 
  one.assign(two, 8, 7);
  VERIFY( one == L"England" );

  one.assign(three, 10, 100);
  VERIFY( one == L"trees" );

  three.assign(one, 0, 3);
  VERIFY( three == L"tre" );
}

int main()
{ 
  test02();
  return 0;
}
