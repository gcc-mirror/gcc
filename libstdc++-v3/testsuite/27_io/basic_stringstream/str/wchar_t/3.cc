// Copyright (C) 2004-2019 Free Software Foundation, Inc.
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

// 27.7.6 member functions (stringstream_members)

#include <sstream>
#include <testsuite_hooks.h>

void
test03()
{
  //
  // 1: Automatic formatting of a compound string
  //
  int i = 1024;
  int *pi = &i;
  double d = 3.14159;
  double *pd = &d;
  std::wstring blank;
  std::wostringstream ostrst01; 
  std::wostringstream ostrst02(blank); 
  
  // No buffer, so should be created.
  ostrst01 << L"i: " << i << L" i's address:  " << pi << L'\n'
	     << L"d: " << d << L" d's address: " << pd << std::endl;
  // Buffer, so existing buffer should be overwritten.
  ostrst02 << L"i: " << i << L" i's address:  " << pi << L'\n'
	     << L"d: " << d << L" d's address: " << pd << std::endl;

  std::wstring msg01 = ostrst01.str();
  std::wstring msg02 = ostrst02.str();
  VERIFY( msg01 == msg02 );
  VERIFY( msg02 != blank );
}

int main()
{
  test03();
  return 0;
}
