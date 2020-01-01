// 2001-05-24 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2020 Free Software Foundation, Inc.
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
  std::string blank;
  std::ostringstream ostrst01; 
  std::ostringstream ostrst02(blank); 
  
  // No buffer, so should be created.
  ostrst01 << "i: " << i << " i's address:  " << pi << '\n'
	     << "d: " << d << " d's address: " << pd << std::endl;
  // Buffer, so existing buffer should be overwritten.
  ostrst02 << "i: " << i << " i's address:  " << pi << '\n'
	     << "d: " << d << " d's address: " << pd << std::endl;

  std::string msg01 = ostrst01.str();
  std::string msg02 = ostrst02.str();
  VERIFY( msg01 == msg02 );
  VERIFY( msg02 != blank );
}

int main()
{
  test03();
  return 0;
}
