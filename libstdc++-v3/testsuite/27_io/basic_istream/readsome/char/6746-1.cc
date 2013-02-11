// 1999-08-11 bkoz

// Copyright (C) 1999-2013 Free Software Foundation, Inc.
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

// 27.6.1.3 unformatted input functions

#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/6746   
void test12()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  streamsize sum = 0;
  istringstream iss("shamma shamma");
      
  // test01
  size_t i = iss.rdbuf()->in_avail();
  VERIFY( i != 0 );
    
  // test02
  streamsize extracted;
  do
    {
      char buf[1024];
      extracted = iss.readsome(buf, sizeof buf);
      sum += extracted;
    }
  while (iss.good() && extracted);
  VERIFY( sum != 0 );  
}

int 
main()
{
  test12();
  return 0;
}
