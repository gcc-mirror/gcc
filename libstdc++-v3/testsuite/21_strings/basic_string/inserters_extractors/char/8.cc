// 1999-07-01 bkoz

// Copyright (C) 1999-2018 Free Software Foundation, Inc.
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

// 21.3.7.9 inserters and extractors

// NB: This file is predicated on sstreams, istreams, and ostreams
// working, not to mention other major details like char_traits, and
// all of the string class.

#include <string>
#include <sstream>
#include <iomanip>
#include <testsuite_hooks.h>

// libstdc++/1019
void test08()
{
  using namespace std;

  istringstream istrm("enero:2001");
  int 		year;
  char 		sep;
  string 	month;
  
  istrm >> setw(5) >> month >> sep >> year;
  VERIFY( month.size() == 5 );
  VERIFY( sep == ':' );
  VERIFY( year == 2001 );
}

int main()
{ 
  test08();
  return 0;
}
