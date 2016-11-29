// 2004-11-26  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2016 Free Software Foundation, Inc.
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

#include <sstream>
#include <testsuite_hooks.h>

// DR 243. get and getline when sentry reports failure.
void test01()
{
  using namespace std;

  stringbuf strbuf01;
  stringbuf strbuf02;
  istream istr01(&strbuf01);
  istream istr02(&strbuf02);
  char buf02[2] = "*" ;
 
  istr01.peek();
  VERIFY( istr01.eof() );

  istr01.get(0, 0);
  VERIFY( istr01.gcount() == 0 );
  VERIFY( istr01.fail() );

  istr02.peek();
  VERIFY( istr02.eof() );

  istr02.get(buf02, 1);
  VERIFY( istr02.gcount() == 0 );
  VERIFY( istr02.fail() );
  VERIFY( buf02[0] == char() );  
}

int main() 
{
  test01();
  return 0;
}
