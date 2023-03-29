// 1999-08-11 bkoz

// Copyright (C) 1999-2023 Free Software Foundation, Inc.
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

#include <cstring> // for strlen
#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

// [bug] istream::getline(char*,streamsize) still broken
// http://gcc.gnu.org/ml/libstdc++/2000-07/msg00126.html
// brent verner <brent at rcfile dot org
void
test06()
{
  using namespace std;

  const streamsize it = 5;
  char tmp[it];
  const char* str_lit = "abcd\n";

  stringbuf strbuf(str_lit, std::ios_base::in);
  istream istr(&strbuf);
  
  istr.getline(tmp,it); 
  VERIFY( istr.gcount() == it );  // extracted whole string
  VERIFY( strlen(tmp) == 4 );     // stored all but '\n'
  VERIFY( !istr.eof() );          // extracted up to but not eof
  VERIFY( !istr.fail() );         // failbit not set
  
  char c = 'z';
  istr.get(c);
  VERIFY( c == 'z' );
  VERIFY( istr.eof() );
}

int 
main()
{
  test06();
  return 0;
}
