// 1999-04-12 bkoz

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

// 27.6.1.2.2 arithmetic extractors

#include <istream>
#include <sstream>
#include <locale>
#include <testsuite_hooks.h>

// http://gcc.gnu.org/ml/libstdc++/2000-q1/msg00081.html
// Jim Parsons
void test06()
{
  // default locale, grouping is turned off
  bool test __attribute__((unused)) = true;
  unsigned int h4;
  char c;
  std::string s("205,199,144");
  std::istringstream is(s);
  
  is >> h4; // 205
  VERIFY( h4 == 205 );
  is >> c; // ','
  VERIFY( c == ',' );

  is >> h4; // 199
  VERIFY( h4 == 199 );
  is >> c; // ','
  VERIFY( c == ',' );

  is >> h4; // 144
  VERIFY( is.rdstate() == std::ios_base::eofbit );
  VERIFY( h4 == 144 );
  is >> c; // EOF
  VERIFY( c == ',' );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::failbit) );
}

int main()
{
  test06();
  return 0;
}
