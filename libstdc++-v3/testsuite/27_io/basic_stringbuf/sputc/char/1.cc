// 981208 bkoz test functionality of basic_stringbuf for char_type == char

// Copyright (C) 1997-2025 Free Software Foundation, Inc.
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

#include <sstream>
#include <testsuite_hooks.h>

std::string str_01("mykonos. . . or what?");
std::string str_02("paris, or sainte-maxime?");
std::string str_03;
std::stringbuf strb_01(str_01);
std::stringbuf strb_02(str_02, std::ios_base::in);
std::stringbuf strb_03(str_03, std::ios_base::out);

// test overloaded virtual functions
void test04() 
{
  typedef std::stringbuf::int_type int_type;
  typedef std::stringbuf::traits_type traits_type;

  int_type c1 = strb_01.sbumpc();
  int_type c2 = strb_02.sbumpc();
  int_type c3 = strb_01.sbumpc();

  // PUT
  // int_type sputc(char_type c)
  // if out_cur not avail, return overflow. Else, stores c at out_cur,
  // increments out_cur, and returns c as int_type
  strb_03.str(str_01); //reset
  std::string::size_type sz1 = strb_03.str().length();
  c1 = strb_03.sputc('a'); 
  std::string::size_type sz2 = strb_03.str().length();
  VERIFY( sz1 == sz2 ); //cuz inserting at out_cur, which is at beg to start
  c2 = strb_03.sputc('b'); 
  VERIFY( c1 != c2 );
  VERIFY( strb_03.str() != str_01 );
  c3 = strb_02.sputc('a'); // should be EOF because this is read-only
  VERIFY( c3 == traits_type::eof() );
}

int main()
{
  test04();
  return 0;
}



// more candy!!!
