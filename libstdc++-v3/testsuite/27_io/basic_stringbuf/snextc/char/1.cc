// 981208 bkoz test functionality of basic_stringbuf for char_type == char

// Copyright (C) 1997-2013 Free Software Foundation, Inc.
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
  bool test __attribute__((unused)) = true;
  typedef std::stringbuf::int_type int_type;
  typedef std::stringbuf::traits_type traits_type;

  // GET
  strb_01.in_avail();
  strb_02.in_avail();
  strb_03.in_avail(); 

  strb_01.sbumpc();
  strb_02.sbumpc();
  strb_01.sbumpc();
  strb_02.sbumpc();
  int_type c5 = strb_03.sbumpc();

  // int_type sgetc()
  // if read_cur not avail, return uflow(), else return *read_cur  
  int_type c6 = strb_01.sgetc();
  int_type c7 = strb_02.sgetc();
  int_type c8 = strb_01.sgetc();
  int_type c9 = strb_02.sgetc();
  c5 = strb_03.sgetc();

  // int_type snextc()
  // calls sbumpc and if sbumpc != eof, return sgetc
  c6 = strb_01.snextc();
  c7 = strb_02.snextc();
  VERIFY( c6 != c8 );
  VERIFY( c7 != c9 );
  VERIFY( c6 == traits_type::to_int_type(str_01[3]) );
  VERIFY( c7 == traits_type::to_int_type(str_02[3]) ); //should equal fourth letter at this point
  c5 = strb_03.snextc();
  VERIFY( c5 == traits_type::eof() );
}

int main()
{
  test04();
  return 0;
}



// more candy!!!
