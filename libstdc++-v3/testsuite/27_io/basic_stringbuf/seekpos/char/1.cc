// 981208 bkoz test functionality of basic_stringbuf for char_type == char

// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003
// Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

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
  std::string 		str_tmp;
  std::stringbuf 		strb_tmp;
  typedef std::stringbuf::int_type int_type;
  typedef std::stringbuf::traits_type traits_type;
  typedef std::stringbuf::pos_type pos_type;
  typedef std::stringbuf::off_type off_type;

  int_type c1 = strb_01.sbumpc();
  int_type c2 = strb_02.sbumpc();
  int_type c3 = strb_01.sbumpc();

  pos_type pt_1(off_type(-1));
  pos_type pt_2(off_type(0));
  off_type off_1 = 0;
  off_type off_2 = 0;

  // PUT
  strb_03.str(str_01); //reset
  
  // BUFFER MANAGEMENT & POSITIONING

  // seekpos
  // pubseekpos(pos_type sp, ios_base::openmode)
  // alters the stream position to sp
  strb_01.str(str_01); //in|out ("mykonos. . . or what?");
  strb_02.str(str_02); //in ("paris, or sainte-maxime?");
  strb_03.str(str_03); //out ("")
  //IN|OUT
  //beg
  pt_1 = strb_01.pubseekoff(2, std::ios_base::beg);
  off_1 = off_type(pt_1);
  VERIFY( off_1 >= 0 );
  pt_1 = strb_01.pubseekoff(0, std::ios_base::cur, std::ios_base::out);
  off_1 = off_type(pt_1);
  c1 = strb_01.snextc(); //current in pointer +1
  VERIFY( c1 == 'o' );
  c2 = strb_01.sputc('x');  //test current out pointer
  str_tmp = std::string("myxonos. . . or what?");
  VERIFY( strb_01.str() == str_tmp );
  strb_01.pubsync(); //resets pointers
  pt_2 = strb_01.pubseekpos(pt_1, std::ios_base::in|std::ios_base::out);
  off_2 = off_type(pt_2);
  VERIFY( off_1 == off_2 );
  c3 = strb_01.snextc(); //current in pointer +1
  VERIFY( c1 == c3 );
  c2 = strb_01.sputc('x');  //test current out pointer
  str_tmp = std::string("myxonos. . . or what?");
  VERIFY( strb_01.str() == str_tmp );
}

int main()
{
  test04();
  return 0;
}



// more candy!!!
