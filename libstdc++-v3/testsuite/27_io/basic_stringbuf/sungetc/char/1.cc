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
  std::streamsize 		strmsz_1, strmsz_2;
  typedef std::stringbuf::int_type int_type;
  typedef std::stringbuf::traits_type traits_type;
  typedef std::stringbuf::pos_type pos_type;
  typedef std::stringbuf::off_type off_type;

  int_type c1 = strb_01.sbumpc();
  int_type c2 = strb_02.sbumpc();
  int_type c3 = strb_01.sbumpc();
  int_type c4 = strb_02.sbumpc();

  // PUT
  strb_03.str(str_01); //reset
  std::string::size_type sz1 = strb_03.str().length();
  std::string::size_type sz2 = strb_03.str().length();
  
  // streamsize sputn(const char_typs* s, streamsize n)
  // write up to n chars to out_cur from s, returning number assigned
  // NB *sputn will happily put '\0' into your stream if you give it a chance*
  str_tmp = strb_03.str();
  sz1 = str_tmp.length();
  strmsz_1 = strb_03.sputn("racadabras", 10);//"abracadabras or what?"
  sz2 = strb_03.str().length();
  strmsz_2 = strb_03.sputn(", i wanna reach out and", 10);
  sz2 = strb_03.str().length();
  str_tmp = strb_02.str();
  strmsz_1 = strb_02.sputn("racadabra", 10);

  // PUTBACK

  // int_type sputbackc(char_type c)
  // if in_cur not avail || ! traits::eq(c, gptr() [-1]), return pbfail
  // otherwise decrements in_cur and returns *gptr()
  strmsz_1 = strb_01.in_avail();
  str_tmp = strb_01.str();
  c1 = strb_01.sgetc(); //"mykonos. . . 'o'r what?"
  c2 = strb_01.sputbackc('z');//"mykonos. . .zor what?"
  c3 = strb_01.sgetc();
  //test for _in_cur == _in_beg
  strb_01.str(str_tmp);
  strmsz_1 = strb_01.in_avail();
  c1 = strb_01.sgetc(); //"'m'ykonos. . . or what?"
  c2 = strb_01.sputbackc('z');//"mykonos. . . or what?"
  c3 = strb_01.sgetc();
  // test for replacing char with identical one
  strb_01.str(str_01); //reset
  strmsz_1 = strb_01.in_avail();
  strb_01.sbumpc();
  strb_01.sbumpc();
  c1 = strb_01.sgetc(); //"my'k'onos. . . or what?"
  c2 = strb_01.sputbackc('y');//"mykonos. . . or what?"
  c3 = strb_01.sgetc();
  //test for ios_base::out
  strmsz_2 = strb_03.in_avail();
  c4 = strb_03.sputbackc('x');

  // int_type sungetc()
  // if in_cur not avail, return pbackfail(), else decrement and
  // return to_int_type(*gptr())
  for (int i = 0; i<12; ++i)
    strb_01.sbumpc();
  strmsz_1 = strb_01.in_avail();
  str_tmp = strb_01.str();
  c1 = strb_01.sgetc(); //"mykonos. . . 'o'r what?"
  c2 = strb_01.sungetc();//"mykonos. . . or what?"
  c3 = strb_01.sgetc();
  VERIFY( c1 != c2 );
  VERIFY( c3 == c2 );
  VERIFY( c1 != c3 );
  VERIFY( c2 == ' ' );
  VERIFY( strb_01.str() == str_01 );
  VERIFY( str_01.size() == strb_01.str().size() );
  //test for _in_cur == _in_beg
  strb_01.str(str_tmp);
  strmsz_1 = strb_01.in_avail();
  c1 = strb_01.sgetc(); //"'m'ykonos. . . or what?"
  c2 = strb_01.sungetc();//"mykonos. . . or what?"
  c3 = strb_01.sgetc();
  VERIFY( c1 != c2 );
  VERIFY( c3 != c2 );
  VERIFY( c1 == c3 );
  VERIFY( c2 == traits_type::eof() );
  VERIFY( strb_01.str() == str_01 );
  VERIFY( str_01.size() == strb_01.str().size() );
  // test for replacing char with identical one
  strb_01.str(str_01); //reset
  strmsz_1 = strb_01.in_avail();
  strb_01.sbumpc();
  strb_01.sbumpc();
  c1 = strb_01.sgetc(); //"my'k'onos. . . or what?"
  c2 = strb_01.sungetc();//"mykonos. . . or what?"
  c3 = strb_01.sgetc();
  VERIFY( c1 != c2 );
  VERIFY( c3 == c2 );
  VERIFY( c1 != c3 );
  VERIFY( strb_01.str() == str_01 );
  VERIFY( str_01.size() == strb_01.str().size() );
  //test for ios_base::out
  strmsz_2 = strb_03.in_avail();
  c4 = strb_03.sungetc();
  VERIFY( c4 == traits_type::eof() );
}

int main()
{
  test04();
  return 0;
}



// more candy!!!
