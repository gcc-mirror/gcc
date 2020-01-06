// 981208 bkoz test functionality of basic_stringbuf for char_type == wchar_t

// Copyright (C) 1997-2020 Free Software Foundation, Inc.
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

std::wstring str_01(L"mykonos. . . or what?");
std::wstring str_02(L"paris, or sainte-maxime?");
std::wstring str_03;
std::wstringbuf strb_01(str_01);
std::wstringbuf strb_02(str_02, std::ios_base::in);
std::wstringbuf strb_03(str_03, std::ios_base::out);

// test overloaded virtual functions
void test04() 
{
  std::streamsize 		strmsz_1, strmsz_2;
  typedef std::wstringbuf::int_type int_type;
  typedef std::wstringbuf::traits_type traits_type;

  // GET
  strb_01.in_avail();
  strb_02.in_avail();
  strb_03.in_avail(); 

  int_type c1 = strb_01.sbumpc();
  int_type c2 = strb_02.sbumpc();
  strb_01.sbumpc();
  int_type c4 = strb_02.sbumpc();
  strb_03.sbumpc();

  // int_type sgetc()
  // if read_cur not avail, return uflow(), else return *read_cur  
  int_type c6 = strb_01.sgetc();
  int_type c7 = strb_02.sgetc();
  strb_01.sgetc();
  strb_02.sgetc();
  strb_03.sgetc();

  // int_type snextc()
  // calls sbumpc and if sbumpc != eof, return sgetc
  c6 = strb_01.snextc();
  c7 = strb_02.snextc();
  strb_03.snextc();

  // streamsize sgetn(char_type *s, streamsize n)
  // streamsize xsgetn(char_type *s, streamsize n)
  // assign up to n chars to s from input sequence, indexing in_cur as
  // approp and returning the number of chars assigned
  strmsz_1 = strb_01.in_avail();
  strmsz_2 = strb_02.in_avail();
  VERIFY( strmsz_1 != strmsz_2 );
  VERIFY( strmsz_1 != static_cast<std::streamsize>(str_01.length()) );
  // because now we've moved into string
  VERIFY( strmsz_2 != static_cast<std::streamsize>(str_02.length()) ); 
  wchar_t carray1[11] = L"";
  strmsz_1 = strb_01.sgetn(carray1, 10);
  wchar_t carray2[20] = L"";
  strmsz_2 = strb_02.sgetn(carray2, 10);
  VERIFY( strmsz_1 == strmsz_2 );
  VERIFY( strmsz_1 == 10 );
  c1 = strb_01.sgetc();
  c2 = strb_02.sgetc();
  VERIFY( c6 == c1 ); //just by co-incidence both o's
  VERIFY( c7 != c2 ); // n != i
  VERIFY( c1 == traits_type::to_int_type(str_01[13]) );
  VERIFY( c2 == traits_type::to_int_type(str_02[13]) ); //should equal fourteenth letter at this point
  strmsz_1 = strb_03.sgetn(carray1, 10);
  VERIFY( !strmsz_1 ); //zero
  strmsz_1 = strb_02.in_avail();
  strmsz_2 = strb_02.sgetn(carray2, strmsz_1 + 5);
  VERIFY( strmsz_1 == strmsz_2 ); //write off the end
  c4 = strb_02.sgetc(); // should be EOF
  VERIFY( c4 == traits_type::eof() );
}

int main()
{
  test04();
  return 0;
}



// more candy!!!
