// 981208 bkoz test functionality of basic_stringbuf for char_type == wchar_t

// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004
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

std::wstring str_01(L"mykonos. . . or what?");
std::wstring str_02(L"paris, or sainte-maxime?");
std::wstring str_03;
std::wstringbuf strb_01(str_01);
std::wstringbuf strb_02(str_02, std::ios_base::in);
std::wstringbuf strb_03(str_03, std::ios_base::out);

// test overloaded virtual functions
void test04() 
{
  bool test __attribute__((unused)) = true;
  std::streamoff  		strmof_1(-1), strmof_2;
  typedef std::wstringbuf::int_type int_type;
  typedef std::wstringbuf::traits_type traits_type;

  // GET
  strmof_1 = strb_01.in_avail();
  strmof_2 = strb_02.in_avail();
  strmof_1 = strb_03.in_avail(); 

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
  VERIFY( c6 == str_01[3] );
  VERIFY( c7 == str_02[3] ); //should equal fourth letter at this point
  c5 = strb_03.snextc();
  VERIFY( c5 == traits_type::eof() );
}

int main()
{
  test04();
  return 0;
}



// more candy!!!
