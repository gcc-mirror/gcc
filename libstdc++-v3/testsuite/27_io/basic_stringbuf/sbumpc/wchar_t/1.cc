// 981208 bkoz test functionality of basic_stringbuf for char_type == wchar_t

// Copyright (C) 1997-2023 Free Software Foundation, Inc.
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
  typedef std::wstringbuf::int_type int_type;
  typedef std::wstringbuf::traits_type traits_type;

  // GET
  strb_01.in_avail();
  strb_02.in_avail();
  strb_03.in_avail(); 

  // int_type sbumpc()
  // if read_cur not avail, return uflow(), else return *read_cur & increment
  int_type c1 = strb_01.sbumpc();
  int_type c2 = strb_02.sbumpc();
  VERIFY( c1 != c2 );
  VERIFY( c1 == traits_type::to_int_type(str_01[0]) );
  VERIFY( c2 == traits_type::to_int_type(str_02[0]) ); //should equal first letter at this point
  int_type c3 = strb_01.sbumpc();
  int_type c4 = strb_02.sbumpc();
  VERIFY( c1 != c2 );
  VERIFY( c1 != c3 );
  VERIFY( c2 != c4 );
  int_type c5 = strb_03.sbumpc();
  VERIFY( c5 == traits_type::eof() );
}

int main()
{
  test04();
  return 0;
}



// more candy!!!
