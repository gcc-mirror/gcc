// 981208 bkoz test functionality of basic_stringbuf for char_type == wchar_t

// Copyright (C) 1997-2017 Free Software Foundation, Inc.
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
std::wstring str_03;
std::wstringbuf strb_01(str_01);
std::wstringbuf strb_03(str_03, std::ios_base::out);

// test overloaded virtual functions
void test04() 
{
  std::wstring 		str_tmp;

  // PUT
  strb_03.str(str_01); //reset
  
  // BUFFER MANAGEMENT & POSITIONING
  // setbuf
  // pubsetbuf(char_type* s, streamsize n)
  str_tmp = std::wstring(L"naaaah, go to cebu");
  strb_01.pubsetbuf(const_cast<wchar_t*> (str_tmp.c_str()), str_tmp.size());
  VERIFY( strb_01.str() == str_tmp );
  strb_01.pubsetbuf(0,0);
  VERIFY( strb_01.str() == str_tmp );
}

int main()
{
  test04();
  return 0;
}



// more candy!!!
