// 981208 bkoz test functionality of basic_stringbuf for char_type == wchar_t

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

std::wstring str_01(L"mykonos. . . or what?");
std::wstring str_02(L"paris, or sainte-maxime?");
std::wstring str_03;
std::wstringbuf strb_01(str_01);
std::wstringbuf strb_02(str_02, std::ios_base::in);
std::wstringbuf strb_03(str_03, std::ios_base::out);

// test member functions
void test03() 
{
  // stringbuf::str()
  VERIFY( strb_01.str() == str_01 );
  VERIFY( strb_02.str() == str_02 );
  VERIFY( strb_03.str() == str_03 );
 
  // stringbuf::str(string&)
  strb_03.str(L"none of the above, go to the oberoi in cairo, egypt.");
  strb_03.str(str_01);
  std::streamsize d1 = strb_01.in_avail();
  std::streamsize d2 = strb_03.in_avail();
  VERIFY( d1 ); // non-zero
  VERIFY( d2 == -1 ); // -1, cuz ios_base::out
  VERIFY( d1 != d2 ); //these should be the same
  VERIFY( static_cast<std::streamsize>(str_01.length()) == d1 );  
  VERIFY( strb_01.str() == strb_03.str() ); //ditto

  // stringbuf::str(string&) and stringbuf::stringbuf(string&), where the
  // string in question contains embedded NUL characters.  Note that in this
  // embedded-NUL situation, the size must be passed to the string ctor.
  std::wstring str_nulls(L"eschew \0 obfuscation", 20);  // tested in 21_strings
  std::wstringbuf strb_normal(str_01);
  std::wstringbuf strb_nulls(str_nulls);
  strb_normal.str(str_nulls);  // tried using 'strb_01' rather than declaring
                               // another variable, but then test04 broke!
  VERIFY( strb_nulls.in_avail() == static_cast<std::streamsize>(str_nulls.size())  );
  VERIFY( strb_nulls.str().size() == 20 );
  VERIFY( strb_normal.in_avail() == static_cast<std::streamsize>(str_nulls.size()) );
}

int main()
{
  test03();
  return 0;
}



// more candy!!!
