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
  std::streamoff  		strmof_1(-1), strmof_2;

  // GET
  // int in_avail()
  strmof_1 = strb_01.in_avail();
  strmof_2 = strb_02.in_avail();
  VERIFY( strmof_1 != strmof_2 );
  VERIFY( strmof_1 == static_cast<std::streamoff>(str_01.length()) );
  VERIFY( strmof_2 == static_cast<std::streamoff>(str_02.length()) );
  strmof_1 = strb_03.in_avail(); 
  // zero cuz write-only, or eof() to match basic_filebuf
  VERIFY( strmof_1 == -1 ); 
}

int main()
{
  test04();
  return 0;
}



// more candy!!!
