// 981208 bkoz test functionality of basic_stringbuf for char_type == char

// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2005
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <sstream>
#include <testsuite_hooks.h>

std::string str_01("mykonos. . . or what?");
std::string str_02("paris, or sainte-maxime?");
std::string str_03;
std::stringbuf strb_01(str_01);
std::stringbuf strb_02(str_02, std::ios_base::in);
std::stringbuf strb_03(str_03, std::ios_base::out);

// test member functions
void test03() 
{
  bool test __attribute__((unused)) = true;

  //stringbuf::str()
  VERIFY( strb_01.str() == str_01 );
  VERIFY( strb_02.str() == str_02 );
  VERIFY( strb_03.str() == str_03 );
 
  //stringbuf::str(string&)
  strb_03.str("none of the above, go to the oberoi in cairo, egypt.");
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
  std::string str_nulls ("eschew \0 obfuscation", 20);  // tested in 21_strings
  std::stringbuf strb_normal (str_01);
  std::stringbuf strb_nulls (str_nulls);
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
