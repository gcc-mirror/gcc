// 981208 bkoz test functionality of basic_stringbuf for char_type == char

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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <sstream>
#include <testsuite_hooks.h>

std::string str_01("mykonos. . . or what?");
std::stringbuf strb_01(str_01);

// test the streambuf/stringbuf locale settings
void test02() 
{
  bool test __attribute__((unused)) = true;

  std::locale loc_c = std::locale::classic();
  loc_c = strb_01.getloc();
  strb_01.pubimbue(loc_c); //This should initialize _M_init to true
  std::locale loc_tmp = strb_01.getloc(); 
  VERIFY( loc_tmp == loc_c );
}

int main()
{
  test02();
  return 0;
}



// more candy!!!
