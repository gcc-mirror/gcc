// 981208 bkoz test functionality of basic_streambuf for char_type == wchar_t

// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2009
// Free Software Foundation, Inc.
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

#include <streambuf>
#include <locale>
#include <testsuite_hooks.h>

class testbuf : public std::wstreambuf
{
public:
  typedef std::wstreambuf::traits_type traits_type;

  testbuf() : std::wstreambuf() { }
};

// test the streambuf locale settings
void test02() 
{
  testbuf buf;
  std::locale loc_c = std::locale::classic();
  loc_c = buf.getloc();
  buf.pubimbue(loc_c); //This should initialize _M_init to true
  std::locale loc_tmp = buf.getloc(); 
  VERIFY( loc_tmp == loc_c );
}

int main()
{
  test02();
  return 0;
}



// more candy!!!
