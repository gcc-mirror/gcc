// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2025 Free Software Foundation, Inc.
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

// C++98 27.8.1.4 Overridden virtual functions

// { dg-additional-files "filebuf_virtuals-1.txt" }

#include <fstream>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

const char name_01[] = "filebuf_virtuals-1.txt"; // file with data in it

void test05() 
{
  using namespace std;
  using namespace __gnu_test;

  typedef filebuf::int_type 	int_type;
  typedef filebuf::traits_type 	traits_type;

  int_type 			c3;

  // int_type sputc(char_type c)
  // if out_cur not avail, return overflow(traits_type::to_int_type(c)) 
  // else, stores c at out_cur,
  // increments out_cur, and returns c as int_type

  // in 
  {
    constraint_filebuf fb_01; 
    fb_01.pubsetbuf(0, 0);
    fb_01.open(name_01, ios_base::in);
    VERIFY( fb_01.unbuffered() );
    c3 = fb_01.sputc('a'); 
    VERIFY( c3 == traits_type::eof() );
    VERIFY( fb_01.unbuffered() );
   }
}

int main() 
{
  test05();
  return 0;
}
