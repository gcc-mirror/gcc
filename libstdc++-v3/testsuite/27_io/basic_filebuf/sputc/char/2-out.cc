// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2016 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %*.txt

const char name_02[] = "tmp_sputc_2out.tst"; // empty file, need to create

void test05() 
{
  using namespace std;
  using namespace __gnu_test;

  typedef filebuf::int_type 	int_type;
  typedef filebuf::traits_type 	traits_type;

  int_type 			c1, c2;

  // int_type sputc(char_type c)
  // if out_cur not avail, return overflow(traits_type::to_int_type(c)) 
  // else, stores c at out_cur,
  // increments out_cur, and returns c as int_type

  // out
  {
    constraint_filebuf fb_02; 
    fb_02.pubsetbuf(0, 0);
    fb_02.open(name_02, ios_base::out | ios_base::trunc);
    VERIFY( fb_02.unbuffered() );
    VERIFY( !fb_02.read_position() );
    c1 = fb_02.sputc('a');
    VERIFY( c1 == 'a' ); 
    c2 = fb_02.sputc('c'); 
    VERIFY( c2 == 'c' );
    for (int i = 50; i <= 90; ++i) 
      c2 = fb_02.sputc(char(i));
    VERIFY( fb_02.unbuffered() );
    VERIFY( !fb_02.read_position() );
  }
}

int main() 
{
  test05();
  return 0;
}
