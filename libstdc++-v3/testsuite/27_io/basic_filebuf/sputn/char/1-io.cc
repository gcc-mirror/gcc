// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %*.txt

const char name_01[] = "filebuf_virtuals-1.txt"; // file with data in it
const char name_03[] = "tmp_sputn_1io.tst"; // empty file, need to create

void test05() 
{
  using namespace std;
  using namespace __gnu_test;

  typedef filebuf::int_type 	int_type;
  typedef filebuf::traits_type 	traits_type;
  typedef size_t 			size_type;

  bool test __attribute__((unused)) = true;
  streamsize 			strmsz_1, strmsz_2;

  // streamsize sputn(const char_typs* s, streamsize n)
  // write up to n chars to out_cur from s, returning number assigned
  // NB *sputn will happily put '\0' into your stream if you give it a chance*

  // in | out
  {
    constraint_filebuf fb_03; 
    fb_03.open(name_03, ios_base::out | ios_base::in | ios_base::trunc);
    VERIFY( !fb_03.write_position() );
    VERIFY( !fb_03.read_position() );
    strmsz_1 = fb_03.sputn("racadabras", 10);//"abracadabras or what?"
    VERIFY( strmsz_1 == 10 );
    strmsz_2 = fb_03.sputn(", i wanna reach out and", 10);
    VERIFY( strmsz_2 == 10 );
    VERIFY( strmsz_1 == strmsz_2 ); 
    VERIFY( fb_03.write_position() );
    VERIFY( !fb_03.read_position() );
  }
}

int main() 
{
  test05();
  return 0;
}
