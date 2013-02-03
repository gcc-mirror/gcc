// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2013 Free Software Foundation, Inc.
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

const char name_01[] = "sgetc.txt"; // file with data in it
const char name_03[] = "tmp_sgetc_1io.tst"; // empty file, need to create

// Test overloaded virtual functions.
void test05() 
{
  using namespace std;
  using namespace __gnu_test;
  typedef std::filebuf::int_type 	int_type;
  typedef filebuf::traits_type 		traits_type;

  bool test __attribute__((unused)) = true;
  int_type 			c1, c2, c3;

  // int_type sgetc()
  // if read_cur not avail, return uflow(), else return *read_cur  

  // in | out 1
  {
    constraint_filebuf fb_03; // in | out
    fb_03.open(name_03, ios::out | ios::in | ios::trunc); 
    VERIFY( !fb_03.write_position() );
    VERIFY( !fb_03.read_position() );
    c1 = fb_03.sgetc();
    c2 = fb_03.sbumpc();
    VERIFY( c1 == traits_type::eof() );
    VERIFY( c1 == c2 );
    VERIFY( !fb_03.write_position() );
    VERIFY( !fb_03.read_position() );
  }

  // in | out 2
  {
    constraint_filebuf fb_01; // in 
    fb_01.open(name_01, ios::in | ios::out);
    VERIFY( !fb_01.write_position() );
    c1 = fb_01.sgetc();
    VERIFY( c1 == '/' );
    c2 = fb_01.sgetc();
    VERIFY( c1 == c2 );
    fb_01.sbumpc();
    c1 = fb_01.sbumpc();
    c2 = fb_01.sgetc();
    c3 = fb_01.sgetc();
    VERIFY( c1 == '/' );
    VERIFY( c2 == ' ' );
    VERIFY( c3 == ' ' );
    VERIFY( !fb_01.write_position() );
    VERIFY( fb_01.read_position() );
  }
}

int main() 
{
  test05();
  return 0;
}
