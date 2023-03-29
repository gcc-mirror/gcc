// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

const char name_01[] = "seekpos.txt"; // file with data in it

void test05() 
{
  using namespace std;
  using namespace __gnu_test;

  typedef filebuf::int_type 	int_type;
  typedef filebuf::pos_type 	pos_type;
  typedef filebuf::off_type 	off_type;
  typedef filebuf::traits_type 	traits_type;

  int_type c1;
  int_type c2;
  int_type c3;

  pos_type pt_1(off_type(-1));
  pos_type pt_2(off_type(0));
  pos_type pt_3;
  off_type off_1 = 0;
  off_type off_2 = 0;

  // seekpos
  // pubseekpos(pos_type sp, ios_base::openmode)
  // alters the stream position to sp

  // in 
  {
    constraint_filebuf fb;
    fb.pubsetbuf(0, 0);
    fb.open(name_01, ios_base::in);
    VERIFY( fb.unbuffered() );

    // beg
    pt_1 = fb.pubseekoff(78, ios_base::beg);
    off_1 = off_type(pt_1);
    VERIFY( off_1 > 0 );
    c1 = fb.snextc(); //current in pointer +1
    VERIFY( c1 == 't' );

    // cur
    pt_3 = fb.pubseekoff(0, ios_base::cur);
    fb.pubseekpos(pt_3);
    c2 = fb.sputc('\n'); //test current out pointer
    pt_3 = fb.pubseekoff(0, ios_base::cur);
    fb.pubseekpos(pt_3);
    c3 = fb.sgetc();
    fb.pubsync(); //resets pointers
    pt_2 = fb.pubseekpos(pt_1);
    off_2 = off_type(pt_2);
    VERIFY( off_1 == off_2 );
    c3 = fb.snextc(); //current in pointer +1
    VERIFY( c2 == traits_type::eof() );
    VERIFY( c2 != c3 );

    // end
    pt_1 = fb.pubseekoff(0, ios_base::end);
    off_1 = off_type(pt_1);
    VERIFY( off_1 > off_2 );
    fb.sputn("\nof the wonderful things he does!!\nok", 37);
    fb.pubsync();
    VERIFY( fb.unbuffered() );
    fb.close();
    VERIFY( !fb.is_open() );
  }
}

int main() 
{
  test05();
  return 0;
}
