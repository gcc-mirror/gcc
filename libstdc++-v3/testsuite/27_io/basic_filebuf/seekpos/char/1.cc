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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <testsuite_hooks.h>

// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %*.txt

// NB: This test assumes that _M_buf_size == 40, and not the usual
// buffer_size length of BUFSIZ (8192), so that overflow/underflow can be
// simulated a bit more readily.
// NRB (Nota Really Bene): setting it to 40 breaks the test, as intended.
const int buffer_size = 8192;
//const int buffer_size = 40;

const char name_01[] = "filebuf_virtuals-1.txt"; // file with data in it

class derived_filebuf: public std::filebuf
{
 public:
  void
  set_size(int_type __size) { _M_buf_size = __size; }
};

derived_filebuf fb_03; // in | out

// Initialize filebufs to be the same size regardless of platform.
void test03()
{
  fb_03.set_size(buffer_size);
}

// Test overloaded virtual functions.
void test05() 
{
  using namespace std;
  typedef filebuf::int_type 	int_type;
  typedef filebuf::traits_type 	traits_type;
  typedef filebuf::pos_type 	pos_type;
  typedef filebuf::off_type 	off_type;
  typedef size_t 			size_type;

  bool 					test = true;
  filebuf 				f_tmp;
  streamsize 			strmsz_1, strmsz_2;
  streamoff  			strmof_1, strmof_2;
  int 					i = 0, j = 0, k = 0;

  fb_03.open(name_01, ios_base::out | ios_base::in);

  int_type c1;
  int_type c2;
  int_type c3;

  // seekpos
  // pubseekpos(pos_type sp, ios_base::openmode)
  // alters the stream position to sp
  pos_type pt_1(off_type(-1));
  pos_type pt_2(off_type(0));
  off_type off_1 = 0;
  off_type off_2 = 0;
  //IN|OUT
  //beg
  pt_1 = fb_03.pubseekoff(78, ios_base::beg);
  off_1 = pt_1;
  VERIFY( off_1 > 0 );
  c1 = fb_03.snextc(); 		//current in pointer +1
  VERIFY( c1 == 't' );
  c2 = fb_03.sputc('\n');  	//test current out pointer
  c3 = fb_03.sgetc();
  fb_03.pubsync(); 		//resets pointers
  pt_2 = fb_03.pubseekpos(pt_1);
  off_2 = pt_2;
  VERIFY( off_1 == off_2 );
  c3 = fb_03.snextc(); 		//current in pointer +1
  VERIFY( c2 == c3 );
  pt_1 = fb_03.pubseekoff(0, ios_base::end);
  off_1 = pt_1;
  VERIFY( off_1 > off_2 );
  fb_03.sputn("\nof the wonderful things he does!!\nok", 37);
  fb_03.pubsync();

  // IN
  // OUT

  // VIRTUALS (indirectly tested)
  // underflow
  // if read position avail, returns *gptr()

  // pbackfail(int_type c)
  // put c back into input sequence

  // overflow
  // appends c to output seq

  // NB Have to close these suckers. . .
  // filebuf_type* close()
  fb_03.close();
  VERIFY( !fb_03.is_open() );
}

main() 
{
  test03();
  test05();
  return 0;
}
