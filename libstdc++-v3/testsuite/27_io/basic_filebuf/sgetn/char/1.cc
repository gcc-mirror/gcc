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
const char name_02[] = "filebuf_virtuals-2.txt"; // empty file, need to create
const char name_03[] = "filebuf_virtuals-3.txt"; // empty file, need to create

class derived_filebuf: public std::filebuf
{
 public:
  void
  set_size(int_type __size) { _M_buf_size = __size; }
};

derived_filebuf fb_01; // in 
derived_filebuf fb_02; // out
derived_filebuf fb_03; // in | out

// Initialize filebufs to be the same size regardless of platform.
void test03()
{
  fb_01.set_size(buffer_size);
  fb_02.set_size(buffer_size);
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

  // GET
  fb_01.open(name_01, ios_base::in);
  fb_02.open(name_02, ios_base::out | ios_base::trunc);
  fb_03.open(name_03, ios_base::out | ios_base::in | ios_base::trunc); 
  strmof_1 = fb_01.in_avail();
  strmof_2 = fb_02.in_avail();
  strmof_1 = fb_03.in_avail(); 

  int_type c1 = fb_01.sbumpc();
  int_type c2 = fb_02.sbumpc();
  int_type c3 = fb_01.sbumpc();
  int_type c4 = fb_02.sbumpc();
  int_type c5 = fb_03.sbumpc();

  int_type c6 = fb_01.sgetc();
  int_type c7 = fb_02.sgetc();
  int_type c8 = fb_01.sgetc();
  int_type c9 = fb_02.sgetc();
  c5 = fb_03.sgetc();

  // int_type snextc()
  // calls sbumpc and if sbumpc != eof, return sgetc
  c6 = fb_01.snextc();
  c7 = fb_02.snextc();
  c6 = fb_01.snextc();
  c7 = fb_02.snextc();
  c5 = fb_03.snextc();

  // streamsize sgetn(char_type *s, streamsize n)
  // streamsize xsgetn(char_type *s, streamsize n)
  // assign up to n chars to s from input sequence, indexing in_cur as
  // approp and returning the number of chars assigned
  strmsz_1 = fb_01.in_avail();
  strmsz_2 = fb_02.in_avail();
  test = strmsz_1 != strmsz_2;
  char carray1[13] = "";
  strmsz_1 = fb_01.sgetn(carray1, 10);
  char carray2[buffer_size] = "";
  strmsz_2 = fb_02.sgetn(carray2, 10);
  VERIFY( strmsz_1 != strmsz_2 );
  VERIFY( strmsz_1 == 10 );
  VERIFY( strmsz_2 == 0 );
  c1 = fb_01.sgetc();
  c2 = fb_02.sgetc();
  VERIFY( c1 == '\n' );  
  VERIFY( c7 == c2 ); // n != i
  strmsz_1 = fb_03.sgetn(carray1, 10);
  VERIFY( !strmsz_1 ); //zero
  strmsz_1 = fb_01.in_avail();
  strmsz_2 = fb_01.sgetn(carray2, strmsz_1 + 5);
  VERIFY( strmsz_1 == strmsz_2 - 5 ); 
  c4 = fb_01.sgetc(); // buffer should have underflowed from above.
  VERIFY( c4 == 'i' );
  strmsz_1 = fb_01.in_avail();
  VERIFY( strmsz_1 > 0 );
  strmsz_2 = fb_01.sgetn(carray2, strmsz_1 + 5);
  VERIFY( strmsz_1 == strmsz_2 ); //at the end of the actual file 
  strmsz_1 = fb_02.in_avail();
  strmsz_2 = fb_02.sgetn(carray2, strmsz_1 + 5);
  VERIFY( strmsz_1 == -1 );
  VERIFY( strmsz_2 == 0 );
  c4 = fb_02.sgetc(); // should be EOF
  VERIFY( c4 == traits_type::eof() );
}

main() 
{
  test03();
  test05();
  return 0;
}
