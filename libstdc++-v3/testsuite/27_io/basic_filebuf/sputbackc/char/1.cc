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
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <locale>
#include <testsuite_hooks.h>

// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %*.txt

// NB: This test assumes that _M_buf_size == 40, and not the usual
// buffer_size length of BUFSIZ (8192), so that overflow/underflow can be
// simulated a bit more readily.
// NRB (Nota Really Bene): setting it to 40 breaks the test, as intended.
const int buffer_size = 8192;
//const int buffer_size = 40;

const char carray_01[] = "santa cruz or sandiego?";
const char carray_02[] = "memphis, new orleans, and savanah";
const char name_01[] = "filebuf_virtuals-1.txt"; // file with data in it
const char name_02[] = "filebuf_virtuals-2.txt"; // empty file, need to create
const char name_03[] = "filebuf_virtuals-3.txt"; // empty file, need to create
const char name_04[] = "filebuf_virtuals-4.txt"; // empty file, need to create
const char name_05[] = "filebuf_virtuals-5.txt"; // empty file, need to create
const char name_06[] = "filebuf_virtuals-6.txt"; // empty file, need to create
const char name_07[] = "filebuf_virtuals-7.txt"; // empty file, need to create
const char name_08[] = "filebuf_virtuals-8.txt"; // empty file, need to create

class derived_filebuf: public std::filebuf
{
 public:
  void
  set_size(int_type __size) { _M_buf_size_opt = __size; }
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
  typedef std::filebuf::int_type 	int_type;
  typedef std::filebuf::traits_type 	traits_type;
  typedef std::filebuf::pos_type 	pos_type;
  typedef std::filebuf::off_type 	off_type;
  typedef size_t 			size_type;

  bool 					test = true;
  std::filebuf 				f_tmp;
  std::streamsize 			strmsz_1, strmsz_2;
  std::streamoff  			strmof_1, strmof_2;
  int 					i = 0, j = 0, k = 0;

  fb_01.open(name_01, std::ios_base::in);
  fb_02.open(name_02, std::ios_base::out | std::ios_base::trunc);
  fb_03.open(name_03, std::ios_base::out | std::ios_base::in | std::ios_base::trunc);

  int_type c1 = fb_01.sbumpc();
  int_type c2 = fb_02.sbumpc();
  int_type c3 = fb_01.sbumpc();
  int_type c4 = fb_02.sbumpc();
  int_type c5 = fb_03.sbumpc();
  int_type c6 = fb_01.sgetc();
  int_type c7 = fb_02.sgetc();
  int_type c8 = fb_01.sgetc();
  int_type c9 = fb_02.sgetc();

  // PUT
  strmsz_1 = fb_03.sputn("racadabras", 10);//"abracadabras or what?"
  strmsz_2 = fb_03.sputn(", i wanna reach out and", 10);
  strmsz_1 = fb_02.sputn("racadabras", 10);
  strmsz_1 = fb_01.sputn("racadabra", 10);

  // PUTBACK
  // int_type pbfail(int_type c)
  // called when gptr() null, gptr() == eback(), or traits::eq(*gptr, c) false
  // "pending sequence" is:
  //	1) everything as defined in underflow
  // 	2) + if (traits::eq_int_type(c, traits::eof()), then input
  // 	sequence is backed up one char before the pending sequence is
  // 	determined.
  //	3) + if (not 2) then c is prepended. Left unspecified is
  //	whether the input sequence is backedup or modified in any way
  // returns traits::eof() for failure, unspecified other value for success

  // int_type sputbackc(char_type c)
  // if in_cur not avail || ! traits::eq(c, gptr() [-1]), return pbfail
  // otherwise decrements in_cur and returns *gptr()
  c1 = fb_03.sgetc(); // -1
  c2 = fb_03.sputbackc('z');
  strmsz_2 = fb_03.in_avail();
  c3 = fb_03.sgetc();
  VERIFY( c3 == c2 );
  VERIFY( c1 != c3 );
  VERIFY( 1 == strmsz_2 );
  //test for _in_cur == _in_beg
  // fb_03._M_out_beg = "bd23456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZracada" etc
  fb_03.pubseekoff(10, std::ios_base::beg, 
		   std::ios_base::in | std::ios_base::out);
  fb_03.sputc('m');
  strmsz_1 = fb_03.in_avail(); 
  c1 = fb_03.sgetc(); 
  fb_03.snextc();
  c2 = fb_03.sputbackc('z');  
  strmsz_2 = fb_03.in_avail(); 
  c3 = fb_03.sgetc();  
  VERIFY( c1 != c2 );
  VERIFY( c3 == c2 );
  VERIFY( c1 != c3 );
  VERIFY( c2 == 'z' );
  //  VERIFY( strmsz_1 == strmsz_2 );
  // test for replacing char with identical one
  fb_03.snextc();
  fb_03.sputc('u');
  fb_03.sputc('v');
  fb_03.sputc('a');
  strmsz_1 = fb_03.in_avail();
  c2 = fb_03.sputbackc('a');
  strmsz_2 = fb_03.in_avail();
  c3 = fb_03.sgetc();
  VERIFY( c3 == c2 );
  VERIFY( strmsz_1 + 1 == strmsz_2 );
  //test for ios_base::out
  c1 = fb_02.sgetc(); // undefined
  c2 = fb_02.sputbackc('a');
  VERIFY( c1 == c2 );
  VERIFY( c1 == -1 );
}

main() 
{
  test03();
  test05();
  return 0;
}
