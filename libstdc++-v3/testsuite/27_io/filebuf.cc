// 990117 bkoz test functionality of basic_filebuf for char_type == char

// Copyright (C) 1997-1999, 2000 Free Software Foundation, Inc.
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

// NB: this test assumes that _M_buf_size == 40, and not the usual
// buffer_size length of 8092, so that overflow/underflow can be
// simulated a bit more readily.

#include <fstream>
#ifdef DEBUG_ASSERT
#include <assert.h>
#endif

const char carray_01[] = "santa cruz or sandiego?";
const char carray_02[] = "memphis, new orleans, and savanah";
const char name_01[] = "testsuite/filebuf-1.txt"; // file with data in it
const char name_02[] = "testsuite/filebuf-2.txt"; // empty file, need to create
const char name_03[] = "testsuite/filebuf-3.txt"; // empty file, need to create

class derived_filebuf: public std::filebuf
{
 public:
  void
  set_size(int_type __size) { _M_buf_size_opt = __size; }
};

derived_filebuf fb_01; // in 
derived_filebuf fb_02; // out
derived_filebuf fb_03; // in | out

const int buffer_size = 8192;

// initialize filebufs to be the same size regardless of platform
void test00()
{
  fb_01.set_size(buffer_size);
  fb_02.set_size(buffer_size);
  fb_03.set_size(buffer_size);
}

// test the filebuf/stringbuf locale settings
bool test01() {
  std::locale loc_tmp;
  loc_tmp = fb_01.getloc();
  fb_01.pubimbue(loc_tmp); //This should initialize _M_init to true
  fb_01.getloc(); //This should just return _M_locale

  return true;
}


// test member functions functions
bool test02() {
  bool 			test = true;

  // bool is_open()
  test &= !fb_01.is_open();
  test &= !fb_02.is_open();
  test &= !fb_03.is_open();

  // filebuf_type* open(const char* __s, ios_base::openmode __mode)
  fb_01.open(name_01, std::ios_base::in |  std::ios_base::ate);
  fb_02.open(name_02, std::ios_base::in | std::ios_base::out | std::ios_base::trunc);
  // Try to open two different files without closing the first:
  // Should keep the old file attached, and disregard attempt to overthrow.
  fb_02.open(name_03, std::ios_base::in | std::ios_base::out);
  fb_03.open(name_03, std::ios_base::out | std::ios_base::trunc);
  test &= fb_01.is_open();
  test &= fb_02.is_open();
  test &= fb_03.is_open();

  // filebuf_type* close()
  fb_01.close();
  fb_02.close();
  fb_03.close();
  test &= !fb_01.is_open();
  test &= !fb_02.is_open();
  test &= !fb_03.is_open();

#ifdef DEBUG_ASSERT
  assert(test);
#endif

  return test;
}


// test overloaded virtual functions
bool test03() {
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

  // GET
  // int showmanyc()
  // returns an estimate of the numbers of chars in the seq, or -1.
  // if __retval > 0, then calls to underflow won't return
  // traits_type::eof() till at least __retval chars. 
  // if __retval == -1, then calls to underflow or uflow will fail.
  // NB overriding def if it can determine more chars can be read from 
  // the input sequence.

  // int in_avail()
  // if a read position is available, return _M_in_end - _M_in_cur.
  // else return showmanyc.
  strmof_1 = fb_01.in_avail();
  strmof_2 = fb_02.in_avail();
  test &= strmof_1 == -1;
  test &= strmof_1 == strmof_2; //fail because not open
  strmof_1 = fb_03.in_avail();
  test &= strmof_1 == strmof_2;
  fb_01.open(name_01, std::ios_base::in);
  fb_02.open(name_02, std::ios_base::out | std::ios_base::trunc);
  fb_03.open(name_03, std::ios_base::out | std::ios_base::in | std::ios_base::trunc); 
  strmof_1 = fb_01.in_avail();
  strmof_2 = fb_02.in_avail();
  test &= strmof_1 != strmof_2;
  test &= strmof_1 >= 0;
  test &= strmof_2 == -1;  // empty file
  strmof_1 = fb_03.in_avail(); 
  test &= strmof_1  == -1; // empty file

  // int_type sbumpc()
  // if read_cur not avail returns uflow(), else return *read_cur & increment
  int_type c1 = fb_01.sbumpc();
  int_type c2 = fb_02.sbumpc();
  test &= c1 != c2;
  test &= c1 == '/';
  test &= c2 == -1;
  int_type c3 = fb_01.sbumpc();
  int_type c4 = fb_02.sbumpc();
  test &= c3 != c4;
  test &= c1 == c3; // fluke, both happen to be '/'
  test &= c2 == c4;
  int_type c5 = fb_03.sbumpc();
  test &= c5 == traits_type::eof();
  // XXX should do some kind of test to make sure that internal
  // buffers point ot the same thing, to check consistancy.

  // int_type sgetc()
  // if read_cur not avail, return uflow(), else return *read_cur  
  int_type c6 = fb_01.sgetc();
  int_type c7 = fb_02.sgetc();
  test &= c6 != c3;
  test &= c7 == c4; // both -1
  int_type c8 = fb_01.sgetc();
  int_type c9 = fb_02.sgetc();
  test &= c6 == c8;
  test &= c7 == c9;
  c5 = fb_03.sgetc();
  test &= c5 == traits_type::eof();

  // int_type snextc()
  // calls sbumpc and if sbumpc != eof, return sgetc
  c6 = fb_01.snextc();
  c7 = fb_02.snextc();
  test &= c6 != c8;
  test &= c7 == c9; // -1
  test &= c6 == '9';
  c6 = fb_01.snextc();
  c7 = fb_02.snextc();
  test &= c6 != c8;
  test &= c7 == c9; // -1
  test &= c6 == '9';
  c5 = fb_03.snextc();
  test &= c5 == traits_type::eof();

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
  test &= strmsz_1 != strmsz_2;
  test &= strmsz_1 == 10;
  test &= strmsz_2 == 0;
  c1 = fb_01.sgetc();
  c2 = fb_02.sgetc();
  test &= c1 == '\n';  
  test &= c7 == c2; // n != i
  strmsz_1 = fb_03.sgetn(carray1, 10);
  test &= !strmsz_1; //zero
  strmsz_1 = fb_01.in_avail();
  strmsz_2 = fb_01.sgetn(carray2, strmsz_1 + 5);
  test &= strmsz_1 == strmsz_2 - 5; 
  c4 = fb_01.sgetc(); // buffer should have underflowed from above.
  test &= c4 == 'i';
  strmsz_1 = fb_01.in_avail();
  test &= strmsz_1 > 0;
  strmsz_2 = fb_01.sgetn(carray2, strmsz_1 + 5);
  test &= strmsz_1 == strmsz_2; //at the end of the actual file 
  strmsz_1 = fb_02.in_avail();
  strmsz_2 = fb_02.sgetn(carray2, strmsz_1 + 5);
  test &= strmsz_1 == -1;
  test &= strmsz_2 == 0;
  c4 = fb_02.sgetc(); // should be EOF
  test &= c4 == traits_type::eof();

  // PUT
  // int_type sputc(char_type c)
  // if out_cur not avail, return overflow(traits_type::to_int_type(c)) 
  // else, stores c at out_cur,
  // increments out_cur, and returns c as int_type
  // strmsz_1 = fb_03.in_avail();  // XXX valid for in|out??
  c1 = fb_02.sputc('a'); 
  c2 = fb_03.sputc('b'); 
  test &= c1 != c2;
  c1 = fb_02.sputc('c'); 
  c2 = fb_03.sputc('d'); 
  test &= c1 != c2;
  // strmsz_2 = fb_03.in_avail();
  // test &= strmsz_1 != strmsz_2;
  for (int i = 50; i <= 90; ++i) 
    c2 = fb_02.sputc(char(i));
  // 27filebuf-2.txt == ac23456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWX
  // fb_02._M_out_cur = '2'
  strmsz_1 = fb_03.in_avail();
  for (int i = 50; i <= 90; ++i) 
    c2 = fb_03.sputc(char(i));
  strmsz_2 = fb_03.in_avail();
  // test &= strmsz_1 != strmsz_2;
  // test &= strmsz_1 > 0;
  // test &= strmsz_2 > 0;
  // 27filebuf-2.txt == bd23456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWX
  // fb_02._M_out_cur = '2'
  c3 = fb_01.sputc('a'); // should be EOF because this is read-only
  test &= c3 == traits_type::eof();

  // streamsize sputn(const char_typs* s, streamsize n)
  // write up to n chars to out_cur from s, returning number assigned
  // NB *sputn will happily put '\0' into your stream if you give it a chance*
  strmsz_1 = fb_03.sputn("racadabras", 10);//"abracadabras or what?"
  test &= strmsz_1 == 10;
  strmsz_2 = fb_03.sputn(", i wanna reach out and", 10);
  test &= strmsz_2 == 10;
  test &= strmsz_1 == strmsz_2; 
  // fb_03._M_out_beg = "YZracadabras, i wanna FGHIJKLMNOPQRSTUVW"
  // fb_03._M_out_cur = "FGHIJKLMNOPQRSTUVW"
  strmsz_1 = fb_02.sputn("racadabras", 10);
  test &= strmsz_1 == 10;  
  // fb_02._M_out_beg = "YZracadabras<=>?@ABCDEFGHIJKLMNOPQRSTUVW"
  // fb_02._M_out_cur = "<=>?@ABCDEFGHIJKLMNOPQRSTUVW"
  strmsz_1 = fb_01.sputn("racadabra", 10);
  test &= strmsz_1 == 0;  

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
  test &= c3 == c2;
  test &= c1 != c3;
  test &= 1 == strmsz_2;
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
  test &= c1 != c2;
  test &= c3 == c2;
  test &= c1 != c3;
  test &= c2 == 'z';
  test &= strmsz_1 == strmsz_2;
  // test for replacing char with identical one
  fb_03.snextc();
  fb_03.sputc('u');
  fb_03.sputc('v');
  fb_03.sputc('a');
  strmsz_1 = fb_03.in_avail();
  c2 = fb_03.sputbackc('a');
  strmsz_2 = fb_03.in_avail();
  c3 = fb_03.sgetc();
  test &= c3 == c2;
  test &= strmsz_1 + 1 == strmsz_2;
  //test for ios_base::out
  c1 = fb_02.sgetc(); // undefined
  c2 = fb_02.sputbackc('a');
  test &= c1 == c2;
  test &= c1 == -1;

  // int_type sungetc()
  // if in_cur not avail, return pbackfail(), else decrement and
  // return to_int_type(*gptr())
  // fb_03._M_out_beg = "uvaacadabras, i wannaZ[\\]^_`abcdefghijkl"
  // fb_03._M_out_cur = "aacadabras, i wannaZ[\\]^_`abcdefghijkl"
  strmsz_1 = fb_03.in_avail();
  c2 = fb_03.sungetc(); // delete the 'a'
  strmsz_2 = fb_03.in_avail();
  test &= c2 == 'v';
  test &= strmsz_1 + 1 == strmsz_2;
  //test for _in_cur == _in_beg
  for (int i = 50; i < 32 + 29; ++i)
    fb_02.sputc(char(i));
  fb_02.pubseekoff(0, std::ios_base::beg, std::ios_base::out);
  strmsz_1 = fb_02.in_avail();
  c1 = fb_02.sgetc(); 
  c2 = fb_02.sungetc();
  strmsz_2 = fb_02.in_avail();
  c3 = fb_02.sgetc();
  test &= c1 == c2;
  test &= c3 == c2;
  test &= c1 == c3;
  test &= c2 == traits_type::eof();
  test &= strmsz_1 == strmsz_2;
  //test for _in_cur == _in_end
  fb_03.pubseekoff(0, std::ios_base::end);
  strmsz_1 = fb_03.in_avail(); // -1 cuz at the end
  c1 = fb_03.sgetc(); 
  c2 = fb_03.sungetc();
  strmsz_2 = fb_03.in_avail(); // 1
  c3 = fb_03.sgetc();
  test &= c1 != c2;
  // test &= c2 == c3 || c2 == traits_type::not_eof(int(c3));
  test &= strmsz_2 != strmsz_1;
  test &= strmsz_2 == 1;
  //test for ios_base::out

  // BUFFER MANAGEMENT & POSITIONING
  // int sync()
  // if a put area exists, overflow. 
  // if a get area exists, do something undefined. (like, nothing)
  strmsz_1 = fb_01.in_avail(); 
  fb_01.pubsync();
  strmsz_2 = fb_01.in_avail();
  test &= strmsz_2 == strmsz_1;
  strmsz_1 = fb_02.in_avail(); 
  fb_02.pubsync();		
  // 27filebuf-2.txt == 53 bytes after this.
  strmsz_2 = fb_02.in_avail();
  test &= strmsz_2 == -1;
  test &= strmsz_2 == strmsz_1;
  strmsz_1 = fb_03.in_avail(); 
  fb_03.pubsync();
  // 27filebuf-3.txt 
  // bd23456789mzuva?@ABCDEFGHIJKLMNOPQRSTUVWXYZracadabras, i wannaz 
  // 63 bytes.
  strmsz_2 = fb_03.in_avail();
  test &= strmsz_1 == 1;
  test &= strmsz_2 == 1;

  // setbuf
  // pubsetbuf(char_type* s, streamsize n)
  fb_01.pubsetbuf(0,0);
  fb_02.pubsetbuf(0,0);
  fb_03.pubsetbuf(0,0);
  // Need to test unbuffered output, which means calling this on some
  // things that have just been opened.


  // seekoff
  // pubseekoff(off_type off, ios_base::seekdir way, ios_base::openmode which)
  // alters the stream position to off
  pos_type pt_1(off_type(-1));
  pos_type pt_2(off_type(0));
  off_type off_1 = 0;
  off_type off_2 = 0;
  //IN|OUT
  // 27filebuf-3.txt = bd23456789:;<=>?...
  //beg
  strmsz_1 = fb_03.in_avail(); 
  pt_1 = fb_03.pubseekoff(2, std::ios_base::beg);
  strmsz_2 = fb_03.in_avail(); 
  off_1 = pt_1._M_position();
  test &= off_1 > 0;
  c1 = fb_03.snextc(); //current in pointer +1
  test &= c1 == '3';
  c2 = fb_03.sputc('\n');  //current in pointer +1
  c3 = fb_03.sgetc();
  test &= c2 != c3; 
  test &= c3 == '4';
  fb_03.pubsync(); 
  c1 = fb_03.sgetc();
  test &= c1 == c3;
  //cur
  // 27filebuf-3.txt = bd2\n456789:;<=>?...
  pt_2 = fb_03.pubseekoff(2, std::ios_base::cur);
  off_2 = pt_2._M_position();
  test &= (off_2 == (off_1 + 2 + 1 + 1));
  c1 = fb_03.snextc(); //current in pointer +1
  test &= c1 == '7';
  c2 = fb_03.sputc('x');  //test current out pointer
  c3 = fb_03.sputc('\n');
  c1 = fb_03.sgetc();
  fb_03.pubsync(); 
  c3 = fb_03.sgetc();
  test &= c1 == c3;
  //end
  // 27filebuf-3.txt = "bd2\n456x\n9" 
  pt_2 = fb_03.pubseekoff(0, std::ios_base::end, 
			  std::ios_base::in|std::ios_base::out);
  off_1 = pt_2._M_position();
  test &= off_1 > off_2; //weak, but don't know exactly where it ends
  c3 = fb_03.sputc('\n');
  strmsz_1 = fb_03.sputn("because because because. . .", 28);  
  test &= strmsz_1 == 28;
  c1 = fb_03.sungetc();
  fb_03.pubsync(); 
  c3 = fb_03.sgetc();
  test &= c1 == c3;
  // IN
  // OUT


  // seekpos
  // pubseekpos(pos_type sp, ios_base::openmode)
  // alters the stream position to sp
  //IN|OUT
  //beg
  pt_1 = fb_03.pubseekoff(78, std::ios_base::beg);
  off_1 = pt_1._M_position();
  test &= off_1 > 0;
  c1 = fb_03.snextc(); 		//current in pointer +1
  test &= c1 == ' ';
  c2 = fb_03.sputc('\n');  	//test current out pointer
  c3 = fb_03.sgetc();
  fb_03.pubsync(); 		//resets pointers
  pt_2 = fb_03.pubseekpos(pt_1);
  off_2 = pt_2._M_position();
  test &= off_1 == off_2;
  c3 = fb_03.snextc(); 		//current in pointer +1
  test &= c2 == c3;
  pt_1 = fb_03.pubseekoff(0, std::ios_base::end);
  off_1 = pt_1._M_position();
  test &= off_1 > off_2;
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
  fb_01.close();
  fb_02.close();
  fb_03.close();
  test &= !fb_01.is_open();
  test &= !fb_02.is_open();
  test &= !fb_03.is_open();

#ifdef DEBUG_ASSERT
  assert(test);
#endif

  return test;
}

bool test04()
{
  using namespace std;
  typedef istream::int_type	int_type;

  bool test = true;
  ifstream ifs(name_02);
  char buffer[] = "xxxxxxxxxx";
  int_type len1 = ifs.rdbuf()->sgetn(buffer, sizeof(buffer));
  test &= len1 == sizeof(buffer);
  test &= buffer[0] == 'a';

#ifdef DEBUG_ASSERT
  assert(test);
#endif
  return test;
}

int main() 
{
  test00();
  test01();
  test02();
  test03();
  test04();

  return 0;
}



// more surf!!!










