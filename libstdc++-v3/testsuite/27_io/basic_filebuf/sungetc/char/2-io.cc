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

#include <fstream>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %*.txt

const char name_01[] = "tmp_sungetc_2io.tst"; // empty file, need to create

void test01() 
{
  using namespace std;
  using namespace __gnu_test;

  typedef filebuf::int_type 	int_type;
  typedef filebuf::traits_type 	traits_type;
  typedef size_t 		size_type;

  bool test __attribute__((unused)) = true;
  streamsize 			strmsz_1, strmsz_2;
  int_type 			c1, c2, c3;

  // int_type sungetc()
  // if in_cur not avail, return pbackfail(), else decrement and
  // return to_int_type(*gptr())

  // in | out
  {
    constraint_filebuf fb_01;
    fb_01.pubsetbuf(0, 0);
    fb_01.open(name_01, ios_base::out | ios_base::in | ios_base::trunc);
    VERIFY( fb_01.unbuffered() );
    fb_01.sputc('u');
    fb_01.sputc('v');
    fb_01.pubseekoff(-1, std::ios_base::end);
    c3 = fb_01.sbumpc();
    strmsz_1 = fb_01.in_avail();
    c2 = fb_01.sungetc(); 
    strmsz_2 = fb_01.in_avail();
    VERIFY( c2 == 'v' ); //  VERIFY( c2 != traits_type::eof() );
    VERIFY( strmsz_1 + 1 == strmsz_2 );
    //test for _in_cur == _in_end
    fb_01.pubseekoff(0, std::ios_base::end);
    strmsz_1 = fb_01.in_avail(); // -1 cuz at the end
    c1 = fb_01.sgetc(); 
    c2 = fb_01.sungetc();
    strmsz_2 = fb_01.in_avail(); // 1
    c3 = fb_01.sgetc();
    VERIFY( c1 != c2 );
    VERIFY( strmsz_2 != strmsz_1 );
    VERIFY( strmsz_2 == 1 );
    VERIFY( fb_01.unbuffered() );
  }
}

int main() 
{
  test01();
  return 0;
}
