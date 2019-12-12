// Copyright (C) 2010-2019 Free Software Foundation, Inc.
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

// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>
#include <testsuite_character.h>

const char name_01[] = "tmp_seekoff_45628.tst";

unsigned underflows, overflows;

class my_filebuf
: public std::basic_filebuf<__gnu_test::pod_uchar>
{
  virtual int_type
  underflow()
  {
    ++underflows;
    return std::basic_filebuf<__gnu_test::pod_uchar>::underflow();
  }
  virtual int_type
  overflow(int_type c)
  {
    ++overflows;
    return std::basic_filebuf<__gnu_test::pod_uchar>::overflow(c);
  }
};

// libstdc++/45628
void test01()
{
  using __gnu_test::pod_uchar;
  std::locale loc(std::locale::classic(),
	     new std::codecvt<my_filebuf::traits_type::char_type, char,
	     my_filebuf::traits_type::state_type>);

  my_filebuf::pos_type opos[3], ipos[3];
  my_filebuf q;
  q.pubimbue(loc);
   
  q.open(name_01, std::ios_base::in | std::ios_base::out 
	 | std::ios_base::trunc); 

  q.sputc(pod_uchar::from<char>('a'));
  opos[0] = q.pubseekoff(0, std::ios_base::cur);
  q.sputc(pod_uchar::from<char>('b'));
  opos[1] = q.pubseekoff(0, std::ios_base::cur);
  q.sputc(pod_uchar::from<char>('c'));
  opos[2] = q.pubseekoff(0, std::ios_base::cur);

  VERIFY( overflows <= 9 ); // pubseekoff calls overflow twice if converting.
  // NB: checking opos==ipos is not very rigorous as long as it flushes, since
  // all positions will be at initial state.
  q.pubseekoff(0, std::ios_base::beg);

  q.sbumpc();
  VERIFY( underflows == 1 );

  ipos[0] = q.pubseekoff(0, std::ios_base::cur);
  VERIFY( ipos[0] == opos[0] );
  q.sbumpc();
  ipos[1] = q.pubseekoff(0, std::ios_base::cur);
  VERIFY( ipos[1] == opos[1] );
  q.sbumpc();
  ipos[2] = q.pubseekoff(0, std::ios_base::cur);
  VERIFY( ipos[2] == opos[2] );

  VERIFY( underflows == 1 ); // pubseekoff never flushes get area
  
  // Bonus test: check automatic mode switches.
  q.sputc(pod_uchar::from<char>('d'));
  
  q.pubseekpos( ipos[1] );
  q.sputc(pod_uchar::from<char>('e'));
  
  VERIFY( my_filebuf::traits_type::eq(
			my_filebuf::traits_type::to_char_type(q.sgetc()),
			pod_uchar::from<char>('d')) );
}

int main()
{
  test01();
  return 0;
}
