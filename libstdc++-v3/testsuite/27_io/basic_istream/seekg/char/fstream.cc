// 2000-06-29 bkoz

// Copyright (C) 2000-2014 Free Software Foundation, Inc.
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

// 27.6.1.3 unformatted input functions
// NB: ostream has a particular "seeks" category. Adopt this for istreams too.
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

// { dg-require-fileio "" }

#include <istream>
#include <sstream>
#include <fstream>
#include <testsuite_hooks.h>

// fstreams
void test04(void)
{
  typedef std::istream::off_type off_type;

  bool test __attribute__((unused)) = true;
  std::istream::pos_type pos01, pos02, pos03, pos04, pos05, pos06;
  std::ios_base::iostate state01, state02;
  const char str_lit01[] = "istream_seeks-1.txt";
  const char str_lit02[] = "istream_seeks-2.txt";
  std::ifstream if01(str_lit01, std::ios_base::in | std::ios_base::out);
  std::ifstream if02(str_lit01, std::ios_base::in);
  std::ifstream if03(str_lit02, std::ios_base::out | std::ios_base::trunc); 
  VERIFY( if01.good() );
  VERIFY( if02.good() );
  VERIFY( if03.good() );

  std::istream is01(if01.rdbuf());
  std::istream is02(if02.rdbuf());
  std::istream is03(if03.rdbuf());

  pos01 = is01.tellg();
  pos02 = is01.tellg();
  pos03 = is02.tellg();
  pos04 = is02.tellg();
  pos05 = is03.tellg();
  pos06 = is03.tellg();

  // istream& seekg(pos_type)
  // istream& seekg(off_type, ios_base::seekdir)

  // cur 
  // NB: see library issues list 136. It's the v-3 interp that seekg
  // only sets the input buffer, or else istreams with buffers that
  // have _M_mode == ios_base::out will fail to have consistency
  // between seekg and tellg.
  state01 = is01.rdstate();
  is01.seekg(10, std::ios_base::cur);
  state02 = is01.rdstate();
  pos01 = is01.tellg(); 
  VERIFY( pos01 == pos02 + off_type(10) ); 
  VERIFY( state01 == state02 );
  pos02 = is01.tellg(); 
  VERIFY( pos02 == pos01 ); 

  state01 = is02.rdstate();
  is02.seekg(10, std::ios_base::cur);
  state02 = is02.rdstate();
  pos03 = is02.tellg(); 
  VERIFY( pos03 == pos04 + off_type(10) ); 
  VERIFY( state01 == state02 );
  pos04 = is02.tellg(); 
  VERIFY( pos03 == pos04 ); 

  state01 = is03.rdstate();
  is03.seekg(10, std::ios_base::cur);
  state02 = is03.rdstate();
  pos05 = is03.tellg(); 
  VERIFY( pos05 == pos06 + off_type(10) ); 
  VERIFY( state01 == state02 );
  pos06 = is03.tellg(); 
  VERIFY( pos05 == pos06 ); 

  // beg
  state01 = is01.rdstate();
  is01.seekg(20, std::ios_base::beg);
  state02 = is01.rdstate();
  pos01 = is01.tellg(); 
  VERIFY( pos01 == pos02 + off_type(10) ); 
  VERIFY( state01 == state02 );
  pos02 = is01.tellg(); 
  VERIFY( pos02 == pos01 ); 

  state01 = is02.rdstate();
  is02.seekg(20, std::ios_base::beg);
  state02 = is02.rdstate();
  pos03 = is02.tellg(); 
  VERIFY( pos03 == pos04 + off_type(10) ); 
  VERIFY( state01 == state02 );
  pos04 = is02.tellg(); 
  VERIFY( pos03 == pos04 ); 

  state01 = is03.rdstate();
  is03.seekg(20, std::ios_base::beg);
  state02 = is03.rdstate();
  pos05 = is03.tellg(); 
  VERIFY( pos05 == pos06 + off_type(10) );
  VERIFY( state01 == state02 );
  pos06 = is03.tellg(); 
  VERIFY( pos05 == pos06 );
}

int main()
{
  test04();
  return 0;
}
