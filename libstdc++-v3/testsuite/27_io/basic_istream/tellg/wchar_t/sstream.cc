// Copyright (C) 2004-2017 Free Software Foundation, Inc.
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

#include <istream>
#include <sstream>
#include <fstream>
#include <testsuite_hooks.h>

// stringstreams
void test05(void)
{
  typedef std::wistream::off_type off_type;

  std::wistream::pos_type pos01, pos02, pos03, pos04, pos05, pos06;
  std::ios_base::iostate state01, state02;
  const char str_lit01[] = "wistream_seeks-1.tst";
  std::wifstream if01(str_lit01);
  std::wifstream if02(str_lit01);
  std::wifstream if03(str_lit01);
  VERIFY( if01.good() );
  VERIFY( if02.good() );
  VERIFY( if03.good() );

  std::wstringbuf strbuf01(std::ios_base::in | std::ios_base::out);
  if01 >> &strbuf01; 
  // initialize stringbufs that are ios_base::out
  std::wstringbuf strbuf03(strbuf01.str(), std::ios_base::out);
  // initialize stringbufs that are ios_base::in
  std::wstringbuf strbuf02(strbuf01.str(), std::ios_base::in);

  std::wistream is01(&strbuf01);
  std::wistream is02(&strbuf02);
  std::wistream is03(&strbuf03);

  // pos_type tellg()
  // in | out
  pos01 = is01.tellg();
  pos02 = is01.tellg();
  VERIFY( pos01 == pos02 );

  // in
  pos03 = is02.tellg();
  pos04 = is02.tellg();
  VERIFY( pos03 == pos04 );

  // out
  pos05 = is03.tellg();
  pos06 = is03.tellg();
  VERIFY( pos05 == pos06 );

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
}

int main()
{
  test05();
  return 0;
}
