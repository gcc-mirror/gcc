// 1999-08-16 bkoz
// 1999-11-01 bkoz

// Copyright (C) 1999-2023 Free Software Foundation, Inc.
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

// 27.6.2.5.4 basic_ostream character inserters
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

// { dg-require-fileio "" }

#include <ostream>
#include <sstream>
#include <fstream>
#include <testsuite_hooks.h>

const int size = 1000;
const char name_01[] = "ostream_inserter_other-1.tst";
const char name_02[] = "ostream_inserter_other-1.txt";
const char name_03[] = "ostream_inserter_other-2.tst";
const char name_04[] = "ostream_inserter_other-2.txt";

// fstream
void
test02() 
{
  typedef std::ios_base::iostate iostate;

  // basic_ostream<_CharT, _Traits>::operator<<(__streambuf_type* __sb)
  // filebuf-> NULL 
  std::ifstream f_in1(name_01);
  std::ofstream f_out1(name_02);
  std::stringbuf* strbuf01 = 0;
  iostate state01 = f_in1.rdstate();
  f_in1 >> strbuf01;
  iostate state02 = f_in1.rdstate();
  VERIFY( state01 != state02 );
  VERIFY( (state02 & std::ios_base::failbit) != 0 );
  state01 = f_out1.rdstate();
  f_out1 << strbuf01;
  state02 = f_out1.rdstate();
  VERIFY( state01 != state02 );
  VERIFY( (state02 & std::ios_base::badbit) != 0 );

  // filebuf->filebuf
  std::ifstream f_in(name_01);
  std::ofstream f_out(name_02);
  f_out << f_in.rdbuf();
  f_in.close();
  f_out.close();

  // filebuf->stringbuf->filebuf
  std::ifstream f_in2(name_03);
  std::ofstream f_out2(name_04); // should be different name
  std::stringbuf strbuf02;
  f_in2 >> &strbuf02;
  f_out2 << &strbuf02;
  f_in2.close();
  f_out2.close();
}

int 
main()
{
  test02();
  return 0;
}
