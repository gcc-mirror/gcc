// 1999-08-16 bkoz
// 1999-11-01 bkoz

// Copyright (C) 1999 Free Software Foundation
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

// 27.6.2.5.4 basic_ostream character inserters

#include <ostream>
#include <sstream>
#include <fstream>
#ifdef DEBUG_ASSERT
  #include <assert.h>
#endif

const int size = 1000;
const char name_01[] = "testsuite/ostream_inserter_other-1.tst";
const char name_02[] = "testsuite/ostream_inserter_other-1.txt";
const char name_03[] = "testsuite/ostream_inserter_other-2.tst";
const char name_04[] = "testsuite/ostream_inserter_other-2.txt";


// stringstream
bool test01() {
  bool test = true;
#ifdef DEBUG_ASSERT
  assert(test);
#endif
  return test;
}

// fstream
bool test02() {
  typedef std::ios_base::iostate iostate;
  bool test = true;

  // basic_ostream<_CharT, _Traits>::operator<<(__streambuf_type* __sb)
  // filebuf-> NULL 
  std::ifstream f_in1(name_01);
  std::ofstream f_out1(name_02);
  std::stringbuf* strbuf01 = NULL;
  iostate state01 = f_in1.rdstate();
  f_in1 >> strbuf01;
  iostate state02 = f_in1.rdstate();
  test &= state01 != state02;
  test &= (state02 & std::ios_base::failbit) != 0;
  state01 = f_out1.rdstate();
  f_out1 << strbuf01;
  state02 = f_out1.rdstate();
  test &= state01 != state02;
  test &= (state02 & std::ios_base::failbit) != 0;

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

  // no characters inserted

#ifdef DEBUG_ASSERT
  assert(test);
#endif
 
  return test;
}

int main()
{
  test01();
  test02();

  return 0;
}

