// 2000-06-29 bkoz

// Copyright (C) 2000-2019 Free Software Foundation, Inc.
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

// 27.6.2.4  basic_ostream seek members  [lib.ostream.seeks]
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

// { dg-require-fileio "" }

#include <ostream>
#include <istream>
#include <fstream>
#include <cstdlib>
#include <testsuite_hooks.h>

const char* s = " lootpack, peanut butter wolf, rob swift, madlib, quasimoto";
const int times = 10;

void write_rewind(std::iostream& stream)
{
  for (int j = 0; j < times; j++) 
    {
      std::streampos begin = stream.tellp();
      
      for (int i = 0; i < times; ++i)
	stream << j << '-' << i << s << '\n';
      
      stream.seekp(begin);
    }
  VERIFY( stream.good() );
}

void check_contents(std::iostream& stream)
{
  stream.clear();
  stream.seekg(0, std::ios::beg);
  int i = 0;
  int loop = times * times + 2;
  while (i < loop)
    {
      stream.ignore(80, '\n');
      if (stream.good())
	++i;
      else
	break;
    }
  VERIFY( i == times );
}

// fstream
// libstdc++/2346
void test02()
{	 
  std::fstream ofstrm;
  ofstrm.open("istream_seeks-3.txt", std::ios::out);
  if (!ofstrm)
    std::abort();
  write_rewind(ofstrm);
  ofstrm.close();

  std::fstream ifstrm;
  ifstrm.open("istream_seeks-3.txt", std::ios::in);
  check_contents(ifstrm);
  ifstrm.close();
}

int main()
{
  test02();
  return 0;
}
