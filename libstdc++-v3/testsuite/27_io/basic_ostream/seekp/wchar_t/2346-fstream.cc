// Copyright (C) 2005-2016 Free Software Foundation, Inc.
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

// 27.6.2.4  basic_ostream seek members  [lib.ostream.seeks]
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

#include <ostream>
#include <istream>
#include <fstream>
#include <cstdlib>
#include <testsuite_hooks.h>

const wchar_t* s = L" lootpack, peanut butter wolf, rob swift, madlib, quasimoto";
const int times = 10;

void write_rewind(std::wiostream& stream)
{
  for (int j = 0; j < times; j++) 
    {
      std::streampos begin = stream.tellp();
      
      for (int i = 0; i < times; ++i)
	stream << j << L'-' << i << s << L'\n';
      
      stream.seekp(begin);
    }
  VERIFY( stream.good() );
}

void check_contents(std::wiostream& stream)
{
  stream.clear();
  stream.seekg(0, std::wios::beg);
  int i = 0;
  int loop = times * times + 2;
  while (i < loop)
    {
      stream.ignore(80, L'\n');
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
  std::wfstream ofstrm;
  ofstrm.open("wistream_seeks-3.txt", std::wios::out);
  if (!ofstrm)
    std::abort();
  write_rewind(ofstrm);
  ofstrm.close();

  std::wfstream ifstrm;
  ifstrm.open("wistream_seeks-3.txt", std::wios::in);
  check_contents(ifstrm);
  ifstrm.close();
}

int main()
{
  test02();
  return 0;
}
