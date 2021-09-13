// Copyright (C) 2004-2021 Free Software Foundation, Inc.
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

// 21.3.7.9 inserters and extractors

// { dg-options "-DMAX_SIZE=505" { target simulator } }
// { dg-require-fileio "" }

#ifndef MAX_SIZE
#define MAX_SIZE 777
#endif

#include <istream>
#include <string>
#include <fstream>
#include <cstdlib>
#include <testsuite_hooks.h>

using namespace std;

wstring prepare(wstring::size_type len, unsigned nchunks, wchar_t delim)
{
  wstring ret;
  for (unsigned i = 0; i < nchunks; ++i)
    {
      for (wstring::size_type j = 0; j < len; ++j)
	ret.push_back(L'a' + rand() % 26);
      len *= 2;
      ret.push_back(delim);
    }
  return ret;
}

void check(wistream& stream, const wstring& str, unsigned nchunks, wchar_t delim)
{
  wstring chunk;
  wstring::size_type index = 0, index_new = 0;
  unsigned n = 0;

  while (getline(stream, chunk, delim))
    {
      index_new = str.find(delim, index);
      VERIFY( !str.compare(index, index_new - index, chunk) );
      index = index_new + 1;
      ++n;
    }
  VERIFY( stream.eof() );
  VERIFY( n == nchunks );
}

// istream& getline(istream&, string&, char)
void test01()
{
  const char filename[] = "inserters_extractors-2.txt";

  const wchar_t delim = L'|';
  const unsigned nchunks = 10;
  const wstring data = prepare(MAX_SIZE, nchunks, delim);

  wofstream ofstrm;
  ofstrm.open(filename);
  ofstrm.write(data.data(), data.size());
  ofstrm.close();

  wifstream ifstrm;
  ifstrm.open(filename);
  check(ifstrm, data, nchunks, delim);
  ifstrm.close();
}

int main()
{
  test01();
  return 0;
}
