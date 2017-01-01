// 2005-07-22  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2017 Free Software Foundation, Inc.
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

// 27.6.1.2.3 basic_istream::operator>>

// { dg-require-fileio "" }

#include <istream>
#include <string>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <testsuite_hooks.h>

using namespace std;

string prepare(string::size_type len, unsigned nchunks)
{
  string ret;
  for (unsigned i = 0; i < nchunks; ++i)
    {
      for (string::size_type j = 0; j < len; ++j)
	ret.push_back('a' + rand() % 26);
      len *= 2;
      ret.push_back(' ');
    }
  return ret;
}

void check(istream& stream, const string& str, unsigned nchunks)
{
  char* chunk = new char[str.size()];
  memset(chunk, 'X', str.size());

  string::size_type index = 0, index_new = 0;
  unsigned n = 0;

  while (stream >> chunk)
    {
      index_new = str.find(' ', index);
      VERIFY( !str.compare(index, index_new - index, chunk) );
      index = index_new + 1;
      ++n;
      memset(chunk, 'X', str.size());
    }
  VERIFY( stream.eof() );
  VERIFY( n == nchunks );

  delete[] chunk;
}

// istream& operator>>(istream&, charT*)
void test01()
{
  const char filename[] = "inserters_extractors-4.txt";

  const unsigned nchunks = 10;
  const string data = prepare(666, nchunks);

  ofstream ofstrm;
  ofstrm.open(filename);
  ofstrm.write(data.data(), data.size());
  ofstrm.close();

  ifstream ifstrm;
  ifstrm.open(filename);
  check(ifstrm, data, nchunks);
  ifstrm.close();
}

int main()
{
  test01();
  return 0;
}
