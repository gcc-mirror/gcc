// Copyright (C) 2004 Free Software Foundation
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

// 21.3.7.9 inserters and extractors

#include <istream>
#include <string>
#include <fstream>
#include <testsuite_hooks.h>

using namespace std;

wstring prepare(wstring::size_type len, unsigned nchunks)
{
  wstring ret;
  for (unsigned i = 0; i < nchunks; ++i)
    {
      for (wstring::size_type j = 0; j < len; ++j)
	ret.push_back(L'a' + rand() % 26);
      len *= 2;
      ret.push_back(L' ');
    }
  return ret;
}

void check(wistream& stream, const wstring& str, unsigned nchunks)
{
  bool test __attribute__((unused)) = true;

  wstring chunk;
  wstring::size_type index = 0, index_new = 0;
  unsigned n = 0;

  while (stream >> chunk)
    {
      index_new = str.find(L' ', index);
      VERIFY( !str.compare(index, index_new - index, chunk) );
      index = index_new + 1;
      ++n;
    }
  VERIFY( stream.eof() );
  VERIFY( n == nchunks );
}

// istream& operator>>(istream&, string&)
void test01()
{
  const char filename[] = "inserters_extractors-3.txt";

  const unsigned nchunks = 10;
  const wstring data = prepare(666, nchunks);

  wofstream ofstrm;
  ofstrm.open(filename);
  ofstrm.write(data.data(), data.size());
  ofstrm.close();

  wifstream ifstrm;
  ifstrm.open(filename);
  check(ifstrm, data, nchunks);
  ifstrm.close();
}

int main()
{
  test01();
  return 0;
}
