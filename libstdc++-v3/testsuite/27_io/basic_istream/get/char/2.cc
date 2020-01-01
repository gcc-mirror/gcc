// 1999-08-11 bkoz

// Copyright (C) 1999-2020 Free Software Foundation, Inc.
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
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

// { dg-require-fileio "" }

#include <istream>
#include <sstream>
#include <fstream>
#include <testsuite_hooks.h>

//libstdc++/92: Bug in istream::get(basic_streambuf*)
// bug reported by bgarcia@laurelnetworks.com
// http://gcc.gnu.org/ml/libstdc++-prs/2000-q3/msg00041.html
void
test07()
{
  const char* tfn = "istream_unformatted-1.txt";
  std::ifstream infile;
  infile.open(tfn);
  VERIFY( !(!infile) );
  while (infile)
    {
      std::string line;
      std::ostringstream line_ss;
      while (infile.peek() == '\n')
	infile.get();
      infile.get(*(line_ss.rdbuf()));
      line = line_ss.str();
      VERIFY( line == "1234567890" || line == "" );
    }
}

int 
main()
{
  test07();
  return 0;
}
