// 2000-06-29 bkoz

// Copyright (C) 2000, 2001, 2002, 2003 Free Software Foundation
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

// 27.6.1.3 unformatted input functions
// NB: ostream has a particular "seeks" category. Adopt this for istreams too.
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

#include <istream>
#include <sstream>
#include <fstream>
#include <testsuite_hooks.h>

const char* s = " lootpack, peanut butter wolf, rob swift, madlib, quasimoto";
const int times = 10;

void write_rewind(std::iostream& stream)
{
  for (int j = 0; j < times; j++) 
    {
      bool test = true;
      std::streampos begin = stream.tellg();
      
      for (int i = 0; i < times; ++i)
	stream << j << '-' << i << s << '\n';
      
      stream.seekg(begin);
      std::streampos end = stream.tellg(); 
      std::streampos badpos = std::streampos(std::streambuf::off_type(-1));
    }
}

void check_contents(std::iostream& stream)
{
  bool test = true;

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

// stringstream
// libstdc++/2346
void test03()
{	 
  std::stringstream sstrm;

  write_rewind(sstrm);
  check_contents(sstrm);
}

int main()
{
  test03();
  return 0;
}
