// 1999-07-01 bkoz

// Copyright (C) 1999, 2000, 2001, 2002, 2003, 2009
// Free Software Foundation, Inc.
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

// NB: This file is predicated on sstreams, istreams, and ostreams
// working, not to mention other major details like char_traits, and
// all of the string class.

#include <string>
#include <sstream>
#include <testsuite_hooks.h>

// http://gcc.gnu.org/ml/libstdc++/2000-q1/msg00085.html
// istream::operator>>(string)
// sets failbit
// NB: this is a defect in the standard.
void test07(void)
{
  bool test __attribute__((unused)) = true;
  const std::string name("z6.cc");
  std::istringstream iss (name);
  int i = 0;
  std::string s;
  while (iss >> s) 
    ++i;

  VERIFY( i < 3 );
  VERIFY( static_cast<bool>(iss.rdstate() & std::ios_base::failbit) );
}

int main()
{ 
  test07();
  return 0;
}
