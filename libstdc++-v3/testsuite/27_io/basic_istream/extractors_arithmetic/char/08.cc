// 1999-04-12 bkoz

// Copyright (C) 1999-2019 Free Software Foundation, Inc.
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

// 27.6.1.2.2 arithmetic extractors

#include <istream>
#include <sstream>
#include <locale>
#include <testsuite_hooks.h>

namespace std {
  class test_numpunct2 : public numpunct<char>
  {
  protected:
    string
    do_grouping() const 
    { return string("\002\003"); }
  };
} // namespace std

void test08()
{
  // manufactured locale, grouping is turned on
  unsigned int h4 = 0, h3 = 0, h2 = 0;
  const std::string s1("1,22 205,19 22,123,22");

  std::istringstream is(s1);
  is.imbue(std::locale(std::locale(), new std::test_numpunct2));  

  // Basic operation.
  is >> h4; 
  VERIFY( h4 == 122 );
  VERIFY( is.good() );

  is.clear();
  is >> h3; 
  VERIFY( h3 == 20519 );
  VERIFY( is.good() );

  is.clear();
  is >> h2; 
  VERIFY( h2 == 2212322 );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::eofbit) );
}

int main()
{
  test08();
  return 0;
}
