// 1999-04-12 bkoz

// Copyright (C) 1999-2023 Free Software Foundation, Inc.
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
  class test_numpunct1 : public numpunct<char>
  {
  protected:
    string
    do_grouping() const 
    { return string(1, '\003'); }
  };
} // namespace std

void test07()
{
  // manufactured locale, grouping is turned on
  unsigned int h4 = 0, h3 = 0, h2 = 0;
  float f1 = 0.0;
  const std::string s1("205,199 23,445.25 1,024,365 123,22,24");
  std::istringstream is(s1);
  is.imbue(std::locale(std::locale(), new std::test_numpunct1));  

  // Basic operation.
  is >> h4; 
  VERIFY( h4 == 205199 );
  VERIFY( is.good() );

  is.clear();
  is >> f1; 
  VERIFY( f1 == 23445.25 );
  VERIFY( is.good() );

  is.clear();
  is >> h3; 
  VERIFY( h3 == 1024365 );
  VERIFY( is.good() );

  is.clear();
  is >> h2; 
  VERIFY( h2 == 1232224 );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::failbit) );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::eofbit) );

  // Stress tests for explicit errors in grouping corner cases.  The
  // validity of these tests and results have been hammered out in
  // private email between bkoz and ncm between Jan 25 and Jan 27, 2000.
  // Thanks nate -- benjamin
  const std::string s2(",111 4,,4 0.25,345 5..25 156,, 1,000000 1000000 1234,567");
  h3 = h4 = h2 = 0;
  f1 = 0.0;
  const char c_control = '?';
  char c = c_control;
  is.clear();
  is.str(s2);

  is >> h4; 
  VERIFY( h4 == 0 );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::failbit) );
  is.clear();
  is >> c;
  VERIFY( c == ',' );
  VERIFY( is.good() );

  is.ignore(3);
  is >> f1; 
  VERIFY( f1 == 0.0 );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::failbit) );
  is.clear();
  is >> c;
  VERIFY( c == ',' );
  is >> c;
  VERIFY( c == '4' );
  VERIFY( is.good() );

  is >> f1; 
  VERIFY( f1 == 0.25 );
  VERIFY( is.good() );
  is >> c;
  VERIFY( c == ',' );
  is >> h2;
  VERIFY( h2 == 345 );
  VERIFY( is.good() );
  f1 = 0.0;
  h2 = 0;

  is >> f1; 
  VERIFY( f1 == 5.0 );
  VERIFY( is.good() );
  is >> f1; 
  VERIFY( f1 == .25 );
  VERIFY( is.good() );

  is >> h3; 
  VERIFY( h3 == 0 );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::failbit) );
  is.clear();
  is >> c;
  VERIFY( c == ',' ); // second one
  VERIFY( is.good() );

  is >> h2; 
  VERIFY( h2 == 1000000 );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::failbit) );
  h2 = 0;
  is.clear();

  is >> h2; 
  VERIFY( h2 == 1000000 );
  VERIFY( is.good() );
  h2 = 0;

  is >> h2; 
  VERIFY( h2 == 1234567 );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::failbit) );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::eofbit) );
  is.clear();
}

int main()
{
  test07();
  return 0;
}
