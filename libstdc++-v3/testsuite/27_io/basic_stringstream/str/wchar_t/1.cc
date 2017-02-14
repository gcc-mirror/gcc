// Copyright (C) 2004-2017 Free Software Foundation, Inc.
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

// 27.7.6 member functions (stringstream_members)

#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  std::wstringstream is01;
  const std::wstring str00; 
  const std::wstring str01 = L"123";
  std::wstring str02;
  const int i01 = 123;
  int a = 0, b = 0;

  std::ios_base::iostate state1, state2, stateeof;
  stateeof = std::ios_base::eofbit;

  // string str() const
  str02 = is01.str();
  VERIFY( str00 == str02 );

  // void str(const basic_string&)
  is01.str(str01);
  str02 = is01.str();
  VERIFY( str01 == str02 );
  state1 = is01.rdstate();
  is01 >> a;
  state2 = is01.rdstate();
  VERIFY( a == i01 );
  // 22.2.2.1.2 num_get virtual functions
  // p 13
  // in any case, if stage 2 processing was terminated by the test for
  // in == end then err != ios_base::eofbit is performed.
  VERIFY( state1 != state2 );
  VERIFY( state2 == stateeof ); 

  is01.str(str01);
  is01 >> b;
  VERIFY( b != a ); 
  // as is01.good() is false, istream::sentry blocks extraction.

  is01.clear();
  state1 = is01.rdstate();
  is01 >> b;
  state2 = is01.rdstate();
  VERIFY( b == a ); 
  VERIFY( state1 != state2 );
  VERIFY( state2 == stateeof ); 
}

int main()
{
  test01();
  return 0;
}
