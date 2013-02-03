// 1999-07-28 bkoz

// Copyright (C) 1999-2013 Free Software Foundation, Inc.
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

#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

void test03() 
{
  using namespace std;  
  bool test __attribute__((unused)) = true;

  // template<_CharT, _Traits>
  //  basic_istream& operator>>(ios_base& (*pf) (ios_base&))
  {
    int i = 0;
    std::istringstream iss(" 43");
    iss >> std::noskipws >> i;
    VERIFY ( !iss ); //should set failbit
  }

  // template<_CharT, _Traits>
  //  basic_istream& operator>>(basic_ios& (*pf) (basic_ios&))

  // template<_CharT, _Traits>
  //  basic_istream& operator>>(basic_istream& (*pf) (basic_istream&))
}

int main()
{
  test03();
  return 0;
}
