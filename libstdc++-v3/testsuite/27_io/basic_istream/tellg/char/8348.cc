// 2000-06-29 bkoz

// Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2009, 2010
// Free Software Foundation
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
// NB: ostream has a particular "seeks" category. Adopt this for istreams too.

#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/8348
void test06(void)
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  string num1("555");

  // tellg
  {
    istringstream iss(num1);
    iss.tellg();
    int asNum = 0;
    iss >> asNum;
    VERIFY( test = iss.eof() );
    VERIFY( test = !iss.fail() );
    iss.clear();
    iss.tellg();
    VERIFY( test = !iss.fail() );
  }

  // seekg
  {
    istringstream iss(num1);
    iss.tellg();
    int asNum = 0;
    iss >> asNum;
    VERIFY( test = iss.eof() );
    VERIFY( test = !iss.fail() );
    iss.seekg(0, ios_base::beg);
    VERIFY( test = !iss.fail() );
  }

  // seekg
  {
    istringstream iss(num1);
    istream::pos_type pos1 = iss.tellg();
    int asNum = 0;
    iss >> asNum;
    VERIFY( test = iss.eof() );
    VERIFY( test = !iss.fail() );
    iss.seekg(pos1);
    VERIFY( test = !iss.fail() );
  }
}

int main()
{
  test06();
  return 0;
}
