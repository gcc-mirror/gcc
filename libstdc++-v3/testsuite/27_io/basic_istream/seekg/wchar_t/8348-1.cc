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
  wstring num1(L"555");

  // seekg
  {
    wistringstream iss(num1);
    wistream::pos_type pos1 = iss.tellg();
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
