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

// 27.6.1.2.2 arithmetic extractors

#include <istream>
#include <sstream>
#include <locale>
#include <limits>
#include <testsuite_hooks.h>

// libstdc++/3720 part two
void test13()
{
  using namespace std;
  const wchar_t* l2 = L"1.2345678901234567890123456789012345678901234567890123456"
                      L"  "
                      L"1246.9";

  // 1 
  // used to core.
  double d;
  wistringstream iss1(l2);
  iss1 >> d;
  iss1 >> d;
  VERIFY ( d > 1246 && d < 1247 );

  // 2
  // quick test for failbit on maximum length extraction.
  int i;
  int max_digits = numeric_limits<int>::digits10 + 1;
  wstring digits;
  for (int j = 0; j < max_digits; ++j)
    digits += L'1';
  wistringstream iss2(digits);
  iss2 >> i;
  VERIFY( !iss2.fail() );

  digits += L'1';
  i = 0;
  iss2.str(digits);
  iss2.clear();
  iss2 >> i;
  VERIFY( i == numeric_limits<int>::max() );
  VERIFY( iss2.fail() );
}

int main()
{
  test13();
  return 0;
}
