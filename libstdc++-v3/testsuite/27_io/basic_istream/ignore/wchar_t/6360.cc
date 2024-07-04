// Copyright (C) 2004-2024 Free Software Foundation, Inc.
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

#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

// 2002-04-19 PR libstdc++ 6360
void
test08()
{
  using namespace std;

  wstringstream ss(L"abcd" L"\xFF" L"1234ina donna coolbrith");  
  wchar_t c;
  ss >> c;
  VERIFY( c == L'a' );
  ss.ignore(8);
  ss >> c;
  VERIFY( c == L'i' );
}

int 
main()
{
  test08();
  return 0;
}
