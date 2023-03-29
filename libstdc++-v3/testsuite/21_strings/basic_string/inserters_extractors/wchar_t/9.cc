// 1999-07-01 bkoz

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

// 21.3.7.9 inserters and extractors

// NB: This file is predicated on sstreams, istreams, and ostreams
// working, not to mention other major details like char_traits, and
// all of the string class.

#include <string>
#include <sstream>
#include <iomanip>
#include <testsuite_hooks.h>

// libstdc++/2830
void test09()
{
  std::wstring blanks(3, L'\0');
  std::wstring foo = L"peace";
  foo += blanks;
  foo += L"& love";
  
  std::wostringstream oss1;
  oss1 << foo;
  VERIFY( oss1.str() == foo );
  
  std::wostringstream oss2;
  oss2.width(20);
  oss2 << foo;
  VERIFY( oss2.str() != foo );
  VERIFY( oss2.str().size() == 20 );
}

int main()
{ 
  test09();
  return 0;
}
