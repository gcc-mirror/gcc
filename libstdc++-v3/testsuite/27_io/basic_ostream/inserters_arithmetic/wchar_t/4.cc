// Copyright (C) 2005-2020 Free Software Foundation, Inc.
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

#include <iomanip>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/3655
int
test04()
{
  using namespace std;

  wstringbuf strbuf1, strbuf2;
  wostream o1(&strbuf1), o2(&strbuf2);

  o1 << hex << showbase << setw(6) << internal << 0xff;
  VERIFY( strbuf1.str() == L"0x  ff" );
  
  // ... vs internal-adjusted const char*-type objects
  o2 << hex << showbase << setw(6) << internal << L"0xff";
  VERIFY( strbuf2.str() == L"  0xff" );

  return 0;
}

int 
main()
{
  test04();
  return 0;
}
