// Copyright (C) 2004-2018 Free Software Foundation, Inc.
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

// libstdc++/70220
void
test10()
{
  using namespace std;
  typedef wstring string_type;
  typedef wstringbuf stringbuf_type;
  typedef wistream istream_type;

  streamsize n;
  string_type  input(L"abcdefg\n");
  stringbuf_type sbuf(input);
  istream_type  istr(&sbuf);
  
  istr.ignore(0);
  if (istr.gcount() != 0) 
    VERIFY( false );
  
  istr.ignore(0, L'b');
  if (istr.gcount() != 0) 
    VERIFY( false );
  
  istr.ignore();	// Advance to next position.
  istr.ignore(0, L'b');
  if ((n=istr.gcount()) != 0) 
    VERIFY( false );
  
  if (istr.peek() != L'b')
    VERIFY( false );
}

int 
main()
{
  test10();
  return 0;
}
