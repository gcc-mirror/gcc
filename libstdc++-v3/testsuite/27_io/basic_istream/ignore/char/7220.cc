// 1999-08-11 bkoz

// Copyright (C) 1999, 2000, 2001, 2002, 2003 Free Software Foundation
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 27.6.1.3 unformatted input functions

#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/70220
void
test10()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  typedef string string_type;
  typedef stringbuf stringbuf_type;
  typedef istream istream_type;

  streamsize n;
  string_type  input("abcdefg\n");
  stringbuf_type sbuf(input);
  istream_type  istr(&sbuf);
  
  istr.ignore(0);
  if (istr.gcount() != 0) 
    test = false;
  VERIFY( test );
  
  istr.ignore(0, 'b');
  if (istr.gcount() != 0) 
    test = false;
  VERIFY( test );
  
  istr.ignore();	// Advance to next position.
  istr.ignore(0, 'b');
  if ((n=istr.gcount()) != 0) 
    test = false;
  VERIFY( test );
  
  if (istr.peek() != 'b')
    test = false;
  VERIFY( test );
}

int 
main()
{
  test10();
  return 0;
}
