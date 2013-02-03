// 2001-05-23 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2013 Free Software Foundation, Inc.
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

// 27.7.3.2 member functions (ostringstream_members)

#include <sstream>
#include <testsuite_hooks.h>

// 03: sanity checks for strings, stringbufs
void
test03()
{
  bool test __attribute__((unused)) = false;

  // Empty string sanity check.
  std::string str01;
  std::string::iterator __i_start = str01.begin();
  std::string::iterator __i_end = str01.end();
  std::string::size_type len = str01.size();
  test = __i_start == __i_end;
  VERIFY( len == 0 );

  // Full string sanity check.
  std::string str02("these golden days, i spend waiting for you:\n"
		    "Betty Carter on Verve with I'm Yours and You're Mine.");
  __i_start = str02.begin();
  __i_end = str02.end();
  len = str02.size();
  VERIFY( __i_start != __i_end );
  VERIFY( len != 0 );
 
  // Test an empty ostringstream for sanity.
  std::ostringstream ostrstream0;
  std::string str03 = ostrstream0.str();
  __i_start = str03.begin();
  __i_end = str03.end();
  len = str03.size();
  VERIFY( __i_start == __i_end );
  VERIFY( len == 0 );
  VERIFY( str01 == str03 );
}

int main()
{
  test03();
  return 0;
}
