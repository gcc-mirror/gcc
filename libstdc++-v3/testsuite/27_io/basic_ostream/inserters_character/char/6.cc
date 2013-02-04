// 1999-08-16 bkoz

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

// 27.6.2.5.4 basic_ostream character inserters

#include <string>
#include <ostream>
#include <sstream>
#include <testsuite_hooks.h>

// ostringstream and positioning, multiple writes
// http://gcc.gnu.org/ml/libstdc++/2000-q1/msg00326.html
void test06()
{
  bool test __attribute__((unused)) = true;
  const char carray01[] = "mos def & talib kweli are black star";

  // normal
  std::ostringstream ostr1("mos def");
  VERIFY( ostr1.str() == "mos def" ); 
  ostr1 << " & talib kweli";  // should overwrite first part of buffer
  VERIFY( ostr1.str() == " & talib kweli" );
  ostr1 << " are black star";  // should append to string from above
  VERIFY( ostr1.str() != carray01 );
  VERIFY( ostr1.str() == " & talib kweli are black star" );

  // appending
  std::ostringstream ostr2("blackalicious", 
			   std::ios_base::out | std::ios_base::ate);
  VERIFY( ostr2.str() == "blackalicious" ); 
  ostr2 << " NIA ";  // should not overwrite first part of buffer
  VERIFY( ostr2.str() == "blackalicious NIA " );
  ostr2 << "4: deception (5:19)";  // should append to full string from above
  VERIFY( ostr2.str() == "blackalicious NIA 4: deception (5:19)" );
}

int main()
{
  test06();
  return 0;
}
