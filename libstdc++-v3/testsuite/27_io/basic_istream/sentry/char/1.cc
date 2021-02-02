// 1999-10-14 bkoz

// Copyright (C) 1999-2021 Free Software Foundation, Inc.
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


// 27.6.1.1.2 class basic_istream::sentry

#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  const char* lit01 = "stereolab on the bolsinga tip";
  const std::string str01(lit01);

  std::stringbuf strbuf01;
  std::stringbuf strbuf02(str01);
  std::istream istr01(&strbuf01);
  std::istream istr02(&strbuf02);
  
  // test negatives
  std::istream::sentry sentry01(istr01);	
  VERIFY( bool(sentry01) == false ); 

  std::istream::sentry sentry02(istr01, true);
  VERIFY( bool(sentry02) == false ); 

  // positive tests
  std::istream::sentry sentry03(istr02);	
  VERIFY( bool(sentry03) == true ); 

  std::istream::sentry sentry04(istr02, true);
  VERIFY( bool(sentry04) == true ); 
}

int main() 
{
  test01();
  return 0;
}
