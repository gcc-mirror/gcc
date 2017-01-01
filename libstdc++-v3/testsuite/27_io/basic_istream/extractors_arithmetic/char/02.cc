// 1999-04-12 bkoz

// Copyright (C) 1999-2017 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>
 
// elaborated test for ints
void test02()
{
  const std::string str_01("20000AB");
  std::stringbuf strb_01(str_01, std::ios_base::in);
  std::istream is(&strb_01);

  int n = 15;
  is >> n;
  VERIFY( n == 20000 );
  char c = is.peek();
  VERIFY( c == 65 );
}

int main()
{
  test02();
  return 0;
}
