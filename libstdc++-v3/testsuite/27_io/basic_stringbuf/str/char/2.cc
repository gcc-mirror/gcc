// 981208 bkoz test functionality of basic_stringbuf for char_type == char

// Copyright (C) 1997-2021 Free Software Foundation, Inc.
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

#include <sstream>
#include <testsuite_hooks.h>

// http://gcc.gnu.org/ml/libstdc++/2003-02/msg00269.html
// Growing and then seeking to ios_base::beg triggered a bug in str(),
// which didn't notice the grow.
void test07()
{
  std::stringbuf strb_01;
  strb_01.sputc('s');
  strb_01.pubseekoff(0, std::ios_base::beg);
  std::string tmp = strb_01.str();
  VERIFY( tmp == "s" );

  std::string str("strivi,");
  std::stringbuf strb_02(str);
  strb_02.pubseekoff(0, std::ios_base::end);
  strb_02.sputn(" no better!", 11);
  strb_02.pubseekoff(0, std::ios_base::beg);
  tmp = strb_02.str();
  VERIFY( tmp == "strivi, no better!" );
}

int main()
{
  test07();
  return 0;
}



// more candy!!!
