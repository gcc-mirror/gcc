// 1999-06-08 bkoz

// Copyright (C) 1999, 2000, 2002, 2003, 2009 Free Software Foundation, Inc.
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

// 23.3.5.1 bitset constructors

#include <string>
#include <bitset>
#include <stdexcept>
#include <testsuite_hooks.h>

// boundary condition:  a zero-sized set
// libstdc++/6282
bool test02(void)
{
  using std::char_traits;  using std::allocator;
  bool test __attribute__((unused)) = true;

  std::bitset<0>  z1;
  VERIFY( z1.any() == false );

  std::bitset<0>  z2(12345);
  VERIFY( z2.any() == false );

  std::bitset<0>  z3(std::string("10101010101"));
  VERIFY( z3.any() == false );

  try {
    z1.set(0);
    VERIFY( false );
  }
  catch(std::out_of_range& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  VERIFY( z1.to_ulong() == 0 );
  VERIFY( (z1.to_string<char,char_traits<char>,allocator<char> >().empty() ));
  return test;
}

int main()
{
  test02();
  return 0;
}
