// Copyright (C) 2000-2017 Free Software Foundation, Inc.
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

// 27.8.1.7 ifstream member functions
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

// { dg-require-fileio "" }

#include <istream>
#include <fstream>
#include <testsuite_hooks.h>

const char name_01[] = "ifstream_members-1.tst";

// http://gcc.gnu.org/ml/libstdc++/2000-06/msg00136.html
void test01()
{
  std::ifstream ifs1;
  ifs1.close();
  
  // false as expected:
  VERIFY( !ifs1.is_open() );
   // this is now true:
  VERIFY( !(ifs1) );
  
  ifs1.open(name_01);
  VERIFY( ifs1.is_open() );

  // As per the resolution of DR 409.
  VERIFY( (ifs1) );
  VERIFY( ifs1.rdstate() == std::ios_base::goodbit );

  ifs1.close();
}

int main()
{
  test01();
  return 0;
}



