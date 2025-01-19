// Copyright (C) 2000-2025 Free Software Foundation, Inc.
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

// C++98 27.8.1.10 ofstream member functions

// { dg-require-fileio "" }
// { dg-additional-files "ofstream_members-1.tst" }
// { dg-final { file-io-diff "ofstream_members-1" } }

#include <ostream>
#include <fstream>
#include <testsuite_hooks.h>

const char name_01[] = "ofstream_members-1.tst";
const char name_02[] = "ofstream_members-1.txt";

// http://gcc.gnu.org/ml/libstdc++/2000-06/msg00136.html
void test01()
{
  std::ofstream ofs1;
  ofs1.close();
  
  // false as expected:
  VERIFY( !ofs1.is_open() );
   // this is now true:
  VERIFY( !(ofs1) );
  
  ofs1.open(name_02);
  VERIFY( ofs1.is_open() );

  // As per the resolution of DR 409.
  VERIFY( (ofs1) );
  VERIFY( ofs1.rdstate() == std::ios_base::goodbit );

  ofs1.close();
}

int main()
{
  test01();
  return 0;
}



