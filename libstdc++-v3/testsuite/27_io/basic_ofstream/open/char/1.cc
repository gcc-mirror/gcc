// Copyright (C) 2000, 2001, 2003, 2005 Free Software Foundation, Inc.
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

// 27.8.1.10 ofstream member functions
// @require@ %-*.tst
// @diff@ %-*.tst %-*.txt

#include <ostream>
#include <fstream>
#include <testsuite_hooks.h>

const char name_01[] = "ofstream_members-1.tst";
const char name_02[] = "ofstream_members-1.txt";

// http://gcc.gnu.org/ml/libstdc++/2000-06/msg00136.html
void test01()
{
  bool test __attribute__((unused)) = true;
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



