// Copyright (C) 2000, 2001, 2003 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 27.8.1.7 ifstream member functions
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

#include <istream>
#include <fstream>
#include <testsuite_hooks.h>

const char name_01[] = "ifstream_members-1.tst";

// http://gcc.gnu.org/ml/libstdc++/2000-06/msg00136.html
void test01()
{
  bool test __attribute__((unused)) = true;
  std::ifstream ifs1;
  ifs1.close();
  
  // false as expected:
  VERIFY( !ifs1.is_open() );
   // this is now true:
  VERIFY( !(ifs1) );
  
  ifs1.open(name_01);
  VERIFY( ifs1.is_open() );
  // fail bit still true
  VERIFY( !(ifs1) );
  VERIFY( ifs1.rdstate() == std::ios_base::failbit );

  ifs1.close();
}

int main()
{
  test01();
  return 0;
}



