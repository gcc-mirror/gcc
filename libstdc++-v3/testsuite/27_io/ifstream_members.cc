// Copyright (C) 2000 Free Software Foundation, Inc.
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
#include <debug_assert.h>

const char name_01[] = "ifstream_members-1.tst";
const char name_02[] = "ifstream_members-1.txt";

// http://gcc.gnu.org/ml/libstdc++/2000-06/msg00136.html
bool test00()
{
  bool test = true;
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

#ifdef DEBUG_ASSERT
  assert(test);
#endif
  
  return test;
}

// http://gcc.gnu.org/ml/libstdc++/2000-07/msg00004.html
bool test01()
{
  bool test = true;
  const int more_than_max_open_files = 8200;
  
  for(int i = 0; ++i < more_than_max_open_files;)
    {
      std::ifstream ifs(name_01);
      VERIFY( static_cast<bool>(ifs) );
    }

#ifdef DEBUG_ASSERT
  assert(test);
#endif
 
  return test;
}


int main()
{
  test00();
  test01();
}
