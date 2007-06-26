// 2001-02-26 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007
// Free Software Foundation, Inc.
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

// 19.1 Exception classes

#include <string>
#include <ios>
#include <cstring>
#include <testsuite_hooks.h>

// test copy ctors, assignment operators, and persistence of member string data
// libstdc++/1972
// via Greg Bumgardner <bumgard@roguewave.com>
void allocate_on_stack(void) 
{
  const size_t num = 512;
  __extension__ char array[num];
  for (size_t i = 0; i < num; i++) 
    array[i]=0;
}

void test04()
{
  bool test __attribute__((unused)) = true;
  const std::string s("CA ISO emergency once again:immediate power down");
  const char* strlit1 = "wish I lived in Palo Alto";
  const char* strlit2 = "...or Santa Barbara";
  std::ios_base::failure obj1(s);
  
  // block 01
  {
    const std::string s2(strlit1);
    std::ios_base::failure obj2(s2);
    obj1 = obj2;
  }
  allocate_on_stack();
  VERIFY( std::strcmp(strlit1, obj1.what()) == 0 ); 

  // block 02
  {
    const std::string s3(strlit2);
    std::ios_base::failure obj3 = std::ios_base::failure(s3);
    obj1 = obj3;
  }
  allocate_on_stack();     
  VERIFY( std::strcmp(strlit2, obj1.what()) == 0 ); 
}

int main(void)
{
  test04();
  return 0;
}
