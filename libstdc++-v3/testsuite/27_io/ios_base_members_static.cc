// 1999-05-21 bkoz
// 2000-05-21 bkoz

// Copyright (C) 1999, 2000 Free Software Foundation
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

// 27.4.2.4 ios_base static members

#include <sstream>
#include <iostream>

#ifdef DEBUG_ASSERT
#include <assert.h>
#endif

bool test01() 
{
  bool test = true;
  
  std::stringbuf 	strbuf01;
  std::ios		ios01(&strbuf01);

  // 1: basic invocation
  test &= ios01.sync_with_stdio();
  test &= ios01.sync_with_stdio(false); //returns previous state

  // 2: need to test interleaving of C and C++ io on a file object.
  test &= std::cout.good();
  test &= !std::cout.sync_with_stdio(0);
  test &= std::cout.good();
  test &= !std::cout.sync_with_stdio(0);
  test &= std::cout.good();

#ifdef DEBUG_ASSERT
  assert(test);
#endif

  return test;
}


int main(void)
{
  test01();

  return 0;
}




