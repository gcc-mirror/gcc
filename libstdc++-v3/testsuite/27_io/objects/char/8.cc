// 2003-04-28 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2003 Free Software Foundation
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

  // 27.3 - Standard iostream objects p 2

#include <iostream>
#include <testsuite_hooks.h>

void test02() 
{
  bool test __attribute__((unused)) = true;
  
  // 27.3 - Standard iostream objects p 2
  // The objects are not destroyed during program execution.
  void* p1 = &std::cout;
  void* p2 = &std::cin;
  void* p3 = &std::cerr;
  void* p4 = &std::clog;
  std::ios_base::sync_with_stdio(false); 
  void* p1s = &std::cout;
  void* p2s = &std::cin;
  void* p3s = &std::cerr;
  void* p4s = &std::clog;
  VERIFY( p1 == p1s );
  VERIFY( p2 == p2s );
  VERIFY( p3 == p3s );
  VERIFY( p4 == p4s );
}

int main(void)
{
  test02();
  return 0;
}
