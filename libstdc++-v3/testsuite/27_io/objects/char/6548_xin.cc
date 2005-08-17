// 2000-08-02 bkoz

// Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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

#include <iostream>
#include <testsuite_hooks.h>

// libstdc++/6548
void test07()
{
  bool test __attribute__((unused)) = true;
  std::cout << "Enter 'test':";
  std::string s;
  std::getline(std::cin, s, '\n');
  VERIFY( s == "test" );
}

int 
main()
{
  test07();
  return 0;
}
