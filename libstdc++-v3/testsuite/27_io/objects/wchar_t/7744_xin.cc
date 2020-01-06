// 2003-05-01  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2000-2020 Free Software Foundation, Inc.
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
 
#include <iostream>
#include <testsuite_hooks.h>

// libstdc++/7744
void test11()
{
  std::ios::sync_with_stdio(false);

  std::wcout << "Type in the characters 'abc' and press <ENTER>: ";
  std::wcin.peek();
 
  // The number of unread characters should be 4 (a, b, c, \\n)
  VERIFY( 4 == std::wcin.rdbuf()->in_avail() );
}

int 
main()
{
  test11();
  return 0;
}
