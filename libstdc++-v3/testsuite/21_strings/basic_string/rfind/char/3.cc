// 2000-06-22 -=dbv=-  (shamelessy copied from bkoz' find.cc)

// Copyright (C) 2000, 2003 Free Software Foundation, Inc.
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

#include <string>
#include <testsuite_hooks.h>

// 21.3.6.6 basic_string::find_last_not_of
bool test03()
{
  bool test = true;

  // test find_last_not_of

#ifdef DEBUG_ASSERT
  assert(test);
#endif
  return test;
}
int main()
{
  test03();
  return 0;
}
