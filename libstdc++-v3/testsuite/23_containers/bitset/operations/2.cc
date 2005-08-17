// 2000-01-15  Anders Widell  <awl@hem.passagen.se>

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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <string>
#include <set>
#include <bitset>
#include <testsuite_hooks.h>

bool
test02()
{
  bool test __attribute__((unused)) = true;

  std::bitset<66>  b;
  b <<= 400;
  VERIFY( b.count() == 0 );
  return test;
}

int
main() 
{
  test02();
  return 0;
}
