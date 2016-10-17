// 2000-01-15  Anders Widell  <awl@hem.passagen.se>

// Copyright (C) 2000-2016 Free Software Foundation, Inc.
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

#include <string>
#include <set>
#include <bitset>
#include <testsuite_hooks.h>

void
test02()
{
  std::bitset<66>  b;
  b <<= 400;
  VERIFY( b.count() == 0 );
}

int
main() 
{
  test02();
  return 0;
}
