// 2007-08-02  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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

#include <functional>
#include <testsuite_hooks.cc>

// DR 660. Missing Bitwise Operations.
void test01()
{
  for (int i1 = 0; i1 < 1000; ++i1)
    for (int i2 = 0; i2 < 1000; ++i2)
      {
	VERIFY( std::bit_and<int>()(i1, i2) == (i1 & i2) );
	VERIFY( std::bit_or<int>()(i1, i2) == (i1 | i2) );
	VERIFY( std::bit_xor<int>()(i1, i2) == (i1 ^ i2) );
      }
}

int main()
{
  test01();
  return 0;
}
