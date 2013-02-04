// { dg-options "-std=gnu++0x" }
//
// Copyright (C) 2011-2013 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <array>
#include <testsuite_hooks.h>

void
test01() 
{
  bool test __attribute__((unused)) = true;

  const size_t len = 5;
  typedef std::array<int, len> array_type;
  array_type a = { { 0, 1, 2, 3, 4 } };

  // &a[n] == &a[0] + n for all 0 <= n < N.
  for (size_t i = 0; i < len; ++i)
    {
      VERIFY( &a[i] == &a[0] + i );
    }
}

int main()
{
  test01();
  return 0;
}
