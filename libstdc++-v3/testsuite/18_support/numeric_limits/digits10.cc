// { dg-add-options ieee }

// 1999-08-23 bkoz

// Copyright (C) 1999-2025 Free Software Foundation, Inc.
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

// 18.2.1.1 template class numeric_limits

#include <limits>
#include <limits.h>
#include <float.h>
#include <cwchar>
#include <testsuite_hooks.h>

// libstdc++/5045
void test03()
{
  VERIFY( std::numeric_limits<bool>::digits10 == 0 );
  if (__CHAR_BIT__ == 8)
    {
      VERIFY( std::numeric_limits<signed char>::digits10 == 2 );
      VERIFY( std::numeric_limits<unsigned char>::digits10 == 2 );
    }
  if (__CHAR_BIT__ * sizeof(short) == 16)
    {
      VERIFY( std::numeric_limits<signed short>::digits10 == 4 );
      VERIFY( std::numeric_limits<unsigned short>::digits10 == 4 );
    }
  if (__CHAR_BIT__ * sizeof(int) == 32)
    {
      VERIFY( std::numeric_limits<signed int>::digits10 == 9 );
      VERIFY( std::numeric_limits<unsigned int>::digits10 == 9 );
    }
  if (__CHAR_BIT__ * sizeof(long long) == 64)
    {
      VERIFY( std::numeric_limits<signed long long>::digits10 == 18 );
      VERIFY( std::numeric_limits<unsigned long long>::digits10 == 19 );
    }
}

int main()
{
  test03();

  return 0;
}
