// { dg-add-options ieee }

// 1999-08-23 bkoz

// Copyright (C) 1999-2023 Free Software Foundation, Inc.
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

#ifdef __CHAR_UNSIGNED__
#define char_is_signed false
#else
#define char_is_signed true
#endif

void test_sign()
{
  VERIFY( std::numeric_limits<char>::is_signed == char_is_signed );
  VERIFY( std::numeric_limits<signed char>::is_signed == true );
  VERIFY( std::numeric_limits<unsigned char>::is_signed == false );
  VERIFY( std::numeric_limits<short>::is_signed == true );
  VERIFY( std::numeric_limits<unsigned short>::is_signed == false );
  VERIFY( std::numeric_limits<int>::is_signed == true );
  VERIFY( std::numeric_limits<unsigned>::is_signed == false );
  VERIFY( std::numeric_limits<long>::is_signed == true );
  VERIFY( std::numeric_limits<unsigned long>::is_signed == false );
  VERIFY( std::numeric_limits<float>::is_signed == true );
  VERIFY( std::numeric_limits<double>::is_signed == true );
  VERIFY( std::numeric_limits<long double>::is_signed == true );
}

int main()
{
  test_sign();

  return 0;
}
