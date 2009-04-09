// { dg-options "-std=gnu++0x" }
// { dg-require-string-conversions "" }

// 2008-06-15  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008, 2009 Free Software Foundation, Inc.
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

// 21.4 Numeric Conversions [string.conversions]

#include <string>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;
  
  long long ll1 = -2;
  string one(to_string(ll1));
  VERIFY( one == "-2" );

  long long ll2 = 10;
  string two(to_string(ll2));
  VERIFY( two == "10" );

  unsigned long long ull1 = 2;
  string three(to_string(ull1));
  VERIFY( three == "2" );

  unsigned long long ull2 = 3000;
  string four(to_string(ull2));
  VERIFY( four == "3000" );

  long double ld1 = 2.0L;
  string five(to_string(ld1));
  VERIFY( five == "2.000000" );

  long double ld2 = -4.0L;
  string six(to_string(ld2));
  VERIFY( six == "-4.000000" );
}

int main()
{
  test01();
  return 0;
}
