// from tstring.cc, from jason merrill, et. al.

// Copyright (C) 2000-2023 Free Software Foundation, Inc.
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

#include <testsuite_string.h>
#include <testsuite_hooks.h>

// 21.3.6.6 basic_string::find_last_not_of
void test03()
{
  typedef __gnu_test::string::size_type csize_type;
  __gnu_test::string::size_type pos;
  csize_type npos = __gnu_test::string::npos;

  __gnu_test::string x;
  pos = x.find_last_not_of('X');
  VERIFY( pos == npos );
  pos = x.find_last_not_of("XYZ");
  VERIFY( pos == npos );

  __gnu_test::string y("a");
  pos = y.find_last_not_of('X');
  VERIFY( pos == 0 );
  pos = y.find_last_not_of('a');
  VERIFY( pos == npos );
  pos = y.find_last_not_of("XYZ");
  VERIFY( pos == 0 );
  pos = y.find_last_not_of("a");
  VERIFY( pos == npos );

  __gnu_test::string z("ab");
  pos = z.find_last_not_of('X');
  VERIFY( pos == 1 );
  pos = z.find_last_not_of("XYZ");
  VERIFY( pos == 1 );
  pos = z.find_last_not_of('b');
  VERIFY( pos == 0 );
  pos = z.find_last_not_of("Xb");
  VERIFY( pos == 0 );
  pos = z.find_last_not_of("Xa");
  VERIFY( pos == 1 );
}
int main()
{
  test03();
  return 0;
}
