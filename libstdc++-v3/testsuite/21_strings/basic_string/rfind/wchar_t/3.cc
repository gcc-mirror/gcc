// from tstring.cc, from jason merrill, et. al.

// Copyright (C) 2000-2013 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

// 21.3.6.6 basic_string::find_last_not_of
bool test03()
{
  bool test __attribute__((unused)) = true;
  typedef std::wstring::size_type csize_type;
  std::wstring::size_type pos;
  csize_type npos = std::wstring::npos;

  std::wstring x;
  pos = x.find_last_not_of(L'X');
  VERIFY( pos == npos );
  pos = x.find_last_not_of(L"XYZ");
  VERIFY( pos == npos );

  std::wstring y(L"a");
  pos = y.find_last_not_of(L'X');
  VERIFY( pos == 0 );
  pos = y.find_last_not_of(L'a');
  VERIFY( pos == npos );
  pos = y.find_last_not_of(L"XYZ");
  VERIFY( pos == 0 );
  pos = y.find_last_not_of(L"a");
  VERIFY( pos == npos );

  std::wstring z(L"ab");
  pos = z.find_last_not_of(L'X');
  VERIFY( pos == 1 );
  pos = z.find_last_not_of(L"XYZ");
  VERIFY( pos == 1 );
  pos = z.find_last_not_of(L'b');
  VERIFY( pos == 0 );
  pos = z.find_last_not_of(L"Xb");
  VERIFY( pos == 0 );
  pos = z.find_last_not_of(L"Xa");
  VERIFY( pos == 1 );
  return test;
}
int main()
{
  test03();
  return 0;
}
