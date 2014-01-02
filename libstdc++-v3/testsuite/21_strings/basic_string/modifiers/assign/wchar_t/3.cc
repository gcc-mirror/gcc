// 2001-10-30 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2014 Free Software Foundation, Inc.
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

// 21.3.5 string modifiers

#include <string>
#include <testsuite_hooks.h>

// assign(const _CharT* __s, size_type __n)
// assign(const _CharT* __s)
void
test03()
{
  bool test __attribute__((unused)) = true;

  using namespace std;
 
  wstring one; 
  wstring two;
  const wchar_t* source = L"Selling England by the pound";

  one.assign(source);
  VERIFY( one == L"Selling England by the pound" );

  one.assign(source, 28);
  VERIFY( one == L"Selling England by the pound" );

  two.assign(source, 7);
  VERIFY( two == L"Selling" );
  
  one.assign(one.c_str() + 8, 20);
  VERIFY( one == L"England by the pound" );

  one.assign(one.c_str() + 8, 6);
  VERIFY( one == L"by the" );
}

int main()
{ 
  test03();
  return 0;
}
