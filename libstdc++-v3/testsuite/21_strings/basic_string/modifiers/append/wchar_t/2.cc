// 2004-25-10  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2024 Free Software Foundation, Inc.
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

// append(const _CharT* __s, size_type __n)
// append(const _CharT* __s)
void
test02()
{
  using namespace std;
 
  wstring one; 
  wstring two;
  wstring three;
  const wchar_t * source = L"Written in your eyes";

  one.append(source);
  VERIFY( one == L"Written in your eyes" );

  two.append(source, 20);
  VERIFY( two == L"Written in your eyes" );

  three.append(source, 7);
  VERIFY( three == L"Written" );
  
  three.clear();
  three.append(source + 8, 2);
  VERIFY( three == L"in" );

  one.append(one.c_str(), 20);
  VERIFY( one == L"Written in your eyesWritten in your eyes" );

  two.append(two.c_str() + 16, 4);
  VERIFY( two == L"Written in your eyeseyes" );

  two.append(two.c_str(), 3);
  VERIFY( two == L"Written in your eyeseyesWri" );
}

int main()
{ 
  test02();
  return 0;
}
