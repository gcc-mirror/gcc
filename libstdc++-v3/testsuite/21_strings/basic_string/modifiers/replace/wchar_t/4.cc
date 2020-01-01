// 1999-06-10 bkoz

// Copyright (C) 1994-2020 Free Software Foundation, Inc.
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

// 21.3.5.6 basic_string::replace

#include <string>
#include <testsuite_hooks.h>

// Some more tests for 
// template<typename InputIter>
//   wstring& replace(iterator it1, iterator it2, InputIter j1, InputIter j2)
void
test04()
{
  std::wstring str01 = L"geogaddi";
  std::wstring str02;

  typedef std::wstring::iterator iterator;
  typedef std::wstring::const_iterator const_iterator;
  
  iterator it1 = str01.begin();
  iterator it2 = str01.end();
  str02.replace(str02.begin(), str02.end(), it1, it2);
  VERIFY(str02 == L"geogaddi");

  str02 = L"boards";
  const_iterator c_it1 = str01.begin();
  const_iterator c_it2 = str01.end();
  str02.replace(str02.begin(), str02.end(), c_it1, c_it2);
  VERIFY(str02 == L"geogaddi");

  str02 = L"boards";
  const wchar_t* c_ptr1 = str01.c_str();
  const wchar_t* c_ptr2 = str01.c_str() + 8;
  str02.replace(str02.begin(), str02.end(), c_ptr1, c_ptr2);
  VERIFY(str02 == L"geogaddi");

  str02 = L"boards";
  wchar_t* ptr1 = &*str01.begin();
  wchar_t* ptr2 = ptr1 + str01.length();
  str02.replace(str02.begin(), str02.end(), ptr1, ptr2);
  VERIFY(str02 == L"geogaddi");
}

int main()
{ 
  test04();
  return 0;
}
