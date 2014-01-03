// 1999-06-10 bkoz

// Copyright (C) 1994-2014 Free Software Foundation, Inc.
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
#include <algorithm> // for std::find
#include <testsuite_hooks.h>

bool test01(void)
{
  bool test __attribute__((unused)) = true;
  typedef std::wstring::size_type csize_type;
  typedef std::wstring::const_reference cref;
  typedef std::wstring::reference ref;

  const wchar_t str_lit01[] = L"ventura, california";
  const std::wstring str01(str_lit01);
  std::wstring str02(L"del mar, california");
  std::wstring str03(L" and ");
  std::wstring str05;

  // wstring& replace(size_type pos, size_type n, const wstring& string)
  // wstring& replace(size_type pos1, size_type n1, const wstring& string,
  //                 size_type pos2, size_type n2)
  // wstring& replace(size_type pos, size_type n1, const wchar_t* s, size_type n2)
  // wstring& replace(size_type pos, size_type n1, const wchar_t* s)
  // wstring& replace(size_type pos, size_type n1, size_type n2, wchar_t c)
  // wstring& replace(iterator it1, iterator it2, const wstring& str)
  // wstring& replace(iterator it1, iterator it2, const wchar_t* s, size_type n)
  // wstring& replace(iterator it1, iterator it2, const wchar_t* s)
  // wstring& replace(iterator it1, iterator it2, size_type n, char c)
  // template<typename InputIter>
  //   wstring& replace(iterator it1, iterator it2, InputIter j1, InputIter j2)

  // with mods, from tstring.cc, from jason merrill, et. al.
  std::wstring X = L"Hello";
  std::wstring x = X;

  wchar_t ch = x[0];
  VERIFY( ch == L'H' );

  std::wstring z = x.substr(2, 3);
  VERIFY( z == L"llo" );

  x.replace(2, 2, L"r");
  VERIFY( x == L"Hero" );

  x = X;
  x.replace(0, 1, L"j");
  VERIFY( x == L"jello" );

  wchar_t ar[] = { L'H', L'e', L'l', L'l', L'o' };
  x.replace(std::find(x.begin(), x.end(), L'l'), 
	    std::find(x.rbegin(), x.rend(), L'l').base(), ar, 
	    ar + sizeof(ar) / sizeof(ar[0]));
  VERIFY( x == L"jeHelloo" );
  return test;
}

int main()
{ 
  test01();
  return 0;
}
