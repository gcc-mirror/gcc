// 2001-04-02  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

// 21.4: null-terminiated sequence utilities

#include <string>
#include <cstring>
#include <cwchar>

void test01()
{
  wchar_t c = L'a';
  const wchar_t cc = L'b';
  wchar_t* c1 = &c;
  const wchar_t* cc1 = &cc;
  const wchar_t* ccarray1 = L"san francisco roof garden inspectors";
  const wchar_t* ccarray2 = L"san francisco sunny-day park inspectors";
  wchar_t carray[50];
  std::wcscpy(carray, ccarray1);
  
  // const wchar_t* wcschr(const wchar_t* s, wchar_t c);
  // wchar_t* wcschr(wchar_t* s, wchar_t c);
  cc1 = std::wcschr(ccarray1, L'c');
  c1 = std::wcschr(carray, L'c');

  // const char* wcspbrk(const wchar_t* s1, const wchar_t* s2);
  // char* wcspbrk(wchar_t* s1, const wchar_t* s2);
  cc1 = std::wcspbrk(ccarray1, ccarray2);
  c1 = std::wcspbrk(carray, ccarray2);

  // const wchar_t* strrchr(const wchar_t* s, wchar_t c);
  // wchar_t* strrchr(wchar_t* s, wchar_t c);
  cc1 = std::wcsrchr(ccarray1, L'c');
  c1 = std::wcsrchr(carray, L'c');

  // const wchar_t* strstr(const wchar_t* s1, const wchar_t* s2);
  // wchar_t* strstr(wchar_t* s1, const wchar_t* s2);
  cc1 = std::wcsstr(ccarray1, ccarray2);
  c1 = std::wcsstr(carray, carray);

  // const wchar_t* wmemchr(const wchar_t* s, wchar_t c, size_t n);
  // wchar_t* wmemchr(      wchar_t* s, wchar_t c, size_t n);
  cc1 = std::wmemchr(ccarray1, L'a', 3);
  c1 = std::wmemchr(carray, L'a', 3);

  cc1 = cc1; // Suppress unused warnings.
  c1 = c1;
}

int main()
{
  test01();
  return 0;
}
