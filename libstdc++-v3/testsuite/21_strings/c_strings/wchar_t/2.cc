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

void test02()
{
  using namespace std;

  const wchar_t* ccarray1 = L"san francisco roof garden inspectors";
  const wchar_t* ccarray2 = L"san francisco sunny-day park inspectors";
  wchar_t carray[50];
  wcscpy(carray, ccarray1);
  const wchar_t* cw;
  wchar_t* w;

  cw = wmemchr(ccarray1, L'/', 3);
  cw = wcschr(ccarray1, L'/');
  cw = wcspbrk(ccarray1, ccarray2);
  cw = wcsrchr(ccarray1, L'c');
  w = wcsstr(carray, carray);

  cw = cw; // Suppress unused warnings.
  w = w;
}

int main()
{
  test02();
  return 0;
}
