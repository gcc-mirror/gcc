// 2001-04-02  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

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
  wchar_t* w;

  w = wmemchr(ccarray1, L'/', 3);
  w = wcschr(ccarray1, L'/');
  w = wcspbrk(ccarray1, ccarray2);
  w = wcsrchr(ccarray1, L'c');
  w = wcsstr(carray, carray);
}

int main()
{
  test02();
  return 0;
}
