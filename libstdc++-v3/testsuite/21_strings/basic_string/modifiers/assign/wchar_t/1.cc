// 2001-10-30 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2017 Free Software Foundation, Inc.
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
#include <cstdio>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std;

  const wchar_t* strlit = L"../the long pier/Hanalei Bay/Kauai/Hawaii";
  wstring aux = strlit;
  wstring::size_type i = aux.rfind(L"/");
  if (i != wstring::npos)
    aux.assign(aux, i + 1, wstring::npos);
  VERIFY(aux == L"Hawaii");

  aux = strlit;
  i = aux.rfind(L"r/");
  if (i != wstring::npos)
    aux.assign(aux, i + 1, wstring::npos);
  VERIFY(aux.c_str()[9] == L'B');
  VERIFY(aux == L"/Hanalei Bay/Kauai/Hawaii");
}

int main()
{ 
  test01();
  return 0;
}
