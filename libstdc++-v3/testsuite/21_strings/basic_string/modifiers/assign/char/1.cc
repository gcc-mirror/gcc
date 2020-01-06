// 2001-10-30 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2020 Free Software Foundation, Inc.
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

  const char* strlit = "../the long pier/Hanalei Bay/Kauai/Hawaii";
  string aux = strlit;
  string::size_type i = aux.rfind("/");
  if (i != string::npos)
    aux.assign(aux, i + 1, string::npos);
  VERIFY(aux == "Hawaii");

  aux = strlit;
  i = aux.rfind("r/");
  if (i != string::npos)
    aux.assign(aux, i + 1, string::npos);
  VERIFY(aux.c_str()[9] == 'B');
  VERIFY(aux == "/Hanalei Bay/Kauai/Hawaii");

  aux.assign(10, 0);
  VERIFY(aux.length() == 10);
}

int main()
{ 
  test01();
  return 0;
}
