// 2001-04-02  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2003, 2009 Free Software Foundation, Inc.
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

void test02()
{
  using namespace std;

  const char* ccarray1 = "san francisco roof garden inspectors";
  const char* ccarray2 = "san francisco sunny-day park inspectors";
  char carray[50];
  strcpy(carray, ccarray1);
  const void* cv = ccarray1;
  const void* cv1;
  const char* cc;
  char* c;

  cv1 = memchr(cv, '/', 3);
  cc = strchr(ccarray1, '/');
  cc = strrchr(ccarray1, 'c');
  cc = strpbrk(ccarray1, ccarray2);
  c = strstr(carray, carray);
}

int main()
{
  test02();
  return 0;
}
