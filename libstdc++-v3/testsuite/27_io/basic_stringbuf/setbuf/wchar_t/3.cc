// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2013 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

#include <sstream>
#include <testsuite_hooks.h>

void test02()
{
  using namespace std;

  bool test __attribute__((unused)) = true;
  wchar_t buf[512];
  const wchar_t* strlit = L"how to tell a story and other essays: mark twain";
  const size_t strlitsize = std::wcslen(strlit);
  wstring s(L"tmp");
  wstringbuf sbuf(s, ios_base::out);
  sbuf.pubsetbuf(buf, strlitsize);
  sbuf.sputn(strlit, strlitsize);
  VERIFY( std::wcsncmp(strlit, buf, strlitsize) == 0 );
}

int main() 
{
  test02();
  return 0;
}
