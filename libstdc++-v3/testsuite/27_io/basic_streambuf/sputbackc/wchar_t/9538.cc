// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

#include <streambuf>
#include <locale>
#include <cwchar>
#include <testsuite_hooks.h>

class MyTraits : public std::char_traits<wchar_t>
{
public:
  static bool eq(wchar_t c1, wchar_t c2)
  {
    VERIFY( c1 != L'X' );
    VERIFY( c2 != L'X' );
    return std::char_traits<wchar_t>::eq(c1, c2);
  }
};

class MyBuf : public std::basic_streambuf<wchar_t, MyTraits>
{
  wchar_t buffer[8];

public:
  MyBuf()
  {
    std::wmemset(buffer, L'X', sizeof(buffer) / sizeof(buffer[0]));
    std::wmemset(buffer + 2, L'f', 4);
    setg(buffer + 2, buffer + 2, buffer + 6);
  }
};

// libstdc++/9538
void test08()
{
  MyBuf mb;
  mb.sputbackc(L'a');
}

int main() 
{
  test08();
  return 0;
}
