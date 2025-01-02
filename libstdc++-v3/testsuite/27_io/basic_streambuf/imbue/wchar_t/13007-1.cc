// { dg-require-namedlocale "is_IS.UTF-8" }

// Copyright (C) 2003-2025 Free Software Foundation, Inc.
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

// 27.5.2.4.1 Locales

#include <streambuf>
#include <locale>
#include <testsuite_hooks.h>

class Buf1 : public std::wstreambuf
{
protected:
  void imbue(const std::locale&)
  { }
};

// libstdc++/13007
void test01()
{
  Buf1 buf;
  std::locale loc(std::locale("is_IS.UTF-8"));

  buf.pubimbue(loc);

  VERIFY( buf.getloc() == loc );
}

int main()
{
  test01();
  return 0;
}
